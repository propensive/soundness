/*
    Exoskeleton, version 0.4.0. Copyright 2017-22 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package exoskeleton

import jovian.*
import gossamer.*
import rudiments.*
import turbulence.*
import escapade.*
import profanity.*
import slalom.*
import eucalyptus.*

import scala.concurrent.*
import scala.util.*

import sun.misc as sm

import java.util.concurrent.atomic.AtomicInteger

import encodings.Utf8

import unsafeExceptions.canThrowAny
import rendering.ansi

transparent inline def cli(using cli: CommandLine): CommandLine = cli

case class CommandLine(args: List[Text], env: Map[Text, Text], script: File,
                           stdin: DataStream, stdout: DataStream => Unit, exit: Int => Unit,
                           pwd: Directory, shutdown: () => Unit, interactive: () => Unit,
                           resize: LazyList[Unit])
extends Stdout, InputSource:
  def write(msg: Text): Unit = stdout(LazyList(msg.bytes))
  def cleanup(tty: Tty): Unit = ()
  
  def init()(using Log): Tty throws TtyError =
    interactive()
    Tty(System.out.nn, stdin)

trait App:
  def main(using CommandLine): ExitStatus

enum Signal:
  case Hup, Int, Quit, Ill, Trap, Abrt, Bus, Fpe, Kill, Usr1, Segv, Usr2, Pipe, Alrm, Term, Chld,
      Cont, Stop, Tstp, Ttin, Ttou, Urg, Xcpu, Xfsz, Vtalrm, Prof, Winch, Io, Pwr, Sys
  
  def shortName: Text = this.toString.show.upper
  def name: Text = t"SIG${this.toString.show.upper}"
  def id: Int = if ordinal < 15 then ordinal - 1 else ordinal

trait Daemon() extends App:
  daemon =>

  private val spawnCount: AtomicInteger = AtomicInteger(0)

  val signalHandler: PartialFunction[Signal, Unit] = PartialFunction.empty

  final def main(args: IArray[Text]): Unit =
    Signal.values.foreach:
      signal =>
        if signalHandler.isDefinedAt(signal)
        then sm.Signal.handle(sm.Signal(signal.shortName.s), _ => signalHandler(signal))

    args.to(List) match
      case _ =>
        val script = Sys.exoskeleton.script()
        val fifo = Sys.exoskeleton.fifo()
        val pid = Sys.exoskeleton.pid().toString.toInt
        val watch = Sys.exoskeleton.watch().toString.toInt
        val runnable: Runnable = () => server(script, fifo, pid.toString.toInt, watch)
        Thread(runnable, "exoskeleton-dispatcher").start()

  def server(script: Text, fifo: Text, serverPid: Int, watchPid: Int): Unit =
    val socket: Unix.DiskPath = Unix.parse(fifo)
    
    val death: Runnable = () => try socket.file().delete() catch case e: Exception => ()
    
    ProcessHandle.of(watchPid).nn.get.nn.onExit.nn.thenRun:
      () => sys.exit(2)
   
    Runtime.getRuntime.nn.addShutdownHook:
      Thread(death, "exoskeleton-cleanup")

    case class AppInstance(pid: Int, spawnId: Int, scriptFile: Maybe[File] = Unset,
                               args: List[Text] = Nil, env: Map[Text, Text] = Map(),
                               runDir: Maybe[Directory] = Unset):
      val signals: Funnel[Unit] = Funnel()
      def pwd: Directory throws PwdError =
        try Unix.parse(env.getOrElse(t"PWD", throw PwdError())).directory(Expect)
        catch
          case err: IoError          => throw PwdError()
          case err: InvalidPathError => throw PwdError()

      def resize(): Unit = signals.put(())

      def spawn(): Unit =
        val runnable: Runnable = () =>
          lazy val out = Fifo:
            val file = (runDir.otherwise(sys.exit(1)) / t"$script-$pid.stdout.sock").file(Expect)
            file.javaFile.deleteOnExit()
            file
          
          try
            val script = scriptFile.otherwise(sys.exit(1)).name
            val fifoIn = (runDir.otherwise(sys.exit(1)) / t"$script-$pid.stdin.sock").file(Expect)
            fifoIn.javaFile.deleteOnExit()
            val terminate = Promise[Int]()
            val workDir = pwd.otherwise(sys.exit(1))
            
            lazy val exitFile = (runDir.otherwise(sys.exit(1)) / t"$script-$pid.exit").file()
            exitFile.javaFile.deleteOnExit()
            
            def interactive(): Unit = 99.show.bytes.writeTo(exitFile)
            
            val commandLine = CommandLine(args, env, scriptFile.otherwise(sys.exit(1)),
                LazyList() #::: fifoIn.read[DataStream](1.mb), _.writeTo(out),
                exit => terminate.complete(util.Success(exit)), workDir, () => sys.exit(0),
                () => interactive(), signals.stream)
            
            val exit = main(using commandLine)
            out.close()
            exit().show.bytes.writeTo(exitFile)
          
          catch
            case err: IoError =>
              ()
            case NonFatal(err) =>
              given Stdout = Stdout(out)
              Out.println(StackTrace(err).ansi)
        
        val thread = Thread(runnable, t"exoskeleton-$spawnId".s)
        thread.start()
    
    def parseEnv(env: List[Text]): Map[Text, Text] = env.flatMap:
      pair =>
        pair.cut(t"=", 2).to(List) match
          case List(key, value) => List(key -> value)
          case _                => Nil
    .to(Map)

    socket.file(Expect).read[LazyList[Line]](256.kb).foldLeft(Map[Int, AppInstance]()):
      case (map, line) =>
        line.text.cut(t"\t").to(List) match
          case t"PROCESS" :: As[Int](pid) :: _ =>
            map.updated(pid.toString.toInt, AppInstance(pid, spawnCount.getAndIncrement()))
          
          case t"RUNDIR" :: As[Int](pid) :: dir :: _ =>
            safely(Unix.parse(dir)).option.fold(map): dir =>
              map.updated(pid, map(pid).copy(runDir = dir.directory(Expect)))
          
          case t"SCRIPT" :: As[Int](pid) :: scriptDir :: script :: _ =>
            safely(Unix.parse(t"$scriptDir/$script")).option.map(_.file(Expect)).fold(map): file =>
              map.updated(pid, map(pid).copy(scriptFile = file))
          
          case t"ARGS" :: As[Int](pid) :: As[Int](count) :: args =>
            map.updated(pid, map(pid).copy(args = args.take(count)))
          
          case t"RESIZE" :: As[Int](pid) :: _ =>
            map(pid).resize()
            map
          
          case t"ENV" :: As[Int](pid) :: env =>
            map.updated(pid, map(pid).copy(env = parseEnv(env)))
          
          case t"START" :: As[Int](pid) :: _ =>
            map(pid).spawn()
            map
          
          case t"SHUTDOWN" :: _ =>
            sys.exit(0)
            map
          
          case msg =>
            map
