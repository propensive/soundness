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

import joviality.*, filesystems.unix
import anticipation.integration.jovialityPath
import gossamer.*
import rudiments.*
import parasitism.*
import turbulence.*
import escapade.*
import profanity.*
import tetromino.*
import serpentine.*
import eucalyptus.*

import scala.util.*

import sun.misc as sm

import java.util.concurrent.atomic.AtomicInteger

import encodings.Utf8

import unsafeExceptions.canThrowAny
import rendering.ansi

inline def cli(using cli: CommandLine): CommandLine = cli

case class CommandLine(args: List[Text], env: Map[Text, Text], script: File[Unix],
                           stdin: DataStream, stdout: DataStream => Unit, exit: Int => Unit,
                           shutdown: () => Unit, interactive: () => Unit, resize: LazyList[Unit])
extends Stdout, InputSource:
  def write(msg: Text): Unit = stdout(LazyList(msg.bytes))
  def cleanup(tty: Tty): Unit = ()
  
  def init()(using Log, Allocator): Tty throws TtyError =
    interactive()
    Tty(System.out.nn, stdin)

trait App:
  def main(using CommandLine, Environment): ExitStatus

enum Signal:
  case Hup, Int, Quit, Ill, Trap, Abrt, Bus, Fpe, Kill, Usr1, Segv, Usr2, Pipe, Alrm, Term, Chld,
      Cont, Stop, Tstp, Ttin, Ttou, Urg, Xcpu, Xfsz, Vtalrm, Prof, Winch, Io, Pwr, Sys
  
  def shortName: Text = this.toString.show.upper
  def name: Text = t"SIG${this.toString.show.upper}"
  def id: Int = if ordinal < 15 then ordinal - 1 else ordinal

trait Daemon() extends App:
  private val spawnCount: Counter = Counter(0)
  import threading.platform
  val signalHandler: PartialFunction[Signal, Unit] = PartialFunction.empty

  final def main(args: IArray[Text]): Unit =
    Signal.values.foreach: signal =>
      if signalHandler.isDefinedAt(signal)
      then sm.Signal.handle(sm.Signal(signal.shortName.s), _ => signalHandler(signal))
    
    supervise(t"exoskeleton"):
      given Allocator = allocators.default
      
      args.to(List) match
        case _ =>
          val script = Sys.exoskeleton.script()
          val fifo = Sys.exoskeleton.fifo()
          val pid = Sys.exoskeleton.pid().toString.toInt
          val watch = Sys.exoskeleton.watch().toString.toInt
          
          Task(t"dispatcher")(server(script, fifo, pid.toString.toInt, watch))

  def server(script: Text, fifo: Text, serverPid: Int, watchPid: Int, rubrics: Rubric*)
            (using Allocator, Monitor)
            : Unit =
    val socket: DiskPath[Unix] = Unix.parse(fifo)
    socket.file().javaFile.deleteOnExit()
    
    val death: Runnable = () => try socket.file().delete() catch case e: Exception => ()
    
    ProcessHandle.of(watchPid).nn.get.nn.onExit.nn.thenRun:
      () => sys.exit(2)
   
    case class AppInstance(pid: Int, spawnId: Int, scriptFile: Maybe[File[Unix]] = Unset,
                               args: List[Text] = Nil, instanceEnv: Map[Text, Text] = Map(),
                               runDir: Maybe[Directory[Unix]] = Unset):
      given Environment(instanceEnv.get(_), key => Option(System.getProperty(key.s)).map(_.nn.show))
      val signals: Funnel[Unit] = Funnel()
      def pwd: Directory[Unix] throws EnvError = env.pwd[DiskPath[Unix]].directory(Expect)


      def resize(): Unit = signals.put(())

      def spawn(): Task[Unit] = Task(t"spawn"):
        lazy val out = Fifo[Unix]:
          val file = (runDir.otherwise(sys.exit(1)) / t"$script-$pid.stdout.sock").file(Expect)
          file.javaFile.deleteOnExit()
          file.path
        
        try
          val script = scriptFile.otherwise(sys.exit(1)).name
          val fifoIn = (runDir.otherwise(sys.exit(1)) / t"$script-$pid.stdin.sock").file(Expect)
          fifoIn.javaFile.deleteOnExit()
          val terminate = Promise[Int]()
          
          lazy val exitFile = (runDir.otherwise(sys.exit(1)) / t"$script-$pid.exit").file()
          exitFile.javaFile.deleteOnExit()
          
          def interactive(): Unit = 99.show.bytes.writeTo(exitFile)

          def term(): Unit =
            0.show.bytes.writeTo(exitFile)
            sys.exit(0)
            

          val commandLine = CommandLine(args, instanceEnv, scriptFile.otherwise(sys.exit(1)),
              LazyList() #::: fifoIn.read[DataStream](), _.writeTo(out),
              exit => terminate.supply(exit), term,
              () => interactive(), signals.stream)
          
          val exit = main(using commandLine, env)
          out.close()
          exit().show.bytes.writeTo(exitFile)
        
        catch
          case err: IoError =>
            ()
          case NonFatal(err) =>
            given Stdout = Stdout(out)
            Out.println(StackTrace(err).ansi)
    
    def parseEnv(env: List[Text]): Map[Text, Text] = env.flatMap:
      pair =>
        pair.cut(t"=", 2).to(List) match
          case List(key, value) => List(key -> value)
          case _                => Nil
    .to(Map)

    socket.file(Expect).read[LazyList[Line]](rubrics*).foldLeft(Map[Int, AppInstance]()):
      case (map, line) =>
        line.text.cut(t"\t").to(List) match
          case t"PROCESS" :: As[Int](pid) :: _ =>
            map.updated(pid.toString.toInt, AppInstance(pid, spawnCount()))
          
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
            map.updated(pid, map(pid).copy(instanceEnv = parseEnv(env)))
          
          case t"START" :: As[Int](pid) :: _ =>
            map(pid).spawn()
            map
          
          case t"SHUTDOWN" :: _ =>
            sys.exit(0)
            map
          
          case msg =>
            map
