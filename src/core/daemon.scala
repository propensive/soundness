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

import scala.concurrent.*
import scala.util.*

import java.util.concurrent.atomic.AtomicInteger

import encodings.Utf8

import unsafeExceptions.canThrowAny
import rendering.ansi


case class CommandLine(args: List[Text], env: Map[Text, Text], script: File,
                           stdin: () => DataStream, stdout: DataStream => Unit, exit: Int => Unit,
                           pwd: Directory, shutdown: () => Unit)
extends Stdout:
  def write(msg: Text): Unit = stdout(LazyList(msg.bytes))

trait App:
  def main(using CommandLine): ExitStatus

trait Daemon() extends App:
  daemon =>

  private val spawnCount: AtomicInteger = AtomicInteger(0)

  final def main(args: IArray[Text]): Unit =
    args.to(List) match
      case _ =>
        val script = Sys.exoskeleton.script()
        val fifo = Sys.exoskeleton.fifo()
        val Int(pid) = Sys.exoskeleton.pid()
        val Int(watch) = Sys.exoskeleton.watch()
        val runnable: Runnable = () => server(script, fifo, pid, watch)
        Thread(runnable, "exoskeleton-dispatcher").start()
      
      case args =>
        val env = System.getenv.nn.asScala.map(_.nn.show -> _.nn.show).to(Map)
        val systemStdin = Option(System.in).getOrElse(sys.exit(10)).nn
        def stdin(): DataStream = Util.readInputStream(systemStdin, 1.mb)
        
        def stdout(ds: DataStream): Unit = ds.map(_.unsafeMutable).foreach:
          bytes => System.out.nn.write(bytes, 0, bytes.length)

        val dir =
          try Unix.parse(Sys.user.dir()).getOrElse(sys.exit(10)).directory(Expect)
          catch case err: KeyNotFoundError => sys.exit(10)
        
        val scriptFile = Unix.parse(List.getClass.nn.getProtectionDomain.nn.getCodeSource.nn
            .getLocation.nn.getPath.nn.show).get.file(Expect)
        
        val commandLine = CommandLine(args, env, scriptFile, () => stdin(), stdout, sys.exit(_), dir, () => sys.exit(0))
        
        main(using commandLine)()

  def server(script: Text, fifo: Text, serverPid: Int, watchPid: Int): Unit =
    val socket: DiskPath = Unix.parse(fifo).getOrElse(sys.exit(10))
    
    val death: Runnable = () =>
      try socket.file().delete() catch case e: Exception => ()
    
    ProcessHandle.of(watchPid).nn.get.nn.onExit.nn.thenRun:
      () => sys.exit(2)
   
    Runtime.getRuntime.nn.addShutdownHook:
      Thread(death, "exoskeleton-cleanup")

    case class AppInstance(pid: Int, spawnId: Int, scriptFile: Maybe[File] = Unset,
                               args: List[Text] = Nil, env: Map[Text, Text] = Map(),
                               runDir: Maybe[Directory] = Unset):
      val shutdown: Promise[Unit] = Promise()
      val termination: Gun = Gun()
      def pwd: Directory throws PwdError =
        try Unix.parse(env.getOrElse(t"PWD", throw PwdError())).getOrElse(throw PwdError())
            .directory(Expect)
        catch case err: IoError => throw PwdError()

      def stop(): Unit =
        if !shutdown.isCompleted then termination.trigger() else shutdown.complete(Success(()))
      
      def kill(): Unit = shutdown.complete(Success(()))
      
      def spawn(): Unit =
        val runnable: Runnable = () =>
          lazy val out = Fifo((runDir.otherwise(sys.exit(10)) / t"$script-$pid.stdout.sock").file(Expect))
          try
            val script = scriptFile.otherwise(sys.exit(10)).name
            val fifoIn = (runDir.otherwise(sys.exit(10)) / t"$script-$pid.stdin.sock").file(Expect)
            val terminate = Promise[Int]()
            val workDir = pwd.otherwise(sys.exit(10))
            
            val commandLine = CommandLine(args, env, scriptFile.otherwise(sys.exit(10)),
                () => fifoIn.read[DataStream](1.mb), _.writeTo(out),
                exit => terminate.complete(util.Success(exit)), workDir, () => sys.exit(0))
            
            val exit = main(using commandLine)
            out.close()
            val exitFile = (runDir.otherwise(sys.exit(10)) / t"$script-$pid.exit").file()
            exit().show.bytes.writeTo(exitFile)
          catch case NonFatal(err) =>
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
          case t"PROCESS" :: Int(pid) :: _ =>
            map.updated(pid, AppInstance(pid, spawnCount.getAndIncrement()))
          
          case t"RUNDIR" :: Int(pid) :: dir :: _ =>
            Unix.parse(dir).fold(map):
              dir => map.updated(pid, map(pid).copy(runDir = dir.directory(Expect)))
          
          case t"SCRIPT" :: Int(pid) :: scriptDir :: script :: _ =>
            Unix.parse(t"$scriptDir/$script").map(_.file(Expect)).fold(map):
              file => map.updated(pid, map(pid).copy(scriptFile = file))
          
          case t"ARGS" :: Int(pid) :: Int(count) :: args =>
            map.updated(pid, map(pid).copy(args = args.take(count)))
          
          case t"ENV" :: Int(pid) :: env =>
            map.updated(pid, map(pid).copy(env = parseEnv(env)))
          
          case t"START" :: Int(pid) :: _ =>
            map(pid).spawn()
            map
          
          case t"STOP" :: Int(pid) :: _ =>
            map(pid).stop()
            map
          
          case t"SHUTDOWN" :: _ =>
            sys.exit(0)
            map
          
          case msg =>
            map

class Gun():
  val stream = LazyList.continually(synchronized(wait()))
  def trigger(): Unit = try synchronized(notify()) catch case _: IllegalMonitorStateException => ()
