/*
    Exoskeleton, version 2.1.0. Copyright 2017-22 Jon Pretty, Propensive OÜ.

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

import scala.concurrent.*
import scala.util.*

import encodings.Utf8

import unsafeExceptions.canThrowAny

case class CommandLine(args: List[Text], env: Map[Text, Text], script: Text,
                           stdin: () => DataStream, stdout: DataStream => Unit, exit: Int => Unit,
                           pwd: Unix.Directory)
extends Drain:
  def write(msg: Text): Unit = stdout(LazyList(msg.bytes))

trait App:
  def main(using CommandLine): ExitStatus

trait ServerApp() extends App:
  final def main(args: IArray[Text]): Unit =
    args.to(List) match
      case t"::start::" :: script :: fifo :: Int(pid) :: Nil =>
        val runnable: Runnable = () => server(script, fifo, pid)
        Thread(runnable, "exoskeleton-dispatcher").start()
      
      case args =>
        val env = System.getenv.nn.asScala.map(_.nn.show -> _.nn.show).to(Map)
        val systemStdin = Option(System.in).getOrElse(sys.exit(10)).nn
        def stdin(): DataStream = Util.readInputStream(systemStdin, 1.mb)
        def stdout(ds: DataStream): Unit = ds.map(_.unsafeMutable).foreach(System.out.nn.writeBytes(_))
        val dir =
          try Unix.parse(Sys.user.dir()).getOrElse(sys.exit(10)).directory(Expect)
          catch case err: KeyNotFoundError => sys.exit(10)
        
        val commandLine = CommandLine(args, env, t"java", () => stdin(), stdout, sys.exit(_), dir)
        
        main(using commandLine)()

  def server(script: Text, fifo: Text, serverPid: Int): Unit =
    val socket: Unix.IoPath = Unix.parse(fifo).getOrElse(sys.exit(10))
   
    Runtime.getRuntime.nn.addShutdownHook:
      val runnable: Runnable = () => try socket.file.delete() catch case e: Exception => ()
      Thread(runnable, "exoskeleton-cleanup")

    case class AppInstance(pid: Int, script: Text = t"", args: List[Text] = Nil,
                               env: Map[Text, Text] = Map(), runDir: Maybe[Unix.Directory] = Unset,
                               scriptDir: Maybe[Unix.Directory] = Unset):
      
      val terminate: Promise[Unit] = Promise()
      def pwd: Unix.Directory throws PwdError =
        try Unix.parse(env.getOrElse(t"PWD", throw PwdError())).getOrElse(throw PwdError()).directory(Expect)
        catch case err: IoError => throw PwdError()

      def stop(): Unit = terminate.complete(Success(()))
      
      def spawn(map: Map[Int, AppInstance]): Map[Int, AppInstance] =
        val runnable: Runnable = () =>
          val fifoIn = (runDir.otherwise(sys.exit(10)) / t"$script-$pid.stdin.sock").file
          val fifoOut = Unix.Fifo((runDir.otherwise(sys.exit(10)) / t"$script-$pid.stdout.sock").file)
          val terminate = Promise[Int]()
          val pwd2 = pwd.otherwise(sys.exit(10))
          val commandLine = CommandLine(args, env, script, () => fifoIn.read[DataStream](1.mb),
              _.writeTo(fifoOut), exit => terminate.complete(util.Success(exit)), pwd2)
          
          val exit = main(using commandLine)
          val exitFile = (runDir.otherwise(sys.exit(10)) / t"$script-$pid.exit").file
          exit().show.bytes.writeTo(exitFile)
        
        val thread = Thread(runnable, "exoskeleton-spawner")
        thread.start()
        map
    
    def parseEnv(env: List[Text]): Map[Text, Text] = env.flatMap:
      pair =>
        pair.cut(t"=", 2).to(List) match
          case List(key, value) => List(key -> value)
          case _                => Nil
    .to(Map)
    
    socket.file.read[LazyList[Line]](256.kb).foldLeft(Map[Int, AppInstance]()):
      case (map, line) =>
        line.text.cut(t"\t").to(List) match
          case t"PROCESS" :: Int(pid) :: _ =>
            map.updated(pid, AppInstance(pid))
          
          case t"RUNDIR" :: Int(pid) :: dir :: _ =>
            Unix.parse(dir).fold(map):
              dir => map.updated(pid, map(pid).copy(runDir = dir.directory(Expect)))
          
          case t"SCRIPT" :: Int(pid) :: script :: _ =>
            map.updated(pid, map(pid).copy(script = script))
          
          case t"SCRIPTDIR" :: Int(pid) :: dir :: _ =>
            Unix.parse(dir).fold(map):
              dir =>
                try map.updated(pid, map(pid).copy(scriptDir = dir.directory(Expect)))
                catch case err: IoError => map
          
          case t"ARGS" :: Int(pid) :: Int(count) :: args =>
            map.updated(pid, map(pid).copy(args = args.take(count)))
          
          case t"ENV" :: Int(pid) :: env =>
            map.updated(pid, map(pid).copy(env = parseEnv(env)))
          
          case t"START" :: Int(pid) :: _ =>
            map(pid).spawn(map)
          
          case t"STOP" :: Int(pid) :: _ =>
            map(pid).stop()
            map
          
          case t"SHUTDOWN" :: _ =>
            sys.exit(0)
            map
          
          case msg =>
            System.out.nn.println(t"Unexpected message: ${msg.toString}")
            map
