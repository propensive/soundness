package exoskeleton

import jovian.*
import gossamer.*
import rudiments.*

import scala.collection.JavaConverters.*
import scala.concurrent.*
import scala.util.*

import encodings.Utf8

import unsafeExceptions.canThrowAny

case class CommandLine(args: List[Text], env: Map[Text, Text], script: Text,
                           stdin: () => DataStream, stdout: DataStream => Unit, exit: Int => Unit)

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
        val commandLine = CommandLine(args, env, t"java", () => stdin(), stdout, sys.exit(_))
        
        main(using commandLine)()

  def server(script: Text, fifo: Text, serverPid: Int): Unit =
    val socket: Unix.IoPath = Unix.parse(fifo).getOrElse(sys.exit(11))
   
    val shutdown: Runnable = () =>
      Thread.sleep(1000)
      sys.exit(2)

    //ProcessHandle.current.nn.parent.nn.get.nn.children.nn.toList.nn.asScala.map(_.nn).filter(_.pid != serverPid).foreach:
    //  handle => handle.onExit.nn.thenRun(shutdown)
    
    Runtime.getRuntime.nn.addShutdownHook:
      val runnable: Runnable = () => try socket.file.delete() catch case e: Exception => ()
      Thread(runnable, "exoskeleton-cleanup")

    case class AppInstance(pid: Int, script: Text = t"", args: List[Text] = Nil,
                               env: Map[Text, Text] = Map(), runDir: Maybe[Unix.Directory] = Unset):
      
      val terminate: Promise[Unit] = Promise()

      def stop(): Unit = terminate.complete(Success(()))
      
      def spawn(map: Map[Int, AppInstance]): Map[Int, AppInstance] =
        val runnable: Runnable = () =>
          val fifoIn = (runDir.otherwise(sys.exit(12)) / t"$script-$pid.stdin.sock").file
          val fifoOut = (runDir.otherwise(sys.exit(13)) / t"$script-$pid.stdout.sock").file
          val terminate = Promise[Int]()
          val commandLine = CommandLine(args, env, script, () => fifoIn.read[DataStream](1.mb),
              _.writeTo(fifoOut), exit => terminate.complete(util.Success(exit)))
          
          val exit = main(using commandLine)
          val exitFile = (runDir.otherwise(sys.exit(14)) / t"$script-$pid.exit").file
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
              dir => map.updated(pid, map(pid).copy(runDir = dir.directory))
          
          case t"SCRIPT" :: Int(pid) :: script :: _ =>
            map.updated(pid, map(pid).copy(script = script))
          
          case t"ARGS" :: Int(pid) :: args =>
            map.updated(pid, map(pid).copy(args = args))
          
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
            println("Unexpected message: "+msg)
            map

object HelloWorld extends ServerApp():
  def main(using cli: CommandLine): ExitStatus =
    cli.stdout(LazyList(t"Hello world ${cli.args.toString.show}\n".bytes))
    ExitStatus.Ok
