/*
    Exoskeleton, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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

import serpentine.*, hierarchies.unix

import galilei.*, filesystemOptions.{dereferenceSymlinks, createNonexistent, createNonexistentParents,
    deleteRecursively}

import anticipation.*, fileApi.galileiApi
import rudiments.*, homeDirectories.default
import perforate.*
import hieroglyph.*, charEncoders.utf8, charDecoders.utf8, badEncodingHandlers.strict
import parasite.*
import profanity.*
import gossamer.*
import turbulence.*
import guillotine.*
import surveillance.*
import eucalyptus.*, logging.silent
import ambience.*
import spectacular.*

import scala.collection.mutable as scm
import scala.compiletime.*

import java.net as jn
import java.io as ji

case class CliSession(pid: Pid, async: Async[Unit], signals: Funnel[Signal], terminate: Promise[Unit], close: () => Unit, exit: Promise[ExitStatus])(using Monitor)

class LazyEnvironment(vars: List[Text]) extends Environment:
  private lazy val map: Map[Text, Text] =
    vars.map: element =>
      element.cut(t"=", 2) match
        case List(key, value) => (key, value)
    .to(Map)
  
  def apply(key: Text): Maybe[Text] = map.get(key).getOrElse(Unset)

object Daemon:
  def listen(block: Environment ?=> ShellSession ?=> ExitStatus): Unit =
    given context: Invocation = Invocation(IArray(), environments.jvm, workingDirectories.default, LazyList(), System.out.nn, System.err.nn)
    Daemon(environment => session => block(using environment)(using session)).invoke.execute(context)

class Daemon(block: Environment => ShellSession => ExitStatus) extends Application:
  daemon =>
  
  import environments.jvm
  import errorHandlers.throwUnsafely

  private val xdg = Xdg()
  private val furyDir: Directory = (xdg.runtimeDir.or(xdg.stateHome) / p"fury").as[Directory]
  private val portFile: Path = furyDir / p"port"
  private val waitFile: Path = furyDir / p"wait"
  private val clients: scm.HashMap[Pid, CliSession] = scm.HashMap()
  private var continue: Boolean = true

  lazy val termination: Unit =
    portFile.wipe()
    System.exit(0)
  
  def shutdown()(using Stdio): Unit =
    Io.println(t"Shutdown daemon")
    termination

  def client(socket: jn.Socket)(using Monitor, Stdio, Raises[StreamCutError], Raises[UndecodableCharError], Raises[NumberError]): Unit =
    val in = socket.getInputStream.nn
    val reader = ji.BufferedReader(ji.InputStreamReader(in, "UTF-8"))

    def line(): Text = reader.readLine().nn.tt
    
    def chunk(): Text =
      var buffer: Text = line()
      var current: Text = t""
      
      while
        current = line()
        current != t"##"
      do buffer += t"\n$current"
      
      buffer

    val message = line() match
      case t"s" =>
        val pid: Pid = line().decodeAs[Pid]
        val signal: Signal = line().decodeAs[Signal]
        Interrupt(pid, signal)
      
      case t"x" =>
        GetExitCode(line().decodeAs[Pid])
      
      case t"i" =>
        val stdin: Stdin = if line() == t"p" then Stdin.Pipe else Stdin.Term
        val pid: Pid = Pid(line().decodeAs[Int])
        val script: Text = line()
        val pwd: Text = line()
        val argCount: Int = line().decodeAs[Int]
        val args: List[Text] = chunk().cut(t"\u0000").take(argCount)
        val env: List[Text] = chunk().cut(t"\u0000").init

        Initialize(pid, pwd, script, stdin, args, env)
    
    message match
      case Interrupt(process, signal) =>
        Io.println(t"Received signal $signal")
        clients(process).signals.put(signal)
        socket.close()
      
      case GetExitCode(process) =>
        Io.println(t"Received exit status request from $process")
        val exitStatus: ExitStatus = clients(process).exit.await()
        
        socket.getOutputStream.nn.write(exitStatus().show.bytes.mutable(using Unsafe))
        socket.close()
        clients -= process

      case Initialize(process, directory, scriptName, inputType, args, env) =>
        Io.println(t"Received init $process")
        val promise: Promise[Unit] = Promise()
        val exit: Promise[ExitStatus] = Promise()
        val signalFunnel: Funnel[Signal] = Funnel()
        
        val session = ShellSession(
          out = socket.getOutputStream.nn, 
          shutdown = () => daemon.shutdown(),
          arguments = IArray.from:
            args.zipWithIndex.map: (arg, index) =>
              Argument(index + 1, arg, Unset),
          signals = signalFunnel.stream,
          input = reader.stream[Char],
          stdin = inputType,
          script = scriptName.decodeAs[Unix.Path],
          workDir = directory
        )

        val async: Async[Unit] = Async:
          try exit.fulfill(block(LazyEnvironment(env))(session))
          catch case error: Exception => exit.fulfill(ExitStatus.Fail(1))
          finally
            socket.close()
            Io.println(t"Closed connection to ${process.value}")
        
        clients(process) = CliSession(process, async, signalFunnel, promise, () => socket.close(), exit)

  def invoke(using context: CliContext): Execution = context match
    case given Invocation =>
      Async.onShutdown:
        waitFile.wipe()
        portFile.wipe()
      
      execute:
        supervise:
          Async:
            sh"flock -u -x -n $portFile cat".exec[Unit]()
            shutdown()

          Async:
            safely(furyDir.watch()).mm: watcher =>
              watcher.stream.foreach:
                case Delete(_, t"port") => shutdown()
                case _ => ()
              
          val socket: jn.ServerSocket = jn.ServerSocket(0)
          val port: Int = socket.getLocalPort
          port.show.writeTo(portFile)
          waitFile.touch()
          waitFile.wipe()
          while continue do safely(client(socket.accept().nn))

        ExitStatus.Ok

case class Initialize(process: Pid, work: Text, script: Text, input: Stdin, arg: List[Text], env: List[Text])
case class Interrupt(process: Pid, signal: Signal)
case class GetExitCode(process: Pid)

@main
def fury(): Unit =
  import errorHandlers.throwUnsafely
  
  Daemon.listen:
    Io.println(t"Hello world 1")
    supervise:
      terminal:
        Io.println(t"Hello world 2")
        //Io.println(tty.mode.debug)
        Io.println(arguments.debug)
        Io.println(shell.script.debug)
        Io.println(shell.stdin.debug)
        Io.println(shell.directory.or(t"unknown"))
    
        // tty.events.takeWhile(_ != Keypress.Printable('Q')).foreach:
        //   //case Keypress.Printable(ch) => Io.print(ch)
        //   case other                  => println(other)

        //Io.println(tty.mode.debug)
        //println(t"Rows: ${tty.rows}")
        //println(t"Cols: ${tty.columns}")
        Io.println(Environment.foo[Text])
        Io.println(t"Done")
        ExitStatus.Fail(32)

case class ShellSession
    (out: ji.OutputStream, shutdown: () => Unit, arguments: IArray[Argument], signals: LazyList[Signal],
        input: LazyList[Char], stdin: Stdin, script: Unix.Path, workDir: Text)
    (using Monitor)
extends WorkingDirectory(workDir), ProcessContext:
  def putErrBytes(bytes: Bytes): Unit = putOutBytes(bytes)
  def putErrText(text: Text): Unit = putOutText(text)
  
  def putOutBytes(bytes: Bytes): Unit =
    out.write(bytes.mutable(using Unsafe))
    out.flush()
  
  def putOutText(text: Text): Unit =
    out.write(text.bytes.mutable(using Unsafe))
    out.flush()

def arguments(using session: ShellSession): IArray[Argument] = session.arguments
def shell(using session: ShellSession): ShellSession = session

case class Argument(position: Int, value: Text, cursor: Maybe[Int])