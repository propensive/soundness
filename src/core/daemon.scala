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
import hieroglyph.*, charEncoders.utf8
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

case class ClientConnection(pid: Pid, async: Async[Unit], signals: Funnel[Signal], terminate: Promise[Unit], close: () => Unit, exit: Promise[ExitStatus])(using Monitor)

class LazyEnvironment(vars: List[Text]) extends Environment:
  private lazy val map: Map[Text, Text] =
    vars.map(_.cut(t"=", 2)).collect:
      case List(key, value) => (key, value)
    .to(Map)
  
  def apply(key: Text): Maybe[Text] = map.get(key).getOrElse(Unset)

def application(arguments: Iterable[Text])(block: Cli ?=> Execution): Unit =
  val arguments2 = arguments.to(List).zipWithIndex.map: (text, index) =>
    Argument(index, text, Unset)

  block(using CliInvocation(arguments2, environments.jvm, workingDirectories.default, ProcessContext(stdioSources.jvm)))

def daemon(block: Cli ?=> DaemonClient ?=> Execution): Unit =
  given invocation: CliInvocation = CliInvocation(Nil, environments.jvm, workingDirectories.default, ProcessContext(stdioSources.jvm))
  Daemon(cli => session => block(using cli)(using session)).invoke.execute(invocation)

class Daemon(block: Cli => DaemonClient => Execution) extends Application:
  daemon =>
  
  import environments.jvm
  import errorHandlers.throwUnsafely

  private val xdg = Xdg()
  private val furyDir: Directory = (xdg.runtimeDir.or(xdg.stateHome) / p"fury").as[Directory]
  private val portFile: Path = furyDir / p"port"
  private val waitFile: Path = furyDir / p"wait"
  private val clients: scm.HashMap[Pid, ClientConnection] = scm.HashMap()
  private var continue: Boolean = true

  lazy val termination: Unit =
    portFile.wipe()
    System.exit(0)
  
  def shutdown()(using Stdio): Unit =
    Out.println(t"Shutdown daemon")
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

    val message: Maybe[LauncherEvent] = line() match
      case t"s" =>
        val pid: Pid = line().decodeAs[Pid]
        val signal: Signal = line().decodeAs[Signal]
        LauncherEvent.Trap(pid, signal)
      
      case t"x" =>
        LauncherEvent.Exit(line().decodeAs[Pid])
      
      case t"i" =>
        val stdin: ShellInput = if line() == t"p" then ShellInput.Pipe else ShellInput.Terminal
        val pid: Pid = Pid(line().decodeAs[Int])
        val script: Text = line()
        val pwd: Text = line()
        val argCount: Int = line().decodeAs[Int]
        val textArguments: List[Text] = chunk().cut(t"\u0000").take(argCount)
        val env: List[Text] = chunk().cut(t"\u0000").init

        LauncherEvent.Init(pid, pwd, script, stdin, textArguments, env)
      
      case _ =>
        Unset
    
    message match
      case Unset =>
        Out.println(t"Received unrecognized message")
        socket.close()

      case LauncherEvent.Trap(process, signal) =>
        Out.println(t"Received signal $signal")
        clients(process).signals.put(signal)
        socket.close()
      
      case LauncherEvent.Exit(process) =>
        Out.println(t"Received exit status request from $process")
        val exitStatus: ExitStatus = clients(process).exit.await()
        
        socket.getOutputStream.nn.write(exitStatus().show.bytes.mutable(using Unsafe))
        socket.close()
        clients -= process

      case LauncherEvent.Init(process, directory, scriptName, shellInput, textArguments, env) =>
        Out.println(t"Received init $process")
        val promise: Promise[Unit] = Promise()
        val exit: Promise[ExitStatus] = Promise()
        val signalFunnel: Funnel[Signal] = Funnel()

        val clientStdio: Stdio =
          Stdio(ji.PrintStream(socket.getOutputStream.nn), ji.PrintStream(socket.getOutputStream.nn), in)
        
        val session = DaemonClient(clientStdio, () => daemon.shutdown(), signalFunnel.stream, shellInput,
            scriptName.decodeAs[Unix.Path], directory)

        val environment = LazyEnvironment(env)
        
        val async: Async[Unit] = Async:
          try
            makeCli(textArguments, environment, WorkingDirectory(directory), session) match
              case invocation: CliInvocation =>
                exit.fulfill(block(invocation)(session).execute(invocation))
              
              case completion: CliCompletion =>
                exit.fulfill:
                  block(completion)(session)
                  completion.serialize.foreach(Out.println(_)(using completion.context.stdio))
                  ExitStatus.Ok
              
          catch case error: Exception => exit.offer(ExitStatus.Fail(1))
          finally
            socket.close()
            Out.println(t"Closed connection to ${process.value}")
        
        clients(process) = ClientConnection(process, async, signalFunnel, promise, () => socket.close(), exit)

  def invoke(using cli: Cli): Execution = cli match
    case given CliInvocation =>
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

case class DaemonClient
    (stdio: Stdio, shutdown: () => Unit, signals: LazyList[Signal], shellInput: ShellInput, script: Unix.Path,
        workDir: Text)
    (using Monitor)
extends WorkingDirectory(workDir), ProcessContext

enum LauncherEvent:
  case Init(process: Pid, work: Text, script: Text, input: ShellInput, arguments: List[Text], environment: List[Text])
  case Trap(process: Pid, signal: Signal)
  case Exit(process: Pid)

def arguments(using cli: Cli): List[Argument] = cli.arguments

def parameters
    [ParametersType]
    (using interpreter: CliInterpreter[ParametersType], cli: Cli)
    : ParametersType =
  interpreter(arguments)

def shell(using session: DaemonClient): DaemonClient = session