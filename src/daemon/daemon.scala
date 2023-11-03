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

object CliInput:
  given decoder: Decoder[CliInput] = text => valueOf(text.lower.capitalize.s)
  given encoder: Encoder[CliInput] = _.toString.tt.lower

enum CliInput:
  case Terminal, Pipe

case class ClientConnection
    (pid: Pid, async: Async[Unit], signals: Funnel[Signal], terminate: Promise[Unit], close: () => Unit,
        exitPromise: Promise[ExitStatus])

class LazyEnvironment(vars: List[Text]) extends Environment:
  private lazy val map: Map[Text, Text] =
    vars.map(_.cut(t"=", 2)).collect:
      case List(key, value) => (key, value)
    .to(Map)
  
  def variable(key: Text): Maybe[Text] = map.get(key).getOrElse(Unset)

def daemon
    (using executive: Executive)
    (block: DaemonClient ?=> executive.CliType ?=> executive.Return)
    : Unit =
  
  import environments.jvm
  import errorHandlers.throwUnsafely
  
  val xdg = Xdg()
  val furyDir: Directory = (xdg.runtimeDir.or(xdg.stateHome) / p"fury").as[Directory]
  val portFile: Path = furyDir / p"port"
  val waitFile: Path = furyDir / p"wait"
  val clients: scm.HashMap[Pid, ClientConnection] = scm.HashMap()
  var continue: Boolean = true
  
  lazy val termination: Unit =
    portFile.wipe()
    System.exit(0)
  
  def shutdown()(using Stdio): Unit =
    Out.println(t"Shutdown daemon")
    termination

  def client
      (socket: jn.Socket)
      (using Monitor, Stdio, Raises[StreamCutError], Raises[UndecodableCharError], Raises[NumberError])
      : Unit =

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

    val message: Maybe[DaemonEvent] = line() match
      case t"s" =>
        val pid: Pid = line().decodeAs[Pid]
        val signal: Signal = line().decodeAs[Signal]
        DaemonEvent.Trap(pid, signal)
      
      case t"x" =>
        DaemonEvent.Exit(line().decodeAs[Pid])
      
      case t"i" =>
        val cliInput: CliInput = if line() == t"p" then CliInput.Pipe else CliInput.Terminal
        val pid: Pid = Pid(line().decodeAs[Int])
        val script: Text = line()
        val pwd: Text = line()
        val argCount: Int = line().decodeAs[Int]
        val textArguments: List[Text] = chunk().cut(t"\u0000").take(argCount)
        val environment: List[Text] = chunk().cut(t"\u0000").init

        DaemonEvent.Init(pid, pwd, script, cliInput, textArguments, environment)
      
      case _ =>
        Unset
    
    message match
      case Unset =>
        Out.println(t"Received unrecognized message")
        socket.close()

      case DaemonEvent.Trap(process, signal) =>
        Out.println(t"Received signal $signal")
        clients(process).signals.put(signal)
        socket.close()
      
      case DaemonEvent.Exit(process) =>
        Out.println(t"Received exit status request from $process")
        val exitStatus: ExitStatus = clients(process).exitPromise.await()
        
        socket.getOutputStream.nn.write(exitStatus().show.bytes.mutable(using Unsafe))
        socket.close()
        clients -= process

      case DaemonEvent.Init(process, directory, scriptName, shellInput, textArguments, env) =>
        Out.println(t"Received init $process")
        val promise: Promise[Unit] = Promise()
        val exitPromise: Promise[ExitStatus] = Promise()
        val signalFunnel: Funnel[Signal] = Funnel()

        val stdio: Stdio =
          Stdio(ji.PrintStream(socket.getOutputStream.nn), ji.PrintStream(socket.getOutputStream.nn), in)
        
        val client = DaemonClient(() => shutdown(), shellInput, scriptName.decodeAs[Unix.Path])
        val environment = LazyEnvironment(env)
        val workingDirectory = WorkingDirectory(directory)
        
        val async: Async[Unit] = Async:
          try
            val cli: executive.CliType = executive.cli(textArguments, environment, workingDirectory, stdio,
                signalFunnel.stream)
            
            val exitStatus = executive.process(cli, block(using client)(using cli))
            exitPromise.fulfill(exitStatus)
          catch case error: Exception => exitPromise.fulfill(ExitStatus.Fail(1))
          finally
            socket.close()
            Out.println(t"Closed connection to ${process.value}")
        
        clients(process) = ClientConnection(process, async, signalFunnel, promise, () => socket.close(), exitPromise)

  application(using executives.direct)(Nil):
    import stdioSources.jvm
    import workingDirectories.default

    Async.onShutdown:
      waitFile.wipe()
      portFile.wipe()
      
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
  
case class DaemonClient(shutdown: () => Unit, cliInput: CliInput, script: Unix.Path)

enum DaemonEvent:
  case Init(process: Pid, work: Text, script: Text, cliInput: CliInput, arguments: List[Text], environment: List[Text])
  case Trap(process: Pid, signal: Signal)
  case Exit(process: Pid)

def shell(using session: DaemonClient): DaemonClient = session
