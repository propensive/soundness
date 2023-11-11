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
import digression.*
import fulminate.*
import gossamer.*
import turbulence.*
import guillotine.*
import surveillance.*
import eucalyptus.*
import ambience.*, systemProperties.jvm
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
    [BusType <: Matchable]
    (pid: Pid, async: Async[Unit], signals: Funnel[Signal], terminate: Promise[Unit], close: () => Unit,
        exitPromise: Promise[ExitStatus], bus: Funnel[BusType]):
  def receive(message: BusType): Unit = bus.put(message)

class LazyEnvironment(variables: List[Text]) extends Environment:
  private lazy val map: Map[Text, Text] =
    variables.map(_.cut(t"=", 2)).collect:
      case List(key, value) => (key, value)
    .to(Map)
  
  def variable(key: Text): Maybe[Text] = map.get(key).getOrElse(Unset)

def daemon[BusType <: Matchable]
    (using executive: Executive)
    (block: DaemonService[BusType] ?=> executive.CliType ?=> executive.Return)
    (using CliInterpreter)
    : Unit =

  import environments.jvm
  import errorHandlers.throwUnsafely

  val name: Text = Properties.exoskeleton.name[Text]()
  val script: Text = Properties.exoskeleton.script[Text]()
  val command: Text = Properties.exoskeleton.command[Text]()
  val fpath: List[Text] = Properties.exoskeleton.fpath[Text]().cut(t"\n")
  
  val xdg = Xdg()
  val baseDir: Directory = (xdg.runtimeDir.or(xdg.stateHome) / PathName(name)).as[Directory]
  val portFile: Path = baseDir / p"port"
  val waitFile: Path = baseDir / p"wait"
  val clients: scm.HashMap[Pid, ClientConnection[BusType]] = scm.HashMap()
  var continue: Boolean = true
  
  lazy val termination: Unit =
    portFile.wipe()
    System.exit(0)
  
  def shutdown()(using Stdio, Log): Unit =
    Log.info(t"Shutdown daemon")
    termination

  def client
      (socket: jn.Socket)
      (using Monitor, Log, Stdio, Raises[StreamCutError], Raises[UndecodableCharError], Raises[NumberError])
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
        Log.warn(t"Received unrecognized message")
        socket.close()

      case DaemonEvent.Trap(pid, signal) =>
        Log.fine(t"Received signal $signal")
        clients(pid).signals.put(signal)
        socket.close()
      
      case DaemonEvent.Exit(pid) =>
        Log.fine(t"Received exit status request from $pid")
        val exitStatus: ExitStatus = clients(pid).exitPromise.await()
        
        socket.getOutputStream.nn.write(exitStatus().show.bytes.mutable(using Unsafe))
        socket.close()
        clients -= pid

      case DaemonEvent.Init(pid, directory, scriptName, shellInput, textArguments, env) =>
        Log.envelop(pid):
          Log.fine(t"Received init")
          val promise: Promise[Unit] = Promise()
          val exitPromise: Promise[ExitStatus] = Promise()
          val signalFunnel: Funnel[Signal] = Funnel()
          val busFunnel: Funnel[BusType] = Funnel()
  
          val stdio: Stdio =
            Stdio(ji.PrintStream(socket.getOutputStream.nn), ji.PrintStream(socket.getOutputStream.nn), in)
          
          def deliver(sourcePid: Pid, message: BusType): Unit = clients.foreach: (pid, client) =>
            if sourcePid != pid then client.receive(message)
  
          val client: DaemonService[BusType] =
            DaemonService[BusType](pid, () => shutdown(), shellInput, scriptName.decodeAs[Unix.Path],
                deliver(pid, _), busFunnel.stream)
          
          val environment = LazyEnvironment(env)
          val workingDirectory = WorkingDirectory(directory)
          
          val async: Async[Unit] = Async:
            Log.pin()
            Log.info(t"Creating new CLI")
            try
              val cli: executive.CliType =
                executive.cli(textArguments, environment, workingDirectory, stdio, signalFunnel.stream)
              
              val exitStatus = executive.process(cli, block(using client)(using cli))
              exitPromise.fulfill(exitStatus)

            catch
              case exception: Exception =>
                Log.fail(t"Oh no")
                Log.fail(exception.toString.show)
                Maybe(exception.getStackTrace).mm: stackTrace =>
                  stackTrace.foreach: frame =>
                    Log.fail(frame.toString.tt)
                exitPromise.fulfill(ExitStatus.Fail(1))
            finally
              socket.close()
              Log.fine(t"Closed connection to ${pid.value}")
        
          clients(pid) = ClientConnection[BusType](pid, async, signalFunnel, promise, () => socket.close(),
              exitPromise, busFunnel)

  application(using executives.direct)(Nil):
    import stdioSources.jvm
    import workingDirectories.default

    Async.onShutdown:
      waitFile.wipe()
      portFile.wipe()
      
    supervise:
      given Log = Log.route:
        case _ => Syslog(t"exoskeleton")

      Async:
        sh"flock -u -x -n $portFile cat".exec[Unit]()
        shutdown()

      Async:
        safely(baseDir.watch()).mm: watcher =>
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
  
case class DaemonService
    [BusType <: Matchable]
    (pid: Pid, shutdown: () => Unit, cliInput: CliInput, script: Unix.Path, deliver: BusType => Unit,
        bus: LazyList[BusType]):

  def broadcast(message: BusType): Unit = deliver(message)

enum DaemonEvent:
  case Init(pid: Pid, work: Text, script: Text, cliInput: CliInput, arguments: List[Text], environment: List[Text])
  case Trap(pid: Pid, signal: Signal)
  case Exit(pid: Pid)

def service[BusType <: Matchable](using service: DaemonService[BusType]): DaemonService[BusType] = service