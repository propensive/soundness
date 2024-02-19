/*
    Ethereal, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package ethereal

import serpentine.*, hierarchies.unix

import galilei.*, filesystemOptions.{dereferenceSymlinks, createNonexistent, createNonexistentParents,
    deleteRecursively}

import anticipation.*, filesystemInterfaces.galileiApi
import rudiments.*, homeDirectories.default
import vacuous.*
import contingency.*
import exoskeleton.*
import hieroglyph.*, charEncoders.utf8, charDecoders.utf8, badEncodingHandlers.strict
import parasite.*
import profanity.*
import digression.*
import fulminate.*
import gossamer.*
import turbulence.*
import guillotine.*
import hellenism.*, classloaders.threadContext
import surveillance.*
import eucalyptus.*
import ambience.*, systemProperties.virtualMachine
import spectacular.*

import scala.collection.mutable as scm
import scala.compiletime.*

//import language.experimental.captureChecking

import java.net as jn
import java.io as ji

object CliInput:
  given decoder: Decoder[CliInput] = text => valueOf(text.lower.capitalize.s)
  given encoder: Encoder[CliInput] = _.toString.tt.lower

enum CliInput:
  case Terminal, Pipe

package daemonConfig:
  given doNotSupportStderr: StderrSupport = () => false
  given supportStderr: StderrSupport = () => true

trait StderrSupport:
  def apply(): Boolean

case class ClientConnection
    [BusType <: Matchable]
    (pid: Pid, async: Async[Unit], signals: Funnel[Signal], terminate: Promise[Unit], close: () => Unit,
        exitPromise: Promise[ExitStatus], bus: Funnel[BusType], stderr: Promise[ji.OutputStream]):
  def receive(message: BusType): Unit = bus.put(message)

class LazyEnvironment(variables: List[Text]) extends Environment:
  private lazy val map: Map[Text, Text] =
    variables.map(_.cut(t"=", 2)).collect:
      case List(key, value) => (key, value)
    .to(Map)
  
  def variable(key: Text): Optional[Text] = map.get(key).getOrElse(Unset)

def daemon[BusType <: Matchable]
    (using executive: Executive)
    (block: DaemonService[BusType] ?=> executive.CliType ?=> executive.Return)
    (using interpreter: CliInterpreter, stderrSupport: StderrSupport = daemonConfig.supportStderr,
        model: ThreadModel)
    : Unit =
  
  given Realm: Realm = realm"ethereal"

  import environments.virtualMachine
  import errorHandlers.throwUnsafely

  val name: Text = Properties.ethereal.name[Text]()
  val baseDir: Directory = (Xdg.runtimeDir.or(Xdg.stateHome) / PathName(name)).as[Directory]
  val portFile: Path = baseDir / p"port"
  val clients: scm.HashMap[Pid, ClientConnection[BusType]] = scm.HashMap()
  var continue: Boolean = true
  val terminatePid: Promise[Pid] = Promise()
  
  lazy val termination: Unit =
    portFile.wipe()
    System.exit(0)
  
  def shutdown(pid: Optional[Pid])(using Stdio, Log[Text]): Unit =
    Log.info(t"Shutdown daemon")
    pid.let(terminatePid.fulfill(_)).or(termination)

  def client
      (socket: jn.Socket)
      (using Monitor, Log[Text], Stdio, Raises[StreamError], Raises[UndecodableCharError], Raises[NumberError])
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

    val message: Optional[DaemonEvent] = line() match
      case t"e" =>
        val pid: Pid = line().decodeAs[Pid]
        DaemonEvent.Stderr(pid)

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
        if terminatePid() == pid then termination
      
      case DaemonEvent.Stderr(pid) =>
        Log.fine(t"Received STDERR connection request from $pid")
        clients(pid).stderr.offer(socket.getOutputStream.nn)

      case DaemonEvent.Init(pid, directory, scriptName, shellInput, textArguments, env) =>
        Log.envelop(pid):
          Log.fine(t"Received init")
          val promise: Promise[Unit] = Promise()
          val exitPromise: Promise[ExitStatus] = Promise()
          val signalFunnel: Funnel[Signal] = Funnel()
          val busFunnel: Funnel[BusType] = Funnel()
          val stderrPromise: Promise[ji.OutputStream] = Promise()

          val lazyStderr: ji.OutputStream =
            if !stderrSupport() then socket.getOutputStream.nn
            else new ji.OutputStream():
              private lazy val wrapped = stderrPromise.await()
              def write(i: Int): Unit = wrapped.write(i)
              override def write(bytes: Array[Byte]): Unit = wrapped.write(bytes)
              
              override def write(bytes: Array[Byte], offset: Int, length: Int): Unit =
                wrapped.write(bytes, offset, length)
  
          val stdio: Stdio =
            Stdio(ji.PrintStream(socket.getOutputStream.nn), ji.PrintStream(lazyStderr), in)
          
          def deliver(sourcePid: Pid, message: BusType): Unit = clients.each: (pid, client) =>
            if sourcePid != pid then client.receive(message)
  
          val client: DaemonService[BusType] =
            DaemonService[BusType](pid, () => shutdown(pid), shellInput, scriptName.decodeAs[Unix.Path],
                deliver(pid, _), busFunnel.stream, name)
          
          val environment = LazyEnvironment(env)
          val workingDirectory: WorkingDirectory = () => directory
          
          val async = Async:
            Log.pin()
            Log.info(t"Creating new CLI")
            try
              val cli: executive.CliType =
                executive.cli(textArguments, environment, workingDirectory, stdio, signalFunnel.stream)
              
              val result = block(using client)(using cli)
              val exitStatus: ExitStatus = executive.process(cli)(result)
              
              exitPromise.fulfill(exitStatus)

            catch
              case exception: Exception =>
                Log.fail(exception.toString.show)
                Optional(exception.getStackTrace).let: stackTrace =>
                  stackTrace.each: frame =>
                    Log.fail(frame.toString.tt)
                exitPromise.fulfill(ExitStatus.Fail(1))
            finally
              socket.close()
              Log.fine(t"Closed connection to ${pid.value}")
        
          clients(pid) = ClientConnection[BusType](pid, async, signalFunnel, promise, () => socket.close(),
              exitPromise, busFunnel, stderrPromise)

  application(using executives.direct(using unhandledErrors.silent))(Nil):
    import stdioSources.virtualMachine
    import workingDirectories.default

    Async.onShutdown:
      portFile.wipe()
      
    supervise:
      given Log[Text] = Log.route[Text]:
        case _ => Syslog(t"ethereal")

      Log.pin()

      Async:
        sh"flock -u -x -n $portFile cat".exec[Unit]()
        Log.info(t"The file $portFile was changed; terminating immediately")
        termination

      Async:
        safely(baseDir.watch()).let: watcher =>
          watcher.stream.each:
            case Delete(_, t"port") =>
              Log.info(t"The file $portFile was deleted; terminating immediately")
              termination
            
            case _ =>
              ()
          
      val socket: jn.ServerSocket = jn.ServerSocket(0)
      val port: Int = socket.getLocalPort
      val buildId = (Classpath / p"build.id")().readAs[Text].trim.decodeAs[Int]
      val stderr = if stderrSupport() then 1 else 0
      t"$port $buildId $stderr".writeTo(portFile)
      while continue do safely(client(socket.accept().nn))

    ExitStatus.Ok
  
case class DaemonService
    [BusType <: Matchable]
    (pid: Pid, shutdown: () => Unit, cliInput: CliInput, script: Unix.Path, deliver: BusType => Unit,
        bus: LazyList[BusType], scriptName: Text)
extends ShellContext:
  def broadcast(message: BusType): Unit = deliver(message)

enum DaemonEvent:
  case Init(pid: Pid, work: Text, script: Text, cliInput: CliInput, arguments: List[Text], environment: List[Text])
  case Trap(pid: Pid, signal: Signal)
  case Exit(pid: Pid)
  case Stderr(pid: Pid)

def service[BusType <: Matchable](using service: DaemonService[BusType]): DaemonService[BusType] = service

