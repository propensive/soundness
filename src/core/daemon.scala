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

import anticipation.*, filesystemApi.galileiPath
import rudiments.*, homeDirectories.default
import vacuous.*
import contingency.*
import exoskeleton.*
import feudalism.*
import eucalyptus.*
import hieroglyph.*, charEncoders.utf8, charDecoders.utf8, encodingMitigation.strict
import parasite.*
import profanity.*
import symbolism.*
import digression.*
import fulminate.*
import gossamer.{has as _, take as _, *}
import turbulence.*
import guillotine.*
import hellenism.*, classloaders.threadContext
import surveillance.*
import ambience.*, systemProperties.virtualMachine
import spectacular.*

import scala.compiletime.*

import language.experimental.pureFunctions

given DaemonLogEvent is Recordable into Message = _.communicate

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

case class ClientConnection[BusType <: Matchable](pid: Pid):
  val stderr: Promise[ji.OutputStream] = Promise()
  val signals: Funnel[Signal] = Funnel()
  val bus: Funnel[BusType] = Funnel()
  val terminatePid: Promise[Pid] = Promise()
  val exitPromise: Promise[ExitStatus] = Promise()
  def receive(message: BusType): Unit = bus.put(message)
  val socket: Promise[jn.Socket] = Promise()
  def close(): Unit = safely(socket.await(1000L).close())

class LazyEnvironment(variables: List[Text]) extends Environment:
  private lazy val map: Map[Text, Text] =
    variables.map(_.cut(t"=", 2).to(List)).collect:
      case List(key, value) => (key, value)
    .to(Map)

  def variable(key: Text): Optional[Text] = map.get(key).getOrElse(Unset)

def cliService[BusType <: Matchable](using executive: Executive)
    (block: DaemonService[BusType] ?=> executive.CliType ?=> executive.Return)
    (using interpreter:   CliInterpreter,
           stderrSupport: StderrSupport = daemonConfig.supportStderr,
           model:         ThreadModel)
      : Unit =

  given Realm: Realm = realm"ethereal"

  import environments.virtualMachine
  import errorHandlers.throwUnsafely

  val name: Text = Properties.ethereal.name[Text]()
  val baseDir: Directory = (Xdg.runtimeDir.or(Xdg.stateHome) / PathName(name)).as[Directory]
  val portFile: Path = baseDir / p"port"
  val pidFile: Path = baseDir / p"pid"
  val clients: Mutex[Map[Pid, ClientConnection[BusType]]] = Mutex(Map())
  val terminatePid: Promise[Pid] = Promise()

  def client(pid: Pid): ClientConnection[BusType] =
    val map = clients.replace: clients =>
      if clients.has(pid) then clients else clients.updated(pid, ClientConnection(pid))

    map(pid)

  lazy val termination: Unit =
    portFile.wipe()
    pidFile.wipe()
    System.exit(0)

  def shutdown(pid: Optional[Pid])(using Stdio): Unit logs DaemonLogEvent =
    Log.warn(DaemonLogEvent.Shutdown)
    pid.let(terminatePid.fulfill(_)).or(termination)

  def makeClient(socket: jn.Socket)
      (using Monitor, Stdio, Codicil)
        : Unit logs DaemonLogEvent raises StreamError raises CharDecodeError raises NumberError =

    async:
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
          val textArguments: List[Text] = chunk().cut(t"\u0000").take(argCount).to(List)
          val environment: List[Text] = chunk().cut(t"\u0000").init.to(List)

          DaemonEvent.Init(pid, pwd, script, cliInput, textArguments, environment)

        case _ =>
          Unset

      message match
        case Unset =>
          Log.warn(DaemonLogEvent.UnrecognizedMessage)
          socket.close()

        case DaemonEvent.Trap(pid, signal) =>
          Log.info(DaemonLogEvent.ReceivedSignal(signal))
          client(pid).signals.put(signal)
          socket.close()

        case DaemonEvent.Exit(pid) =>
          Log.fine(DaemonLogEvent.ExitStatusRequest(pid))
          val exitStatus: ExitStatus = client(pid).exitPromise.await()

          socket.getOutputStream.nn.write(exitStatus().show.bytes.mutable(using Unsafe))
          socket.close()
          clients.replace(_ - pid)
          if terminatePid() == pid then termination

        case DaemonEvent.Stderr(pid) =>
          Log.fine(DaemonLogEvent.StderrRequest(pid))
          client(pid).stderr.offer(socket.getOutputStream.nn)

        case DaemonEvent.Init(pid, directory, scriptName, shellInput, textArguments, env) =>
          Log.fine(DaemonLogEvent.Init(pid))
          val connection = client(pid)
          connection.socket.fulfill(socket)

          val lazyStderr: ji.OutputStream =
            if !stderrSupport() then socket.getOutputStream.nn
            else new ji.OutputStream():
              private lazy val wrapped = connection.stderr.await()
              def write(i: Int): Unit = wrapped.write(i)
              override def write(bytes: Array[Byte]): Unit = wrapped.write(bytes)

              override def write(bytes: Array[Byte], offset: Int, length: Int): Unit =
                wrapped.write(bytes, offset, length)

          given environment: Environment = LazyEnvironment(env)

          val termcap: Termcap = new Termcap:
            def ansi: Boolean = true

            lazy val color: ColorDepth =
              import workingDirectories.default
              if safely(Environment.colorterm[Text]) == t"truecolor" then ColorDepth.TrueColor
              else ColorDepth(safely(mute[ExecEvent](sh"tput colors".exec[Text]().decodeAs[Int])).or(-1))

          val stdio: Stdio =
            Stdio(ji.PrintStream(socket.getOutputStream.nn), ji.PrintStream(lazyStderr), in, termcap)

          def deliver(sourcePid: Pid, message: BusType): Unit = clients.use: clients =>
            clients().each: (pid, client) =>
              if sourcePid != pid then client.receive(message)

          val service: DaemonService[BusType] =
            DaemonService[BusType]
             (pid,
              () => shutdown(pid),
              shellInput,
              scriptName.decodeAs[Unix.Path],
              deliver(pid, _),
              connection.bus.stream,
              name)

          val workingDirectory: WorkingDirectory = () => directory

          Log.fine(DaemonLogEvent.NewCli)

          try
            val cli: executive.CliType =
              executive.cli
               (textArguments, environment, workingDirectory, stdio, connection.signals)

            val result = block(using service)(using cli)
            val exitStatus: ExitStatus = executive.process(cli)(result)

            connection.exitPromise.fulfill(exitStatus)

          catch
            case exception: Exception =>
              Log.warn(DaemonLogEvent.Failure)
              //Optional(exception.getStackTrace).let: stackTrace =>
              //  stackTrace.map(_.toString.tt).each(Log.fail(_))

              connection.exitPromise.fulfill(ExitStatus.Fail(1))

          finally
            socket.close()
            Log.info(DaemonLogEvent.CloseConnection(pid))

  application(using executives.direct(using unhandledErrors.silent))(Nil):
    import stdioSources.virtualMachine.ansi
    import asyncOptions.waitForOrphans

    Hook.onShutdown:
      portFile.wipe()
      pidFile.wipe()

    supervise:
      import logFormats.standard
      given Message is Loggable = Log(Syslog(t"ethereal"))

      val socket: jn.ServerSocket = jn.ServerSocket(0)
      val port: Int = socket.getLocalPort
      val buildId = safely((Classpath / p"build.id")().readAs[Text].trim.decodeAs[Int]).or(0)
      val stderr = if stderrSupport() then 1 else 0
      t"$port $buildId $stderr".writeTo(portFile.as[File])
      OsProcess().pid.value.show.writeTo(pidFile.as[File])

      task(t"pid-watcher"):
        safely:
          List(portFile, pidFile).watch: watcher =>
            watcher.stream.each:
              case Delete(_, _) | Modify(_, _) =>
                Log.warn(DaemonLogEvent.Termination)
                termination

              case other =>
                ()

      loop(safely(makeClient(socket.accept().nn))).run()

    ExitStatus.Ok

case class DaemonService[BusType <: Matchable]
    (pid:        Pid,
     shutdown:   () => Unit,
     cliInput:   CliInput,
     script:     Unix.Path,
     deliver:    BusType => Unit,
     bus:        LazyList[BusType],
     scriptName: Text)
extends ShellContext:

  def broadcast(message: BusType): Unit = deliver(message)

enum DaemonEvent:
  case Init
      (pid:         Pid,
       work:        Text,
       script:      Text,
       cliInput:    CliInput,
       arguments:   List[Text],
       environment: List[Text])

  case Trap(pid: Pid, signal: Signal)
  case Exit(pid: Pid)
  case Stderr(pid: Pid)

def service[BusType <: Matchable](using service: DaemonService[BusType]): DaemonService[BusType] = service
