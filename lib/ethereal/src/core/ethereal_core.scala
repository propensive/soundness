                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.54.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package ethereal

import language.experimental.pureFunctions

import java.io as ji
import java.lang as jl
import java.net as jn

import scala.collection.concurrent as scc

import ambience.*, systems.java
import anticipation.*
import contingency.*
import digression.*
import distillate.*
import escapade.*
import eucalyptus.*
import exoskeleton.*
import fulminate.*
import galilei.*
import gossamer.*
import guillotine.*
import hellenism.*, classloaders.threadContext
import hieroglyph.*, charEncoders.utf8, charDecoders.utf8, textSanitizers.strict
import nomenclature.*
import parasite.*
import prepositional.*
import profanity.*
import proscenium.*
import rudiments.*, homeDirectories.system
import serpentine.*
import spectacular.*
import surveillance.*
import symbolism.*
import turbulence.*
import vacuous.*

import filesystemOptions.dereferenceSymlinks.enabled
import filesystemOptions.createNonexistent.enabled
import filesystemOptions.createNonexistentParents.enabled
import filesystemOptions.deleteRecursively.enabled
import filesystemOptions.readAccess.enabled
import filesystemOptions.writeAccess.enabled

given daemonLogEvent: Message transcribes DaemonLogEvent = _.communicate

package daemonConfig:
  given doNotSupportStderr: StderrSupport = () => false
  given supportStderr: StderrSupport = () => true

def service[bus <: Matchable](using service: DaemonService[bus]): DaemonService[bus] = service

def cli[bus <: Matchable](using executive: Executive)
  ( block: (DaemonService[bus], executive.Interface) ?=> executive.Return )
  ( using interpreter:   Interpreter,
          stderrSupport: StderrSupport = daemonConfig.supportStderr,
          threading:     Threading,
          handler:       Backstop )
:   Unit =

  given realm: Realm = realm"ethereal"

  import environments.java
  import strategies.throwUnsafely
  import workingDirectories.system
  import environments.java
  import termcaps.environment
  import stdios.virtualMachine

  val name: Text =
    recover:
      case PropertyError(_) =>
        val jarFile: Path on Linux = System.properties.java.`class`.path[Text]().pipe: jarFile =>
          safely(jarFile.decode[Path on Linux]).or:
            val work: Path on Linux = workingDirectory
            work + jarFile.decode[Relative on Linux]

        safely(System.properties.build.executable[Text]()).absolve match
          case Unset =>
            Out.println(e"$Bold(This application must be invoked with the Ethereal launch script)")
            Out.println(e"To build an Ethereal executable, run:")
            val work: Path on Linux = workingDirectory
            val relativeJar: Relative on Linux = work.toward(jarFile)
            Out.println(e"    java -Dbuild.executable=$Italic(<filename>) -jar $relativeJar")
            Out.println()
            Out.println(e"Other $Italic(-D) Java options:")
            val detail = e"preferred major version, e.g. $Italic(24) (default) or 21"
            Out.println(e"  build.java.preferred  -- $detail")
            val detail2 = e"minimum major version, e.g. $Italic(21) (default) or 16"
            Out.println(e"  build.java.minimum    -- $detail2")
            val detail3 = e"required bundle type, $Italic(jre) (default) or $Italic(jdk)"
            Out.println(e"  build.java.bundle     -- $detail3")
            Out.println()
            Exit.Fail(1).terminate()

          case destination: Text =>
            val javaMinimum = safely(System.properties.build.java.minimum[Int]()).or(21)
            val javaPreferred = safely(System.properties.build.java.preferred[Int]()).or(24)
            val jdk = safely(System.properties.build.java.bundle[Text]() == t"jdk").or(false)

            val path = safely(destination.decode[Path on Linux]).or:
              val work: Path on Linux = workingDirectory
              work + destination.decode[Relative on Linux]

            val buildIdPath: Path on Classpath = Classpath/"build.id"
            val buildId = safely(buildIdPath.read[Text].trim).or(t"0")
            val prefixPath: Path on Classpath = Classpath/"ethereal"/"prefix"
            val prefix = prefixPath.read[Text]

            path.open: file =>
              prefix
              . sub("%%BUILD_ID%%", buildId)
              . sub("%%JAVA_MINIMUM%%", javaMinimum.show)
              . sub("%%JAVA_PREFERRED%%", javaPreferred.show)
              . sub("%%JAVA_BUNDLE%%", if jdk then t"jdk" else t"jre")
              . writeTo(file)

            jarFile.open: jarFile =>
              Eof(path).open(jarFile.stream[Data].writeTo(_))

            path.executable() = true

            Out.println(t"Built executable file $destination")

            Exit.Ok.terminate()

    . within(System.properties.ethereal.name[Text]())

  val userId: Optional[Int] = safely(System.properties.ethereal.user.id[Int]())
  val userName: Optional[Text] = safely(System.properties.ethereal.user.name[Text]())

  val runtimeDir: Optional[Path on Linux] = Xdg.runtimeDir
  val stateHome: Path on Linux = Xdg.stateHome
  val baseDir: Path on Linux = runtimeDir.or(stateHome) //name
  val portFile: Path on Linux = baseDir/name/"port"
  val pidFile: Path on Linux = baseDir/name/"pid"
  val clients: scc.TrieMap[Pid, Client of bus] = scc.TrieMap()
  val terminatePid: Promise[Pid] = Promise()

  def client(pid: Pid): Client of bus = clients.getOrElseUpdate(pid, Client[bus](pid))

  lazy val termination: Unit =
    portFile.wipe()
    pidFile.wipe()
    jl.System.exit(0)

  def shutdown(pid: Optional[Pid])(using Stdio): Unit logs DaemonLogEvent =
    Log.warn(DaemonLogEvent.Shutdown)
    pid.let(terminatePid.fulfill(_)).or(termination)


  def makeClient(socket: jn.Socket)(using Monitor, Stdio, Codicil)
  :   Unit logs DaemonLogEvent raises StreamError raises CharDecodeError raises NumberError =

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
          val pid: Pid = line().decode[Pid]
          DaemonEvent.Stderr(pid)

        case t"s" =>
          val pid: Pid = line().decode[Pid]
          val signal: Signal = line().decode[Signal]
          DaemonEvent.Trap(pid, signal)

        case t"x" =>
          DaemonEvent.Exit(line().decode[Pid])

        case t"i" =>
          val stdin: Stdin = if line() == t"p" then Stdin.Pipe else Stdin.Terminal
          val pid: Pid = Pid(line().decode[Int])
          val userId: Int = line().decode[Int]
          val username: Text = line()
          val login = Login(username, userId)
          val script: Text = line()
          val pwd: Text = line()
          val count: Int = line().decode[Int]
          val textArguments: List[Text] = chunk().cut(t"\u0000").take(count).to(List)
          val environment: List[Text] = chunk().cut(t"\u0000").init.to(List)

          DaemonEvent.Init(pid, login, pwd, script, stdin, textArguments, environment)

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
          val exitStatus: Exit = client(pid).exitPromise.await()

          socket.getOutputStream.nn.write(exitStatus().show.data.mutable(using Unsafe))
          socket.close()
          clients.remove(pid)
          if terminatePid() == pid then termination

        case DaemonEvent.Stderr(pid) =>
          Log.fine(DaemonLogEvent.StderrRequest(pid))
          client(pid).stderr.offer(socket.getOutputStream.nn)

        case DaemonEvent.Init(pid, login, directory, script, shellInput, textArguments, env) =>
          Log.fine(DaemonLogEvent.Init(pid))
          val connection = client(pid)
          connection.socket.fulfill(socket)

          val lazyStderr: ji.OutputStream =
            if !stderrSupport() then socket.getOutputStream.nn
            else new ji.OutputStream():
              private lazy val wrapped = connection.stderr.await()
              def write(i: Int): Unit = wrapped.write(i)
              override def write(bytes: Array[Byte] | Null): Unit = wrapped.write(bytes)

              override def write(bytes: Array[Byte] | Null, offset: Int, length: Int): Unit =
                wrapped.write(bytes, offset, length)

          given environment: Environment = LazyEnvironment(env)

          val termcap: Termcap = new Termcap:
            def ansi: Boolean = true

            lazy val color: ColorDepth =
              import workingDirectories.system

              if safely(Environment.colorterm[Text]) == t"truecolor" then ColorDepth.TrueColor
              else ColorDepth
                    (safely(mute[ExecEvent](sh"tput colors".exec[Text]().decode[Int])).or(-1))

          val stdio: Stdio =
            Stdio
              ( ji.PrintStream(socket.getOutputStream.nn),
                ji.PrintStream(lazyStderr),
                in,
                termcap )

          def deliver(sourcePid: Pid, message: bus): Unit =
            clients.each: (pid, client) =>
              if sourcePid != pid then client.receive(message)

          val service: DaemonService[bus] =
            DaemonService[bus]
              ( pid,
                () => shutdown(pid),
                shellInput,
                script.decode[Path on Linux],
                deliver(pid, _),
                connection.bus.stream,
                name )

          Log.fine(DaemonLogEvent.NewCli)

          try
            val cli: executive.Interface =
              executive.invocation
                ( textArguments,
                  environment,
                  () => directory,
                  stdio,
                  connection.signals,
                  service,
                  login )

            if cli.proceed then
              val result = block(using service, cli)
              val exitStatus: Exit = executive.process(cli)(result)

              connection.exitPromise.fulfill(exitStatus)
            else connection.exitPromise.fulfill(Exit.Ok)

          catch
            case exception: Exception =>
              Log.warn(DaemonLogEvent.Failure)
              connection.exitPromise.fulfill(handler.handle(exception)(using stdio))

          finally
            socket.close()
            Log.info(DaemonLogEvent.CloseConnection(pid))

  application(using executives.direct(using backstops.silent))(Nil):
    import environments.java
    import termcaps.environment
    import stdios.virtualMachine
    import codicils.await

    Os.intercept[Shutdown]:
      portFile.wipe()
      pidFile.wipe()

    supervise:
      import logFormats.standard
      given loggable: Message is Loggable = Log.route(Syslog(t"ethereal"))

      val socket: jn.ServerSocket = jn.ServerSocket(0)
      val port: Int = socket.getLocalPort
      val buildId = safely((Classpath/"build.id").read[Text].trim.decode[Int]).or(0)
      val stderr = if stderrSupport() then 1 else 0
      portFile.open(t"$port $buildId $stderr".writeTo(_))
      val pidValue = OsProcess().pid.value.show
      pidFile.open(pidValue.writeTo(_))

      task(t"pid-watcher"):
        safely:
          List[Path on Linux](portFile, pidFile).watch: watcher =>
            watcher.stream.each:
              case Delete(_, _) | Modify(_, _) =>
                Log.warn(DaemonLogEvent.Termination)
                termination

              case other =>
                ()

      loop(safely(makeClient(socket.accept().nn))).run()

    Exit.Ok

  ???
