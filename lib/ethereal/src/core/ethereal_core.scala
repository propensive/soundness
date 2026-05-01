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
import java.nio as jnio
import java.nio.channels as jnc
import java.util.concurrent.atomic as juca

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
import rudiments.*
import serpentine.*
import spectacular.*
import surveillance.*
import symbolism.*
import turbulence.*
import vacuous.*

import anticipation.abstractables.durationIsAbstractable
import filesystemOptions.createNonexistent.enabled
import filesystemOptions.createNonexistentParents.enabled
import filesystemOptions.deleteRecursively.enabled
import filesystemOptions.dereferenceSymlinks.enabled
import filesystemOptions.readAccess.enabled
import filesystemOptions.writeAccess.enabled

given daemonLogEvent: Message transcribes DaemonLogEvent = _.communicate

def service[bus <: Matchable](using service: DaemonService[bus]): DaemonService[bus] = service

def cli[bus <: Matchable](using executive: Executive)
  ( block: (DaemonService[bus], executive.Interface) ?=> executive.Return )
  ( using interpreter: Interpreter,
          threading:   Threading,
          handler:     Backstop )
:   Unit =

  given realm: Realm = realm"et"

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

            val buildId: Long = safely(System.properties.build.id[Long]()).or:
              safely(buildIdPath.read[Text].trim.decode[Long]).or(0L)

            val platformLabel: Text = safely(System.properties.build.target[Text]()).or:
              val osName = safely(System.properties.os.name[Text]().lower).or(t"")
              val osArch = safely(System.properties.os.arch[Text]().lower).or(t"")

              val os =
                if osName.contains(t"mac") || osName.contains(t"darwin") then t"macos"
                else if osName.contains(t"win")
                then t"windows"
                else t"linux"

              val arch =
                if osArch.contains(t"aarch") || osArch == t"arm64" then t"arm64" else t"x64"
              t"$os-$arch"

            val isWindows: Boolean = platformLabel.starts(t"windows")

            val runnerName: Text =
              if isWindows then t"runner-$platformLabel.exe" else t"runner-$platformLabel"

            val runnerResource: Path on Classpath = Classpath/"ethereal"/runnerName
            val runnerBytes: IArray[Byte] = runnerResource.read[Data]

            val magic: Array[Byte] = Array[Byte](
              'E'.toByte, 'T'.toByte, 'H'.toByte, 'R'.toByte,
              'C'.toByte, 'F'.toByte, 'G'.toByte, 1.toByte)

            val magicOffset: Int =
              var found: Int = -1
              var i = 0
              while found < 0 && i <= runnerBytes.length - magic.length do
                var matches = true
                var j = 0
                while matches && j < magic.length do
                  if runnerBytes(i + j) != magic(j) then matches = false
                  j += 1
                if matches then found = i
                i += 1
              if found < 0 then
                Out.println(e"Runner binary does not contain the ETHRCFG magic marker")
                Exit.Fail(1).terminate()
              found

            val configOffset: Int = magicOffset + magic.length
            val patched: Array[Byte] = IArray.genericWrapArray(runnerBytes).toArray
            val buf = jnio.ByteBuffer.wrap(patched, configOffset, 24).nn
            buf.order(jnio.ByteOrder.LITTLE_ENDIAN).nn
            buf.putLong(buildId)
            buf.putShort(javaMinimum.toShort)
            buf.putShort(javaPreferred.toShort)
            buf.put((if jdk then 1 else 0).toByte)
            while buf.hasRemaining do buf.put(0.toByte)

            path.open: file =>
              Stream(IArray.from(patched): IArray[Byte]).writeTo(file)

            if platformLabel.starts(t"macos") then
              if !isWindows then path.executable() = true
              safely(mute[ExecEvent](sh"codesign --sign - --force $path".exec[Exit]()))

            jarFile.open: jarFile =>
              Eof(path).open(jarFile.stream[Data].writeTo(_))

            if !isWindows then path.executable() = true

            Out.println(t"Built executable file $destination")

            Exit.Ok.terminate()

    . within(System.properties.ethereal.name[Text]())

  val userId: Optional[Int] = safely(System.properties.ethereal.user.id[Int]())
  val userName: Optional[Text] = safely(System.properties.ethereal.user.name[Text]())
  val startTime: Long =
    safely(System.properties.ethereal.startTime[Long]()).or(jl.System.currentTimeMillis())

  val runtimeDir: Optional[Path on Local] = Xdg.runtimeDir
  val stateHome: Path on Local = Xdg.stateHome
  val baseDir: Path on Local = runtimeDir.or(stateHome) //name
  val buildFile: Path on Local = baseDir/name/"build"
  val pidFile: Path on Local = baseDir/name/"pid"
  val socketFile: Path on Local = baseDir/name/"socket"
  val clients: scc.TrieMap[Pid, Client of bus] = scc.TrieMap()
  val terminatePid: Promise[Pid] = Promise()
  val idleTimeout: Long = 6L*60L*60L*1_000_000_000L

  def client(pid: Pid): Client of bus = clients.getOrElseUpdate(pid, Client[bus](pid))

  lazy val termination: Unit =
    safely(socketFile.wipe())
    safely(buildFile.wipe())
    safely(pidFile.wipe())
    jl.System.exit(0)

  def shutdown(pid: Optional[Pid])(using Stdio): Unit logs DaemonLogEvent =
    Log.warn(DaemonLogEvent.Shutdown)
    pid.let(terminatePid.fulfill(_)).or(termination)


  def makeClient(channel: jnc.SocketChannel)(using Monitor, Stdio, Codicil)
  :   Unit logs DaemonLogEvent raises StreamError raises CharDecodeError raises NumberError =

    async:
      val rawIn: ji.InputStream = jnc.Channels.newInputStream(channel).nn
      val rawOut: ji.OutputStream = jnc.Channels.newOutputStream(channel).nn
      val in: ji.BufferedInputStream = ji.BufferedInputStream(rawIn, 16384)

      def line(): Text =
        val sb = jl.StringBuilder()
        var b = in.read()
        while b != '\n' && b != -1 do
          sb.append(b.toChar)
          b = in.read()
        sb.toString.tt

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
          val name: Text = line()

          val signal: UnixSignal | WindowsSignal =
            safely(name.decode[UnixSignal]).or(name.decode[WindowsSignal])
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
          channel.close()

        case DaemonEvent.Trap(pid, signal) =>
          Log.info(DaemonLogEvent.ReceivedSignal(signal))
          val response: SignalResponse =
            safely:
              val invocation = client(pid).invocation.await(250L*1_000_000L)
              invocation.dispatchSignal(signal)

            . or(SignalResponse.Reject)

          val ackByte: Byte = if response == SignalResponse.Accept then 'a' else 'r'
          safely(rawOut.write(Array[Byte](ackByte, '\n'.toByte)))
          safely(rawOut.flush())
          channel.close()

        case DaemonEvent.Exit(pid) =>
          Log.fine(DaemonLogEvent.ExitStatusRequest(pid))
          val exitStatus: Exit = client(pid).exitPromise.await()

          rawOut.write(exitStatus().show.data.mutable(using Unsafe))
          channel.close()
          clients.remove(pid)
          if terminatePid() == pid then termination

        case DaemonEvent.Stderr(pid) =>
          Log.fine(DaemonLogEvent.StderrRequest(pid))
          val stderrClient = client(pid)
          stderrClient.stderr.offer(rawOut)
          safely(stderrClient.exitPromise.await())
          safely(channel.close())

        case DaemonEvent.Init(pid, login, directory, script, shellInput, textArguments, env) =>
          Log.fine(DaemonLogEvent.Init(pid))
          val connection = client(pid)
          connection.socket.fulfill(channel)

          val lazyStderr: ji.OutputStream = new ji.OutputStream():
            private val stderrOut: juca.AtomicReference[ji.OutputStream | Null] =
              juca.AtomicReference(null)

            private def connected(): ji.OutputStream =
              val s = stderrOut.get()
              if s != null then s
              else
                val newS = connection.stderr.await()
                stderrOut.set(newS)
                newS

            def write(i: Int): Unit = connected().write(i)
            override def write(bytes: Array[Byte] | Null): Unit = connected().write(bytes)

            override def write(bytes: Array[Byte] | Null, offset: Int, length: Int): Unit =
              connected().write(bytes, offset, length)

            override def close(): Unit =
              val s = stderrOut.get()
              if s != null then safely(s.close())

          given environment: Environment = LazyEnvironment(env)

          val termcap: Termcap = new Termcap:
            def ansi: Boolean = true

            lazy val color: ColorDepth =
              import workingDirectories.system

              if safely(Environment.colorterm[Text]) == t"truecolor" then ColorDepth.TrueColor
              else
                ColorDepth
                  ( safely(mute[ExecEvent](sh"tput colors".exec[Text]().decode[Int])).or(-1) )

          val stdio: Stdio =
            Stdio
              ( ji.PrintStream(rawOut, true),
                ji.PrintStream(lazyStderr, true),
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
                script.decode[Path on Local],
                deliver(pid, _),
                connection.bus.stream,
                name,
                startTime )

          Log.fine(DaemonLogEvent.NewCli)

          try
            val cli: executive.Interface =
              executive.invocation
                ( textArguments,
                  environment,
                  () => directory,
                  stdio,
                  service,
                  login )

            connection.invocation.offer(cli)

            if cli.proceed then
              val result = block(using service, cli)
              val exitStatus: Exit = executive.process(cli)(result)

              connection.exitPromise.fulfill(exitStatus)
            else
              connection.exitPromise.fulfill(Exit.Ok)

          catch
            case exception: Exception =>
              Log.warn(DaemonLogEvent.Failure)
              connection.exitPromise.fulfill(handler.handle(exception)(using stdio))

          finally
            lazyStderr.close()
            channel.close()
            Log.info(DaemonLogEvent.CloseConnection(pid))

  application(using executives.direct(using backstops.silent))(Nil):
    import environments.java
    import termcaps.environment
    import stdios.virtualMachine
    import codicils.await

    Os.intercept[Shutdown]:
      safely(socketFile.wipe())
      safely(buildFile.wipe())
      safely(pidFile.wipe())

    supervise:
      import logFormats.standard
      given loggable: Message is Loggable = Log.route(Syslog(t"ethereal"))

      safely(socketFile.wipe())

      val channel: jnc.ServerSocketChannel =
        jnc.ServerSocketChannel.open(jn.StandardProtocolFamily.UNIX).nn

      channel.configureBlocking(true)
      channel.bind(jn.UnixDomainSocketAddress.of(socketFile.encode.s))

      val buildId = safely(System.properties.build.id[Int]()).or:
        safely((Classpath/"build.id").read[Text].trim.decode[Int]).or(0)
      buildFile.open(t"$buildId".writeTo(_))
      val pidValue = Process().pid.value.show
      pidFile.open(pidValue.writeTo(_))

      task(t"pid-watcher"):
        safely:
          List[Path on Local](socketFile, buildFile, pidFile).watch: watcher =>
            watcher.stream.each:
              case Delete(_, _) | Modify(_, _) =>
                Log.warn(DaemonLogEvent.Termination)
                termination

              case other =>
                ()

      val inactivityTimer: Timeout = Timeout(idleTimeout):
        Log.warn(DaemonLogEvent.IdleTimeout)
        termination

      loop:
        val socket = channel.accept().nn
        inactivityTimer.nudge()
        safely(makeClient(socket))

      . run()

    Exit.Ok

  ???
