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
┃    Soundness, version 0.64.0.                                                                    ┃
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

import scala.language.experimental.pureFunctions

import scala.caps

import java.io as ji
import java.lang as jl
import java.util.concurrent.atomic as juca

import scala.collection.concurrent as scc

import ambience.*, systems.javaSystem
import anticipation.*
import aperture.*
import coaxial.*
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
import hellenism.*, classloaders.threadContextClassloader
import hieroglyph.*, charEncoders.utf8Encoder, charDecoders.utf8Decoder
import textSanitizers.strictSanitizer
import nomenclature.*
import parasite.*, Async.nominative
import prepositional.*
import profanity.*
import quantitative.*
import rudiments.*
import serpentine.*
import spectacular.*
import surveillance.*
import symbolism.*
import turbulence.*
import vacuous.*

import filesystemOptions.createNonexistentParents.enabled
import filesystemOptions.deleteRecursively.enabled
import filesystemOptions.dereferenceSymlinks.enabled

import filesystemBackends.virtualMachineFilesystem

def service[bus <: Matchable](using service: DaemonService[bus]): DaemonService[bus]^{service} =
  service

def cli[bus <: Matchable](using executive: Executive)
  ( block: (DaemonService[bus], executive.Interface, Environment, Monitor) ?=> executive.Return )
  ( using interpreter: Interpreter,
          threading:   Threading,
          handler:     Backstop )
:   Unit =

  import strategies.throwUnsafely
  import workingDirectories.systemWorkingDirectory
  import environments.javaEnvironment
  import termcaps.environmentTermcap
  import stdios.virtualMachineStdio

  val name: Text =
    recover:
      case Property.Error(_) =>
        val jarFile: Path on Linux = System.properties.java.`class`.path[Text]().pipe: jarFile =>
          safely(jarFile.as[Path on Linux]).or:
            val work: Path on Linux = workingDirectory
            work + jarFile.as[Relative on Linux]

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

            val path = safely(destination.as[Path on Linux]).or:
              val work: Path on Linux = workingDirectory
              work + destination.as[Relative on Linux]

            val buildIdPath: Path on Classpath = Classpath/"build.id"

            val buildId: Long = safely(System.properties.build.id[Long]()).or:
              safely(buildIdPath.read[Text].trim.as[Long]).or(0L)

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

            // The reusable runner stubs are not embedded in the JAR. A locally-built or
            // pre-fetched directory (an explicit `-Dethereal.runners=<dir>`, or `dist/runners`
            // relative to the working directory) takes priority; otherwise the one stub this
            // platform needs is downloaded from the published `runners-<version>` GitHub
            // release, verified against its SHA-256, and cached for subsequent builds.
            val work: Path on Linux = workingDirectory

            val runnersDir: Text =
              safely(System.properties.ethereal.runners[Text]()).or(t"$work/dist/runners")

            val localRunner: Path on Linux = t"$runnersDir/$runnerName".as[Path on Linux]

            val cacheDir: Path on Linux =
              Directories.cacheHome[Path on Linux]/t"ethereal"/t"runners"/Runners.version

            val cacheRunner: Path on Linux = cacheDir/runnerName

            val runnerBytes: Data =
              // Each branch opens and fully reads its own file handle.
              if localRunner.existent() then
                scala.caps.unsafe.unsafeAssumeSeparate(localRunner.open[File]()(file.read[Data]))
              else if cacheRunner.existent() then
                scala.caps.unsafe.unsafeAssumeSeparate(cacheRunner.open[File]()(file.read[Data]))
              else
                mitigate:
                  case Runners.Error(detail) =>
                    Out.println(detail.text)
                    Exit.Fail(1).terminate()

                  case Io.Error(_, _, _, _) =>
                    Out.println(e"Could not cache the downloaded runner stub $runnerName")
                    Exit.Fail(1).terminate()

                  case Truncation.Error(_) =>
                    Out.println(e"The runner stub download was interrupted")
                    Exit.Fail(1).terminate()

                . protect:
                    import filesystemOptions.overwritePreexisting.disabled
                    Out.println(e"Downloading $runnerName from runners-${Runners.version}")
                    val bytes: Data = Runners.download(platformLabel)
                    if !cacheDir.existent() then cacheDir.create[Directory](CreateFlag.Parents)
                    cacheRunner.open[File](Write, OpenFlag.Create)(file.write(Chain(bytes)))
                    bytes

            // ML-DSA-44 public key used by the runner to verify upgrades.
            // When `ethereal.publicKey` is unset the slot stays zero and the
            // runner's verifier rejects every upgrade — the safe default for
            // dev builds where upgrades happen via `make install` rather
            // than the .pending path. Releases that need signed upgrades
            // pass `-Dethereal.publicKey=<path>` pointing at a 1312-byte raw
            // ML-DSA-44 public key file.
            val publicKey: Data =
              safely(System.properties.ethereal.publicKey[Text]()).absolve match
                case Unset =>
                  Array.fill(Assembler.PublicKeyLength)(0.toByte)   // upgrades blocked

                case keyPath: Text =>
                  val resolved: Path on Linux = safely(keyPath.as[Path on Linux]).or:
                    val work: Path on Linux = workingDirectory
                    work + keyPath.as[Relative on Linux]

                  val raw: Data = scala.caps.unsafe.unsafeAssumeSeparate(resolved.open[File]()(file.read[Data]))

                  if raw.length != Assembler.PublicKeyLength then
                    Out.println(e"Public key at $keyPath is the wrong size (expected 1312 bytes)")
                    Exit.Fail(1).terminate()

                  raw

            mitigate:
              case Assembler.Error(_) =>
                Out.println(e"Runner binary does not contain the ETHRCFG\\x02 magic marker")
                Exit.Fail(1).terminate()

            . protect:
                Assembler.assemble
                  ( runnerBytes, jarFile, path, platformLabel, buildId, javaMinimum,
                   javaPreferred, jdk, publicKey )

            Out.println(t"Built executable file $destination")

            Exit.Ok.terminate()

    . protect(System.properties.ethereal.name[Text]())

  val userId: Optional[Int] = scala.caps.unsafe.unsafeAssumeSeparate:
    safely(System.properties.ethereal.user.id[Int]())
  val userName: Optional[Text] = safely(System.properties.ethereal.user.name[Text]())

  val startTime: Long =
    scala.caps.unsafe.unsafeAssumeSeparate:
      safely(System.properties.ethereal.startTime[Long]()).or(jl.System.currentTimeMillis())

  // Use `Directories.*` rather than `Xdg.*` so Windows resolves to a native
  // location (`%LOCALAPPDATA%\Temp`) instead of `%USERPROFILE%\.local\state`,
  // which the Rust launcher in `state.rs` does not look at.
  val runtimeDir: Optional[Path on Local] = Directories.runtimeDir
  val stateHome: Path on Local = Directories.stateHome
  val baseDir: Path on Local = runtimeDir.or(stateHome)
  val buildFile: Path on Local = baseDir/name/"build"
  val pidFile: Path on Local = baseDir/name/"pid"
  val socketFile: Path on Local = baseDir/name/"socket"
  val clients: scc.TrieMap[Pid, Client of bus] = scc.TrieMap()
  val terminatePid: Promise[Pid] = Promise()
  val idleTimeout = Quantity[Hours[1]](6.0)

  def client(pid: Pid): Client of bus = clients.getOrElseUpdate(pid, Client[bus](pid))

  def ownsState: Boolean =
    val recorded: Optional[Text] =
      if pidFile.existent() then safely(pidFile.read[Text].trim)
      else Unset

    recorded.let(_ == Process().pid.value.show).or(false)

  // Only clear the state files if this daemon still owns them. A successor
  // daemon (e.g. after an upgrade) rebinds the socket at the same path and
  // overwrites `pidFile` with its own pid before binding, so a dying daemon
  // that no longer matches the recorded pid must not wipe the live successor's
  // socket out from under it.
  def wipeState(): Unit =
    if ownsState then
      safely(socketFile.wipe())
      safely(buildFile.wipe())
      safely(pidFile.wipe())

  // If the daemon's own jar has been clobbered, cleanup may fail to load classes
  // mid-flight; exit must happen regardless, or the broken daemon lingers and
  // serves silent no-ops to every new client.
  lazy val termination: Unit =
    try wipeState() catch case _: jl.Throwable => ()
    finally jl.System.exit(0)

  val scriptPath: Optional[Path on Local] =
    safely(System.properties.ethereal.script[Text]().as[Path on Local])

  case class ScriptIdentity(size: Long, mtime: Long, hash: Text)

  def hashScript(script: Path on Local): Optional[Text] = safely:
    import gastronomy.*, providers.javaStdlibProvider
    import monotonous.*, alphabets.hexLowerCase
    script.open[File](Read)(file.checksum[Sha2[256]].serialize[Hex])

  // The identity of the launcher this daemon was started from, hashed once at
  // startup — which also preloads the hashing classes, so verification can still
  // run once the jar has been clobbered underneath the JVM. In-memory only: the
  // launcher compares mtimes against the build FILE, but the authoritative record
  // is here, because rewriting the build file would trip the pid-watcher.
  val scriptIdentity: juca.AtomicReference[ScriptIdentity | Null] =
    juca.AtomicReference(null)

  // True if the launcher's content is unchanged. mtime is the cheap gate: after a
  // metadata-only `touch`, the content is hashed once and the remembered mtime is
  // updated, so every later verification is a single `stat` again. Synchronized so
  // that concurrent clients cause at most one hash. Doubt — an unreadable or
  // deleted launcher — resolves to stale.
  def verifyScript(): Boolean = scriptIdentity.synchronized:
    scriptPath.lay(true): script =>
      val recorded = scriptIdentity.get()

      if recorded == null then true else
        safely:
          import anticipation.instantiables.instantInstantiable
          val size = script.size().long
          val mtime = script.modified[Long]()

          if size != recorded.size then false
          else if mtime == recorded.mtime then true
          else hashScript(script).lay(false): hash =>
            if hash == recorded.hash then
              scriptIdentity.set(recorded.copy(mtime = mtime))
              true
            else false

        . or(false)

  def shutdown(pid: Optional[Pid])(using Stdio): Unit logs DaemonLogEvent =
    Log.warn(DaemonLogEvent.Shutdown)
    pid.let(terminatePid.fulfill(_)).or(termination)


  def makeClient(connection: Connection)(using Monitor, Stdio, Probate)
    ( using Tactic[Truncation.Error], Tactic[CharDecoder.Error], Tactic[Number.Error],
            (DaemonLogEvent is Loggable)^ )
  :   Unit =

    val rawIn: ji.InputStream = connection.reader
    val rawOut: ji.OutputStream = connection.writer
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
        val pid: Pid = line().as[Pid]
        DaemonEvent.Stderr(pid)

      case t"m" =>
        val pid: Pid = line().as[Pid]
        DaemonEvent.Control(pid)

      case t"s" =>
        val pid: Pid = line().as[Pid]
        val name: Text = line()

        val signal: UnixSignal | WindowsSignal =
          safely(name.as[UnixSignal]).or(name.as[WindowsSignal])

        DaemonEvent.Trap(pid, signal)

      case t"x" =>
        DaemonEvent.Exit(line().as[Pid])

      case t"v" =>
        DaemonEvent.Verify

      case t"i" =>
        val stdin: Stdin = if line() == t"p" then Stdin.Pipe else Stdin.Terminal
        val pid: Pid = Pid(line().as[Int])
        val userId: Int = line().as[Int]
        val username: Text = line()
        val login = Login(username, userId)
        val script: Text = line()
        val pwd: Text = line()
        val count: Int = line().as[Int]
        val textArguments: List[Text] = List.of(chunk().cut(t"\u0000").stdlib.take(count))
        val environment: List[Text] = List.of(chunk().cut(t"\u0000").stdlib.init)

        DaemonEvent.Init(pid, login, pwd, script, stdin, textArguments, environment)

      case _ =>
        Unset

    (message: @unchecked) match
      case Unset =>
        Log.warn(DaemonLogEvent.UnrecognizedMessage)
        connection.close()

      case DaemonEvent.Trap(pid, signal) =>
        Log.info(DaemonLogEvent.ReceivedSignal(signal))

        val response: SignalResponse =
          safely:
            val invocation = client(pid).invocation.await(250.0*Milli(Second))
            invocation.asInstanceOf[Cli].dispatchSignal(signal)

          . or(SignalResponse.Reject)

        val ackByte: Byte = if response == SignalResponse.Accept then 'a' else 'r'
        safely(rawOut.write(scala.Array[Byte](ackByte, '\n'.toByte)))
        safely(rawOut.flush())
        connection.close()

      // The launcher asks whether this daemon's launcher file still has the content
      // it was started from — sent when the file's mtime disagrees with the build
      // file. The verdict is a single byte: `f` (fresh: a metadata-only change,
      // remembered so the hash is not repeated) or `s` (stale: reply, then take the
      // termination path; the launcher awaits this daemon's death and starts a
      // fresh one).
      case DaemonEvent.Verify =>
        val fresh = verifyScript()
        val verdict: Byte = if fresh then 'f' else 's'
        safely(rawOut.write(scala.Array[Byte](verdict, '\n'.toByte)))
        safely(rawOut.flush())
        connection.close()

        if !fresh then
          Log.warn(DaemonLogEvent.Termination)
          termination

      case DaemonEvent.Exit(pid) =>
        Log.fine(DaemonLogEvent.ExitStatusRequest(pid))
        val exitStatus: Exit = client(pid).exitPromise.await()

        rawOut.write(Array.unsafeJvm(exitStatus().show.in[Data]))
        connection.close()
        clients.remove(pid)
        if terminatePid() == pid then termination

      case DaemonEvent.Stderr(pid) =>
        Log.fine(DaemonLogEvent.StderrRequest(pid))
        val stderrClient = client(pid)
        stderrClient.stderr.offer(rawOut)
        safely(stderrClient.exitPromise.await())
        safely(connection.close())

      case DaemonEvent.Control(pid) =>
        Log.fine(DaemonLogEvent.ControlRequest(pid))
        val controlClient = client(pid)
        controlClient.control.offer(rawOut)
        safely(controlClient.exitPromise.await())
        safely(connection.close())

      case DaemonEvent.Init(pid, login, directory, script, shellInput, textArguments, env) =>
        Log.fine(DaemonLogEvent.Init(pid))
        val clientState = client(pid)
        clientState.socket.fulfill(connection)

        // The stream crosses the capability-erasing `java.io.OutputStream` interface, so the
        // monitor its first write suspends on cannot stay tracked; laundered to pure, sound
        // because it is the daemon-lifetime connection monitor, which outlives the stream.
        val monitor0: Monitor^{} = caps.unsafe.unsafeAssumePure(summon[Monitor])

        val lazyStderr: ji.OutputStream = new ji.OutputStream():
          private val stderrOut: juca.AtomicReference[ji.OutputStream | Null] =
            juca.AtomicReference(null)

          private def connected(): ji.OutputStream =
            val s = stderrOut.get()

            if s != null then s
            else
              val newS = clientState.stderr.await()(using monitor0)
              stderrOut.set(newS)
              newS

          def write(i: Int): Unit = connected().write(i)
          override def write(bytes: scala.Array[Byte] | Null): Unit = connected().write(bytes)

          override def write(bytes: scala.Array[Byte] | Null, offset: Int, length: Int): Unit =
            connected().write(bytes, offset, length)

          override def close(): Unit =
            val s = stderrOut.get()
            if s != null then safely(s.close())

        given environment: Environment = LazyEnvironment(env)

        val termcap: Termcap = new Termcap:
          def ansi: Boolean = true

          lazy val color: ColorDepth =
            import workingDirectories.systemWorkingDirectory

            if safely(Environment.colorterm[Text]) == t"truecolor" then ColorDepth.TrueColor
            else
              ColorDepth
                ( safely(mute[Exec.Event](sh"tput colors".exec[Text]().as[Int])).or(-1) )

        val stdio: Stdio =
          Stdio
            ( ji.PrintStream(rawOut, true),
              ji.PrintStream(lazyStderr, true),
              in,
              termcap )

        // Only the launcher can change the client's terminal mode, so the request travels back
        // over its control channel. A launcher that never opened one (a pipe, or a stub built
        // before the channel existed) leaves the promise unfulfilled: the brief await expires
        // and the command runs with the raw mode it would have had anyway.
        def setMode(mode: Tty): Unit =
          safely(clientState.control.await(0.1*Second)(using monitor = monitor0)).let: out =>
            out.write(mode.byte)
            out.flush()

        def deliver(sourcePid: Pid, message: bus): Unit =
          clients.each: (pid, client) =>
            if sourcePid != pid then client.receive(message)

        // Generated lazily and memoized: re-runs the application's pure portion in
        // tab-completion mode to discover its subcommand/flag tree. Only the completions
        // executive can produce a tree; others yield `Unset` and `service.help()` falls back.
        // The help view, the service handle and each client invocation all share the same
        // single-owner daemon state; none is an aliased writer.
        lazy val helpValue: Optional[Help] =
          scala.caps.unsafe.unsafeAssumeSeparate:
           executive.help(name, environment, () => directory, stdio, login):
             (interface: executive.Interface) ?=> block(using service, interface, environment, summon[Monitor])

        lazy val service: DaemonService[bus] =
          scala.caps.unsafe.unsafeAssumeSeparate:
           DaemonService[bus]
             ( pid,
               () => shutdown(pid),
               shellInput,
               script.as[Path on Local],
               deliver(pid, _),
               clientState.bus.lazyList,
               name,
               startTime,
               () => helpValue,
               setMode )

        Log.fine(DaemonLogEvent.NewCli)

        try
          val cli: executive.Interface =
            scala.caps.unsafe.unsafeAssumeSeparate:
             executive.invocation
               ( textArguments.stdlib,
                 environment,
                 () => directory,
                 stdio,
                 service,
                 login )

          clientState.invocation.offer(cli.asInstanceOf[AnyRef])

          if cli.proceed then
            val result = scala.caps.unsafe.unsafeAssumeSeparate:
              block(using service, cli, environment, summon[Monitor])

            val exitStatus: Exit =
              scala.caps.unsafe.unsafeAssumeSeparate(executive.process(cli)(result))

            clientState.exitPromise.fulfill(exitStatus)
          else
            clientState.exitPromise.fulfill(Exit.Ok)

        catch
          case exception: Exception =>
            Log.fail(DaemonLogEvent.Failure)
            clientState.exitPromise.fulfill(handler.handle(exception)(using stdio))

        finally
          lazyStderr.close()
          connection.close()
          Log.info(DaemonLogEvent.CloseConnection(pid))

  application(using executives.directExecutive(using backstops.silentBackstop))(Nil.stdlib):
    import environments.javaEnvironment
    import termcaps.environmentTermcap
    import stdios.virtualMachineStdio
    import probates.awaitProbate

    Os.intercept[Shutdown]:
      wipeState()

    supervise:
      import logFormats.standardLogFormat
      given syslog: Logger[DaemonLogEvent, Message] = Logger(Syslog(t"ethereal"))

      safely(socketFile.wipe())

      val domainSocket: DomainSocket = DomainSocket(socketFile.encode)

      // The timer's callback logs through the same single-owner syslog.
      val inactivityTimer: Timeout^ = scala.caps.unsafe.unsafeAssumeSeparate:
       Timeout(idleTimeout):
        Log.warn(DaemonLogEvent.IdleTimeout)
        termination

      // Bind and start accepting *before* the build- and pid-files are written:
      // a launcher waits for those readiness files to appear and then connects,
      // so the socket must already be listening by the time they exist.
      // (`listenConnections` binds synchronously and serves on its own daemon, so
      // the bound socket queues connections immediately even before the files are
      // created.) It is a loan, so the whole serving lifetime — readiness files,
      // the pid-watcher, and the park — nests inside its block; the daemon only
      // ever leaves it via `termination`'s `System.exit`.
      val acceptor = (connection: Connection) =>
        inactivityTimer.nudge()
        scala.caps.unsafe.unsafeAssumeSeparate(safely(makeClient(connection)))
        ()

      // Everything under the accept loop shares the daemon's single-owner state (the
      // syslog, timer and monitor); nothing is an aliased writer.
      scala.caps.unsafe.unsafeAssumeSeparate:
       safely:
        domainSocket.listenConnections(acceptor):
          val buildId = safely(System.properties.build.id[Int]()).or:
            safely((Classpath/"build.id").read[Text].trim.as[Int]).or(0)

          scriptPath.let: script =>
            safely:
              import anticipation.instantiables.instantInstantiable
              hashScript(script).let: hash =>
                scriptIdentity.set(ScriptIdentity(script.size().long, script.modified[Long](), hash))

          // Record the launcher this daemon was started from as
          // `<buildId> <size> <mtimeMillis>`, so that the launcher's staleness
          // check can compare a later invocation's file against it; the first
          // field is read only by launchers that predate the content check. A size
          // mismatch displaces outright; an mtime mismatch makes the launcher ask
          // this daemon to verify itself (the `v` message), so content is hashed
          // at most once per change, by the party with the memory to avoid
          // repeating it. Without a launcher (plain `java -jar`) there is nothing
          // to compare, so only the build id is written.
          val buildLine: Text =
            val recorded = scriptIdentity.get()
            if recorded == null then t"$buildId"
            else t"$buildId ${recorded.size} ${recorded.mtime}"

          buildFile.open[File](Write, OpenFlag.Create)(file.write(buildLine))
          val pidValue = Process().pid.value.show
          pidFile.open[File](Write, OpenFlag.Create)(file.write(pidValue))

          task(n"pid-watcher"):
            safely:
              // Watching the launcher itself is defence-in-depth: a rebuild that
              // rewrites it in place corrupts the zip this JVM is running from, so
              // the daemon must die promptly rather than serve broken no-ops.
              // Evaluating `ownsState` here forces the classes the handler needs
              // while the jar is still readable.
              ownsState
              val watched: List[Path on Local] =
                scriptPath.let(List(socketFile, buildFile, pidFile, _))
                . or(List(socketFile, buildFile, pidFile))

              watched.stdlib
              . open[Watch](): watcher ?=>
                watcher.stream.each:
                  case event@(Delete(_, _) | Modify(_, _) | NewFile(_, _)) =>
                    val eventFile: Text = event match
                      case Delete(_, file)  => file
                      case Modify(_, file)  => file
                      case NewFile(_, file) => file
                      case other            => t""

                    // A metadata-only change to the launcher (`touch`) leaves its
                    // content intact; verify it before treating the event as fatal
                    // so that only a real rewrite terminates the daemon. State-file
                    // events are always fatal. The launcher and the state files are
                    // in different directories, but filenames suffice to tell them
                    // apart: the state files' names are fixed (`build`/`pid`/
                    // `socket`), and the launcher bears the application's name.
                    val benign = scriptPath.lay(false): script =>
                      eventFile == script.name && verifyScript()

                    if !benign then
                      Log.warn(DaemonLogEvent.Termination)
                      termination

                  case other =>
                    ()

          // The accept loop runs on its own daemon inside `listenConnections`, so
          // park the supervisor here to keep the daemon process alive until
          // `termination` (an idle timeout, a watched-file change, or an explicit
          // shutdown) calls `System.exit`.
          Promise[Unit]().await()

    Exit.Ok

  ???
