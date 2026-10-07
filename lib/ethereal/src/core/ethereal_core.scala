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
import java.nio.charset as jnc
import java.nio.file as jnf
import scala.collection.concurrent as scc

import ambience.*, systems.javaBaseSystem
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
import hieroglyph.*, codepages.utf8Codepage, charsets.utf8Charset
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

import filesystemOptions.deleteRecursively

import filesystemBackends.javaBaseFilesystem

// The `build_id` of the executable's record, which the launcher passes as `build.id`: a 64-bit
// value, which releases derive from their version. A `build.id` resource stands in for it when
// there is no launcher; zero when there is neither.
private[ethereal] def launcherBuildId(): Long =
  // As `startTime`: the decoder and the tactic are both fresh, under one `safely`.
  scala.caps.unsafe.unsafeAssumeSeparate:
    safely(System.properties.build.id[Long]()).or:
      safely((Classpath/"build.id").read[Text].trim.as[Long]).or(0L)

def resident[bus <: Matchable](using resident: Resident over bus)
:   (Resident over bus)^{resident} =

  resident

def cli[bus <: Matchable](using executive: Executive)
  ( block: (Resident over bus, executive.Interface, Environment, Monitor) ?=> executive.Return )
  ( using interpreter: Interpreter,
          threading:   Threading,
          handler:     Backstop )
:   Unit =

  import strategies.throwUnsafely
  import workingDirectories.systemWorkingDirectory
  import environments.javaBaseEnvironment
  import termcaps.environmentTermcap
  import stdios.fileDescriptorStdio

  val name: Text =
    recover:
      case Property.Error(_) =>
        val jarFile: Path on Linux = System.properties.java.`class`.path[Text]().pipe: jarFile =>
          safely(jarFile.as[Path on Linux]).or:
            val work: Path on Linux = workingDirectory
            work + jarFile.as[Relative on Linux]

        val work: Path on Linux = workingDirectory
        val relativeJar: Relative on Linux = work.toward(jarFile)
        Out.println(e"$Bold(This application must be invoked through its XEK launcher.)")
        Out.println(e"Build one with:")
        Out.println(e"    xek build $Italic($relativeJar) $Italic(<name>)")
        Out.println()
        Out.println(e"Install `xek` with $Italic(curl -fsSL https://propensive.dev/xek | sh); see $Italic(https://github.com/propensive/xek)")
        Exit.Fail(1).terminate()

    . protect(System.properties.ethereal.name[Text]())

  val userId: Optional[UserId] = scala.caps.unsafe.unsafeAssumeSeparate:
    safely(System.properties.ethereal.user.id[Text]()).let(UserId(_))
  val userName: Optional[Text] = safely(System.properties.ethereal.user.name[Text]())

  val startTime: Long =
    scala.caps.unsafe.unsafeAssumeSeparate:
      safely(System.properties.ethereal.startTime[Long]()).or(jl.System.currentTimeMillis())

  // Read now, before the socket is bound: the launcher sends the environment it was started with
  // only while it waits for the socket to appear.
  DaemonEnvironment.capture()

  // Use `Directories.*` rather than `Xdg.*` so Windows resolves to a native
  // location (`%LOCALAPPDATA%\Temp`) instead of `%USERPROFILE%\.local\state`,
  // which the Rust launcher in `state.rs` does not look at.
  val runtimeDir: Optional[Path on Local] = Directories.runtimeDir
  val stateHome: Path on Local = Directories.stateHome
  val baseDir: Path on Local = runtimeDir.or(stateHome)
  val buildFile: Path on Local = baseDir/name/"build"
  val pidFile: Path on Local = baseDir/name/"pid"
  val socketFile: Path on Local = baseDir/name/"socket"
  val acceptanceFile: Path on Local = baseDir/name/"acceptance"
  val clients: scc.TrieMap[Pid, Client of bus] = scc.TrieMap()
  val idleTimeout = Quantity[Hours[1]](6.0)

  // Set once the daemon has been asked to go — by `shutdown`, by a stale verdict, or by an
  // invocation's `retire` — after which no new invocation is served, and the daemon exits
  // when the last in flight ends, or after `drainLimit` regardless: a launcher displacing
  // a stale daemon waits a bounded time for its death, and a successor must never be held
  // up by a predecessor's long job.
  val draining: Atomic.Bool = Atomic(false)
  val drainLimit: Long = 30_000L

  // The user who owns the socket — this daemon's user — which every connecting peer must be.
  lazy val socketOwner: Optional[Text] =
    safely(jnf.Files.getOwner(jnf.Path.of(socketFile.encode.s)).nn.getName().nn.tt)

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
      safely(acceptanceFile.wipe())

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
    import gastronomy.*, providers.javaBaseProvider
    import monotonous.*, alphabets.hexLowerCase
    script.open[File](Read)(file.checksum[Sha2[256]].serialize[Hex])

  // The identity of the launcher this daemon was started from, hashed once at
  // startup — which also preloads the hashing classes, so verification can still
  // run once the jar has been clobbered underneath the JVM. In-memory only: the
  // launcher compares mtimes against the build FILE, but the authoritative record
  // is here, because rewriting the build file would trip the pid-watcher.
  val scriptIdentity: Atomic[Optional[ScriptIdentity]] = Atomic(Unset)

  // `verifyScript` hashes under this rather than under the cell it reads, which is what
  // `scriptIdentity.synchronized` used to do. An opaque type does not expose `synchronized` —
  // its public face is its declared bound, not `AnyRef` — and locking on the cell was in any
  // case locking on the thing being read rather than on the operation being made exclusive.
  val hashing: Mutex = Mutex()

  // True if the launcher's content is unchanged. mtime is the cheap gate: after a
  // metadata-only `touch`, the content is hashed once and the remembered mtime is
  // updated, so every later verification is a single `stat` again. Synchronized so
  // that concurrent clients cause at most one hash. Doubt — an unreadable or
  // deleted launcher — resolves to stale.
  def verifyScript(): Boolean = hashing:
    scriptPath.lay(true): script =>
      scriptIdentity().lay(true): recorded =>
        safely:
          import anticipation.instantiables.epochMillisecondsInstantiable
          val size = script.filesize().long
          val mtime = script.modified[Long]()

          if size != recorded.size then false
          else if mtime == recorded.mtime then true
          else hashScript(script).lay(false): hash =>
            if hash == recorded.hash then
              scriptIdentity() = recorded.copy(mtime = mtime)
              true
            else false

        . or(false)

  // Stops serving new invocations and lets those in flight finish, then exits; at once if
  // nothing is in flight. Logging is best-effort: after a stale verdict the jar may already
  // have been rewritten underneath this JVM, and the exit must not depend on a class loading.
  def drain(): Unit logs DaemonLogEvent =
    if !draining.swap(true) then
      try Log.warn(DaemonLogEvent.Draining) catch case _: jl.Throwable => ()

      val limit: Runnable = () =>
        jl.Thread.sleep(drainLimit)
        termination

      val bound = jl.Thread(limit)
      bound.setDaemon(true)
      bound.start()
      if clients.isEmpty then termination


  def makeClient(connection: Connection)(using Monitor, Stdio, Probate)
    ( using Tactic[Truncation.Error], Tactic[Charset.Error], Tactic[Number.Error],
            (DaemonLogEvent is Loggable)^ )
  :   Unit =

    val rawIn: ji.InputStream = connection.reader
    val rawOut: ji.OutputStream = connection.writer
    val in: ji.BufferedInputStream = ji.BufferedInputStream(rawIn, 16384)

    // The kernel's account of the peer's user, where the platform gives one, must be this
    // daemon's own: the socket's mode is a snapshot, credentials are the fact (#18). A peer
    // that is refused is not read at all.
    val peerRefused: Boolean =
      connection.peer.let { peer => socketOwner.let(_ != peer).or(false) }.or(false)

    // Every connection opens with one BinTEL document of the launcher schema
    // (`Launcher.schemaText`); the bytes that follow it belong to the message.
    val document: Optional[Data] = if peerRefused then Unset else Launcher.readDocument(in)
    val message: Optional[Launcher.Message] = document.let(Launcher.decode(_))

    def reply(message: Launcher.Message): Unit =
      safely(rawOut.write(Array.unsafeJvm(Launcher.encode(message))))
      safely(rawOut.flush())

    (message: @unchecked) match
      case Unset =>
        // A document that framed but carried another schema's signature is a launcher
        // built against a different contract; anything else is not BinTEL at all.
        if peerRefused then Log.warn(DaemonLogEvent.PeerRefused(connection.peer.or(t"")))
        else if document.present then Log.warn(DaemonLogEvent.ProtocolMismatch)
        else Log.warn(DaemonLogEvent.UnrecognizedMessage)
        connection.close()

      // The launcher asks the daemon to exit once in-flight invocations end; not answered.
      case Launcher.Message.Shutdown =>
        connection.close()
        drain()

      // The launcher asks whether this daemon's launcher file still has the content
      // it was started from — sent when the file's mtime disagrees with the build
      // file. The verdict is a document: fresh (a metadata-only change, remembered so
      // the hash is not repeated) or stale (reply, then take the termination path; the
      // launcher awaits this daemon's death and starts a fresh one).
      case Launcher.Message.Verify =>
        val fresh = verifyScript()

        // A stale launcher means the jar has been rewritten underneath this JVM, so a class
        // first touched here — the verdict's, or the log event's — may fail to load, with a
        // linkage error. The termination must not depend on any of that succeeding: a
        // daemon which survives its own displacement serves nothing, while the launcher,
        // which awaits its death, fails every client that tries.
        try
          reply(Launcher.Message.Verdict(fresh))
          connection.close()
          if !fresh then Log.warn(DaemonLogEvent.Termination)
        finally
          if !fresh then drain()

      case _: (Launcher.Message.SignalAck | Launcher.Message.Verdict | Launcher.Message.Mode
               | Launcher.Message.ExitStatus | Launcher.Message.Open | Launcher.Message.Data
               | Launcher.Message.End | Launcher.Message.Credit | Launcher.Message.Closed
               | Launcher.Message.Signal) =>
        // Replies travel from the daemon to the launcher, never the other way, and the rest
        // belong inside a session, after `init`.
        Log.warn(DaemonLogEvent.UnrecognizedMessage)
        connection.close()

      // A daemon that is draining serves no new invocation, and a client whose claimed user
      // is not this daemon's is refused (#18). Either way the session ends at once with the
      // exit status, rather than closing on a launcher that would read that as a daemon of
      // another protocol.
      case Launcher.Message.Init(pid0, uid, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)
          if draining() || userId.let(_ != UserId(uid)).or(false) =>
        val pid = Pid(pid0)
        if draining() then Log.warn(DaemonLogEvent.Refused(pid))
        else Log.warn(DaemonLogEvent.PeerRefused(uid))
        reply(Launcher.Message.ExitStatus(2))
        connection.close()

      case Launcher.Message.Init(pid0, uid, username, script, directory, stdinTty, stdoutTty,
                                 stderrTty, textArguments, env, invokedAs, umask, columns, rows,
                                 inputCodepage, outputCodepage, descriptors, raws) =>
        val pid = Pid(pid0)
        val login = Login(username, UserId(uid))

        // Each stream is reported separately by the launcher, and they genuinely differ:
        // `command > file` run from a terminal has stdin on the terminal and stdout on a
        // file. This is the only place the daemon can learn it.
        def terminus(tty: Boolean): Terminus = if tty then Terminus.Terminal else Terminus.Pipe
        val shellInput = terminus(stdinTty)
        val shellOutput = terminus(stdoutTty)
        val shellError = terminus(stderrTty)
        Log.fine(DaemonLogEvent.Init(pid))
        val clientState = client(pid)

        // The signal's trap runs on the invocation's `Cli`, which the session's reader may
        // be asked for before the invocation has made it: a brief wait, then a rejection.
        val monitor0: Monitor^{} = caps.unsafe.unsafeAssumePure(summon[Monitor])

        def dispatch(signal: Signal): SignalResponse =
          safely:
            val invocation = clientState.invocation.await(250.0*Milli(Second))
            invocation.asInstanceOf[Cli].dispatchSignal(signal)

          . or(SignalResponse.Reject)

        val dispatch0: Signal -> SignalResponse = caps.unsafe.unsafeAssumePure(dispatch)
        val log0: DaemonLogEvent -> Unit = caps.unsafe.unsafeAssumePure(event => Log.info(event))
        val session: Session = Session(connection, in, descriptors, dispatch0, log0)
        columns.let { columns => rows.let { rows => session.windowSize() = (columns, rows) } }
        session.start()

        given environment: Environment = LazyEnvironment(env)

        // The size is read through a block local, not through `session`, so the termcap
        // — and the stdio built on it — stay pure, as `Stdio`'s `termcap` member requires.
        val windowSize0 = session.windowSize

        val termcap: Termcap = new Termcap:
          def ansi: Boolean = true

          // The client terminal's size, measured by the launcher with `TIOCGWINSZ` and sent in
          // the `init` document and with every `WINCH`, so both read live; `COLUMNS` and
          // `LINES`, which the launcher also injects, are the fallback for a launcher that sent
          // no size. Absent when the client's output is not a terminal, in which case the
          // width stays unbounded and nothing wraps.
          override def width: Int =
            windowSize0().let(_(0)).or(safely(Environment.columns[Text].s.toInt).or(Int.MaxValue))

          override def height: Int =
            windowSize0().let(_(1)).or(safely(Environment.lines[Text].s.toInt).or(Int.MaxValue))

          lazy val color: ColorDepth =
            import workingDirectories.systemWorkingDirectory

            if safely(Environment.colorterm[Text]) == t"truecolor" then ColorDepth.TrueColor
            else
              ColorDepth
                ( safely(mute[Exec.Event](sh"tput colors".exec[Text]().as[Int])).or(-1) )

        // A Windows console's code pages (#17): what the client's bytes mean, and what it can
        // display. Absent elsewhere, and for a console that is UTF-8, where the JVM's default
        // applies.
        def charset(page: Optional[Int]): Optional[jnc.Charset] = page.let: page =>
          if page == 65001 then jnc.StandardCharsets.UTF_8.nn
          else Encoding.unapply(t"cp$page").map(_.charset).getOrElse(Unset)

        val stdout: ji.OutputStream = Outlet(t"stdout", session.stdout, session.stdout.severed)
        val stderr: ji.OutputStream = Outlet(t"stderr", session.stderr, session.stderr.severed)

        def printStream(out: ji.OutputStream, page: Optional[Int]): ji.PrintStream =
          charset(page).lay(ji.PrintStream(out, true)) { charset => ji.PrintStream(out, true, charset) }

        val input: ji.InputStream =
          charset(inputCodepage).lay(session.stdin): charset =>
            if charset == jnc.StandardCharsets.UTF_8 then session.stdin
            else Transcoder(session.stdin, charset)

        val stdio: Stdio =
          Stdio
            ( printStream(stdout, outputCodepage),
              printStream(stderr, outputCodepage),
              input,
              termcap )

        // Only the launcher can change the client's terminal mode, so the request travels back
        // over the session; a launcher whose stdin is not a terminal it owns ignores it, and
        // the command runs with the raw mode it would have had anyway.
        def setMode(mode: Tty): Unit =
          session.send(Launcher.Message.Mode(mode.canonical, mode.echo))

        def deliver(sourcePid: Pid, message: bus): Unit =
          clients.each: (pid, client) =>
            if sourcePid != pid then client.receive(message)

        // Generated lazily and memoized: re-runs the application's pure portion in
        // tab-completion mode to discover its subcommand/flag tree. Only the completions
        // executive can produce a tree; others yield `Unset` and `resident.help()` falls back.
        // The help view, the resident handle and each client invocation all share the same
        // single-owner daemon state; none is an aliased writer.
        lazy val helpValue: Optional[Help] =
          scala.caps.unsafe.unsafeAssumeSeparate:
           executive.help(name, environment, () => directory, stdio, login):
             (interface: executive.Interface) ?=> block(using resident, interface, environment, summon[Monitor])

        lazy val resident: Resident over bus =
          scala.caps.unsafe.unsafeAssumeSeparate:
           new Resident
             ( pid,
               () => drain(),
               shellInput,
               shellOutput,
               shellError,
               script.as[Path on Local],
               name,
               startTime,
               () => helpValue,
               setMode,
               session.terminal,
               invokedAs,
               () => windowSize0(),
               umask.let(Umask.parse(_)),
               session.fdtable,
               raws ):
             type Transport = bus
             def bus: Chain[Transport] = clientState.bus.chain
             def broadcast(message: Transport): Unit = deliver(this.pid, message)

        Log.fine(DaemonLogEvent.NewCli)

        // Whatever happens below — even the backstop itself throwing — the session must end
        // with a status, or the launcher waits forever (#2033).
        var exitStatus: Exit = Exit(2)

        try
          val cli: executive.Interface =
            scala.caps.unsafe.unsafeAssumeSeparate:
             executive.invocation
               ( textArguments,
                 environment,
                 () => directory,
                 stdio,
                 resident,
                 login )

          clientState.invocation.offer(cli.asInstanceOf[AnyRef])

          if cli.proceed then
            val result = scala.caps.unsafe.unsafeAssumeSeparate:
              block(using resident, cli, environment, summon[Monitor])

            exitStatus = scala.caps.unsafe.unsafeAssumeSeparate(executive.process(cli)(result))
          else exitStatus = Exit.Ok

        // `Throwable`, not `Exception`: a `java.lang.Error` (a linkage error from a stale class
        // file, say) would otherwise escape. The backstop already distinguishes the two,
        // exiting with status 2 for a non-exception throwable.
        catch
          // A write to a stream whose reader is gone ends the invocation as `SIGPIPE` would
          // end a process, with the status a shell reports for that death (128 + 13). The
          // backstop is not consulted: it would write to the same stream.
          case error: Outlet.Error =>
            exitStatus = Exit(141)

          case throwable: Throwable =>
            Log.fail(DaemonLogEvent.Failure(throwable.toString.tt))
            exitStatus = handler.handle(throwable)(using stdio)

        finally
          // The outputs' last chunks go behind everything the invocation wrote, and the exit
          // status behind them; the connection closes once the launcher has it all.
          safely(stdout.flush())
          safely(stderr.flush())
          session.finish(exitStatus())
          session.awaitWritten()
          connection.close()
          Log.info(DaemonLogEvent.CloseConnection(pid))
          clients.remove(pid)
          if draining() && clients.isEmpty then termination

  // `application` takes a stdlib `Iterable` of arguments, which the opaque `Nil` is not.
  application(using executives.directExecutive(using backstops.silentBackstop))(Nil):
    import environments.javaBaseEnvironment
    import termcaps.environmentTermcap
    import stdios.fileDescriptorStdio
    import probates.awaitProbate

    Os.intercept[Shutdown]:
      wipeState()

    supervise:
      import logFormats.timestampedLogFormat
      given syslog: Logger[DaemonLogEvent, Message] = Logger(Syslog(t"ethereal"))

      safely(socketFile.wipe())

      // What this daemon reads, for the launcher to consult before it connects (xek's
      // `spec/layout.md`, *Negotiating the composition*): a BinTEL §8.4 acceptance in its bare
      // form, with one alternative, the base schema alone — one root child, the `accept`
      // member with one child, its 33-byte `schema`. Written before the socket exists, so a
      // launcher that has found the socket may rely on it.
      safely:
        val acceptance: Data =
          Array.unsafeFrozen:
            scala.Array.concat(scala.Array[Byte](1, 0, 1, 0, 33), Array.unsafeJvm(Launcher.signature))

        acceptanceFile.open[File](Write, OpenFlag.Create, OpenFlag.Truncate)(file.write(Chain(acceptance)))

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
        domainSocket.listenConnections(acceptor, ownerOnly = true):
          val buildId = launcherBuildId()

          scriptPath.let: script =>
            safely:
              import anticipation.instantiables.epochMillisecondsInstantiable
              hashScript(script).let: hash =>
                scriptIdentity() =
                  ScriptIdentity(script.filesize().long, script.modified[Long](), hash)

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
            scriptIdentity().lay(t"$buildId"): recorded =>
              t"$buildId ${recorded.size} ${recorded.mtime}"

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

              // `Watch.allOpenable` is bound to `Iterable`, which the opaque `List` is not.
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

                    // As for a stale verdict above: the rewritten jar may make the log
                    // event's class unloadable, and termination must not wait on it.
                    if !benign then
                      try Log.warn(DaemonLogEvent.Termination) finally termination

                  case other =>
                    ()

          // The accept loop runs on its own daemon inside `listenConnections`, so
          // park the supervisor here to keep the daemon process alive until
          // `termination` (an idle timeout, a watched-file change, or an explicit
          // shutdown) calls `System.exit`.
          Promise[Unit]().await()

    Exit.Ok

  ???
