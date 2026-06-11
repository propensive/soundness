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
package flux

import java.io as ji
import java.lang as jl
import java.net as jn
import java.nio.file as jnf
import java.util.concurrent as juc

import scala.collection.concurrent.TrieMap
import scala.collection.mutable as scm

import soundness.*

import backstops.silent
import charEncoders.utf8
import classloaders.threadContext
import executives.completions
import harlequin.Accent
import internetAccess.enabled
import interpreters.posix
import logging.silent
import probates.cancel
import supervisors.global
import systems.java
import temporaryDirectories.system
import threading.platform

// A front-end for a Flux REPL. With a TCP port, `flux serve <port>` runs a
// server and `flux <port>` connects to one on localhost. With no port,
// `flux serve` opens a per-process UNIX domain socket (named after the JVM's
// PID, under the XDG runtime dir) and `flux` connects to it — starting its own
// background server first if none is running (so `flux` alone is a self-contained
// REPL whose session, living in the separate server process, can be reconnected to by
// running `flux` again), picking the lone running server, or listing them if there
// are several. Either way the client drives an interactive, live-highlighted session
// until Ctrl+D, Ctrl+C or `/quit`.
@main
def repl(): Unit = cli:
  arguments match
    case Argument("serve") :: Argument(As[Int](portNumber)) :: Nil =>
      execute(serve(portNumber))

    case Argument("serve") :: Nil =>
      execute(serveSocket())

    case Argument(As[Int](portNumber)) :: Nil =>
      execute:
        safely(Port[Tcp](portNumber)).lay(invalidPort(portNumber)): port =>
          connect(port)(converse(_)).or(unreachable(portNumber))

    case Nil =>
      execute(connectSocket())

    // TEMPORARY keyboard-diagnostic mode: print each keypress instead of editing.
    case Argument("keys") :: Argument("kitty") :: Nil =>
      execute(keyTest(kitty = true))

    case Argument("keys") :: Nil =>
      execute(keyTest(kitty = false))

    case _ =>
      execute(Exit.Fail(1))

// Runs a REPL server on the given TCP port and blocks until interrupted.
private def serve(portNumber: Int)(using Stdio, Monitor, Probate, System): Exit =
  given Scalac[3.8] = Scalac(Nil)
  given Classloader = serverClassloader

  safely(Port[Tcp](portNumber)).lay(invalidPort(portNumber)): port =>
    whereas:
      case BindError(_) => Out.println(t"flux: port $portNumber is unavailable"); Exit.Fail(5)
      case error: Error => Out.println(t"flux: ${error.message}"); Exit.Fail(6)

    . recover:
        val repl    = Repl()
        val service = repl.serve(port)
        Out.println(t"flux: serving a REPL on port $portNumber (Ctrl+C or /quit to stop)")
        repl.awaitQuit()
        service.stop()
        Exit.Ok

// The directory holding per-process REPL sockets, and this process's socket file.
// UNIX domain sockets are a Unix-only feature, so the directory follows
// `$XDG_RUNTIME_DIR` (then `$TMPDIR`, then `/tmp`) directly, as plain `Text`.
private def envText(name: String): Optional[Text] = Optional(jl.System.getenv(name)).let(_.nn.tt)

private def socketDirectory: Text =
  t"${envText("XDG_RUNTIME_DIR").or(envText("TMPDIR")).or(t"/tmp")}/flux"

private def socketFile: Text = t"$socketDirectory/${ProcessHandle.current.nn.pid}.sock"

// Runs a REPL server on a per-process UNIX domain socket (used when no port is
// given) and blocks until quit, unlinking the socket file on the way out.
private def serveSocket()(using Stdio, Monitor, Probate, System): Exit =
  given Scalac[3.8] = Scalac(Nil)
  given Classloader = serverClassloader

  val socketPath: Text = socketFile

  // Delete the socket on any exit (a clean stop, a crash, or a kill signal), so it
  // never lingers as a stale file a later client has to clean up.
  val removeSocket: Runnable = () =>
    safely(jnf.Files.deleteIfExists(jnf.Path.of(socketPath.s)))
    ()

  jl.Runtime.getRuntime.nn.addShutdownHook(jl.Thread(removeSocket))

  try
    jnf.Files.createDirectories(jnf.Path.of(socketDirectory.s)).nn
    jnf.Files.deleteIfExists(jnf.Path.of(socketPath.s))

    val repl    = Repl()
    val service = repl.serve(socketPath)
    Out.println(t"flux: serving a REPL on $socketPath (Ctrl+C or /quit to stop)")
    repl.awaitQuit()
    service.stop()
    safely(jnf.Files.deleteIfExists(jnf.Path.of(socketPath.s)))
    Exit.Ok
  catch case error: Throwable =>
    Out.println(t"flux: could not serve on $socketPath: ${error.toString.tt}")
    Exit.Fail(6)

// Builds the classloader the REPL compiles against inside the Ethereal daemon.
// The launch classloader is not a `URLClassLoader`, so it exposes no compile
// classpath, and `java.class.path` is this executable — a shebang-prefixed jar
// with no `.jar` suffix, which dotc refuses to read as a library. We symlink it
// to a `.jar` path (a `ZipFile` reads the appended archive past the shebang) and
// hand the REPL a *parent-first* `URLClassLoader` over it: the jar lands on
// dotc's compile classpath so it finds the bundled scala libraries, while
// parent-first delegation keeps runtime classes (notably `ReplBridge`, whose
// session registry must be shared) identical to the daemon's. Outside the daemon
// (e.g. `ethereal.script` unset) we fall back to the thread-context loader.
private def serverClassloader(using System): Classloader =
  try
    val executable: Text = unsafely(System.properties.ethereal.script[Text]())
    val link: jnf.Path = jnf.Files.createTempDirectory("flux").nn.resolve("flux.jar").nn
    jnf.Files.createSymbolicLink(link, jnf.Path.of(executable.s)).nn
    val url: jn.URL = ji.File(link.toString).toURI.nn.toURL.nn
    new Classloader(jn.URLClassLoader(Array(url), threadContext.java))
  catch case _: Throwable => threadContext

private def invalidPort(portNumber: Int)(using Stdio): Exit =
  Out.println(t"flux: $portNumber is not a valid TCP port")
  Exit.Fail(2)

private def unreachable(portNumber: Int)(using Stdio): Exit =
  Out.println(t"flux: could not connect to localhost:$portNumber")
  Exit.Fail(3)

// Runs `body` over a fresh TCP connection — always closing it afterwards — or returns
// `Unset` if the connection is refused.
private def connect[result](port: Port over Tcp)(body: Duplex => result): Optional[result] =
  try (ip"127.0.0.1" via port).duplex(body) catch case _: ji.IOException => Unset

// As `connect`, but over a UNIX domain socket.
private def connectDomain[result](socket: DomainSocket)(body: Duplex => result): Optional[result] =
  try socket.duplex(body) catch case _: ji.IOException => Unset

private def unreachableSocket(path: Text)(using Stdio): Exit =
  Out.println(t"flux: could not connect to $path")
  Exit.Fail(3)

private def failedToLaunch(using Stdio): Exit =
  Out.println(t"flux: could not start a REPL server")
  Exit.Fail(3)

// Starts a REPL server in the background — a detached `flux serve` process on its
// own per-process domain socket — waits for it to bind, and connects to it. Because
// the server is a separate process it outlives this client, so the same session can
// be reconnected to later by running `flux` again. Returns the live connection,
// or `Unset` if no server became reachable in time.
private def launchServer[result]()(using Stdio, System)(body: Duplex => result): Optional[result] =
  safely(System.properties.ethereal.script[Text]()).lay(Unset): executable =>
    val before: List[Text] = socketPaths(socketDirectory)

    val builder = jl.ProcessBuilder(executable.s, "serve")
    builder.redirectOutput(jl.ProcessBuilder.Redirect.DISCARD)
    builder.redirectError(jl.ProcessBuilder.Redirect.DISCARD)
    builder.redirectInput(ji.File("/dev/null"))
    safely(builder.start())

    var result: Optional[result] = Unset
    var waited: Int              = 0

    // Poll for a new, connectable socket (the socket file appears once it binds), then
    // run `body` over the first one that connects.
    while result.absent && waited < 10000 do
      jl.Thread.sleep(100)
      waited += 100

      socketPaths(socketDirectory).each: candidate =>
        if result.absent && !before.contains(candidate) then
          result = connectDomain(DomainSocket(candidate))(body)

    result

// TEMPORARY keyboard diagnostic. Instead of the editor, print each terminal event
// as a Profanity `Keypress` (or other `TerminalEvent`), so we can see exactly how
// keys — including Shift+Enter — are decoded. With `kitty = true` the kitty
// keyboard protocol is enabled first. Ctrl+C or Ctrl+D stops it.
private def keyTest(kitty: Boolean)(using Stdio, Monitor, Probate, Console, Environment): Exit =
  whereas:
    case TerminalError() =>
      Out.println(t"flux: the terminal could not be initialised")
      Exit.Fail(4)

  . recover:
      interactive: terminal ?=>
        given Stdio = terminal.stdio
        if kitty then Out.print(t"\e[>1u")
        Out.print(t"flux: press keys to see how they decode; Ctrl+C or Ctrl+D to stop\r\n")
        val events = terminal.eventIterator()
        var running = true

        try
          while running && events.hasNext do
            val event = events.next()

            val rendered = event match
              case keypress: Keypress => keypress.show
              case other              => other.toString.tt

            Out.print(t"$rendered\r\n")

            event match
              case Keypress.Ctrl('C' | 'D') => running = false
              case _                        => ()
        finally
          if kitty then Out.print(t"\e[<u")

        Exit.Ok

// The full paths of the `.sock` files in the socket directory.
private def socketPaths(directory: Text): List[Text] =
  val listing: Array[ji.File | Null] | Null = ji.File(directory.s).listFiles()
  val names: scm.ArrayBuffer[Text] = scm.ArrayBuffer()

  if listing != null then
    var index = 0

    while index < listing.nn.length do
      val file = listing.nn(index)

      if file != null then
        val name: Text = file.nn.getName.nn.tt
        if name.ends(t".sock") then names += t"$directory/$name"

      index += 1

  names.to(List)

// Connects to a per-process UNIX domain socket. With no server running, launches one
// in the background and attaches to it (so `flux` alone is a self-contained REPL,
// reconnectable later); with exactly one, connects to it; with several, lists them.
private def connectSocket()(using Stdio, Monitor, Probate, Console, Environment, System): Exit =
  // Probe every socket file: a connectable one is live; one that refuses (a crashed or
  // killed server that never cleaned up) is a stale leftover, so delete it. Then attach
  // to the lone live server, list several, or — when none survive — start a fresh one.
  val live: scm.ArrayBuffer[Text] = scm.ArrayBuffer()

  socketPaths(socketDirectory).each: path =>
    if connectDomain(DomainSocket(path)) { _ => () }.absent
    then safely(jnf.Files.deleteIfExists(jnf.Path.of(path.s)))
    else live += path

  live.to(List) match
    case Nil =>
      Out.println(t"flux: starting a REPL server…")
      launchServer()(converse(_)).or(failedToLaunch)

    case path :: Nil =>
      connectDomain(DomainSocket(path))(converse(_)).or(unreachableSocket(path))

    case paths =>
      Out.println(t"flux: several REPL servers are running:")

      paths.each: path =>
        Out.println(t"  $path")

      Out.println(t"flux: stop all but one, or use a TCP server with 'flux <port>'")
      Exit.Fail(7)

// The read/edit/print loop. The server's reply is printed verbatim. Ctrl+C/Ctrl+D
// dismiss the line editor (`DismissError`) and end the session.
private def converse(duplex: Duplex)(using Stdio, Monitor, Probate, Console, Environment): Exit =
  // The kitty keyboard protocol (applied by `interactive`) makes the terminal report
  // Shift+Enter distinctly, so the editor can submit on it.
  import terminalFeatures.kittyKeyboard

  val state       = LiveState()
  val pending     = TrieMap[Int, Int]()                  // tokenize id → version
  val submits     = juc.LinkedBlockingQueue[Repl.Reply]()
  val completions = juc.LinkedBlockingQueue[List[Repl.CompletionItem]]()
  val nextId      = juc.atomic.AtomicInteger(1)          // 0 is reserved for submit

  whereas:
    case TerminalError() =>
      Out.println(t"flux: the terminal could not be initialised")
      Exit.Fail(4)

  . recover:
      interactive: terminal ?=>
        runRepl(duplex, state, pending, nextId, submits, completions)
        Exit.Ok

// One REPL input line is an inline block at the bottom of the console — a bordered,
// live-highlighted editor with, when completions are active, a pane above it listing
// them. On submit the block is frozen in place (`finish`), the result is printed below
// it, and the next line opens a fresh block beneath, so output scrolls up into the
// console's own scrollback above the editor. Rendering goes through ultimatum's
// `paint`/`InlineRoot`, so the editor's syntax colours and wide-character widths are
// exactly those of the cell grid.
private def runRepl
  ( duplex:      Duplex,
    state:       LiveState,
    pending:     TrieMap[Int, Int],
    nextId:      juc.atomic.AtomicInteger,
    submits:     juc.LinkedBlockingQueue[Repl.Reply],
    completions: juc.LinkedBlockingQueue[List[Repl.CompletionItem]] )
  ( using terminal: Terminal, monitor: Monitor )
:   Unit =

  given Stdio = terminal.stdio

  // `duplex.stream` blocks on its first socket read, so force the iterator lazily —
  // only once a request has been sent — otherwise it would deadlock before the editor
  // even starts (the server sends nothing until it receives a message).
  lazy val chunks: Iterator[Data] = duplex.stream.iterator
  @volatile var live = true

  // Background reader: route replies by kind. A `tokenize` reply refines the live
  // highlight and posts a `Redraw`, so the refined colour appears as soon as the server
  // answers — not one keypress later. Other replies are a `submit` result the loop
  // awaits, or the `completions` the Tab handler blocks on.
  async:
    // Construct the reader here, not on the main thread: forcing `chunks`
    // (`duplex.stream.iterator`) blocks until the first byte arrives, and the main
    // thread must stay free to render the editor and drive the event loop.
    val frames: FrameReader = FrameReader(chunks)

    while live do
      val data: Optional[Data] = frames.next()

      if data.absent then live = false
      else safely[Exception](Bintel.read[Repl.Reply](data.vouch)).let:
        case Repl.Reply.Tokenized(id, highlight) =>
          pending.remove(id).foreach(state.reconcile(_, highlight))
          terminal.events.put(TerminalInfo.Redraw)

        case Repl.Reply.Completed(_, items) =>
          completions.put(items)

        case reply =>
          submits.put(reply)

  val events = terminal.eventIterator()
  var running = true

  while running do
    val root = InlineRoot(terminal)
    var editor: LineEditor = LineEditor(mode = LineEditor.Mode.Multiline(balanced))
    var candidates: List[Repl.CompletionItem] = Nil
    var tokens: List[Repl.Token] = Nil
    var lastValue: Text = t""
    var lastVersion: Int = -1

    // Record the live-highlight edit and fire an async server `tokenize` to refine it.
    def refresh(): Unit =
      val (version, refreshed) = state.record(lastValue, editor.value)
      tokens = refreshed
      lastValue = editor.value

      if editor.value.length > 0 && version != lastVersion then
        lastVersion = version
        val id = nextId.getAndIncrement
        pending(id) = version
        duplex.send(Stream(framed(encode(Repl.Request.Tokenize(id, editor.value)))))

    def frame(): Unit =
      val innerWidth = (terminal.knownColumns - 2).max(1)
      val rows = LineEditor.cursorPosition(editor.value, editor.value.length, innerWidth)._1 + 1
      paint(root, replPane(editor, tokens, candidates, rows))
      root.flush()

    refresh()
    frame()
    var editing = true
    var submitted: Optional[Text] = Unset

    // The block is already drawn; now wait for events. `hasNext` blocks until a
    // keypress (or returns false when the input stream closes, ending the session).
    while editing do
      if !events.hasNext then
        editing = false
        running = false
      else
        events.next() match
          case Keypress.Ctrl('C' | 'D') => running = false; editing = false
          case Keypress.Escape          => running = false; editing = false

          case _: TerminalInfo.WindowSize =>
            root.invalidate()
            frame()

          // A tokenize reply arrived (posted by the reader): re-derive the highlight
          // from the reconciled checkpoint and repaint, with no edit and no new request.
          case TerminalInfo.Redraw =>
            tokens = state.record(editor.value, editor.value)._2
            frame()

          case Keypress.Tab =>
            val (advanced, shown) = completeAt(editor, duplex, completions)
            editor = advanced
            candidates = shown
            refresh()
            frame()

          case event if editor.submitsOn(event) =>
            submitted = editor.value
            editing = false

          case keypress: Keypress =>
            editor = editor(keypress)
            candidates = Nil
            refresh()
            frame()

          case _ =>
            ()

    // Freeze the block onto the screen (cursor drops below it) and print the result
    // there, so it scrolls above the next line's fresh block.
    root.finish()

    submitted.let: line =>
      if line == t"/disconnect" then
        running = false
      else if line == t"/quit" then
        duplex.send(Stream(framed(encode(Repl.Request.Quit(0)))))
        running = false
      else if line.starts(t"/") then
        Out.println(t"flux: unknown command: $line")
      else
        duplex.send(Stream(framed(encode(Repl.Request.Submit(0, line)))))

        submits.take().nn match
          case Repl.Reply.Ran(_, value, output, tpe, diagnostics, _) =>
            if output != t"" then Out.print(output)

            value.let: rendered =>
              tpe.lay(Out.println(rendered)): typeName =>
                Out.println(t"$rendered : $typeName")

            if diagnostics != t"" then Out.println(diagnostics)

          case Repl.Reply.Threw(_, output, diagnostics, _) =>
            if output != t"" then Out.print(output)
            Out.println(diagnostics)

          case Repl.Reply.Rejected(_, diagnostics, _) => Out.println(diagnostics)
          case Repl.Reply.Crashed(_, diagnostics, _)  => Out.println(diagnostics)
          case Repl.Reply.Failed(_, message)          => Out.println(message)
          case Repl.Reply.Tokenized(_, _)             => ()
          case Repl.Reply.Completed(_, _)             => ()

  // The loop has exited (Ctrl+D/C, Escape, /quit, or a closed stream): stop the reader.
  live = false

// The inline block: a bordered editor box showing the live-highlighted line with its
// caret, and — when completions are active — a pane above it listing them, which
// auto-sizes to the candidate count (capped) and vanishes when there are none.
private def replPane
  ( editor:     LineEditor,
    tokens:     List[Repl.Token],
    candidates: List[Repl.CompletionItem],
    rows:       Int )
:   Pane =

  val box = border(BorderStyle.rounded):
    panel(minHeight = rows, maxHeight = rows):
      val extent = summon[Extent]
      val cols = extent.width.max(1)
      extent.put(colourful(tokens))
      val (curRow, curCol) = LineEditor.cursorPosition(editor.value, editor.position, cols)
      extent.showCaret(curCol.z, curRow.z)

  if candidates.isEmpty then box
  else
    val shown = candidates.length.min(10)

    val list = panel(minHeight = shown, maxHeight = shown):
      candidates.take(shown).each: item =>
        Out.println(t"${item.name}  ${item.signature}")

    rank(list, box)

// On Tab, complete: a `/`-command line completes locally against `slashCommands`;
// otherwise the server is asked and we block for the reply. A unique candidate is
// inserted; with several, the longest common prefix is inserted and the candidates are
// returned for display.
private def completeAt
  ( editor:      LineEditor,
    duplex:      Duplex,
    completions: juc.LinkedBlockingQueue[List[Repl.CompletionItem]] )
:   (LineEditor, List[Repl.CompletionItem]) =

  if editor.value.starts(t"/") then
    slashCommands.filter { (name, _) => name.starts(editor.value) } match
      case (name, _) :: Nil =>
        (LineEditor(name, name.length, editor.mode), Nil)

      case matches =>
        val items =
          matches.map: (name, help) =>
            Repl.CompletionItem(name, t"command", help)

        val prefix = longestCommonPrefix(matches.map(_._1))

        val advanced =
          if prefix.length > editor.value.length then LineEditor(prefix, prefix.length, editor.mode)
          else editor

        (advanced, items)
  else
    val request = encode(Repl.Request.Complete(0, editor.value, editor.position))
    duplex.send(Stream(framed(request)))

    val candidates: List[Repl.CompletionItem] = completions.take().nn

    candidates match
      case single :: Nil =>
        (insertCompletion(editor, single.name), Nil)

      case _ =>
        val prefix = longestCommonPrefix(candidates.map(_.name))

        val advanced =
          if prefix.length > partialLength(editor) then insertCompletion(editor, prefix) else editor

        (advanced, candidates)

// The editor's syntax-highlighting colours, *specified* here as an iridescence
// `Palette` rather than hardcoded per accent. Harlequin's `syntaxHighlighting`
// renderer turns tokens into a `Teletype` using these colours, and the terminal's
// colour depth is applied when the `Teletype` is rendered (see `render`). Swap this
// given — or `import` a different `ScalaSyntaxPalette` — to retheme the REPL.
private given palette: ScalaSyntaxPalette = new Palette:
  type Form = Srgb
  def background:       Color in Srgb = WebColors.Black
  def foreground:       Color in Srgb = WebColors.Gainsboro
  def scalaError:       Color in Srgb = WebColors.Crimson
  def scalaNumber:      Color in Srgb = WebColors.Goldenrod
  def scalaString:      Color in Srgb = WebColors.ForestGreen
  def scalaIdentifier:  Color in Srgb = WebColors.DodgerBlue
  def scalaTerm:        Color in Srgb = WebColors.Gainsboro
  def scalaType:        Color in Srgb = WebColors.SteelBlue
  def scalaKeyword:     Color in Srgb = WebColors.MediumPurple
  def scalaSymbol:      Color in Srgb = WebColors.SlateGray
  def scalaParenthesis: Color in Srgb = WebColors.SlateGray
  def scalaModifier:    Color in Srgb = WebColors.MediumPurple
  def scalaComment:     Color in Srgb = WebColors.Gray
  def subdued:          Color in Srgb = WebColors.DimGray
  def accented:         Color in Srgb = WebColors.White
  def margin:           Color in Srgb = WebColors.Black

// Maps a Harlequin accent name (as carried on the wire) back to its `Accent`.
private def accentOf(accent: Text): Accent =
  if accent == t"keyword"       then Accent.Keyword
  else if accent == t"modifier" then Accent.Modifier
  else if accent == t"typed"    then Accent.Typed
  else if accent == t"ident"    then Accent.Ident
  else if accent == t"string"   then Accent.String
  else if accent == t"number"   then Accent.Number
  else if accent == t"symbol"   then Accent.Symbol
  else if accent == t"parens"   then Accent.Parens
  else if accent == t"error"    then Accent.Error
  else if accent == t"unparsed" then Accent.Unparsed
  else Accent.Term

// Reconstructs the source line from the highlight tokens, colouring each through
// the palette via Harlequin's syntax-highlighting renderer.
private def colourful(tokens: List[Repl.Token]): Teletype =
  import harlequin.syntaxHighlighting.teletypeable

  tokens.map: token =>
    harlequin.Token(token.text, accentOf(token.accent)).teletype

  . join

// ── Live-highlight heuristic ────────────────────────────────────────────────
// A single-character edit and the "kind" of a character, for guessing accents
// before the server's tokenization arrives.
private enum Edit:
  case Insert(at: Int, char: Char)
  case Delete(at: Int)

private enum CharKind:
  case Word, Symbol, Space

private def charKind(c: Char): CharKind =
  if c.isLetterOrDigit || c == '_' then CharKind.Word
  else if c.isWhitespace then CharKind.Space
  else CharKind.Symbol

// The character kind a token's accent stands for, so an inserted character can be
// matched against the token it touches.
private def accentKind(accent: Text): CharKind =
  if accent == t"symbol" || accent == t"parens" then CharKind.Symbol
  else if accent == t"unparsed" then CharKind.Space
  else CharKind.Word

private def defaultAccent(kind: CharKind): Text = kind match
  case CharKind.Word   => t"term"
  case CharKind.Symbol => t"symbol"
  case CharKind.Space  => t"unparsed"

private def commonPrefix(a: Text, b: Text): Int =
  val n = a.length.min(b.length)
  var i = 0
  while i < n && a.s.charAt(i) == b.s.charAt(i) do i += 1
  i

// Classifies `oldBuf -> newBuf` as a single-character insert or delete, or `None`.
private def diff(oldBuf: Text, newBuf: Text): Option[Edit] =
  val p = commonPrefix(oldBuf, newBuf)

  if newBuf.length == oldBuf.length + 1 && newBuf.skip(p + 1) == oldBuf.skip(p)
  then Some(Edit.Insert(p, newBuf.s.charAt(p)))
  else if newBuf.length == oldBuf.length - 1 && oldBuf.skip(p + 1) == newBuf.skip(p)
  then Some(Edit.Delete(p))
  else None

// Inserts `c` at offset `p`, giving it an accent from the touching token(s): join
// the token it lands in/next to if their kinds match, otherwise split / start a
// new token (anything joins a `string`). Preserves the text, so widths are exact.
private def insertChar(tokens: List[Repl.Token], p: Int, c: Char): List[Repl.Token] =
  val kind     = charKind(c)
  val ch: Text = c.toString.tt
  def tok(text: Text, accent: Text): Repl.Token = Repl.Token(text, accent, Unset)
  val arr      = tokens.toVector
  val offsets  = arr.scanLeft(0)(_ + _.text.length)
  val total    = offsets(arr.length)

  if arr.isEmpty then List(tok(ch, defaultAccent(kind)))
  else arr.indices.find { i => p > offsets(i) && p < offsets(i + 1) } match
    case Some(i) =>
      val t  = arr(i)
      val at = p - offsets(i)

      val mid =
        if kind == accentKind(t.accent) || t.accent == t"string"
        then Vector(tok(t.text.keep(at) + ch + t.text.skip(at), t.accent))
        else Vector(tok(t.text.keep(at), t.accent), tok(ch, defaultAccent(kind)),
                    tok(t.text.skip(at), t.accent))

      (arr.take(i) ++ mid ++ arr.drop(i + 1)).to(List)

    case None =>
      if p <= 0 then
        val r = arr(0)

        if kind == accentKind(r.accent) then (tok(ch + r.text, r.accent) +: arr.drop(1)).to(List)
        else (tok(ch, defaultAccent(kind)) +: arr).to(List)
      else if p >= total then
        val l = arr(arr.length - 1)

        if kind == accentKind(l.accent)
        then (arr.dropRight(1) :+ tok(l.text + ch, l.accent)).to(List)
        else (arr :+ tok(ch, defaultAccent(kind))).to(List)
      else
        val i  = arr.indices.find { j => offsets(j) == p }.getOrElse(arr.length)
        val l  = arr(i - 1)
        val r  = arr(i)
        val lk = accentKind(l.accent)
        val rk = accentKind(r.accent)

        if kind == lk
        then (arr.take(i - 1) ++ Vector(tok(l.text + ch, l.accent)) ++ arr.drop(i)).to(List)
        else if kind == rk
        then (arr.take(i) ++ Vector(tok(ch + r.text, r.accent)) ++ arr.drop(i + 1)).to(List)
        else (arr.take(i) ++ Vector(tok(ch, defaultAccent(kind))) ++ arr.drop(i)).to(List)

private def deleteChar(tokens: List[Repl.Token], p: Int): List[Repl.Token] =
  var offset = 0

  tokens.flatMap: t =>
    val s = offset
    offset += t.text.length

    if p >= s && p < offset then
      val text = t.text.keep(p - s) + t.text.skip(p - s + 1)
      if text.length == 0 then Nil else List(Repl.Token(text, t.accent, t.tpe))
    else
      List(t)

private def replay(base: List[Repl.Token], edits: List[Edit]): List[Repl.Token] =
  edits.foldLeft(base): (tokens, edit) =>
    edit match
      case Edit.Insert(at, c) => insertChar(tokens, at, c)
      case Edit.Delete(at)    => deleteChar(tokens, at)

// Tracks the live highlight: an authoritative server checkpoint at some buffer
// version, plus the log of edits made since, replayed through the heuristic so
// the buffer stays coloured without waiting for the server. Thread-safe — the
// editor records edits, the background reader adopts checkpoints.
private class LiveState:
  private var version: Int = 0
  private var checkpointVersion: Int = 0
  private var checkpointTokens: List[Repl.Token] = Nil
  private var log: List[(Int, Edit)] = Nil

  // Records the keystroke `oldBuf -> newBuf`, returning this buffer's version and
  // the heuristic tokens to draw now.
  def record(oldBuf: Text, newBuf: Text): (Int, List[Repl.Token]) = synchronized:
    if newBuf.length == 0 then
      checkpointTokens = Nil
      log = Nil
      checkpointVersion = version
      (version, Nil)
    else
      if newBuf != oldBuf then diff(oldBuf, newBuf) match
        case Some(edit) =>
          version += 1
          log = log :+ (version, edit)

        case None =>   // not a single-char edit (paste, kill-line): plain fallback
          checkpointTokens = List(Repl.Token(newBuf, t"term", Unset))
          checkpointVersion = version
          version += 1
          log = Nil

      (version, replay(checkpointTokens, log.map(_._2)))

  // A `tokenize` reply for buffer version `v` arrived: if newer than our
  // checkpoint, adopt it and drop the edits it already accounts for.
  def reconcile(v: Int, tokens: List[Repl.Token]): Unit = synchronized:
    if v > checkpointVersion then
      checkpointVersion = v
      checkpointTokens = tokens
      log = log.filter(_._1 > v)

// The client `/`-commands, with help text, offered as Tab completions when the line
// starts with `/`. Keep in step with the dispatch in `converse`.
private val slashCommands: List[(Text, Text)] =
  List
    ( t"/disconnect" -> t"leave the session, keeping the server running",
      t"/quit"       -> t"stop the server and quit" )

// Replaces the partial identifier ending at the cursor with the completion `name`,
// returning the editor with the cursor just after the inserted text.
private def insertCompletion(editor: LineEditor, name: Text): LineEditor =
  val start:  Int  = identifierStart(editor)
  val prefix: Text = editor.value.keep(start)
  val suffix: Text = editor.value.skip(editor.position)

  LineEditor(t"$prefix$name$suffix", start + name.length, editor.mode)

// The offset at which the identifier ending at the cursor begins.
private def identifierStart(editor: LineEditor): Int =
  val before: String = editor.value.keep(editor.position).s
  var start:  Int    = before.length

  while start > 0 && isIdentifierChar(before.charAt(start - 1)) do start -= 1

  start

// The length of the partial identifier the user has typed up to the cursor.
private def partialLength(editor: LineEditor): Int = editor.position - identifierStart(editor)

private def isIdentifierChar(char: Char): Boolean =
  jl.Character.isLetterOrDigit(char) || char == '_'

// The longest prefix shared by every name (empty if there are none).
private def longestCommonPrefix(names: List[Text]): Text = names match
  case Nil => t""

  case head :: tail =>
    tail.foldLeft(head): (prefix, name) =>
      prefix.keep(commonPrefix(prefix, name))

// Whether the editor content is "complete" enough to submit on Enter: non-empty
// with every bracket closed. Open brackets continue the input onto a new line.
private def balanced(text: Text): Boolean =
  val string: String = text.s
  var depth:  Int    = 0
  var index:  Int    = 0

  while index < string.length do
    string.charAt(index) match
      case '(' | '[' | '{' => depth += 1
      case ')' | ']' | '}' => depth -= 1
      case _               => ()

    index += 1

  string.length > 0 && depth <= 0


// Serializes a `Request` to BinTEL body bytes, deriving the schema from the type
// (`value.bintel`). A valid request always type-assigns, so the encode is total.
private def encode(request: Repl.Request): Data = unsafely(request.bintel)

// Prefixes BinTEL body bytes with a 4-byte big-endian length, the on-wire frame the
// server reads with `DataInputStream.readInt` + `readFully`.
private def framed(data: Data): Data =
  val length: Int      = data.length
  val out:    Array[Byte] = new Array[Byte](4 + length)
  out(0) = (length >>> 24).toByte
  out(1) = (length >>> 16).toByte
  out(2) = (length >>> 8).toByte
  out(3) = length.toByte
  var i = 0

  while i < length do
    out(4 + i) = data(i)
    i += 1

  out.immutable(using Unsafe)

// Reassembles length-prefixed frames from the (chunk-at-a-time) socket stream, keeping
// a buffer across calls so a frame split over chunks — or several frames in one chunk —
// is handled. `next()` yields one frame's body, or `Unset` when the stream ends.
private class FrameReader(chunks: Iterator[Data]):
  private val buffer: scm.ArrayBuffer[Byte] = scm.ArrayBuffer()

  private def fill(count: Int): Boolean =
    while buffer.length < count && chunks.hasNext do chunks.next().each(buffer += _)
    buffer.length >= count

  def next(): Optional[Data] =
    if !fill(4) then Unset else
      val length: Int =
        ((buffer(0) & 0xff) << 24) | ((buffer(1) & 0xff) << 16)
        | ((buffer(2) & 0xff) << 8) | (buffer(3) & 0xff)

      if !fill(4 + length) then Unset else
        val body: Data = IArray.from(buffer.slice(4, 4 + length))
        buffer.remove(0, 4 + length)
        body
