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
import probates.cancel
import executives.completions
import harlequin.Accent
import internetAccess.enabled
import interpreters.posix
import jsonDiscriminables.discriminatedUnionByKind
import logging.silent
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
          connect(port).lay(unreachable(portNumber)): duplex =>
            try converse(duplex) finally duplex.close()

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

// Opens a TCP connection to the server, or `Unset` if it is refused.
private def connect(port: Port over Tcp): Optional[Duplex] =
  try (ip"127.0.0.1" via port).duplex() catch case _: Exception => Unset

// Opens a UNIX domain socket connection, or `Unset` if it is refused.
private def connectDomain(socket: DomainSocket): Optional[Duplex] =
  try socket.duplex() catch case _: Exception => Unset

private def unreachableSocket(path: Text)(using Stdio): Exit =
  Out.println(t"flux: could not connect to $path")
  Exit.Fail(3)

// Drives an interactive session over a connection, closing it on the way out.
private def session(duplex: Duplex)(using Stdio, Monitor, Probate, Console, Environment): Exit =
  try converse(duplex) finally duplex.close()

private def failedToLaunch(using Stdio): Exit =
  Out.println(t"flux: could not start a REPL server")
  Exit.Fail(3)

// Starts a REPL server in the background — a detached `flux serve` process on its
// own per-process domain socket — waits for it to bind, and connects to it. Because
// the server is a separate process it outlives this client, so the same session can
// be reconnected to later by running `flux` again. Returns the live connection,
// or `Unset` if no server became reachable in time.
private def launchServer()(using Stdio, System): Optional[Duplex] =
  safely(System.properties.ethereal.script[Text]()).lay(Unset): executable =>
    val before: List[Text] = socketPaths(socketDirectory)

    val builder = jl.ProcessBuilder(executable.s, "serve")
    builder.redirectOutput(jl.ProcessBuilder.Redirect.DISCARD)
    builder.redirectError(jl.ProcessBuilder.Redirect.DISCARD)
    builder.redirectInput(ji.File("/dev/null"))
    safely(builder.start())

    var duplex: Optional[Duplex] = Unset
    var waited: Int              = 0

    // Poll for a new, connectable socket (the socket file appears once it binds).
    while duplex.absent && waited < 10000 do
      jl.Thread.sleep(100)
      waited += 100

      socketPaths(socketDirectory).each: candidate =>
        if duplex.absent && !before.contains(candidate) then
          connectDomain(DomainSocket(candidate)).let: connection =>
            duplex = connection

    duplex

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
  socketPaths(socketDirectory) match
    case Nil =>
      Out.println(t"flux: starting a REPL server…")
      launchServer().lay(failedToLaunch)(session(_))

    case path :: Nil =>
      connectDomain(DomainSocket(path)).lay(unreachableSocket(path))(session(_))

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

  // `duplex.stream` blocks on its first socket read, so force the iterator lazily
  // — only after a line has been sent — otherwise it would deadlock here before
  // the editor starts (the server sends nothing until it receives a message).
  lazy val chunks: Iterator[Data] = duplex.stream.iterator

  val state                 = LiveState()
  val pending               = TrieMap[Int, Int]()                  // tokenize id → version
  val submits               = juc.LinkedBlockingQueue[Repl.Reply]()
  val completions           = juc.LinkedBlockingQueue[List[Repl.CompletionItem]]()
  val nextId                = juc.atomic.AtomicInteger(1)           // 0 is reserved for submit
  @volatile var live        = true

  // Background reader: replies may arrive in any order, so route each by kind —
  // `tokenize` replies refine the live highlight (re-associated by id → version),
  // everything else is a `submit` result the editor awaits.
  async:
    while live do
      val raw = reply(chunks).trim

      if raw.length == 0 then live = false
      else safely[Exception](Json.parseTracked(raw).as[Repl.Reply]).let:
        case Repl.Reply.Tokenized(id, highlight) =>
          pending.remove(id).foreach(state.reconcile(_, highlight))

        // Hand completions to the editor thread (which blocks on Tab) rather than
        // printing them here, so it can redraw the prompt below them.
        case Repl.Reply.Completed(_, items) =>
          completions.put(items)

        case reply =>
          submits.put(reply)

  whereas:
    case TerminalError() =>
      Out.println(t"flux: the terminal could not be initialised")
      Exit.Fail(4)

  . recover:
      interactive:
        given Interaction[Text, LineEditor] =
          liveHighlighting(duplex, state, pending, nextId, completions)

        var running = true

        while running do
          Out.print(t"> ")

          // Multi-line editing: Enter submits when brackets are balanced, and
          // otherwise inserts a newline to continue the input; Shift+Enter always
          // submits.
          val editor = LineEditor(mode = LineEditor.Mode.Multiline(balanced))

          whereas:
            case DismissError() => running = false

          . recover:
              editor.ask: line =>
                // A line beginning with `/` is a client command, handled locally
                // rather than submitted: `/disconnect` ends the session (like
                // Ctrl+D); `/quit` also tells the server to shut down.
                if line == t"/disconnect" then running = false
                else if line == t"/quit" then
                  duplex.send(Stream((encode(Repl.Request.Quit(0)) + t"\n\n").data))
                  running = false
                else if line.starts(t"/") then
                  Out.println(t"flux: unknown command: $line")
                else
                  // The editor already showed the (live-highlighted) line; submit
                  // it and print the awaited result value and any diagnostics.
                  duplex.send(Stream((encode(Repl.Request.Submit(0, line)) + t"\n\n").data))

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

        live = false
        Exit.Ok

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

// Prints tab completions below the editor as a minimal Escritoire table of name and
// signature, with no heading row (the header section is dropped from the grid). The
// caller redraws the prompt afterwards so it reappears beneath the table.
private def showCompletions(completions: List[Repl.CompletionItem])(using Stdio): Unit =
  import tableStyles.minimal
  import columnAttenuation.ignore
  import textMetrics.uniform

  Out.print(t"\r\n")

  if completions.isEmpty then Out.print(t"(no completions)\r\n")
  else
    val table =
      Scaffold[Repl.CompletionItem]
        ( Column(t"Name")(_.name),
          Column(t"Signature")(_.signature) )

    // `tabulate(…).grid(…)` yields a header section then a body section; dropping the
    // header (`sections.tail`) renders the rows alone, with no titles or rule.
    val grid = table.tabulate(completions.take(20)).grid(80)

    Grid(grid.sections.tail, grid.style).render.each: line =>
      Out.print(t"$line\r\n")

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

// A line-editor interaction that highlights the buffer live: each keystroke is
// applied to the live highlight via the heuristic and drawn immediately (never
// waiting on the server), then an async `tokenize` is fired to refine it. Mirrors
// Profanity's default cursor handling — colour codes are zero-width.
private def liveHighlighting
  ( duplex:      Duplex,
    state:       LiveState,
    pending:     TrieMap[Int, Int],
    nextId:      juc.atomic.AtomicInteger,
    completions: juc.LinkedBlockingQueue[List[Repl.CompletionItem]] )
  ( using terminal: Terminal )
:   Interaction[Text, LineEditor] =

  new Interaction[Text, LineEditor]:
    given Stdio = terminal.stdio
    var lastVersion: Int     = -1
    var redrawFresh: Boolean = false        // draw the next frame fresh, below a table
    override def after(): Unit = Out.println()

    def result(editor: LineEditor): Text = editor.value

    // The editor's mode (set when it is created) decides whether Enter submits or
    // inserts a newline.
    override def submits(event: TerminalEvent, editor: LineEditor): Boolean =
      editor.submitsOn(event)

    // On Tab, complete. A line beginning with `/` completes the client `/`-commands
    // locally; otherwise the server is asked for completions at the cursor and we
    // block for the reply (delivered by the background reader). Either way a unique
    // candidate is inserted; with several, the candidates are shown as a table and the
    // editor is advanced by the longest prefix common to all their names — so a set of
    // overloads, which share a name, completes that whole name.
    override def react(editor: LineEditor, event: TerminalEvent): LineEditor = event match
      case Keypress.Tab if editor.value.starts(t"/") =>
        completeCommand(editor)

      case Keypress.Tab =>
        val request = encode(Repl.Request.Complete(0, editor.value, editor.position))
        duplex.send(Stream((request + t"\n\n").data))
        val candidates: List[Repl.CompletionItem] = completions.take().nn

        candidates match
          case single :: Nil =>
            insertCompletion(editor, single.name)

          case _ =>
            showCompletions(candidates)
            redrawFresh = true
            val prefix = longestCommonPrefix(candidates.map(_.name))

            if prefix.length > partialLength(editor) then insertCompletion(editor, prefix)
            else editor

      case _ =>
        editor

    // Completes the `/`-commands against the whole line: a unique match fills in the
    // command; otherwise the matches are listed and the longest common prefix added.
    def completeCommand(editor: LineEditor): LineEditor =
      val typed: Text = editor.value

      slashCommands.filter { (name, _) => name.starts(typed) } match
        case (name, _) :: Nil =>
          LineEditor(name, name.length, editor.mode)

        case matches =>
          val items =
            matches.map: (name, help) =>
              Repl.CompletionItem(name, t"command", help)

          showCompletions(items)
          redrawFresh = true
          val prefix = longestCommonPrefix(matches.map(_._1))

          if prefix.length > typed.length then LineEditor(prefix, prefix.length, editor.mode)
          else editor

    def render(old: Optional[LineEditor], editor: LineEditor): Unit =
      val cols              = terminal.knownColumns.max(1)
      val len               = editor.value.length
      val (curRow, curCol)  = LineEditor.cursorPosition(editor.value, editor.position, cols)
      val (endRow, _)       = LineEditor.cursorPosition(editor.value, len, cols)
      val (version, tokens) = state.record(old.lay(t"")(_.value), editor.value)
      val coloured          = colourful(tokens).render(terminal.stdio.termcap)

      // After a table the cursor sits below it, so ignore the previous frame's
      // position and draw fresh where the cursor now is.
      val anchor            = if redrawFresh then Unset else old
      redrawFresh           = false

      Out.print:
        Text.build:
          anchor.let: o =>
            val (oldRow, _) = LineEditor.cursorPosition(o.value, o.position, cols)
            if oldRow > 0 then append(t"\e[${oldRow}F") else append(t"\r")

          append(t"\e[J")
          // In raw mode a bare newline only moves down, so emit CR+LF for each.
          append(coloured.sub(t"\n", t"\r\n"))

          if len > 0 then
            if endRow > 0 then append(t"\e[${endRow}F") else append(t"\r")

          if curRow > 0 then append(t"\e[${curRow}B")
          if curCol > 0 then append(t"\e[${curCol + 1}G")

      // Fire an async `tokenize` (correlated by id → version); the background
      // reader reconciles the reply, refining the next render.
      if len > 0 && version != lastVersion then
        lastVersion = version
        val id = nextId.getAndIncrement
        pending(id) = version
        duplex.send(Stream((encode(Repl.Request.Tokenize(id, editor.value)) + t"\n\n").data))

// Serializes a `Request` as compact JSON for the wire. `Json` is `Dynamic`, so
// `.show` is intercepted; summon the `Encodable in Text` instance explicitly.
private def encode(request: Repl.Request): Text =
  summon[Json is Encodable in Text].encoded(request.json)

// Pulls chunks from the (persistent) socket stream, buffering until the `"\n\n"`
// message delimiter, and decodes the buffered bytes verbatim.
private def reply(chunks: Iterator[Data]): Text =
  val buffer: scm.ArrayBuffer[Byte] = scm.ArrayBuffer()
  var done = false

  while !done && chunks.hasNext do
    chunks.next().each(buffer += _)

    done =
      buffer.length >= 2 && buffer(buffer.length - 1) == '\n'.toByte
      && buffer(buffer.length - 2) == '\n'.toByte

  IArray.from(buffer).utf8
