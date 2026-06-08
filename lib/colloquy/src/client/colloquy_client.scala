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
package colloquy

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
import codicils.cancel
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

// A front-end for a Colloquy REPL. With a TCP port, `colloquy serve <port>` runs a
// server and `colloquy <port>` connects to one on localhost. With no port,
// `colloquy serve` opens a per-process UNIX domain socket (named after the JVM's
// PID, under the XDG runtime dir) and `colloquy` connects to it — picking the lone
// running server, or listing them if there are several. Either way the client
// drives an interactive, live-highlighted session until Ctrl+D, Ctrl+C or `/quit`.
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

    case _ =>
      execute(Exit.Fail(1))

// Runs a REPL server on the given TCP port and blocks until interrupted.
private def serve(portNumber: Int)(using Stdio, Monitor, Codicil, System): Exit =
  given Scalac[3.8] = Scalac(Nil)
  given Classloader = serverClassloader

  safely(Port[Tcp](portNumber)).lay(invalidPort(portNumber)): port =>
    whereas:
      case BindError(_) => Out.println(t"colloquy: port $portNumber is unavailable"); Exit.Fail(5)
      case error: Error => Out.println(t"colloquy: ${error.message}"); Exit.Fail(6)

    . recover:
        val repl    = Repl()
        val service = repl.serve(port)
        Out.println(t"colloquy: serving a REPL on port $portNumber (Ctrl+C or /quit to stop)")
        repl.awaitQuit()
        service.stop()
        Exit.Ok

// The directory holding per-process REPL sockets, and this process's socket file.
// UNIX domain sockets are a Unix-only feature, so the directory follows
// `$XDG_RUNTIME_DIR` (then `$TMPDIR`, then `/tmp`) directly, as plain `Text`.
private def envText(name: String): Optional[Text] = Optional(jl.System.getenv(name)).let(_.nn.tt)

private def socketDirectory: Text =
  t"${envText("XDG_RUNTIME_DIR").or(envText("TMPDIR")).or(t"/tmp")}/colloquy"

private def socketFile: Text = t"$socketDirectory/${ProcessHandle.current.nn.pid}.sock"

// Runs a REPL server on a per-process UNIX domain socket (used when no port is
// given) and blocks until quit, unlinking the socket file on the way out.
private def serveSocket()(using Stdio, Monitor, Codicil, System): Exit =
  given Scalac[3.8] = Scalac(Nil)
  given Classloader = serverClassloader

  val socketPath: Text = socketFile

  try
    jnf.Files.createDirectories(jnf.Path.of(socketDirectory.s)).nn
    jnf.Files.deleteIfExists(jnf.Path.of(socketPath.s))

    val repl    = Repl()
    val service = repl.serve(socketPath)
    Out.println(t"colloquy: serving a REPL on $socketPath (Ctrl+C or /quit to stop)")
    repl.awaitQuit()
    service.stop()
    safely(jnf.Files.deleteIfExists(jnf.Path.of(socketPath.s)))
    Exit.Ok
  catch case error: Throwable =>
    Out.println(t"colloquy: could not serve on $socketPath: ${error.toString.tt}")
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
    val link: jnf.Path = jnf.Files.createTempDirectory("colloquy").nn.resolve("colloquy.jar").nn
    jnf.Files.createSymbolicLink(link, jnf.Path.of(executable.s)).nn
    val url: jn.URL = ji.File(link.toString).toURI.nn.toURL.nn
    new Classloader(jn.URLClassLoader(Array(url), threadContext.java))
  catch case _: Throwable => threadContext

private def invalidPort(portNumber: Int)(using Stdio): Exit =
  Out.println(t"colloquy: $portNumber is not a valid TCP port")
  Exit.Fail(2)

private def unreachable(portNumber: Int)(using Stdio): Exit =
  Out.println(t"colloquy: could not connect to localhost:$portNumber")
  Exit.Fail(3)

// Opens a TCP connection to the server, or `Unset` if it is refused.
private def connect(port: Port over Tcp): Optional[Duplex] =
  try (ip"127.0.0.1" via port).duplex() catch case _: Exception => Unset

// Opens a UNIX domain socket connection, or `Unset` if it is refused.
private def connectDomain(socket: DomainSocket): Optional[Duplex] =
  try socket.duplex() catch case _: Exception => Unset

private def unreachableSocket(path: Text)(using Stdio): Exit =
  Out.println(t"colloquy: could not connect to $path")
  Exit.Fail(3)

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

// Connects to a per-process UNIX domain socket. With exactly one server running,
// connects to it; with several, lists them and exits so the user can pick.
private def connectSocket()(using Stdio, Monitor, Codicil, Console, Environment): Exit =
  socketPaths(socketDirectory) match
    case Nil =>
      Out.println(t"colloquy: no running REPL server found (start one with 'colloquy serve')")
      Exit.Fail(3)

    case path :: Nil =>
      connectDomain(DomainSocket(path)).lay(unreachableSocket(path)): duplex =>
        try converse(duplex) finally duplex.close()

    case paths =>
      Out.println(t"colloquy: several REPL servers are running:")

      paths.each: path =>
        Out.println(t"  $path")

      Out.println(t"colloquy: stop all but one, or use a TCP server with 'colloquy <port>'")
      Exit.Fail(7)

// The read/edit/print loop. The server's reply is printed verbatim. Ctrl+C/Ctrl+D
// dismiss the line editor (`DismissError`) and end the session.
private def converse(duplex: Duplex)(using Stdio, Monitor, Codicil, Console, Environment): Exit =
  // `duplex.stream` blocks on its first socket read, so force the iterator lazily
  // — only after a line has been sent — otherwise it would deadlock here before
  // the editor starts (the server sends nothing until it receives a message).
  lazy val chunks: Iterator[Data] = duplex.stream.iterator

  val state                 = LiveState()
  val pending               = TrieMap[Int, Int]()                  // tokenize id → version
  val submits               = juc.LinkedBlockingQueue[Repl.Reply]()
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

        case reply =>
          submits.put(reply)

  whereas:
    case TerminalError() =>
      Out.println(t"colloquy: the terminal could not be initialised")
      Exit.Fail(4)

  . recover:
      interactive:
        given Interaction[Text, LineEditor] = liveHighlighting(duplex, state, pending, nextId)

        // Enable the kitty keyboard protocol so the terminal reports Shift+Enter
        // (as a CSI-u sequence) distinctly from plain Enter; pop it on the way out.
        Out.print(t"\e[>1u")
        var running = true

        try while running do
          Out.print(t"> ")

          whereas:
            case DismissError() => running = false

          . recover:
              LineEditor().ask: line =>
                // A line beginning with `/` is a client command, handled locally
                // rather than submitted: `/disconnect` ends the session (like
                // Ctrl+D); `/quit` also tells the server to shut down.
                if line == t"/disconnect" then running = false
                else if line == t"/quit" then
                  duplex.send(Stream((encode(Repl.Request.Quit(0)) + t"\n\n").data))
                  running = false
                else if line.starts(t"/") then
                  Out.println(t"colloquy: unknown command: $line")
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

        finally Out.print(t"\e[<u")

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

// A line-editor interaction that highlights the buffer live: each keystroke is
// applied to the live highlight via the heuristic and drawn immediately (never
// waiting on the server), then an async `tokenize` is fired to refine it. Mirrors
// Profanity's default cursor handling — colour codes are zero-width.
// The (row, column) of `position` when `text` is laid out in a terminal `cols`
// wide, counting embedded newlines and wrapping long lines. Reduces to the
// single-line `position/cols`, `position%cols` when there are no newlines.
private def visualPosition(text: Text, position: Int, cols: Int): (Int, Int) =
  val string: String = text.s
  val limit:  Int    = position.min(string.length)
  var rows:   Int    = 0
  var lineStart: Int = 0
  var index:  Int    = 0

  while index < limit do
    if string.charAt(index) == '\n' then
      rows += (index - lineStart)/cols + 1
      lineStart = index + 1

    index += 1

  val column = limit - lineStart
  (rows + column/cols, column%cols)

private def liveHighlighting
  ( duplex: Duplex, state: LiveState, pending: TrieMap[Int, Int], nextId: juc.atomic.AtomicInteger )
  ( using terminal: Terminal )
:   Interaction[Text, LineEditor] =

  new Interaction[Text, LineEditor]:
    given Stdio = terminal.stdio
    var lastVersion: Int = -1
    override def after(): Unit = Out.println()

    def result(editor: LineEditor): Text = editor.value

    // Plain Enter inserts a newline (handled by `LineEditor`); only Shift+Enter
    // submits the buffer.
    override def submits(event: TerminalEvent): Boolean = event match
      case Keypress.Shift(Keypress.Enter) => true
      case _                              => false

    def render(old: Optional[LineEditor], editor: LineEditor): Unit =
      val cols              = terminal.knownColumns.max(1)
      val len               = editor.value.length
      val (curRow, curCol)  = visualPosition(editor.value, editor.position, cols)
      val (endRow, _)       = visualPosition(editor.value, len, cols)
      val (version, tokens) = state.record(old.lay(t"")(_.value), editor.value)
      val coloured          = colourful(tokens).render(terminal.stdio.termcap)

      Out.print:
        Text.build:
          old.let: o =>
            val (oldRow, _) = visualPosition(o.value, o.position, cols)
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
