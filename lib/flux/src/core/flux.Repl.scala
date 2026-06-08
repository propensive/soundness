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
import java.nio.channels as jnc
import java.nio.file as jnf

import scala.quoted.*

import ambience.*
import anthology.*
import anticipation.*
import coaxial.*
import contingency.*
import denominative.*
import digression.*
import distillate.*
import gossamer.*
import harlequin.*
import hellenism.*
import inimitable.*
import jacinta.*
import parasite.*
import prepositional.*
import rudiments.*
import serpentine.*
import turbulence.*
import urticose.*
import vacuous.*

import hieroglyph.charEncoders.utf8
import interfaces.paths.pathOnLinux
import jsonDiscriminables.discriminatedUnionByKind
import stenography.Syntax

object Repl:
  object Layout:
    class Standard() extends Layout:
      def objectName(index: Int): Text = t"rs$$line$$$index"

      def wrap(index: Int, history: List[Text], code: Text): Text =
        val imports = history.map: previous =>
          t"  import $previous.*"

        val body = code.cut(t"\n").map: line =>
          t"  $line"

        (t"object ${objectName(index)}:" :: imports ::: body).join(t"\n")

  trait Layout:
    def objectName(index: Int): Text
    def wrap(index: Int, history: List[Text], code: Text): Text

  enum Outcome:
    case Ran(notices: List[Notice], value: Optional[Text], output: Text)
    case Threw(notices: List[Notice], error: Throwable, output: Text)
    case Rejected(notices: List[Notice])
    case Crashed(notices: List[Notice], error: StackTrace)

  // One syntax-highlighting token of the submitted line: its verbatim text, the
  // lowercased name of its Harlequin accent (`keyword`, `ident`, `number`, …),
  // and — where the typechecker resolved one — the fully-qualified Scala type of
  // the token (`Unset` for keywords, punctuation, whitespace, …). Sent in the
  // JSON response for the client to colourise; ANSI rendering is the client's
  // concern.
  case class Token(text: Text, accent: Text, tpe: Optional[Text])

  // One tab-completion candidate: the `name` to insert, its `kind` (term, method,
  // type, …) and its rendered type `signature`. The Harlequin `Completion`'s
  // `Syntax` signature is rendered to text here so the reply serializes simply.
  case class CompletionItem(name: Text, kind: Text, signature: Text)

  // A request from a connected client. `id` is echoed in the reply so the client
  // can re-associate replies that arrive out of order (a fast `tokenize` may
  // overtake a slow `submit`). Serialized as JSON with a `kind` discriminator.
  enum Request:
    case Submit(id: Int, code: Text)
    case Tokenize(id: Int, code: Text)
    case Complete(id: Int, code: Text, offset: Int)
    case Quit(id: Int)

  // A reply to a connected client, echoing the request's `id`. `highlight` is the
  // Harlequin tokenization of the submitted line. Serialized as JSON with a `kind`
  // discriminator.
  enum Reply:
    case Tokenized(id: Int, highlight: List[Token])
    case Completed(id: Int, completions: List[CompletionItem])

    case Ran(id: Int, value: Optional[Text], output: Text, tpe: Optional[Text],
             diagnostics: Text, highlight: List[Token])

    case Rejected(id: Int, diagnostics: Text, highlight: List[Token])
    case Threw(id: Int, output: Text, diagnostics: Text, highlight: List[Token])
    case Crashed(id: Int, diagnostics: Text, highlight: List[Token])
    case Failed(id: Int, message: Text)

  // Highlights `code` with Harlequin's typechecked pipeline (the compiler
  // resolves symbols, so accents are accurate and each token carries its type).
  // Needs the session's `Scalac` and compile classpath; used for `submit`.
  def highlight(code: Text)(using Scalac[?], LocalClasspath): List[Token] =
    import highlighting.typecheckedScala
    project(Scala.highlight(code))

  // Highlights `code` with Harlequin's standalone lexer — no compiler, so it is
  // fast enough to run on every keystroke for live editing (no type information).
  def tokenize(code: Text): List[Token] =
    project(Scala.highlight(code))

  private def project(source: SourceCode): List[Token] =
    val lines: List[List[Token]] = source.lines.to(List).map: line =>
      line.map: token =>
        Token(token.text, token.accent.toString.tt.lower, token.meta.let(_.tpe.qualified))

    // `SourceCode.lines` was split on (and dropped) the newlines, so re-insert a
    // newline token between consecutive lines — otherwise multi-line code collapses
    // to a single line on the client and its cursor maths drift apart.
    lines match
      case Nil          => Nil
      case head :: rest => head ::: rest.flatMap(Token(t"\n", t"unparsed", Unset) :: _)

  // Tab completions at character `offset` in `code`, from Harlequin's typechecked
  // pipeline (so it needs the session's `Scalac` and compile classpath). The line is
  // wrapped as `val __completion = <code>` so a bare expression is valid top-level
  // Scala to complete against; the caret is shifted past the wrapper. Each
  // candidate's `Syntax` signature is rendered to text for the wire.
  def complete(code: Text, offset: Int)(using Scalac[?], LocalClasspath): List[CompletionItem] =
    import highlighting.typecheckedScala

    val prefix:  Text = t"val __completion = "
    val wrapped: Text = t"$prefix$code"
    val caret:   Int  = prefix.length + offset

    Scala.highlight(wrapped, caret = caret.z).completions.lay(Nil): completions =>
      completions.items.map: item =>
        CompletionItem(item.name, item.kind.toString.tt, item.signature.qualified)

  // The Scala type of an expression, read from a typechecked highlight of
  // `val __result = <code>`: the binding's token carries the resolved type as a
  // `Syntax`. Only meaningful for expression lines (statements have no value).
  def resultType(code: Text)(using Scalac[?], LocalClasspath): Optional[Syntax] =
    import highlighting.typecheckedScala
    val tokens = Scala.highlight(t"val __result = $code").lines.to(List).flatten

    tokens.find(_.text == t"__result") match
      case Some(token) => token.meta.let(_.tpe)
      case None        => Unset

  object Prelude:
    val empty: Prelude = Prelude(Nil, Nil)

  // Declarations lifted from an inline binding block to seed the REPL context:
  // `imports` are re-injected into every line; `seedTasty` is a TASTy-pickled
  // block of the lifted definitions and binding accessors (carried as data from
  // macro-expansion time), recompiled once into a seed object with full type
  // fidelity.
  case class Prelude(imports: List[String], seedTasty: List[String])

  def make[version <: Scalac.Versions]
    ( prelude: Repl.Prelude )
    ( using Scalac[version], Classloader, TemporaryDirectory )
  :   Repl[version] =

    new Repl[version](prelude = prelude)

  inline def apply[version <: Scalac.Versions](inline body: Unit = ())
    ( using scalac: Scalac[version], classloader: Classloader, temporary: TemporaryDirectory )
  :   Repl[version] =

    ${ReplMacro.bound[version]('body, 'scalac, 'classloader, 'temporary)}

class Repl[version <: Scalac.Versions]
  ( layout:  Repl.Layout  = Repl.Layout.Standard(),
    prelude: Repl.Prelude = Repl.Prelude.empty )
  ( using scalac: Scalac[version], classloader: Classloader, temporary: TemporaryDirectory ):

  import Repl.Outcome

  val session: Long = ReplBridge.freshSession()

  // Fulfilled when a connected client sends a `Quit` request; a server host can
  // `attend` it to block until then and shut down cleanly.
  private val quit: Promise[Unit] = Promise()

  def awaitQuit(): Unit = quit.attend()

  // Serializes `interpret` across connections: the shared `Scalac` compiler is
  // not reentrant and the REPL's state is mutable.
  private val mutex: Mutex = Mutex()

  private var index:   Int        = 0
  private var result:  Int        = 0
  private var history: List[Text] = Nil
  private var seeded:  Boolean     = false

  private val out: Path on Linux = unsafely(temporaryDirectory/Uuid())
  locally(jnf.Files.createDirectories(jnf.Path.of(out.encode.s)).nn)

  private lazy val loader: Classloader =
    LocalClasspath((Classpath.Directory(out) :: Nil)*).classloader(classloader)

  // The compile classpath must come from the *same* loader the wrapper objects
  // are run against (`classloader`, below), not from `classloaders.threadContext`:
  // `interpret` may run on a background worker thread (the TCP accept loop) whose
  // thread-context loader differs from the REPL's, which would compile against one
  // copy of the soundness classes and run against another — a loader-constraint
  // violation.
  private def classpath(using System): LocalClasspath =
    val entries = Classpath.Directory(out) :: (classloader.classpath.match
      case classpath: LocalClasspath => classpath.entries

      case _ =>
        unsafely(System.properties.java.`class`.path().decode[LocalClasspath]).entries)

    LocalClasspath(entries*)

  // Compiles `code` as the next wrapper object and, on success, loads it (which
  // runs its body). `rendered` is evaluated after a successful run to supply the
  // `Outcome.Ran` value — `Unset` for statements, or the inspected result for an
  // expression line.
  private def compile(code: Text)(rendered: => Optional[Text])(using Monitor, System, Codicil)
  :   Outcome logs CompileEvent raises CompilerError raises AsyncError =

    val name:    Text = layout.objectName(index)
    val source:  Text = layout.wrap(index, history, code)
    val process       = scalac(classpath)(Map(t"$name.scala" -> source), out)
    val outcome       = process.complete()
    val notices       = process.notices.to(List)

    outcome match
      case CompileResult.Crash(trace) =>
        Outcome.Crashed(notices, trace)

      case CompileResult.Failure =>
        Outcome.Rejected(notices)

      case CompileResult.Success =>
        index += 1
        history = history :+ name

        // Capture whatever the user's code prints to stdout. Scala's `println`
        // writes to `scala.Console.out` (a thread-local), while Java code writes
        // to `System.out` (process-global), so redirect both. `System.out` is
        // process-global, but this runs under `submit`'s `mutex`, so only one run
        // ever redirects it at a time, and the window is just the run itself.
        val captured: ji.ByteArrayOutputStream = ji.ByteArrayOutputStream()
        val stream:   ji.PrintStream           = ji.PrintStream(captured, true, "UTF-8")
        val previous: ji.PrintStream           = jl.System.out.nn
        jl.System.setOut(stream)

        def output: Text =
          stream.flush()
          captured.toString("UTF-8").nn.tt

        try
          // Seed accessors read their session from this thread-local.
          ReplBridge.setCurrentSession(session)
          scala.Console.withOut(stream)(loader.on(t"$name$$"))
          Outcome.Ran(notices, rendered, output)
        catch
          case error: ExceptionInInitializerError =>
            Outcome.Threw(notices, Optional(error.getCause).or(error), output)

          // Running arbitrary user code can throw anything, including `Error`s
          // (`LinkageError`, `StackOverflowError`, …), which must not escape and
          // kill the session.
          case error: Throwable =>
            Outcome.Threw(notices, error, output)
        finally
          jl.System.setOut(previous)

  private def statementCode(line: Text): Text =
    (prelude.imports.map(_.tt) :+ line).join(t"\n")

  // Wraps an expression line as `val resN = <line>`, then renders the bound value
  // through `Inspectable` and stashes the rendering in `ReplBridge`. The renderer
  // sits in an `@experimental` scope because `Inspectable` is `@experimental`, so
  // this compiles even when the contextual `Scalac` is not in experimental mode.
  private def expressionCode(name: Text, key: Text, line: Text): Text =
    val put: Text = t"flux.ReplBridge.put(${session.toString.tt}L, \"$key\", $name.inspect)"

    val lines: List[Text] =
      List
        ( t"val $name = $line",
          t"@scala.annotation.experimental private val ${name}_inspected: scala.Unit =",
          t"  { import spectacular.inspect; $put }" )

    (prelude.imports.map(_.tt) ::: lines).join(t"\n")

  private def evaluate(line: Text)(using Monitor, System, Codicil)
  :   Outcome logs CompileEvent raises CompilerError raises AsyncError =

    val name: Text = t"res${result.toString.tt}"
    val key:  Text = t"result:${session.toString.tt}:${index.toString.tt}"

    // Try the line as an expression first; a definition or import fails to parse
    // as `val resN = …` and falls back to being compiled as a plain statement.
    val expression: Outcome = compile(expressionCode(name, key, line)):
      Optional(ReplBridge.fetch[String](session, key.s)).let(_.tt)

    expression match
      case _: Outcome.Rejected =>
        compile(statementCode(line))(Unset)

      case ran: Outcome.Ran =>
        result += 1
        ran

      case other =>
        other

  // The prelude's pickled definitions and binding accessors are recompiled once,
  // as the first object (`rs$line$0`), straight from TASTy — preserving the
  // original types rather than re-rendering them as source. Later lines see its
  // members via the history import. Imports create no members, so they are
  // re-injected into every line instead.
  private def ensureSeeded()(using Monitor, System, Codicil)
  :   Optional[Outcome] logs CompileEvent raises CompilerError raises AsyncError =

    if seeded || prelude.seedTasty.isEmpty then Unset
    else
      seeded = true
      val name:   Text       = layout.objectName(index)

      val errors: List[Text] =
        ReplModuleCompiler.compile(classpath)(name, out.encode)(prelude.seedTasty)

      if errors.isEmpty then
        index += 1
        history = history :+ name
        Outcome.Ran(Nil, Unset, t"")
      else
        Outcome.Rejected(errors.map(Notice(Importance.Error, t"<seed>", _, Unset)))

  def interpret(line: Text)(using Monitor, System, Codicil)
  :   Outcome logs CompileEvent raises CompilerError raises AsyncError =

    def lineOutcome: Outcome = evaluate(line)

    ensureSeeded().lay(lineOutcome):
      case _: Outcome.Ran => lineOutcome
      case failure        => failure

  // Starts a TCP server on `port` and accepts connections. Each connection is an
  // interactive session over the *same* REPL state. Messages are JSON `Request`/
  // `Reply` values, one per line, each terminated by a blank line (compact JSON
  // has no embedded newline, so the delimiter is unambiguous). Returns a handle
  // whose `stop()` shuts the server down.
  def serve(port: Port over Tcp)(using Monitor, System, Codicil)
  :   SocketService logs CompileEvent raises BindError raises StreamError =

    port.listen: socket =>
      converse(socket.getInputStream.nn, socket.getOutputStream.nn)
      Data()

  // Serves the REPL over a UNIX domain socket at `socketPath` (used when no TCP
  // port is given). Coaxial's domain-socket `Connection` does not expose its
  // streams for the bidirectional, asynchronously-written protocol this server
  // needs, so the accept loop runs directly over an NIO channel.
  def serve(socketPath: Text)(using Monitor, System, Codicil): SocketService logs CompileEvent =
    val address: jn.UnixDomainSocketAddress = jn.UnixDomainSocketAddress.of(socketPath.s).nn

    val channel: jnc.ServerSocketChannel =
      jnc.ServerSocketChannel.open(jn.StandardProtocolFamily.UNIX).nn

    channel.configureBlocking(true)
    channel.bind(address)

    @volatile var listening: Boolean = true

    val task = async:
      while listening do
        safely:
          val client: jnc.SocketChannel = channel.accept().nn
          val input  = jnc.Channels.newInputStream(client).nn
          val output = jnc.Channels.newOutputStream(client).nn

          async:
            try converse(input, output) finally safely(client.close())

    new SocketService:
      def stop(): Unit =
        listening = false
        safely(channel.close())
        safely(task.await())

  private def converse(input: ji.InputStream, output: ji.OutputStream)
    ( using Monitor, System, Codicil )
  :   Unit logs CompileEvent =

    // The TCP caller's `listen` owns the accepted socket — it writes this lambda's
    // result to the socket after we return — so we must NOT close it here, or
    // that write fails. And any I/O error (typically the client disconnecting)
    // must not escape: it would propagate out of the accept loop and stop the
    // server from accepting any further connections.
    //
    // Each message is handled in its own task, so a slow `submit` doesn't hold up
    // the `tokenize` replies a client fires while editing: `tokenize` is stateless
    // (runs concurrently), `submit` is serialized by `mutex`, and replies may go
    // back out of order. A write mutex stops concurrent replies interleaving.
    val writes: Mutex = Mutex()

    try
      val reader = ji.BufferedReader(ji.InputStreamReader(input, "UTF-8"))
      val writer = ji.OutputStreamWriter(output, "UTF-8")
      val buffer: StringBuilder = StringBuilder()
      var line: String | Null = reader.readLine()

      while line != null do
        if line.nn.isEmpty then
          if buffer.length > 0 then
            val message: Text = buffer.toString.tt.trim
            buffer.clear()

            async:
              // A stray throwable in one message becomes an error reply, not a
              // dropped connection, so the session survives. `Unset` (a `quit`) is
              // not answered.
              val response: Optional[Text] =
                try respond(message)
                catch case error: Throwable =>
                  encode(Repl.Reply.Failed(0, error.toString.tt))

              response.let: payload =>
                writes:
                  try
                    writer.write(payload.s)
                    writer.write("\n\n")
                    writer.flush()
                  catch case _: Throwable => ()
        else
          buffer.append(line.nn).append("\n")

        line = reader.readLine()

    catch case _: Throwable => ()

  // Decodes one JSON `Request` and dispatches: a `tokenize` only highlights (cheap,
  // for live editing); a `submit` compiles and runs; a `quit` signals the server to
  // shut down and is not answered. A malformed request becomes a `Failed` reply
  // rather than a dropped connection. `Unset` means no reply is sent.
  private def respond(message: Text)(using Monitor, System, Codicil)
  :   Optional[Text] logs CompileEvent =

    val request: Optional[Repl.Request] =
      safely[Exception](Json.parseTracked(message).as[Repl.Request])

    request.lay(encode(Repl.Reply.Failed(0, t"the request could not be parsed"))):
      case Repl.Request.Tokenize(id, code)         => tokenized(id, code)
      case Repl.Request.Submit(id, code)           => submit(id, code)
      case Repl.Request.Complete(id, code, offset) => complete(id, code, offset)
      case Repl.Request.Quit(_)                    => quit.offer(()) yet Unset

  private def tokenized(id: Int, code: Text): Text =
    encode(Repl.Reply.Tokenized(id, Repl.tokenize(code)))

  // Computes tab completions at `offset` in `code` and replies (with `id`). Holds
  // `mutex` like `submit`, since the shared compiler is not reentrant.
  private def complete(id: Int, code: Text, offset: Int)(using Monitor, System, Codicil)
  :   Text logs CompileEvent =

    given LocalClasspath = classpath

    val items: List[Repl.CompletionItem] = mutex(safely(Repl.complete(code, offset)).or(Nil))

    encode(Repl.Reply.Completed(id, items))

  // Typecheck-highlights, compiles, and runs `code`, replying (with `id`) with the
  // highlighting, the result value, and any diagnostics. The whole body holds
  // `mutex`, so concurrent submits serialize and never drive the compiler at once.
  private def submit(id: Int, code: Text)(using Monitor, System, Codicil): Text logs CompileEvent =
    given LocalClasspath = classpath

    val reply: Repl.Reply = mutex:
      val tokens      = Repl.highlight(code)
      val unprocessed = Repl.Reply.Failed(id, t"the input could not be processed")

      safely(interpret(code)).lay(unprocessed):
        case Outcome.Ran(notices, value, output) =>
          // The result type is rendered to text (`Syntax.qualified`) for the wire;
          // sending the structured `Syntax` would need bespoke jacinta instances.
          val tpe: Optional[Text] = value.lay(Unset)(_ => Repl.resultType(code).let(_.qualified))
          Repl.Reply.Ran(id, value, output, tpe, notices.map(_.message).join(t"; "), tokens)

        case Outcome.Rejected(notices) =>
          Repl.Reply.Rejected(id, notices.map(_.message).join(t"; "), tokens)

        case Outcome.Threw(_, error, output) =>
          Repl.Reply.Threw(id, output, error.toString.tt, tokens)

        case Outcome.Crashed(notices, _) =>
          Repl.Reply.Crashed(id, notices.map(_.message).join(t"; "), tokens)

    encode(reply)

  // `Json` is `Dynamic`, so `.show`/`.root` are intercepted; summon the
  // `Encodable in Text` instance explicitly to render compact JSON.
  private def encode(reply: Repl.Reply): Text =
    summon[Json is Encodable in Text].encoded(reply.json)
