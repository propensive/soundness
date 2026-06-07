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
import java.net as jn
import java.nio.file as jnf

import scala.quoted.*

import ambience.*
import anthology.*
import anticipation.*
import coaxial.*
import contingency.*
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

import interfaces.paths.pathOnLinux

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
    case Ran(notices: List[Notice], value: Optional[Text])
    case Threw(notices: List[Notice], error: Throwable)
    case Rejected(notices: List[Notice])
    case Crashed(notices: List[Notice], error: StackTrace)

  // One syntax-highlighting token of the submitted line: its verbatim text, the
  // lowercased name of its Harlequin accent (`keyword`, `ident`, `number`, …),
  // and — where the typechecker resolved one — the fully-qualified Scala type of
  // the token (`Unset` for keywords, punctuation, whitespace, …). Sent in the
  // JSON response for the client to colourise; ANSI rendering is the client's
  // concern.
  case class Token(text: Text, accent: Text, tpe: Optional[Text])

  // The reply sent to a connected client, serialized as JSON. The `highlight`
  // tokens are the Harlequin tokenization of the submitted line.
  case class Response(status: Text, value: Text, diagnostics: Text, highlight: List[Token])

  // Highlights a line of Scala with Harlequin's typechecked pipeline (the
  // compiler resolves symbols, so accents are more accurate than the bare lexer)
  // and projects each token to the minimal `(text, accent)` pair carried in the
  // response. Needs the session's `Scalac` and compile classpath.
  def highlight(code: Text)(using Scalac[?], LocalClasspath): List[Token] =
    import highlighting.typecheckedScala
    Scala.highlight(code).lines.to(List).flatten.map: token =>
      Token(token.text, token.accent.toString.tt.lower, token.meta.let(_.tpe.qualified))

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

        try
          // Seed accessors read their session from this thread-local.
          ReplBridge.setCurrentSession(session)
          loader.on(t"$name$$")
          Outcome.Ran(notices, rendered)
        catch
          case error: ExceptionInInitializerError =>
            Outcome.Threw(notices, Optional(error.getCause).or(error))

          // Running arbitrary user code can throw anything, including `Error`s
          // (`LinkageError`, `StackOverflowError`, …), which must not escape and
          // kill the session.
          case error: Throwable =>
            Outcome.Threw(notices, error)

  private def statementCode(line: Text): Text =
    (prelude.imports.map(_.tt) :+ line).join(t"\n")

  // Wraps an expression line as `val resN = <line>`, then renders the bound value
  // through `Inspectable` and stashes the rendering in `ReplBridge`. The renderer
  // sits in an `@experimental` scope because `Inspectable` is `@experimental`, so
  // this compiles even when the contextual `Scalac` is not in experimental mode.
  private def expressionCode(name: Text, key: Text, line: Text): Text =
    val put: Text = t"colloquy.ReplBridge.put(${session.toString.tt}L, \"$key\", $name.inspect)"

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
        Outcome.Ran(Nil, Unset)
      else
        Outcome.Rejected(errors.map(Notice(Importance.Error, t"<seed>", _, Unset)))

  def interpret(line: Text)(using Monitor, System, Codicil)
  :   Outcome logs CompileEvent raises CompilerError raises AsyncError =

    def lineOutcome: Outcome = evaluate(line)

    ensureSeeded().lay(lineOutcome):
      case _: Outcome.Ran => lineOutcome
      case failure        => failure

  // Starts a TCP server on `port` and accepts connections. Each connection is an
  // interactive session over the *same* REPL state, speaking a plain-text,
  // double-newline-delimited protocol (a blank line ends a message), so it can
  // be driven with `telnet`. Returns a handle whose `stop()` shuts the server
  // down. A binary protocol and a dedicated client will replace the text framing
  // later.
  def serve(port: Port over Tcp)(using Monitor, System, Codicil)
  :   SocketService logs CompileEvent raises BindError raises StreamError =

    port.listen: socket =>
      converse(socket)
      Data()

  private def converse(socket: jn.Socket)(using Monitor, System, Codicil)
  :   Unit logs CompileEvent =

    val reader = ji.BufferedReader(ji.InputStreamReader(socket.getInputStream.nn, "UTF-8"))
    val writer = ji.OutputStreamWriter(socket.getOutputStream.nn, "UTF-8")

    try
      val buffer: StringBuilder = StringBuilder()
      var line: String | Null = reader.readLine()

      while line != null do
        if line.nn.isEmpty then
          if buffer.length > 0 then
            val message: Text = buffer.toString.tt.trim
            buffer.clear()

            // A stray throwable in one message becomes an error reply, not a
            // dropped connection, so the session survives.
            val response: Text =
              try respond(message) catch case error: Throwable => t"! error: ${error.toString.tt}"

            writer.write(response.s)
            writer.write("\n\n")
            writer.flush()
        else
          buffer.append(line.nn).append("\n")

        line = reader.readLine()

    finally
      safely(reader.close())
      safely(writer.close())
      safely(socket.close())

  private def respond(message: Text)(using Monitor, System, Codicil): Text logs CompileEvent =
    given LocalClasspath = classpath
    val tokens      = Repl.highlight(message)
    val unprocessed = Repl.Response(t"error", t"", t"the input could not be processed", tokens)

    val response: Repl.Response =
      safely(mutex(interpret(message))).lay(unprocessed):
        case Outcome.Ran(notices, value) =>
          Repl.Response(t"ran", value.or(t""), notices.map(_.message).join(t"; "), tokens)

        case Outcome.Rejected(notices) =>
          Repl.Response(t"rejected", t"", notices.map(_.message).join(t"; "), tokens)

        case Outcome.Threw(_, error) =>
          Repl.Response(t"threw", t"", error.toString.tt, tokens)

        case Outcome.Crashed(notices, _) =>
          Repl.Response(t"crashed", t"", notices.map(_.message).join(t"; "), tokens)

    // `Json` is `Dynamic`, so `.show`/`.root` are intercepted; summon the
    // `Encodable in Text` instance explicitly to render compact JSON.
    summon[Json is Encodable in Text].encoded(response.json)
