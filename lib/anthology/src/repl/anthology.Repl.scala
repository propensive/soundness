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
package anthology

import java.nio.file as jnf

import scala.quoted.*
import scala.util.control as suc

import ambience.*
import anticipation.*
import contingency.*
import digression.*
import distillate.*
import gossamer.*
import hellenism.*
import inimitable.*
import parasite.*
import prepositional.*
import serpentine.*
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
    case Ran(notices: List[Notice])
    case Threw(notices: List[Notice], error: Throwable)
    case Rejected(notices: List[Notice])
    case Crashed(notices: List[Notice], error: StackTrace)

  // A value/variable lifted from an inline binding block: its REPL-visible name
  // and the source rendering of its type, used to build a typed accessor.
  case class Binding(mutable: Boolean, name: String, typeName: String)

  object Prelude:
    val empty: Prelude = Prelude(Nil, Nil, Nil)

  // Declarations lifted from an inline binding block to seed the REPL context:
  // `imports` are re-injected into every line; `definitions` and binding
  // accessors are established once in a seed object.
  case class Prelude
    ( imports:     List[String],
      definitions: List[String],
      bindings:    List[Binding] )

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
  ( layout:  Repl.Layout   = Repl.Layout.Standard(),
    prelude: Repl.Prelude  = Repl.Prelude.empty )
  ( using scalac: Scalac[version], classloader: Classloader, temporary: TemporaryDirectory ):

  import Repl.Outcome

  val session: Long = ReplBridge.freshSession()

  private var index:   Int        = 0
  private var history: List[Text] = Nil
  private var seeded:  Boolean     = false

  private val out: Path on Linux = unsafely(temporaryDirectory/Uuid())
  locally(jnf.Files.createDirectories(jnf.Path.of(out.encode.s)).nn)

  private lazy val loader: Classloader =
    LocalClasspath((Classpath.Directory(out) :: Nil)*).classloader(classloader)

  private def classpath(using System): LocalClasspath =
    val entries = Classpath.Directory(out) :: (classloaders.threadContext.classpath.match
      case classpath: LocalClasspath => classpath.entries

      case _ =>
        unsafely(System.properties.java.`class`.path().decode[LocalClasspath]).entries)

    LocalClasspath(entries*)

  private def run(code: Text)(using Monitor, System, Codicil)
  :   Outcome logs CompileEvent raises CompilerError raises AsyncError =

    val name:    Text = layout.objectName(index)
    val source:  Text = layout.wrap(index, history, code)
    val process       = scalac(classpath)(Map(t"$name.scala" -> source), out)
    val result        = process.complete()
    val notices       = process.notices.to(List)

    result match
      case CompileResult.Crash(trace) =>
        Outcome.Crashed(notices, trace)

      case CompileResult.Failure =>
        Outcome.Rejected(notices)

      case CompileResult.Success =>
        index += 1
        history = history :+ name

        try
          loader.on(t"$name$$")
          Outcome.Ran(notices)
        catch
          case error: ExceptionInInitializerError =>
            Outcome.Threw(notices, Optional(error.getCause).or(error))

          case suc.NonFatal(error) =>
            Outcome.Threw(notices, error)

  // The prelude's definitions and binding accessors are compiled once, as the
  // first object (`rs$line$0`); later lines see them via the history import.
  // Imports create no members, so they are re-injected into every line instead.
  private def seedBody: Text =
    val accessors = prelude.bindings.map: binding =>
      val keyword:  Text = if binding.mutable then t"var" else t"val"
      val name:     Text = binding.name.tt
      val typeName: Text = binding.typeName.tt
      val key:      Text = t"${session.toString.tt}L, \"$name\""

      t"$keyword $name: $typeName = anthology.ReplBridge.fetch[$typeName]($key)"

    (prelude.imports.map(_.tt) ::: prelude.definitions.map(_.tt) ::: accessors).join(t"\n")

  private def ensureSeeded()(using Monitor, System, Codicil)
  :   Optional[Outcome] logs CompileEvent raises CompilerError raises AsyncError =

    if seeded || prelude.definitions.isEmpty && prelude.bindings.isEmpty then Unset
    else
      seeded = true
      run(seedBody)

  def interpret(line: Text)(using Monitor, System, Codicil)
  :   Outcome logs CompileEvent raises CompilerError raises AsyncError =

    def lineOutcome: Outcome = run((prelude.imports.map(_.tt) :+ line).join(t"\n"))

    ensureSeeded().lay(lineOutcome):
      case _: Outcome.Ran => lineOutcome
      case failure        => failure
