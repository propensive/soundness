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
package harlequin

import anthology.*
import anticipation.*
import gossamer.*
import hellenism.*
import rudiments.*
import vacuous.*

// The compiler an anthology `Scalac` and a hellenism `LocalClasspath` describe, as typechecked
// and compiled highlighting run it.
private[harlequin] class ScalacCompilation(scalac: Scalac[?, ?], localClasspath: LocalClasspath)
extends Highlight.Compilation:
  def arguments: List[Text] = scalac.commandLineArguments
  def compiler(): dotty.tools.dotc.Compiler = Scalac.compiler()

  lazy val classpath: Text =
    localClasspath.entries.flatMap:
      case Classpath.Entry.Directory(directory) => List(directory)
      case Classpath.Entry.Jar(jar)             => List(jar)
      case _                                    => Nil

    . join(java.io.File.pathSeparator.nn.tt)

// A warm anthology compiler session, as typechecked highlighting runs in it: on the session's
// own compiler and in a child of its base context, so a snippet is typed against everything the
// session has compiled, without the classpath's symbol table being loaded for it.
private[harlequin] class SessionCompilation(frontend: Scalac.Session.Frontend)
extends Highlight.Compilation:
  def arguments: List[Text] = frontend.options
  def classpath: Text = frontend.classpath
  def compiler(): dotty.tools.dotc.Compiler = frontend.compiler
  override def context(): Optional[dotty.tools.dotc.core.Contexts.Context] = frontend.context

package highlighting:
  // Typechecked highlighting within a warm compiler session (`scalac.on(classpath).session`),
  // from the `frontend` the session gives of itself. It must run on the session's thread, as
  // the session's compiles do; and since a snippet is typed as a source of the session, the
  // top-level names it defines stay in scope there until the next snippet replaces them.
  given sessionScala(using frontend: Scalac.Session.Frontend): Highlight =
    new Highlight:
      def depth: Depth = Depth.Typechecked
      val compilation: Optional[Highlight.Compilation] = SessionCompilation(frontend)

  given typecheckedScala(using scalac: Scalac[?, ?], classpath: LocalClasspath): Highlight =
    new Highlight:
      def depth: Depth = Depth.Typechecked
      val compilation: Optional[Highlight.Compilation] = ScalacCompilation(scalac, classpath)

  given compiledScala(using scalac: Scalac[?, ?], classpath: LocalClasspath): Highlight =
    new Highlight:
      def depth: Depth = Depth.Compiled
      val compilation: Optional[Highlight.Compilation] = ScalacCompilation(scalac, classpath)
