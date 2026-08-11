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
package anthology

import java.nio.file as jnf

import ambience.*
import anticipation.*
import contingency.*
import fulminate.*
import galilei.*
import gossamer.*
import parasite.*
import prepositional.*
import serpentine.*

import errorDiagnostics.emptyDiagnostics
import probates.cancelProbate

// The Java compile edge of a toolchain: `Language.Java` to the classfile universe. Java has one
// backend, so unlike Scala there is nothing to choose.
object javacEdges:
  def apply(javac: Javac): List[Edge] =
    List(Edge(Language.Java, Universe.Classfile, JavacTool(javac)))

  private case class JavacTool(javac: Javac) extends Tool:
    type Settings = Unit

    def name: Text = t"javac"
    def initial: Unit = ()

    def run
      ( settings:    Unit,
        input:       Deliverable,
        entryPoints: List[EntryPoint],
        out:         Path on Linux )
      ( using Monitor, System, WorkingDirectory )
      ( using tactic: Tactic[LinkError], linkEvents: LinkEvent is Loggable )
    :   Deliverable =

      val (sources, classpath) = input.sources(Universe.Classfile)
      jnf.Files.createDirectories(jnf.Paths.get(out.encode.s))

      given compileEvents: (CompileEvent is Loggable) = CompileEvents.relay(using linkEvents)

      mitigate:
        case CompilerError() => LinkError(LinkError.Reason.CompilerUnusable(t"javac"))
        case Async.Error(_)   => LinkError(LinkError.Reason.CompilerUnusable(t"javac"))

      . protect:
          val process = javac(classpath)(sources, out)

          process.complete() match
            case CompileResult.Success  => Deliverable.Emission(out, classpath)
            case CompileResult.Crash(_) => abort(LinkError(LinkError.Reason.CompilerCrash))

            case CompileResult.Failure =>
              abort(LinkError(LinkError.Reason.CompilationFailed(process.errors)))
