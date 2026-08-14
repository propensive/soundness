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

// The Kotlin compile edge of a toolchain: `Language.Kotlin` to the classfile universe. Kotlin's
// other backends emit klib rather than a universe anthology models, so only the JVM one is here.
object kotlincEdges:
  def apply[version <: Kotlinc.Versions](kotlinc: Kotlinc[version]): List[Edge] =
    List(Edge(Language.Kotlin, Universe.Classfile, KotlincTool(kotlinc)))

  private case class KotlincTool[version <: Kotlinc.Versions](kotlinc: Kotlinc[version])
  extends Tool:
    type Settings = Unit

    def name: Text = t"kotlinc"
    def initial: Unit = ()

    def run
      ( settings:    Unit,
        input:       Deliverable,
        entryPoints: List[EntryPoint],
        out:         Path on Linux )
      ( using Monitor, System, WorkingDirectory )
      ( using tactic: Tactic[Link.Error], linkEvents: LinkEvent is Loggable )
    :   Deliverable =

      val (sources, classpath) = input.sources(Universe.Classfile)
      jnf.Files.createDirectories(jnf.Paths.get(out.encode.s))

      given compileEvents: (CompileEvent is Loggable) = CompileEvents.relay(using linkEvents)

      mitigate:
        case Compiler.Error() => Link.Error(Link.Error.Reason.CompilerUnusable(t"kotlinc"))
        case Async.Error(_)   => Link.Error(Link.Error.Reason.CompilerUnusable(t"kotlinc"))

      . protect:
          val process = kotlinc(classpath)(sources, out)

          process.complete() match
            case CompileResult.Success  => Deliverable.Emission(out, classpath)
            case CompileResult.Crash(_) => abort(Link.Error(Link.Error.Reason.CompilerCrash))

            case CompileResult.Failure =>
              abort(Link.Error(Link.Error.Reason.CompilationFailed(process.errors)))
