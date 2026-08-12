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

// The Scala compile edges of a toolchain: `Language.Scala` to each universe the compiler can
// emit. Each edge carries the `Scalac` it drives — options and all — so the compiler's own
// configuration stays where the caller wrote it, and the edge's settings are empty.
object scalacEdges:
  def classfile[version <: Scalac.Versions](scalac: Scalac[version, ?]): Edge =
    edge[version, Universe.Classfile](scalac, Universe.Classfile)

  def sjsir[version <: Scalac.Versions](scalac: Scalac[version, ?]): Edge =
    edge[version, Universe.Sjsir](scalac, Universe.Sjsir)

  // NIR is emitted by the Scala Native compiler plugin rather than by a backend built into the
  // compiler, so this edge exists only given evidence of the plugin's location.
  def nir[version <: Scalac.Versions](scalac: Scalac[version, ?])(using NirPlugin): Edge =
    edge[version, Universe.Nir](scalac, Universe.Nir)

  private def edge[version <: Scalac.Versions, universe <: Universe]
    ( scalac: Scalac[version, ?], universe: universe )
    ( using Universe.Emission[universe] )
  :   Edge =

    Edge(Language.Scala, universe, ScalacTool(scalac.targeting[universe], universe))

  private case class ScalacTool[version <: Scalac.Versions, universe <: Universe]
    ( scalac: Scalac[version, universe], universe: universe )
    ( using Universe.Emission[universe] )
  extends Tool:
    type Settings = Unit

    def name: Text = t"scalac"
    def initial: Unit = ()

    def run
      ( settings:    Unit,
        input:       Deliverable,
        entryPoints: List[EntryPoint],
        out:         Path on Linux )
      ( using Monitor, System, WorkingDirectory )
      ( using tactic: Tactic[LinkError], linkEvents: (LinkEvent is Loggable)^ )
    :   Deliverable =

      val (sources, classpath) = input.sources(universe)
      jnf.Files.createDirectories(jnf.Paths.get(out.encode.s))

      // The compiler's diagnostics are relayed onto the toolchain's own event channel, so a
      // build watching one path sees compilation and linking alike.
      given compileEvents: ((CompileEvent is Loggable)^{linkEvents}) =
        CompileEvents.relay(using linkEvents)

      mitigate:
        case Compiler.Error() => LinkError(LinkError.Reason.CompilerUnusable(t"scalac"))
        case Async.Error(_)   => LinkError(LinkError.Reason.CompilerUnusable(t"scalac"))

      . protect:
          val process = scalac(classpath)(sources, out)

          process.complete() match
            case CompileResult.Success  => Deliverable.Emission(out, classpath)
            case CompileResult.Crash(_) => abort(LinkError(LinkError.Reason.CompilerCrash))

            case CompileResult.Failure =>
              abort(LinkError(LinkError.Reason.CompilationFailed(process.errors)))
