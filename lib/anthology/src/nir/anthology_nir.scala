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

import scala.scalanative.build.{GC, LTO, Mode, NativeConfig}

import ambience.*
import anticipation.*
import contingency.*
import galilei.*
import gossamer.*
import parasite.*
import prepositional.*
import rudiments.*
import serpentine.*
import vacuous.*

// The native edges of a toolchain: `Nir` to a `Binary` per target triple, each driving the
// Scala Native build through the C toolchain probed once for all of them. With no arguments,
// the build host's own triple is the single target.
object nativeEdges:
  def apply(triples: Triple*)(using WorkingDirectory): List[Edge] raises ToolchainError =
    val clang = NativeLinkage.probe(t"clang")
    val clangpp = NativeLinkage.probe(t"clang++")

    val targets: List[Triple] =
      // An unrecognized build host cannot name its own triple, and in any case has no Scala
      // Native runtime; report it as the C toolchain's absence.
      if triples.isEmpty
      then List(Triple.host.or(abort(ToolchainError(t"clang"))))
      else List(triples*)

    targets.map: triple => Edge(Universe.Nir, Binary(triple), NativeTool(triple, clang, clangpp))

  private case class NativeTool(triple: Triple, clang: Text, clangpp: Text) extends Tool:
    type Settings = NativeConfig

    def name: Text = Binary(triple).id

    def initial: NativeConfig =
      NativeLinkage.configuration(clang, clangpp).withTargetTriple(Some(triple.text.s))

    def run
      ( settings:    NativeConfig,
        input:       Deliverable,
        entryPoints: List[EntryPoint],
        out:         Path on Linux )
      ( using Monitor, System, WorkingDirectory )
      ( using Tactic[LinkError], LinkEvent is Loggable )
    :   Deliverable =

      val main = entryPoints match
        case List(entry) => entry.mainClass.text
        case _           => abort(LinkError(LinkError.Reason.NoEntryPoint))

      val (directory, classpath) = input.emission(Binary(triple))
      Deliverable.Product(NativeLinkage.link0(settings, directory, classpath, main, out))

object nativeOptions:
  private def native(edit: NativeConfig => NativeConfig): Linker.Option[Artifact.Binary] =
    Linker.Option(edit)

  def target(triple: Triple): Linker.Option[Artifact.Binary] =
    native(_.withTargetTriple(Some(triple.text.s)))

  object gc:
    val immix: Linker.Option[Artifact.Binary] = native(_.withGC(GC.immix))
    val commix: Linker.Option[Artifact.Binary] = native(_.withGC(GC.commix))
    val boehm: Linker.Option[Artifact.Binary] = native(_.withGC(GC.boehm))
    val none: Linker.Option[Artifact.Binary] = native(_.withGC(GC.none))

  object mode:
    val debug: Linker.Option[Artifact.Binary] = native(_.withMode(Mode.debug))
    val releaseFast: Linker.Option[Artifact.Binary] = native(_.withMode(Mode.releaseFast))
    val releaseFull: Linker.Option[Artifact.Binary] = native(_.withMode(Mode.releaseFull))

  object lto:
    val none: Linker.Option[Artifact.Binary] = native(_.withLTO(LTO.none))
    val thin: Linker.Option[Artifact.Binary] = native(_.withLTO(LTO.thin))
    val full: Linker.Option[Artifact.Binary] = native(_.withLTO(LTO.full))
