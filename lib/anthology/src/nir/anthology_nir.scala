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

import scala.concurrent.*
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*
import scala.scalanative.build.{Build, Config, GC, LTO, Mode, NativeConfig}
import scala.scalanative.util.Scope
import scala.util.control as suc

import ambience.*
import anticipation.*
import contingency.*
import digression.*
import distillate.*
import eucalyptus.*
import galilei.*
import gossamer.*
import guillotine.*
import hellenism.*
import parasite.*
import prepositional.*
import rudiments.*
import serpentine.*
import vacuous.*

object nativeOptions:
  private def native(edit: NativeConfig => NativeConfig): Toolchain.Setting =
    Toolchain.Setting[NativeConfig](_.isInstanceOf[Binary])(edit)

  object gc:
    val immix: Toolchain.Setting = native(_.withGC(GC.immix))
    val commix: Toolchain.Setting = native(_.withGC(GC.commix))
    val boehm: Toolchain.Setting = native(_.withGC(GC.boehm))
    val none: Toolchain.Setting = native(_.withGC(GC.none))

  object mode:
    val debug: Toolchain.Setting = native(_.withMode(Mode.debug))
    val releaseFast: Toolchain.Setting = native(_.withMode(Mode.releaseFast))
    val releaseFull: Toolchain.Setting = native(_.withMode(Mode.releaseFull))

  object lto:
    val none: Toolchain.Setting = native(_.withLTO(LTO.none))
    val thin: Toolchain.Setting = native(_.withLTO(LTO.thin))
    val full: Toolchain.Setting = native(_.withLTO(LTO.full))

// The native edges of a toolchain: `Nir` to a `Binary` per target triple, each driving the
// Scala Native build through the C toolchain (`clang` and `clang++`) probed once for all of
// them — a native link whose native tooling is absent is not expressible. With no arguments,
// the build host's own triple is the single target; targets beyond the host require a C
// toolchain (and sysroot) capable of cross-compilation.
object nativeEdges:
  def apply(triples: Triple*)(using WorkingDirectory): List[Edge] raises Toolchain.Error =
    val clang = probe(t"clang")
    val clangpp = probe(t"clang++")

    val targets: List[Triple] =
      // An unrecognized build host cannot name its own triple, and in any case has no Scala
      // Native runtime; report it as the C toolchain's absence.
      if triples.isEmpty
      then List(Triple.host.or(abort(Toolchain.Error(t"clang"))))
      else List(triples*)

    targets.map: triple => Edge(Universe.Nir, Binary(triple), NativeTool(triple, clang, clangpp))

  private def probe(tool: Text)(using WorkingDirectory): Text raises Toolchain.Error =
    safely(mute[Exec.Event](sh"which $tool".exec[Text]())).let(_.trim)
    . or(abort(Toolchain.Error(tool)))

  private case class NativeTool(triple: Triple, clang: Text, clangpp: Text) extends Tool:
    type Settings = NativeConfig

    def name: Text = Binary(triple).id

    def initial: NativeConfig =
      NativeConfig.empty
      . withClang(jnf.Paths.get(clang.s).nn)
      . withClangPP(jnf.Paths.get(clangpp.s).nn)
      . withGC(GC.immix)
      . withMode(Mode.debug)
      . withLTO(LTO.none)
      . withBaseName("main")
      . withTargetTriple(Some(triple.text.s))

    def run
      ( settings:    NativeConfig,
        input:       Deliverable,
        entryPoints: List[EntryPoint],
        out:         Path on Linux )
      ( using Monitor, System, WorkingDirectory )
      ( using Tactic[Link.Error], LinkEvent is Loggable )
    :   Deliverable =

      val main = entryPoints match
        case List(entry) => entry.mainClass.text
        case _           => abort(Link.Error(Link.Error.Reason.NoEntryPoint))

      val (directory, classpath) = input.emission(Binary(triple))

      val entries: List[jnf.Path] =
        jnf.Paths.get(directory.encode.s).nn ::
          classpath.entries.bind:
            case Classpath.Entry.Directory(directory) => List(jnf.Paths.get(directory.s).nn)
            case Classpath.Entry.Jar(jar)             => List(jnf.Paths.get(jar.s).nn)
            case _                                   => Nil

      try
        val outPath = jnf.Paths.get(out.encode.s).nn
        jnf.Files.createDirectories(outPath)
        given Scope = Scope.forever

        val config =
          Config.empty
          . withBaseDir(outPath.toAbsolutePath.nn)
          . withMainClass(Some(main.s))
          . withClassPath(entries.stdlib)
          . withModuleName("main")
          . withCompilerConfig(settings)

        val artifact = Await.result(Build.build(config), 1800.seconds)
        Deliverable.Product(unsafely(artifact.toString.tt.as[Path on Linux]))

      catch case suc.NonFatal(error) =>
        abort(Link.Error(Link.Error.Reason.Failed(error.stackTrace)))
