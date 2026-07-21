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
import prepositional.*
import serpentine.*
import vacuous.*

// The Scala Native link family. An instance exists only via the probing `apply`, which verifies
// that the C toolchain (`clang` and `clang++`) the link shells out to is present—so, as with
// WASI components, a native link whose native tooling is absent is not expressible. Put one in
// scope with `given NativeLinkage = NativeLinkage()`.
object NativeLinkage:
  def apply()(using WorkingDirectory)
  :   (Linkage[Artifact.Binary] from Universe.Nir) raises ToolchainError =

    new NativeLinkage(probe(t"clang"), probe(t"clang++"))

  private def probe(tool: Text)(using WorkingDirectory): Text raises ToolchainError =
    safely(mute[ExecEvent](sh"which $tool".exec[Text]())).let(_.trim)
    . or(abort(ToolchainError(tool)))

class NativeLinkage private (clang: Text, clangpp: Text)
extends Linkage[Artifact.Binary]:
  type Origin = Universe.Nir
  private[anthology] type Form = NativeConfig

  private[anthology] def initial: NativeConfig =
    NativeConfig.empty
    . withClang(jnf.Paths.get(clang.s).nn)
    . withClangPP(jnf.Paths.get(clangpp.s).nn)
    . withGC(GC.immix)
    . withMode(Mode.debug)
    . withLTO(LTO.none)
    . withBaseName("main")

  private[anthology] def link
    ( form:        NativeConfig,
      compilation: Compilation[Universe.Nir],
      entryPoints: List[Linker.EntryPoint],
      out:         Path on Linux )
  :   Path on Linux logs LinkEvent raises LinkError =

    val main = entryPoints match
      case List(entry) => entry.mainClass.text
      case _           => abort(LinkError(LinkError.Reason.NoEntryPoint))

    val entries: List[jnf.Path] =
      jnf.Paths.get(compilation.out.encode.s).nn ::
        compilation.classpath.entries.to(List).flatMap:
          case ClasspathEntry.Directory(directory) => List(jnf.Paths.get(directory.s).nn)
          case ClasspathEntry.Jar(jar)             => List(jnf.Paths.get(jar.s).nn)
          case _                                   => Nil

    try
      val outPath = jnf.Paths.get(out.encode.s).nn
      jnf.Files.createDirectories(outPath)
      given Scope = Scope.forever

      val config =
        Config.empty
        . withBaseDir(outPath.toAbsolutePath.nn)
        . withMainClass(Some(main.s))
        . withClassPath(entries)
        . withModuleName("main")
        . withCompilerConfig(form)

      val artifact = Await.result(Build.build(config), 1800.seconds)
      unsafely(artifact.toString.tt.as[Path on Linux])

    catch case suc.NonFatal(error) =>
      abort(LinkError(LinkError.Reason.Failed(error.stackTrace)))
