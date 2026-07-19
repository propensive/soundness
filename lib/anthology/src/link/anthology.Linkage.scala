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
┃    Soundness, version 0.63.0.                                                                    ┃
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
import scala.util.control as suc

import org.scalajs.linker.interface.{ESVersion, ModuleInitializer, ModuleKind, StandardConfig}
import org.scalajs.linker.{PathIRContainer, PathOutputDirectory, StandardImpl}
import org.scalajs.logging.{Level, Logger}

import anticipation.*
import contingency.*
import digression.*
import galilei.*
import hellenism.*
import prepositional.*
import serpentine.*

object Linkage:
  // The sjsir link family: JavaScript, browser Wasm and WASI components share the Scala.js
  // linker pipeline, differing only in the configuration they mandate and the artifact they
  // produce.
  private class Sjs[artifact <: Artifact.Sjs]
    ( base: StandardConfig => StandardConfig, artifact: (Path on Linux) => (Path on Linux) )
  extends Linkage[artifact]:
    type Origin = Universe.Sjsir
    private[anthology] type Form = StandardConfig
    private[anthology] def initial: StandardConfig = base(StandardConfig())

    private[anthology] def link
      ( form:        StandardConfig,
        compilation: Compilation[Universe.Sjsir],
        entryPoints: List[Linker.EntryPoint],
        out:         Path on Linux )
    :   Path on Linux logs LinkEvent raises LinkError =

      val entries: List[jnf.Path] =
        jnf.Paths.get(compilation.out.encode.s).nn ::
          compilation.classpath.entries.to(List).flatMap:
            case ClasspathEntry.Directory(directory) => List(jnf.Paths.get(directory.s).nn)
            case ClasspathEntry.Jar(jar)             => List(jnf.Paths.get(jar.s).nn)
            case _                                   => Nil

      val initializers: List[ModuleInitializer] = entryPoints.map: entry =>
        ModuleInitializer.mainMethodWithArgs(entry.mainClass.text.s, "main")

      object logger extends Logger:
        def log(level: Level, message: => String): Unit =
          if level == Level.Error || level == Level.Warn
          then Log.warn(LinkEvent.Message(message.tt))
          else if level == Level.Info then Log.info(LinkEvent.Message(message.tt))
          else Log.fine(LinkEvent.Message(message.tt))

        def trace(error: => Throwable): Unit = Log.warn(LinkEvent.Message(error.toString.tt))

      try
        val outPath = jnf.Paths.get(out.encode.s).nn
        jnf.Files.createDirectories(outPath)
        val linker = StandardImpl.linker(form)
        val cache = StandardImpl.irFileCache().newCache

        val (containers, _) =
          Await.result(PathIRContainer.fromClasspath(entries), 300.seconds)

        val irFiles = Await.result(cache.cached(containers), 300.seconds)
        val output = PathOutputDirectory(outPath)
        Await.result(linker.link(irFiles, initializers, output, logger), 1800.seconds)
        artifact(out)

      catch case suc.NonFatal(error) =>
        abort(LinkError(LinkError.Reason.Failed(error.stackTrace)))

  given js: (Linkage[Artifact.Js] from Universe.Sjsir) =
    Sjs(_.withModuleKind(ModuleKind.ESModule), _ / "main.js")

  given wasm: (Linkage[Artifact.Wasm] from Universe.Sjsir) =
    Sjs
      ( _.withModuleKind(ModuleKind.ESModule)
        . withESFeatures(_.withESVersion(ESVersion.ES2022).withUseWebAssembly(true)),
        _ / "main.wasm" )

  given wasi(using toolchain: WasiToolchain, world: WitWorld)
  :   (Linkage[Artifact.Wasi[0.2]] from Universe.Sjsir) =
    Sjs
      ( _.withModuleKind(ModuleKind.WasmComponent)
        . withESFeatures(_.withESVersion(ESVersion.ES2022).withUseWebAssembly(true))
        . withWasmFeatures: features =>
            features
            . withWitDirectory(Some(world.directory.encode.s))
            . withWitWorld(Some(world.world.s)),
        _ / "main.wasm" )

// Determines how an artifact is linked from a compilation in its source universe: the
// underlying linker-configuration type (`Form`), the configuration the artifact mandates, and
// the link pipeline itself. Instances exist only for artifacts that are currently producible,
// and those whose runtime prerequisites can be absent are conditional upon evidence of them:
// `wasi` (version 0.2, the one WASI version the linker supports) upon a `WasiToolchain` and a
// `WitWorld`, and the native-binary instance (in the `nir` module) upon probing the C
// toolchain.
trait Linkage[artifact <: Artifact] extends Provenance[artifact]:
  private[anthology] type Form
  private[anthology] def initial: Form

  private[anthology] def link
    ( form:        Form,
      compilation: Compilation[Origin],
      entryPoints: List[Linker.EntryPoint],
      out:         Path on Linux )
  :   Path on Linux logs LinkEvent raises LinkError
