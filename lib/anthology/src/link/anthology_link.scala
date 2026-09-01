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
import scala.util.control as suc

import org.scalajs.linker.interface.{ESVersion, ModuleInitializer, ModuleKind, StandardConfig}
import org.scalajs.linker.{PathIRContainer, PathOutputDirectory, StandardImpl}
import org.scalajs.logging.{Level, Logger}

import ambience.*
import anticipation.*
import contingency.*
import digression.*
import galilei.*
import hellenism.*
import parasite.*
import prepositional.*
import rudiments.*
import serpentine.*

object linkerOptions:
  private def applicable(format: Format): Boolean =
    format.isInstanceOf[Js] || format == Wasm || format.isInstanceOf[Wasi]

  private def sjs(edit: StandardConfig => StandardConfig): Toolchain.Setting =
    Toolchain.Setting[StandardConfig](applicable)(edit)

  val checkIr: Toolchain.Setting = sjs(_.withCheckIR(true))
  val sourceMaps: Toolchain.Setting = sjs(_.withSourceMap(true))

  object esVersion:
    private def of(version: ESVersion): Toolchain.Setting = sjs(_.withESFeatures(_.withESVersion(version)))

    val es2015: Toolchain.Setting = of(ESVersion.ES2015)
    val es2016: Toolchain.Setting = of(ESVersion.ES2016)
    val es2017: Toolchain.Setting = of(ESVersion.ES2017)
    val es2018: Toolchain.Setting = of(ESVersion.ES2018)
    val es2019: Toolchain.Setting = of(ESVersion.ES2019)
    val es2020: Toolchain.Setting = of(ESVersion.ES2020)
    val es2021: Toolchain.Setting = of(ESVersion.ES2021)
    val es2022: Toolchain.Setting = of(ESVersion.ES2022)

  object optimize:
    val none: Toolchain.Setting = sjs(_.withOptimizer(false))
    val fast: Toolchain.Setting = sjs(_.withOptimizer(true))

// The sjsir edges of a toolchain: `Sjsir` to each JavaScript module system, to browser Wasm,
// and—when the WASI toolchain is probed and a WIT world given—to a WASI 0.2 component. All
// share the Scala.js linker and its `StandardConfig` settings.
object sjsEdges:
  def apply(): List[Edge] =
    List
      ( edge(Js(Js.Module.Es), _.withModuleKind(ModuleKind.ESModule), _ / "main.js"),
        edge(Js(Js.Module.CommonJs), _.withModuleKind(ModuleKind.CommonJSModule), _ / "main.js"),
        edge(Js(Js.Module.Script), _.withModuleKind(ModuleKind.NoModule), _ / "main.js"),
        edge
          ( Wasm,
            _.withModuleKind(ModuleKind.ESModule)
             .withESFeatures(_.withESVersion(ESVersion.ES2022).withUseWebAssembly(true)),
            _ / "main.wasm" ) )

  // The component edge exists only where the WASI toolchain (`wasm-tools`, `wit-bindgen`) has
  // been probed and a WIT world chosen; a WASI link whose native tooling is absent is not
  // expressible.
  def wasi()(using toolchain: WasiToolchain, world: Wasi.World): Edge =
    edge(Wasi(Wasi.Version.Wasip2), wasiConfig(world), _ / "main.wasm")

  // The configuration mandated by a WASI component link against the given world.
  private def wasiConfig(world: Wasi.World): StandardConfig => StandardConfig =
    _.withModuleKind(ModuleKind.WasmComponent)
    . withESFeatures(_.withESVersion(ESVersion.ES2022).withUseWebAssembly(true))
    . withWasmFeatures: features =>
        features
        . withWitDirectory(Some(world.directory.encode.s))
        . withWitWorld(Some(world.world.s))

  private def edge
    ( node:     Format.Application,
      base:     StandardConfig => StandardConfig,
      artifact: (Path on Linux) => (Path on Linux) )
  :   Edge =

    Edge(Universe.Sjsir, node, SjsTool(node, base, artifact))

  private case class SjsTool
    ( node:     Format.Application,
      base:     StandardConfig => StandardConfig,
      artifact: (Path on Linux) => (Path on Linux) )
  extends Tool:
    type Settings = StandardConfig

    def name: Text = node.id
    def initial: StandardConfig = base(StandardConfig())

    def run
      ( settings:    StandardConfig,
        input:       Deliverable,
        entryPoints: List[EntryPoint],
        out:         Path on Linux )
      ( using Monitor, System, WorkingDirectory )
      ( using Tactic[Link.Error], LinkEvent is Loggable )
    :   Deliverable =

      val (directory, classpath) = input.emission(node)
      Deliverable.Product(link0(settings, directory, classpath, entryPoints, out, artifact))

  // The link pipeline itself: drives the Scala.js linker over the emission directory and its
  // classpath, and returns the artifact's path within the output directory.
  private def link0
    ( form:        StandardConfig,
      directory:   Path on Linux,
      classpath:   LocalClasspath,
      entryPoints: List[EntryPoint],
      out:         Path on Linux,
      artifact:    (Path on Linux) => (Path on Linux) )
  :   Path on Linux logs LinkEvent raises Link.Error =

    val entries: List[jnf.Path] =
      jnf.Paths.get(directory.encode.s).nn ::
        classpath.entries.bind:
          case Classpath.Entry.Directory(directory) => List(jnf.Paths.get(directory.s).nn)
          case Classpath.Entry.Jar(jar)             => List(jnf.Paths.get(jar.s).nn)
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
        // `.stdlib`: the Scala.js linker API takes stdlib `Seq`s.
        Await.result(PathIRContainer.fromClasspath(entries.stdlib), 300.seconds)

      val irFiles = Await.result(cache.cached(containers), 300.seconds)
      val output = PathOutputDirectory(outPath)
      // `.stdlib`: as above — `Linker#link` takes a stdlib `Seq` of initializers.
      Await.result(linker.link(irFiles, initializers.stdlib, output, logger), 1800.seconds)
      artifact(out)

    catch case suc.NonFatal(error) =>
      abort(Link.Error(Link.Error.Reason.Failed(error.stackTrace)))
