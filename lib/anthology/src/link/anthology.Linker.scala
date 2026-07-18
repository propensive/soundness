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

import org.scalajs.linker.interface.{ModuleInitializer, StandardConfig}
import org.scalajs.linker.{PathIRContainer, PathOutputDirectory, StandardImpl}
import org.scalajs.logging.{Level, Logger}

import anticipation.*
import contingency.*
import digression.*
import galilei.*
import hellenism.*
import prepositional.*
import serpentine.*

object Linker:
  // An entry point whose `main(args: Array[String])` method runs when the linked module loads.
  case class EntryPoint(mainClass: Fqcn)

  object Option:
    private[anthology] def apply[target <: Backend.Portable]
      ( configure0: StandardConfig => StandardConfig )
    :   Option[target] =

      new Option[target]:
        private[anthology] def configure(config: StandardConfig): StandardConfig =
          configure0(config)

  // Options are constructible only through the `linkerOptions` DSL, keeping the underlying
  // Scala.js linker types out of the public API; contravariance permits an option declared for
  // `Backend.Portable` in the options list of any linker.
  trait Option[-target <: Backend.Portable]:
    private[anthology] def configure(config: StandardConfig): StandardConfig

case class Linker[target <: Backend.Portable]
  ( options: List[Linker.Option[target]], entryPoints: List[Linker.EntryPoint] = Nil ):

  def link(compilation: Compilation[target], out: Path on Linux)(using linkage: Linkage[target])
  :   Path on Linux logs LinkEvent raises LinkError =

    Log.info(LinkEvent.Start)

    val config: StandardConfig =
      options.foldLeft(linkage.configure(StandardConfig())): (config, option) =>
        option.configure(config)

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
      val linker = StandardImpl.linker(config)
      val cache = StandardImpl.irFileCache().newCache

      val (containers, _) =
        Await.result(PathIRContainer.fromClasspath(entries), 300.seconds)

      val irFiles = Await.result(cache.cached(containers), 300.seconds)
      val output = PathOutputDirectory(outPath)
      Await.result(linker.link(irFiles, initializers, output, logger), 1800.seconds)
      val result = linkage.artifact(out)
      Log.info(LinkEvent.Linked(result.encode))
      result

    catch case suc.NonFatal(error) => abort(LinkError(error.stackTrace))
