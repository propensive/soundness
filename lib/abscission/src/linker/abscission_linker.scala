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
package abscission

import java.nio.file as jnf

import scala.util.control as suc

import ambience.*
import anthology.*
import anticipation.*
import contingency.*
import digression.*
import galilei.*
import gossamer.*
import hellenism.*
import nomenclature.*
import parasite.*
import prepositional.*
import rudiments.*
import serpentine.*
import turbulence.*
import vacuous.*
import zeppelin.*

import anthology.Toolchain.{Tool, Universe}

object shrinkOptions:
  // A pattern of the classes to keep whole, which reflection reaches.
  def keep(pattern: Text): Toolchain.Setting =
    Toolchain.Setting[Shrink](_ == Shrunk): shrink => shrink.copy(keep = pattern :: shrink.keep)

  // A pattern of the top-level classes whose units are copied as they are, and not recompiled.
  def pin(pattern: Text): Toolchain.Setting =
    Toolchain.Setting[Shrink](_ == Shrunk): shrink => shrink.copy(pin = pattern :: shrink.pin)

  // Whether every implementation which a `META-INF/services` file names is an entry point.
  def services(enabled: Boolean): Toolchain.Setting =
    Toolchain.Setting[Shrink](_ == Shrunk)(_.copy(services = enabled))

  // Whether a library with no TASTy is taken a class at a time, rather than whole.
  def fine(enabled: Boolean): Toolchain.Setting =
    Toolchain.Setting[Shrink](_ == Shrunk)(_.copy(fine = enabled))

  // The filename of the JAR within the output directory.
  def name(name: Text): Toolchain.Setting = Toolchain.Setting[Text](_ == Shrunk.Jar)(_ => name)

// The edges which make shrinking a step of a toolchain: `Classfile` to `Shrunk`, a closed world
// of the classes the entry points reach, and `Shrunk` to `Shrunk.Jar`, an executable JAR of it.
// The first edge carries the options of the `Scalac` which compiled the classes, since they are
// recompiled from their TASTy with them; its settings are the rest of a `Shrink`.
object shrinkEdges:
  def apply[version <: Scalac.Versions](scalac: Scalac[version, ?]): List[Edge] =
    List
      ( Edge(Universe.Classfile, Shrunk, ShrinkTool(Shrink(options = scalac.commandLineArguments))),
        Edge(Shrunk, Shrunk.Jar, JarTool) )

  private case class ShrinkTool(initial: Shrink) extends Tool:
    type Settings = Shrink

    def name: Text = t"shrink"

    def run
      ( settings:    Shrink,
        input:       Deliverable,
        entryPoints: List[EntryPoint],
        out:         Path on Local )
      ( using Monitor, System, WorkingDirectory )
      ( using Tactic[Link.Error], LinkEvent is Loggable )
    :   Deliverable =

      val (directory, classpath) = input.emission(Shrunk)
      val entries = Classpath.Directory(directory) :: classpath.entries

      val report: Shrink.Report =
        mitigate:
          case error: Shrink.Error =>
            val detail: Text = error.message.text

            val reason: Link.Error.Reason = error.reason match
              case Shrink.Error.Reason.Failed(trace) => Link.Error.Reason.Failed(trace)
              case _                                 => Link.Error.Reason.Packaging(detail)

            Link.Error(reason)(using error.diagnostics)

        . protect(settings(LocalClasspath(entries*), entryPoints.map(_.mainClass), out))

      val kept: Text = t"${report.kept} of ${report.classes} classes"
      val read: Text = t"${report.read} of ${report.units} units"
      Log.info(LinkEvent.Message(t"kept $kept, having recompiled $read"))

      Deliverable.Emission(out, LocalClasspath())

  private object JarTool extends Tool:
    type Settings = Text

    def name: Text = t"jar"
    def initial: Text = t"main.jar"

    def run
      ( settings:    Text,
        input:       Deliverable,
        entryPoints: List[EntryPoint],
        out:         Path on Local )
      ( using Monitor, System, WorkingDirectory )
      ( using Tactic[Link.Error], LinkEvent is Loggable )
    :   Deliverable =

      val (directory, classpath) = input.emission(Shrunk.Jar)

      val main: Optional[Fqcn] = entryPoints match
        case Nil         => Unset
        case List(entry) => entry.mainClass
        case _           => abort(Link.Error(Link.Error.Reason.ManyEntryPoints))

      try
        jnf.Files.createDirectories(jnf.Paths.get(out.encode.s))
        val entries = Classpath.Directory(directory) :: classpath.entries

        val jarfile =
          mitigate:
            case error: Zip.Error =>
              Link.Error(Link.Error.Reason.Packaging(error.message.text))(using error.diagnostics)

            case error: Path.Error =>
              Link.Error(Link.Error.Reason.Packaging(error.message.text))(using error.diagnostics)

            case error: Io.Error =>
              Link.Error(Link.Error.Reason.Packaging(error.message.text))(using error.diagnostics)

            case error: Truncation.Error =>
              Link.Error(Link.Error.Reason.Packaging(error.message.text))(using error.diagnostics)

            case error: Name.Error =>
              Link.Error(Link.Error.Reason.Packaging(error.message.text))(using error.diagnostics)

          . protect(Bundler.assemble(LocalClasspath(entries*), out / settings, main))

        Deliverable.Product(jarfile)

      catch case suc.NonFatal(error) =>
        abort(Link.Error(Link.Error.Reason.Failed(error.stackTrace)))
