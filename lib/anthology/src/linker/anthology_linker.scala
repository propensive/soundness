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

import scala.util.control as suc

import ambience.*
import anticipation.*
import contingency.*
import digression.*
import galilei.*
import gossamer.*
import hellenism.*
import parasite.*
import prepositional.*
import serpentine.*
import vacuous.*

object jarOptions:
  // The filename of the JAR within the output directory, for both `Jar` and `Library` nodes.
  def name(name: Text): Toolchain.Setting =
    Toolchain.Setting[Text](format => format == Jar || format.isInstanceOf[Library])(_ => name)

// The JAR-packaging edges of a toolchain: `Classfile` to `Jar` (an executable JAR of the whole
// classpath) and each universe to its `Library` (a JAR of the compilation's own output). Both
// tools' settings are the JAR's filename within the output directory.
object jarEdges:
  def apply(): List[Edge] =
    List
      ( Edge(Universe.Classfile, Jar, JarTool),
        Edge(Universe.Classfile, Library(Universe.Classfile), LibraryTool(Universe.Classfile)),
        Edge(Universe.Sjsir, Library(Universe.Sjsir), LibraryTool(Universe.Sjsir)),
        Edge(Universe.Nir, Library(Universe.Nir), LibraryTool(Universe.Nir)) )

  private object JarTool extends Tool:
    type Settings = Text

    def name: Text = t"jar"
    def initial: Text = t"main.jar"

    def run
      ( settings:    Text,
        input:       Deliverable,
        entryPoints: List[EntryPoint],
        out:         Path on Linux )
      ( using Monitor, System, WorkingDirectory )
      ( using Tactic[Link.Error], LinkEvent is Loggable )
    :   Deliverable =

      val (directory, classpath) = input.emission(Jar)

      val main: Optional[Fqcn] = entryPoints match
        case Nil         => Unset
        case List(entry) => entry.mainClass
        case _           => abort(Link.Error(Link.Error.Reason.ManyEntryPoints))

      try
        jnf.Files.createDirectories(jnf.Paths.get(out.encode.s))
        val entries = Classpath.Directory(directory) :: classpath.entries
        val jarfile = unsafely(Bundler.assemble(LocalClasspath(entries*), out / settings, main))
        Deliverable.Product(jarfile)

      catch case suc.NonFatal(error) =>
        abort(Link.Error(Link.Error.Reason.Failed(error.stackTrace)))

  private case class LibraryTool(universe: Universe) extends Tool:
    type Settings = Text

    def name: Text = t"library"
    def initial: Text = t"main.jar"

    // A library packages only the compilation's own output, and entry points do not apply.
    def run
      ( settings:    Text,
        input:       Deliverable,
        entryPoints: List[EntryPoint],
        out:         Path on Linux )
      ( using Monitor, System, WorkingDirectory )
      ( using Tactic[Link.Error], LinkEvent is Loggable )
    :   Deliverable =

      val (directory, _) = input.emission(Library(universe))

      try
        jnf.Files.createDirectories(jnf.Paths.get(out.encode.s))
        val entries = List(Classpath.Directory(directory))
        val jarfile = unsafely(Bundler.assemble(LocalClasspath(entries*), out / settings, Unset))
        Deliverable.Product(jarfile)

      catch case suc.NonFatal(error) =>
        abort(Link.Error(Link.Error.Reason.Failed(error.stackTrace)))
