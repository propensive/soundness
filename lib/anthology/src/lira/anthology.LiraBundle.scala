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

import scala.jdk.CollectionConverters.IteratorHasAsScala

import anticipation.*
import contingency.*
import gossamer.*
import reliquary.*

// Reads compilation outputs into `LiraAssembler` section inputs. Each universe's section
// carries its IR alongside the TASTy interface files: the shared interface is stored once by
// the root section's tree, and byte-divergent files (a fresh compiler run pickles a fresh UUID)
// surface as minimal overlays — while the atoms, which are semantic, stay identical (L108).
object LiraBundle:
  // Each universe knows its own LIRA section label and the filename suffixes of its stored
  // representations, so one method serves all three.
  def apply[universe <: Universe & Singleton: ValueOf](compilation: Compilation[universe])
  :   LiraAssembler.SectionInput raises Lira.Error =

    val universe: Universe = valueOf[universe]
    section(universe.section, compilation.out.encode, universe.suffixes)

  // The toolchain record for a compilation (§14): the compiler version and the
  // universe-selecting flags of its emission.
  def tool[universe <: Universe](version: Text)(using emission: Universe.Emission[universe])
  :   Lira.Manifest.Tool =

    Lira.Manifest.Tool(t"scala", version, emission.flags)

  private def section(universe: Text, out: Text, suffixes: List[Text])
  :   LiraAssembler.SectionInput raises Lira.Error =

    val root = jnf.Paths.get(out.s).nn

    def wanted(path: jnf.Path): Boolean =
      suffixes.stdlib.exists: suffix => path.toString.endsWith(suffix.s)

    val content = jnf.Files.walk(root).nn.iterator.nn.asScala.to(scala.List)
      . filter(wanted)
      . map: path =>
          val relative = Text(root.relativize(path).nn.toString)
          val data = Array.unsafeFrozen(jnf.Files.readAllBytes(path).nn)
          (TreePath(relative), data)

      . sortBy(_(0).text.s)

    LiraAssembler.SectionInput(universe, content.to(List))
