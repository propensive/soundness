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

  def jvm(compilation: Compilation[Universe.Classfile])
  :   LiraAssembler.SectionInput raises LiraError =

    section(t"jvm", compilation.out.encode, scala.List(".class", ".tasty"))

  def sjsir(compilation: Compilation[Universe.Sjsir])
  :   LiraAssembler.SectionInput raises LiraError =

    section(t"sjsir", compilation.out.encode, scala.List(".sjsir", ".tasty"))

  def nir(compilation: Compilation[Universe.Nir])
  :   LiraAssembler.SectionInput raises LiraError =

    section(t"nir", compilation.out.encode, scala.List(".nir", ".tasty"))

  // The toolchain record for a compilation (§14): the compiler version and the
  // universe-selecting flags of its emission.
  def tool[universe <: Universe](version: Text)(using emission: Universe.Emission[universe])
  :   LiraManifest.Tool =

    LiraManifest.Tool(t"scala", version, emission.flags)

  private def section(universe: Text, out: Text, suffixes: scala.List[String])
  :   LiraAssembler.SectionInput raises LiraError =

    val root = jnf.Paths.get(out.s).nn

    def wanted(path: jnf.Path): Boolean =
      suffixes.exists: suffix => path.toString.endsWith(suffix)

    val content = jnf.Files.walk(root).nn.iterator.nn.asScala.to(scala.List)
      . filter(wanted)
      . map: path =>
          val relative = Text(root.relativize(path).nn.toString)
          val data = Array.unsafeFrozen(jnf.Files.readAllBytes(path).nn)
          (TreePath(relative), data)

      . sortBy(_(0).text.s)

    LiraAssembler.SectionInput(universe, List.from(content))
