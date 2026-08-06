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

import anticipation.*
import gossamer.*
import serpentine.*

object Universe:
  type Classfile = Universe.Classfile.type
  type Sjsir = Universe.Sjsir.type
  type Nir = Universe.Nir.type

  // Determines the additional compiler flags each universe's emission requires.
  trait Emission[universe <: Universe]:
    def flags: List[Text]

  given classfile: Emission[Classfile]:
    def flags: List[Text] = Nil

  given sjsir: Emission[Sjsir]:
    def flags: List[Text] = List(t"-scalajs")

  // NIR is emitted by the Scala Native compiler plugin rather than by a backend built into the
  // compiler, so compiling into the NIR universe requires evidence of the plugin's location.
  given nir(using plugin: NirPlugin): Emission[Nir] =
    new Emission[Nir]:
      def flags: List[Text] = List(t"-Xplugin:${plugin.jar.encode}")

// The universe a compilation inhabits: the intermediate representation it emits, and hence the
// ecosystem of library artifacts it can link with. `Classfile` is JVM classfiles; `Sjsir` is
// Scala.js IR, whose linked representation (JavaScript, browser Wasm or a WASI component) is
// chosen at link time; `Nir` is Scala Native IR, linked to machine code through LLVM. Each
// universe is an intermediate-representation node of a `Toolchain`.
enum Universe extends Format.Ir:
  case Classfile, Sjsir, Nir

  def id: Text = this match
    case Classfile => t"classfile"
    case Sjsir     => t"sjsir"
    case Nir       => t"nir"

  // The label of the LIRA section holding this universe's content.
  def section: Text = this match
    case Classfile => t"jvm"
    case Sjsir     => t"sjsir"
    case Nir       => t"nir"

  // The filename suffixes of this universe's stored representations: each universe's binary
  // form, plus the TASTy that carries its interface.
  def suffixes: List[Text] = this match
    case Classfile => List(t".class", t".tasty")
    case Sjsir     => List(t".sjsir", t".tasty")
    case Nir       => List(t".nir", t".tasty")
