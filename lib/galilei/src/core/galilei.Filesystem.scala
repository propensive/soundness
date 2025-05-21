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
┃    Soundness, version 0.31.0 for Scala 3.7.                                                      ┃
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
package galilei

import java.nio.file as jnf

import anticipation.*
import contingency.*
import gossamer.*
import nomenclature.*
import prepositional.*
import proscenium.*
import serpentine.*

erased trait Filesystem

object Filesystem:
  type Rules = MustNotContain["\\"] & MustNotContain["/"] & MustNotContain[":"] &
      MustNotContain["*"] & MustNotContain["?"] & MustNotContain["\""] & MustNotContain["<"] &
      MustNotContain[">"] & MustNotContain["|"] & MustNotEnd["."] & MustNotEnd[" "] &
      MustNotMatch["(?i)(CON|PRN|AUX|NUL|COM[0-9]|LPT[0-9])(\\.[^.]+)?"] & MustNotEqual["."] &
      MustNotEqual[".."] & MustNotEqual[""]

  given substantiable: [filesystem <: Filesystem] => (Path on filesystem) is Substantiable =
    path => jnf.Files.exists(path.javaPath)

  given radical: Tactic[PathError]
        =>  Filesystem is Radical from WindowsDrive | Linux.Root | MacOs.Root =
    val os = System.getProperty("os.name").nn.tt

    val delegate =
      if os.starts(t"Windows") then Windows.radical
      else if os.starts(t"Mac") then MacOs.radical else Linux.radical

    delegate.absolve match
      case radical: (Filesystem is Radical from WindowsDrive | Linux.Root | MacOs.Root) => radical

  given navigable: Tactic[NameError]
        =>  Filesystem is Navigable by Name[Filesystem] under Rules =
    val os = System.getProperty("os.name").nn.tt

    val delegate =
      if os.starts(t"Windows") then Windows.navigable
      else if os.starts(t"Mac") then MacOs.navigable else Linux.navigable

    delegate.absolve match
      case navigable: (Filesystem is Navigable by Name[Filesystem] under Rules) => navigable
