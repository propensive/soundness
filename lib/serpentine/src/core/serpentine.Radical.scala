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
┃    Soundness, version 0.54.0.                                                                    ┃
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
package serpentine

import anticipation.*
import contingency.*
import denominative.*
import gossamer.*
import prepositional.*
import rudiments.*
import vacuous.*

object Radical:
  given drive: Tactic[PathError] => Drive is Radical:
    type Plane = Windows

    def decode(text: Text): Drive =
      if text.length >= 3 && text.at(Sec) == ':' && text.at(Ter) == '\\'
      then Drive(text.at(Prim).vouch)
      else raise(PathError(_.InvalidRoot)) yet Drive('C')

    def length(text: Text): Int = 3
    def encode(drive: Drive): Text = t"${drive.letter}:\\"

  given linux: Tactic[PathError] => %.type is Radical:
    type Plane = Linux

    def length(text: Text): Int = 1

    def decode(text: Text): %.type =
      if text.starts(t"/") then % else raise(PathError(_.InvalidRoot)) yet %

    def encode(root: %.type): Text = t"/"

  given posix: Tactic[PathError] => %.type is Radical:
    type Plane = Posix

    def length(text: Text): Int = 1

    def decode(text: Text): %.type =
      if text.starts(t"/") then % else raise(PathError(_.InvalidRoot)) yet %

    def encode(root: %.type): Text = t"/"

  given macOs: Tactic[PathError] => %.type is Radical:
    type Plane = MacOs

    def length(text: Text): Int = 1

    def decode(text: Text): %.type =
      if text.starts(t"/") then % else raise(PathError(_.InvalidRoot)) yet %

    def encode(root: %.type): Text = t"/"

  given local: Tactic[PathError] => (%.type | Drive) is Radical:
    type Plane = Local

    def length(text: Text): 1 | 3 =
      if text.starts(t"/") then 1 else if text.s(1) == ':' && text.s(2) == '\\' then 3
      else abort(PathError(_.InvalidRoot))

    def decode(text: Text): %.type | Drive =
      length(text) match
        case 1 => %
        case 3 => Drive(text.s(0).lest(PathError(_.InvalidRoot)))

    def encode(root: %.type | Drive): Text = root match
      case Drive(letter) => t"$letter:\\"
      case %             => t"/"

trait Radical extends Typeclass, Planar:
  def decode(text: Text): Self
  def length(text: Text): Int
  def encode(self: Self): Text
