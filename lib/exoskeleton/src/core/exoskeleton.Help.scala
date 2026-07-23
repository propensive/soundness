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
package exoskeleton

import scala.language.experimental.pureFunctions

import anticipation.*
import escapade.*
import gossamer.*
import hieroglyph.*, textMetrics.uniformMetric
import symbolism.*
import rudiments.*
import vacuous.*

object Help:
  case class Param
    ( name:        Text,
      aliases:     List[Text],
      description: Optional[Text | Teletype],
      repeatable:  Boolean )

  // Render the tree to a `Teletype`, then borrow escapade's `Teletype is Printable` so that a
  // `Help` value can be passed straight to `Out.println` and print nicely on the terminal.
  given printable: Help is Printable = summon[Teletype is Printable].contramap(_.teletype)

case class Help
  ( command:     Text,
    description: Optional[Text | Teletype],
    parameters:  List[Help.Param],
    subcommands: List[Help] ):

  def teletype: Teletype = lines(0).join(e"\n")

  private def label(param: Help.Param): Text =
    ((param.name :: param.aliases): List[Text]).join(t", ")

  private def lines(depth: Int): List[Teletype] =
    val indent: Text = t"  "*depth

    val title: Teletype = description.absolve match
      case Unset              => e"$indent$Bold($command)"
      case text: Text         => e"$indent$Bold($command)  $text"
      case teletype: Teletype => e"$indent$Bold($command)  $teletype"

    val width: Int = parameters.stdlib.map(label(_).length).maxOption.getOrElse(0)

    val paramLines: List[Teletype] = parameters.map: param =>
      param.description.absolve match
        case Unset              => e"$indent    ${label(param)}"
        case text: Text         => e"$indent    ${label(param).fit(width)}  $text"
        case teletype: Teletype => e"$indent    ${label(param).fit(width)}  $teletype"

    val subLines: List[Teletype] = subcommands.bind(_.lines(depth + 1))

    List.of(title :: paramLines.stdlib ::: subLines.stdlib)
