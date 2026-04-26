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
package hyperbole

import anticipation.*
import escritoire.*, tableStyles.default, columnAttenuation.ignore
import escapade.*
import gossamer.*
import hieroglyph.*, textMetrics.uniform
import iridescence.*
import proscenium.*

object TastySymbol:
  given teletypeable: (palette: TastyPalette) => TastySymbol is Teletypeable =
    symbol =>

      val flags =
        symbol.flags.map: (flag, on) =>
          if on then e"${Bg(palette.flagOff)}(${Fg(palette.black)}(·${flag}·))"
          else e"${Fg(palette.flagOff)}($flag)"
        . join(e" ")

      val properties =
        symbol.properties.map: (property, on) =>
          if on then e"${Bg(palette.propertyOn)}(${Fg(palette.black)}(·${property}·))"
          else e"${palette.propertyOff}($property)"
        . join(e" ")

      val details =
        symbol.details.map: detail =>
          detail.absolve match
            case (key, value: Text) =>
              key -> e"${Fg(palette.outline)}($value)"
            case (key, items: List[Text]) =>
              key -> e"${Fg(palette.outline)}(${items.join(t", ")})"
        . to(List)

      val name = (t"Name", e"$Bold(${symbol.prefix}${Fg(palette.foreground)}(${symbol.name}))")

      Scaffold[(Text, Teletype)]
        ( Column(e"$Bold(Property)", textAlign = TextAlignment.Right)(_(0)),
          Column(e"$Bold(Value)", sizing = columnar.ProseOrBreak)(_(1)) )

      . tabulate(name :: (t"Flags", flags) :: (t"Properties", properties) :: details)
      . grid(120)
      . render
      . join(e"\n")

case class TastySymbol
  ( prefix:     Text,
    name:       Text,
    flags:      List[(Text, Boolean)],
    properties: List[(Text, Boolean)],
    details:    List[(Text, List[Text] | Text)] )
