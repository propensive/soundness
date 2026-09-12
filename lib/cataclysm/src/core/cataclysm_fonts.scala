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
package cataclysm

import anticipation.*
import denominative.*
import gossamer.*
import phoenicia.*
import rudiments.*
import spectacular.*
import symbolism.*

// The CSS generic family keywords, which `font-family` takes unquoted; any other name is quoted.
private val genericFamilies: List[Text] =
  List
    ( t"serif", t"sans-serif", t"monospace", t"cursive", t"fantasy", t"system-ui", t"ui-serif",
      t"ui-sans-serif", t"ui-monospace", t"ui-rounded", t"math", t"emoji", t"fangsong" )

extension (face: Face)
  // The declarations that select this face: `font-family`, `font-weight`, and — only when they
  // differ from the defaults — `font-style`, `font-stretch`, `font-variation-settings` for the
  // face's own axes, and `font-feature-settings`. The registered weight, width and italic axes
  // go through the high-level properties, so a static and a variable font are asked for alike.
  def style: Css.Style =
    val name = face.typeface.name
    val family = if genericFamilies.has(name) then name else t"\"$name\""

    val slant: List[(Text, Text)] = face.slant match
      case Slant.Upright        => Nil
      case Slant.Italic         => List(t"font-style" -> t"italic")
      case Slant.Oblique(angle) => List(t"font-style" -> t"oblique ${angle.toString}deg")

    val stretch: List[(Text, Text)] =
      if face.stretch == Stretch.Normal then Nil else List(t"font-stretch" -> face.stretch.show)

    val variations: List[(Text, Text)] =
      if face.variations.nil then Nil
      else
        def variation(variation: Variation): Text =
          t"\"${variation.axis.tag}\" ${variation.value.toString}"

        List(t"font-variation-settings" -> face.variations.map(variation).join(t", "))

    val features: List[(Text, Text)] =
      if face.features.nil then Nil
      else
        def setting(setting: Face.Feature.Setting): Text =
          t"\"${setting.feature.tag}\" ${setting.value.show}"

        List(t"font-feature-settings" -> face.features.map(setting).join(t", "))

    Css.Style.of
      ( List(t"font-family" -> family, t"font-weight" -> face.weight.show) + slant + stretch +
        variations + features )

extension (font: Font) def style: Css.Style = font.face.style
