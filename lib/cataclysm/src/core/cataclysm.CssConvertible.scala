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
package cataclysm

import anticipation.*
import gossamer.*
import iridescence.*
import prepositional.*
import quantitative.*
import spectacular.*

// Records that a native Scala type renders to a CSS value of the value-definition
// type `Topic` (e.g. `"length"`, `"color"`). A single generic `Conversion` (in
// `Css.Value`) lifts any such type into a `Css.Value of Topic`, so a new
// convertible type costs one instance here, not one given per CSS property.
object CssConvertible:
  given pixels: (Quantity[Pixels[1]] is CssConvertible of "length") = q => t"${number(q.value)}px"
  given ems: (Quantity[Ems[1]] is CssConvertible of "length") = q => t"${number(q.value)}em"
  given rems: (Quantity[Rems[1]] is CssConvertible of "length") = q => t"${number(q.value)}rem"
  given exs: (Quantity[Exs[1]] is CssConvertible of "length") = q => t"${number(q.value)}ex"
  given chs: (Quantity[Chs[1]] is CssConvertible of "length") = q => t"${number(q.value)}ch"

  given vws: (Quantity[ViewportWidths[1]] is CssConvertible of "length") =
    q => t"${number(q.value)}vw"

  given vhs: (Quantity[ViewportHeights[1]] is CssConvertible of "length") =
    q => t"${number(q.value)}vh"

  given vmins: (Quantity[ViewportMins[1]] is CssConvertible of "length") =
    q => t"${number(q.value)}vmin"

  given vmaxes: (Quantity[ViewportMaxes[1]] is CssConvertible of "length") =
    q => t"${number(q.value)}vmax"

  given centimetres: (Quantity[Centimetres[1]] is CssConvertible of "length") =
    q => t"${number(q.value)}cm"

  given millimetres: (Quantity[Millimetres[1]] is CssConvertible of "length") =
    q => t"${number(q.value)}mm"

  given inches: (Quantity[Inches[1]] is CssConvertible of "length") = q => t"${number(q.value)}in"
  given points: (Quantity[Points[1]] is CssConvertible of "length") = q => t"${number(q.value)}pt"
  given picas: (Quantity[Picas[1]] is CssConvertible of "length") = q => t"${number(q.value)}pc"

  given srgb: (Srgb is CssConvertible of "color") = hex(_)
  given integer: (Int is CssConvertible of "integer") = _.show
  given decimal: (Double is CssConvertible of "number") = number(_)

  given percentage: (Percentage is CssConvertible of "percentage") =
    percentage => t"${number(percentage.value)}%"

  // Drop a redundant `.0` so a whole number renders as `12px`, not `12.0px`.
  private def number(value: Double): Text =
    if value.isFinite && value == value.floor then value.toLong.show else value.toString.tt

  private def hex(color: Srgb): Text =
    def channel(component: Double): Text =
      String.format("%02x", (component*255).toInt.max(0).min(255)).nn.tt

    t"#${channel(color.red)}${channel(color.green)}${channel(color.blue)}"

trait CssConvertible extends Typeclass, Topical:
  def value(self: Self): Text
