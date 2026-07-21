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

  // Any physical length (Quantitative's `Distance` dimension) renders in `mm`; CSS
  // reads `cm`/`mm`/etc. consistently, so the rendered unit need not match how the
  // value was written. `q.value` is in metres, hence the factor of 1000.
  given metres: (Quantity[Metres[1]] is CssConvertible of "length") =
    q => t"${number(q.value*1000)}mm"

  given inches: (Quantity[Inches[1]] is CssConvertible of "length") = q => t"${number(q.value)}in"
  given points: (Quantity[Points[1]] is CssConvertible of "length") = q => t"${number(q.value)}pt"
  given picas: (Quantity[Picas[1]] is CssConvertible of "length") = q => t"${number(q.value)}pc"

  given percents: (Quantity[Percents[1]] is CssConvertible of "percentage") =
    q => t"${number(q.value)}%"

  given srgb: (Srgb is CssConvertible of "color") =
    color => hex((color.red*255).toInt, (color.green*255).toInt, (color.blue*255).toInt)

  given chroma: (Chroma is CssConvertible of "color") =
    color => hex(color.red, color.green, color.blue)

  // Likewise any time (Quantitative's `Seconds`) renders in `ms`; `q.value` is in
  // seconds, hence the factor of 1000.
  given seconds: (Quantity[Seconds[1]] is CssConvertible of "time") =
    q => t"${number(q.value*1000)}ms"

  given degrees: (Quantity[Degrees[1]] is CssConvertible of "angle") = q => t"${number(q.value)}deg"
  given radians: (Quantity[Radians[1]] is CssConvertible of "angle") = q => t"${number(q.value)}rad"

  given turns: (Quantity[Turns[1]] is CssConvertible of "angle") =
    q => t"${number(q.value)}turn"

  given flexes: (Quantity[Flexes[1]] is CssConvertible of "flex") = q => t"${number(q.value)}fr"

  given integer: (Int is CssConvertible of "integer") = _.show
  given decimal: (Double is CssConvertible of "number") = Css.number(_)

  given keyword: (Css.Keyword is CssConvertible of "*") = _ match
    case Css.Keyword.Inherit     => t"inherit"
    case Css.Keyword.Initial     => t"initial"
    case Css.Keyword.Unset       => t"unset"
    case Css.Keyword.Revert      => t"revert"
    case Css.Keyword.RevertLayer => t"revert-layer"

  given colorKeyword: (Css.ColorKeyword is CssConvertible of "color") = _ match
    case Css.ColorKeyword.Transparent  => t"transparent"
    case Css.ColorKeyword.CurrentColor => t"currentcolor"

  private def number(value: Double): Text = Css.number(value)

  private def hex(red: Int, green: Int, blue: Int): Text =
    def channel(value: Int): Text = String.format("%02x", value.max(0).min(255)).nn.tt
    t"#${channel(red)}${channel(green)}${channel(blue)}"

trait CssConvertible extends Typeclass, Topical:
  def value(self: Self): Text
