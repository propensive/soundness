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

import quantitative.*
import rudiments.*

// CSS's relative and viewport units are modelled as fresh Quantitative dimensions
// with no `Ratio` to anything else, so they are deliberately inconvertible:
// `2.0*Em + 1.0*Vh` and `(1.0*Px).in[Centimetres]` are compile errors. The
// absolute units (cm, mm, in, pt, pc) live in Quantitative's existing `Distance`
// dimension and interconvert with one another as usual.
sealed trait CssPixel extends Dimension
sealed trait CssFontSize extends Dimension
sealed trait CssRootFontSize extends Dimension
sealed trait CssXHeight extends Dimension
sealed trait CssCharacterWidth extends Dimension
sealed trait CssViewportWidth extends Dimension
sealed trait CssViewportHeight extends Dimension
sealed trait CssViewportMin extends Dimension
sealed trait CssViewportMax extends Dimension
sealed trait CssRatio extends Dimension

trait Pixels[Power <: Nat] extends Units[Power, CssPixel]
trait Ems[Power <: Nat] extends Units[Power, CssFontSize]
trait Rems[Power <: Nat] extends Units[Power, CssRootFontSize]
trait Exs[Power <: Nat] extends Units[Power, CssXHeight]
trait Chs[Power <: Nat] extends Units[Power, CssCharacterWidth]
trait ViewportWidths[Power <: Nat] extends Units[Power, CssViewportWidth]
trait ViewportHeights[Power <: Nat] extends Units[Power, CssViewportHeight]
trait ViewportMins[Power <: Nat] extends Units[Power, CssViewportMin]
trait ViewportMaxes[Power <: Nat] extends Units[Power, CssViewportMax]
trait Percents[Power <: Nat] extends Units[Power, CssRatio]

// CSS-named absolute units in Quantitative's `Distance` dimension, alongside the
// existing `Inches`, `Points` and `Picas`.
object Centimetres:
  inline given ratio: Ratio[Centimetres[-1] & Metres[1], 0.01] = !!

trait Centimetres[Power <: Nat] extends Units[Power, Distance]

object Millimetres:
  inline given ratio: Ratio[Millimetres[-1] & Metres[1], 0.001] = !!

trait Millimetres[Power <: Nat] extends Units[Power, Distance]

// One unit of each CSS dimension, to be multiplied by a number, e.g. `4.0*Px` or
// `50.0*Pct`. Absolute lengths reuse Quantitative's `Inch`, with `Cm`/`Mm`/`Pt`/
// `Pc` added here.
val Px: Quantity[Pixels[1]] = Quantity(1.0)
val Em: Quantity[Ems[1]] = Quantity(1.0)
val Rem: Quantity[Rems[1]] = Quantity(1.0)
val Ex: Quantity[Exs[1]] = Quantity(1.0)
val Ch: Quantity[Chs[1]] = Quantity(1.0)
val Vw: Quantity[ViewportWidths[1]] = Quantity(1.0)
val Vh: Quantity[ViewportHeights[1]] = Quantity(1.0)
val Vmin: Quantity[ViewportMins[1]] = Quantity(1.0)
val Vmax: Quantity[ViewportMaxes[1]] = Quantity(1.0)
val Cm: Quantity[Centimetres[1]] = Quantity(1.0)
val Mm: Quantity[Millimetres[1]] = Quantity(1.0)
val Pt: Quantity[Points[1]] = Quantity(1.0)
val Pc: Quantity[Picas[1]] = Quantity(1.0)
val Pct: Quantity[Percents[1]] = Quantity(1.0)
