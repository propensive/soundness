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

import quantitative.*

// CSS's relative and viewport units are modelled as fresh Quantitative dimensions
// with no `Ratio` to anything else, so they are deliberately inconvertible:
// `2.0*Em + 1.0*Vh` and `(1.0*Px).convert[Metres]` are compile errors. The physical
// units (cm, mm, in, pt, pc) reuse Quantitative's `Distance` dimension and
// interconvert with one another as usual.
sealed trait CssPixel extends Dimension
sealed trait CssRootFontSize extends Dimension
sealed trait CssXHeight extends Dimension
sealed trait CssCharacterWidth extends Dimension
sealed trait CssViewportWidth extends Dimension
sealed trait CssViewportHeight extends Dimension
sealed trait CssViewportMin extends Dimension
sealed trait CssViewportMax extends Dimension
sealed trait CssRatio extends Dimension

trait Pixels[Power <: Nat] extends Units[Power, CssPixel]
trait Rems[Power <: Nat] extends Units[Power, CssRootFontSize]
trait Exs[Power <: Nat] extends Units[Power, CssXHeight]
trait Chs[Power <: Nat] extends Units[Power, CssCharacterWidth]
trait ViewportWidths[Power <: Nat] extends Units[Power, CssViewportWidth]
trait ViewportHeights[Power <: Nat] extends Units[Power, CssViewportHeight]
trait ViewportMins[Power <: Nat] extends Units[Power, CssViewportMin]
trait ViewportMaxes[Power <: Nat] extends Units[Power, CssViewportMax]
trait Percents[Power <: Nat] extends Units[Power, CssRatio]

// Angles and grid flex are fresh, deliberately-inconvertible dimensions (like the
// relative lengths above): `1.0*Deg + 1.0*Rad` and `2.0*Fr + 1.0*Px` are errors.
sealed trait CssDegree extends Dimension
sealed trait CssRadian extends Dimension
sealed trait CssTurn extends Dimension
sealed trait CssFlex extends Dimension

trait Degrees[Power <: Nat] extends Units[Power, CssDegree]
trait Radians[Power <: Nat] extends Units[Power, CssRadian]
trait Turns[Power <: Nat] extends Units[Power, CssTurn]
trait Flexes[Power <: Nat] extends Units[Power, CssFlex]

// One unit of each CSS dimension, to be multiplied by a number, e.g. `4.0*Px`,
// `50.0*Pct` or `200.0*Ms`. Physical lengths and times reuse Quantitative's own
// `Metres` and `Seconds`, rendered respectively as `mm` and `ms` (see
// `CssConvertible`), so `Cm`/`Mm`/`S`/`Ms` are convenient magnitudes, not new
// types — `Inch`, `Centi(Metre)`, `Milli(Second)` etc. work just as well.
val Px: Quantity[Pixels[1]] = Quantity(1.0)
val Rem: Quantity[Rems[1]] = Quantity(1.0)
val Ex: Quantity[Exs[1]] = Quantity(1.0)
val Ch: Quantity[Chs[1]] = Quantity(1.0)
val Vw: Quantity[ViewportWidths[1]] = Quantity(1.0)
val Vh: Quantity[ViewportHeights[1]] = Quantity(1.0)
val Vmin: Quantity[ViewportMins[1]] = Quantity(1.0)
val Vmax: Quantity[ViewportMaxes[1]] = Quantity(1.0)
val Cm: Quantity[Metres[1]] = Quantity(0.01)
val Mm: Quantity[Metres[1]] = Quantity(0.001)
val Pt: Quantity[Points[1]] = Quantity(1.0)
val Pc: Quantity[Picas[1]] = Quantity(1.0)
val Pct: Quantity[Percents[1]] = Quantity(1.0)
val S: Quantity[Seconds[1]] = Quantity(1.0)
val Ms: Quantity[Seconds[1]] = Quantity(0.001)
val Deg: Quantity[Degrees[1]] = Quantity(1.0)
val Rad: Quantity[Radians[1]] = Quantity(1.0)
val Turn: Quantity[Turns[1]] = Quantity(1.0)
val Fr: Quantity[Flexes[1]] = Quantity(1.0)
