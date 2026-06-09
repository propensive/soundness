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

// CSS's relative and viewport length units are modelled as fresh Quantitative
// dimensions with no `Ratio` to anything else, so they are deliberately
// inconvertible: `2.0.em + 1.0.vh` and `1.0.px.in[Centimetres]` are both compile
// errors. The absolute units (cm, mm, in, pt, pc) live in Quantitative's existing
// `Distance` dimension and interconvert with one another as usual.
sealed trait CssPixel extends Dimension
sealed trait CssFontSize extends Dimension
sealed trait CssRootFontSize extends Dimension
sealed trait CssXHeight extends Dimension
sealed trait CssCharacterWidth extends Dimension
sealed trait CssViewportWidth extends Dimension
sealed trait CssViewportHeight extends Dimension
sealed trait CssViewportMin extends Dimension
sealed trait CssViewportMax extends Dimension

trait Pixels[Power <: Nat] extends Units[Power, CssPixel]
trait Ems[Power <: Nat] extends Units[Power, CssFontSize]
trait Rems[Power <: Nat] extends Units[Power, CssRootFontSize]
trait Exs[Power <: Nat] extends Units[Power, CssXHeight]
trait Chs[Power <: Nat] extends Units[Power, CssCharacterWidth]
trait ViewportWidths[Power <: Nat] extends Units[Power, CssViewportWidth]
trait ViewportHeights[Power <: Nat] extends Units[Power, CssViewportHeight]
trait ViewportMins[Power <: Nat] extends Units[Power, CssViewportMin]
trait ViewportMaxes[Power <: Nat] extends Units[Power, CssViewportMax]

// CSS-named absolute units in Quantitative's `Distance` dimension, alongside the
// existing `Inches`, `Points` and `Picas`.
object Centimetres:
  inline given ratio: Ratio[Centimetres[-1] & Metres[1], 0.01] = !!

trait Centimetres[Power <: Nat] extends Units[Power, Distance]

object Millimetres:
  inline given ratio: Ratio[Millimetres[-1] & Metres[1], 0.001] = !!

trait Millimetres[Power <: Nat] extends Units[Power, Distance]

// A CSS percentage, e.g. `50.0.pct`.
opaque type Percentage = Double

object Percentage:
  def apply(value: Double): Percentage = value
  extension (percentage: Percentage) def value: Double = percentage

extension (value: Double)
  def px: Quantity[Pixels[1]] = Quantity(value)
  def em: Quantity[Ems[1]] = Quantity(value)
  def rem: Quantity[Rems[1]] = Quantity(value)
  def ex: Quantity[Exs[1]] = Quantity(value)
  def ch: Quantity[Chs[1]] = Quantity(value)
  def vw: Quantity[ViewportWidths[1]] = Quantity(value)
  def vh: Quantity[ViewportHeights[1]] = Quantity(value)
  def vmin: Quantity[ViewportMins[1]] = Quantity(value)
  def vmax: Quantity[ViewportMaxes[1]] = Quantity(value)
  def cm: Quantity[Centimetres[1]] = Quantity(value)
  def mm: Quantity[Millimetres[1]] = Quantity(value)
  def inch: Quantity[Inches[1]] = Quantity(value)
  def pt: Quantity[Points[1]] = Quantity(value)
  def pc: Quantity[Picas[1]] = Quantity(value)
  def pct: Percentage = Percentage(value)
