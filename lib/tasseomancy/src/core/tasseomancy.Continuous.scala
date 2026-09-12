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
package tasseomancy

import anticipation.*
import aviation.*
import gossamer.*
import hypotenuse.*
import prepositional.*
import quantitative.*
import vacuous.*

object Continuous:
  given int: Int is Continuous = _.toDouble
  given long: Long is Continuous = _.toDouble
  given double: Double is Continuous = double => double
  given float: Float is Continuous = _.toDouble
  given f64: F64 is Continuous = _.double

  // Any quantity, by its magnitude in its own units, which name the axis: `distance / m`. Inline,
  // so that the units and the quantity's name are read from the type where the chart is made.
  // The more specific `duration` below takes precedence for seconds, where clock-time spacing
  // reads better than decimal.
  inline given quantity: [units <: Measure] => Quantity[units] is Continuous =
    val notation = Scale.Notation(name = Quantity.dimension[units], unit = Quantity.units[units])
    QuantityContinuous[units](notation)

  // A named class rather than an anonymous instance, which an inline given would duplicate at
  // every expansion site.
  class QuantityContinuous[units <: Measure](notation0: Scale.Notation)
  extends Continuous:
    type Self = Quantity[units]
    def position(value: Quantity[units]): Double = value.value
    override val notation: Scale.Notation = notation0

  given duration: Duration is Continuous:
    def position(value: Duration): Double = value.value

    override val notation: Scale.Notation =
      Scale.Notation(Scale.Spacing.Sexagesimal, Scale.Labelling.Interval, t"time")

  // An instant is placed by its seconds since the epoch, whatever the resolution of its
  // timeline, and labelled as a time of day.
  given instant: [transport] => (resolution: transport is Resolution)
  =>  (Instant over transport) is Continuous:
    def position(value: Instant over transport): Double =
      value.long.toDouble*resolution.nanos/1000000000.0

    override val notation: Scale.Notation =
      Scale.Notation(Scale.Spacing.Sexagesimal, Scale.Labelling.Clock, t"time")

  given estimate: Estimate is Continuous:
    def position(value: Estimate): Double = value.value
    override def bounds(value: Estimate): Optional[(Double, Double)] = (value.lower, value.upper)

  given annotated: Annotated is Continuous:
    def position(value: Annotated): Double = value.value
    override def annotation(value: Annotated): Optional[Text] = value.note

// A value with a position on a numeric axis, and optionally an interval around it. Positions
// are what a line, a scatter plot or a bar's height is drawn from; the notation says how an
// axis of this type is graduated, written and titled, since seconds are read differently from
// counts and a quantity is measured in units.
trait Continuous extends Typeclass.Pure:
  def position(value: Self): Double
  def bounds(value: Self): Optional[(Double, Double)] = Unset
  def annotation(value: Self): Optional[Text] = Unset
  def notation: Scale.Notation = Scale.Notation()
