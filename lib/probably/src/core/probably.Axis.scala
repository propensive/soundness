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
package probably

import scala.deriving.*

import anticipation.*
import distillate.*
import gossamer.*
import prepositional.*
import spectacular.*
import vacuous.*

object Axable:
  given int: Int is Axable:
    override def domain: Axis.Domain = Axis.Domain.Integral
    def label(value: Int): Text = value.show
    override def numeric(value: Int): Optional[Double] = value.toDouble

  given long: Long is Axable:
    override def domain: Axis.Domain = Axis.Domain.Integral
    def label(value: Long): Text = value.show
    override def numeric(value: Long): Optional[Double] = value.toDouble

  given double: Double is Axable:
    override def domain: Axis.Domain = Axis.Domain.Decimal
    def label(value: Double): Text = value.show
    override def numeric(value: Double): Optional[Double] = value

  given text: Text is Axable:
    def label(value: Text): Text = value

  given enumerable: [enumeration <: reflect.Enum: Enumerable as evidence]
  =>  enumeration is Axable:
    def label(value: enumeration): Text = evidence.name(value)

// How a domain value becomes a point on an axis of a test: a stable textual label (used in
// report rendering and by test selections) and, for numeric domains, its numeric value,
// which makes the axis orderable and chartable.
trait Axable extends Typeclass.Pure:
  def domain: Axis.Domain = Axis.Domain.Discrete
  def label(value: Self): Text
  def numeric(value: Self): Optional[Double] = Unset

object Axis:
  // The domain kind of an axis: discrete labels presented in declaration order, or
  // integer- or real-valued coordinates presented in numeric order.
  enum Domain:
    case Discrete, Integral, Decimal

  // The erased description of an axis as it is recorded in a report: what selection,
  // rendering and charting consume. An emergent axis acquires its coordinate values from
  // the run itself (such as a stress sweep's concurrency) rather than from a declared
  // domain.
  case class Spec(label: Text, domain: Domain, emergent: Boolean = false)

  def apply[value: Axable](label: Text)(values: value*): Axis[value] =
    new Axis(label, values.to(List))

  // An axis from an enum's companion object: the axis is labelled with the lowercased enum
  // name and its values are the enum's cases. `Enumerable.name` is the fully-shown type
  // (and, when the type was inferred, may carry a capture annotation), so the label keeps
  // only the simple name.
  def apply[enumeration <: reflect.Enum: Enumerable as evidence]
    ( companion: { def values: Array[enumeration] } )
  :   Axis[enumeration] =

    val label = evidence.name.cut(t" ").head.cut(t".").last.lower
    new Axis(label, evidence.values.to(List))

  def apply(label: Text)(range: Range): Axis[Int] = new Axis(label, range.to(List))

  def emergent[value: Axable](label: Text): Axis[value] = new Axis(label, Nil, emergent = true)

  // An enum's companion object is its `Mirror.SumOf`, so a bare companion object can stand
  // for an axis wherever a concrete `Axis[…]` is expected (as an `into` type). Generic
  // positions such as `over` cannot drive this conversion (the element type is still a
  // type variable there), so they take explicit companion overloads via `Axis(companion)`.
  given companion: [enumeration <: reflect.Enum: Enumerable as evidence]
  =>  Conversion[{ def values: Array[enumeration] }, Axis[enumeration]] =
    Axis(_)

// One input variable of a test: a labelled domain of values, each of which contributes one
// coordinate of the test's cells. Most tests have no axes; a comparative benchmark has one
// discrete axis of implementations; input data of different sizes forms a numeric axis.
// An `into` type, so an enum companion object is accepted (via `Axis.companion`) wherever
// an axis is expected, without a language import at the call site.
into case class Axis[value](label: Text, values: List[value], emergent: Boolean = false)
  ( using val axable: value is Axable ):

  def spec: Axis.Spec = Axis.Spec(label, axable.domain, emergent)
  def point(value0: value): Value = Value(value0)(using axable)
  def coordinate(value0: value): (Axis.Spec, Value) = (spec, point(value0))

object Value:
  def apply[value: Axable as axable](value0: value): Value =
    axable.numeric(value0).lay(Value.Discrete(axable.label(value0))): number =>
      axable.domain match
        case Axis.Domain.Decimal => Value.Decimal(number)
        case _                   => Value.Integral(number.toLong)

// One coordinate on one axis of a test. A cell of a test is addressed by a list of values,
// positionally aligned with the test's axes; a test with no axes has the single address
// `Nil`.
enum Value:
  case Discrete(label: Text)
  case Integral(number: Long)
  case Decimal(number: Double)

  def text: Text = this match
    case Discrete(label)  => label
    case Integral(number) => number.show
    case Decimal(number)  => number.show

  def numeric: Optional[Double] = this match
    case Discrete(_)      => Unset
    case Integral(number) => number.toDouble
    case Decimal(number)  => number

// The distinguished comparison point of a test: one value on one of its axes, against
// which every other cell along that axis is compared, using the `Baseline` settings.
case class Anchor(axis: Axis.Spec, value: Value, baseline: Baseline)
