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
┃    Soundness, version 0.27.0.                                                                    ┃
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
package quantitative

import anticipation.*
import gossamer.*
import hypotenuse.*
import prepositional.*
import proscenium.*
import rudiments.*
import spectacular.*
import symbolism.*

import scala.quoted.*

import language.implicitConversions
import language.experimental.captureChecking

object Quantitative extends Quantitative2:
  opaque type Quantity[units <: Measure] = Double
  opaque type MetricUnit[units <: Measure] <: Quantity[units] = Double

  extension [units <: Measure](quantity: Quantity[units])
    def underlying: Double = quantity

    inline def value: Double =
      val double: Double = compiletime.summonFrom:
        case offset: Offset[`units`] => (quantity - offset.value()): Double
        case _                       => quantity: Double

      double

  object MetricUnit:
    erased given underlying: [units <: Measure] => Underlying[MetricUnit[units], Double] = !!

    def apply[units <: Measure](value: Double): MetricUnit[units] = value

    @targetName("makeDerivedUnit")
    def apply[units <: Measure](value: Quantity[units]): MetricUnit[units] = value

  object Quantity:
    erased given underlying: [units <: Measure] => Underlying[Quantity[units], Double] = !!
    erased given canEqual: [units <: Measure] => CanEqual[Quantity[units], Quantity[units]] = !!
    given zeroic: [units <: Measure] => Quantity[units] is Zeroic = () => Quantity(0.0)
    given numeric: [units <: Measure] => Numeric[Quantity[units]] = summon[Numeric[Double]]

    given genericDuration: Quantity[Seconds[1]] is GenericDuration =
      quantity => (quantity*1000.0).toLong


    given specificDuration: Quantity[Seconds[1]] is SpecificDuration =
      long => Quantity(long/1000.0)

    transparent inline given addable: [left <: Measure, right <: Measure]
                             =>  Quantity[left] is Addable by Quantity[right] =
      ${Quantitative.addTypeclass[left, right]}

    transparent inline given subtractable: [left <: Measure, right <: Measure]
                             =>  Quantity[left] is Subtractable by Quantity[right] =
      ${Quantitative.subTypeclass[left, right]}

    transparent inline given multiplicable: [left <: Measure, right <: Measure]
                             =>  Quantity[left] is Multiplicable by Quantity[right] =
      ${Quantitative.mulTypeclass[left, right]}

    given multiplicable2: [left <: Measure]
          =>  Quantity[left] is Multiplicable by Double into Quantity[left] =
      new Multiplicable:
        type Self = Quantity[left]
        type Operand = Double
        type Result = Quantity[left]

        inline def multiply(left: Quantity[left], right: Double): Quantity[left] =
          left*right

    given multiplicable3: [right <: Measure]
          =>  Double is Multiplicable by Quantity[right] into Quantity[right] =
      new Multiplicable:
        type Self = Double
        type Operand = Quantity[right]
        type Result = Quantity[right]

        inline def multiply(left: Double, right: Quantity[right]): Quantity[right] =
          left*right

    transparent inline given divisible: [left <: Measure, right <: Measure]
                             =>  Quantity[left] is Divisible by Quantity[right] =
      ${Quantitative.divTypeclass[left, right]}

    given divisibleDouble: [left <: Measure] => Quantity[left] is Divisible by Double into Quantity[left] =
      new Divisible:
        type Self = Quantity[left]
        type Result = Quantity[left]
        type Operand = Double
        inline def divide(left: Quantity[left], right: Double): Quantity[left] = left/right

    transparent inline given squareRoot: [value <: Measure]
                             =>  Quantity[value] is Rootable[2] =
      ${Quantitative.sqrtTypeclass[value]}

    transparent inline given cubeRoot: [value <: Measure]
                             =>  Quantity[value] is Rootable[3] =
      ${Quantitative.cbrtTypeclass[value]}

    inline def apply[units <: Measure](value: Double): Quantity[units] = value

    given convertDouble: Conversion[Double, Quantity[Measure]] = Quantity[Measure](_)
    given convertInt: Conversion[Int, Quantity[Measure]] = int => Quantity[Measure](int.toDouble)

    given commensurable: [units <: Measure, units2 <: Measure]
          =>  Quantity[units] is Commensurable:

      type Operand = Quantity[units2]

      inline def compare
         (inline left:        Quantity[units],
inline right:       Quantity[units2],
          inline strict:      Boolean,
          inline greaterThan: Boolean)
      :     Boolean =

        ${Quantitative.greaterThan[units, units2]('left, 'right, 'strict, 'greaterThan)}

    class ShowableQuantity[units <: Measure](fn: Quantity[units] => Text)
       (using Decimalizer)
    extends Showable:
      type Self = Quantity[units]
      def text(value: Quantity[units]): Text = fn(value)

    class InspectableQuantity[units <: Measure](fn: Quantity[units] => Text)(using Decimalizer)
    extends Inspectable:
      type Self = Quantity[units]
      def text(value: Quantity[units]): Text = fn(value)

    inline given showable: [units <: Measure] => Decimalizer => Quantity[units] is Showable =
      ShowableQuantity[units](_.express)

    inline given inspectable: [units <: Measure] => Decimalizer => Quantity[units] is Inspectable =
      InspectableQuantity[units](_.express)

    def expressUnits(units: Map[Text, Int]): Text =
      units.to(List).map: (unit, power) =>
        if power == 1 then unit
        else
          val exponent: Text =
            power.show.mapChars:
              case '0' => '⁰'
              case '1' => '¹'
              case '2' => '²'
              case '3' => '³'
              case '4' => '⁴'
              case '5' => '⁵'
              case '6' => '⁶'
              case '7' => '⁷'
              case '8' => '⁸'
              case '9' => '⁹'
              case '-' => '¯'
              case _   => ' '

          t"$unit$exponent"

      . join(t"·")

export Quantitative.{Quantity, MetricUnit}
