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
  opaque type Quantity[UnitsType <: Measure] = Double
  opaque type MetricUnit[UnitsType <: Measure] <: Quantity[UnitsType] = Double

  extension [UnitsType <: Measure](quantity: Quantity[UnitsType])
    def underlying: Double = quantity

    inline def value: Double =
      val double: Double = compiletime.summonFrom:
        case unitsOffset: UnitsOffset[UnitsType] => (quantity - unitsOffset.value()): Double
        case _                                   => quantity: Double

      double

  object MetricUnit:
    erased given underlying: [UnitsType <: Measure] => Underlying[MetricUnit[UnitsType], Double] =
      !!

    def apply[UnitsType <: Measure](value: Double): MetricUnit[UnitsType] = value

    @targetName("makeDerivedUnit")
    def apply[UnitsType <: Measure](value: Quantity[UnitsType]): MetricUnit[UnitsType] = value

  object Quantity:
    erased given underlying: [UnitsType <: Measure] => Underlying[Quantity[UnitsType], Double] = !!
    erased given [UnitsType <: Measure]: CanEqual[Quantity[UnitsType], Quantity[UnitsType]] = !!
    given [UnitsType <: Measure] => Quantity[UnitsType] is Zeroic = () => Quantity(0.0)
    given numeric: [UnitsType <: Measure] => Numeric[Quantity[UnitsType]] = summon[Numeric[Double]]

    given genericDuration: Quantity[Seconds[1]] is GenericDuration =
      quantity => (quantity*1000.0).toLong


    given specificDuration: Quantity[Seconds[1]] is SpecificDuration =
      long => Quantity(long/1000.0)

    transparent inline given addable: [LeftType <: Measure, RightType <: Measure]
    =>    Quantity[LeftType] is Addable by Quantity[RightType] =
      ${Quantitative.addTypeclass[LeftType, RightType]}

    transparent inline given subtractable: [LeftType <: Measure, RightType <: Measure]
    =>    Quantity[LeftType] is Subtractable by Quantity[RightType] =
      ${Quantitative.subTypeclass[LeftType, RightType]}

    transparent inline given multiplicable: [LeftType <: Measure, RightType <: Measure]
    =>    Quantity[LeftType] is Multiplicable by Quantity[RightType] =
      ${Quantitative.mulTypeclass[LeftType, RightType]}

    given multiplicable2: [LeftType <: Measure]
    =>    Quantity[LeftType] is Multiplicable by Double into Quantity[LeftType] =
      new Multiplicable:
        type Self = Quantity[LeftType]
        type Operand = Double
        type Result = Quantity[LeftType]

        inline def multiply(left: Quantity[LeftType], right: Double): Quantity[LeftType] =
          left*right

    given multiplicable3: [RightType <: Measure]
    =>    Double is Multiplicable by Quantity[RightType] into Quantity[RightType] =
      new Multiplicable:
        type Self = Double
        type Operand = Quantity[RightType]
        type Result = Quantity[RightType]

        inline def multiply(left: Double, right: Quantity[RightType]): Quantity[RightType] =
          left*right

    transparent inline given divisible: [LeftType <: Measure, RightType <: Measure]
    =>    Quantity[LeftType] is Divisible by Quantity[RightType] =
      ${Quantitative.divTypeclass[LeftType, RightType]}

    given divisibleDouble: [LeftType <: Measure] => Quantity[LeftType] is Divisible by Double =
      new Divisible:
        type Self = Quantity[LeftType]
        type Result = Quantity[LeftType]
        type Operand = Double
        inline def divide(left: Quantity[LeftType], right: Double): Quantity[LeftType] = left/right

    transparent inline given squareRoot: [ValueType <: Measure]
    =>    Quantity[ValueType] is Rootable[2] =
      ${Quantitative.sqrtTypeclass[ValueType]}

    transparent inline given cubeRoot: [ValueType <: Measure]
    =>    Quantity[ValueType] is Rootable[3] =
      ${Quantitative.cbrtTypeclass[ValueType]}

    inline def apply[UnitsType <: Measure](value: Double): Quantity[UnitsType] = value

    given convertDouble: Conversion[Double, Quantity[Measure]] = Quantity[Measure](_)
    given convertInt: Conversion[Int, Quantity[Measure]] = int => Quantity[Measure](int.toDouble)

    given commensurable: [UnitsType <: Measure, UnitsType2 <: Measure]
    =>    Quantity[UnitsType] is Commensurable:

      type Operand = Quantity[UnitsType2]

      inline def compare
         (inline left:     Quantity[UnitsType],
          inline right:      Quantity[UnitsType2],
          inline strict:     Boolean,
          inline greaterThan: Boolean)
      :     Boolean =

        ${Quantitative.greaterThan[UnitsType, UnitsType2]('left, 'right, 'strict, 'greaterThan)}

    class ShowableQuantity[UnitsType <: Measure](fn: Quantity[UnitsType] => Text)
       (using Decimalizer)
    extends Showable:
      type Self = Quantity[UnitsType]
      def text(value: Quantity[UnitsType]): Text = fn(value)

    class InspectableQuantity[UnitsType <: Measure](fn: Quantity[UnitsType] => Text)
       (using Decimalizer)
    extends Inspectable:
      type Self = Quantity[UnitsType]
      def text(value: Quantity[UnitsType]): Text = fn(value)

    inline given [UnitsType <: Measure] => Decimalizer => Quantity[UnitsType] is Showable =
      ShowableQuantity[UnitsType](_.express)

    inline given [UnitsType <: Measure] => Decimalizer => Quantity[UnitsType] is Inspectable =
      InspectableQuantity[UnitsType](_.express)

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
