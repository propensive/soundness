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
┃    Soundness, version 0.45.0.                                                                    ┃
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
import probably.*
import proscenium.*
import rudiments.*
import spectacular.*
import symbolism.*

import scala.quoted.*

import language.implicitConversions

object Quantitative extends Quantitative2:
  opaque type Quantity[units <: Measure] = Double
  opaque type MetricUnit[units <: Measure] <: Quantity[units] = Double
  opaque type Temperature = Double

  extension (temperature: Temperature)
    def kelvin: Double = temperature
    def celsius: Double = temperature - 273.15
    def fahrenheit: Double = celsius*9/5 + 32
    def rankine: Double = temperature*9/5

  object Temperature:
    def apply(value: Double): Temperature = value

    given showable: (scale: TemperatureScale, decimalizer: Decimalizer) => Temperature is Showable =
      temperature => t"${scale.kelvin(temperature)} ${scale.suffix}"

    given zeroic: Temperature is Zeroic:
      inline def zero: Temperature = Temperature(0)

    given addable: Temperature is Addable by Quantity[Kelvins[1]] to Temperature =
      _ + _.value

    given addable2: Temperature is Addable by Quantity[Rankines[1]] to Temperature =
      _ + _.value*9/5

    given subtractable: Temperature is Subtractable by Quantity[Kelvins[1]] to Temperature =
      _ - _.value

    given subtractable2: Temperature is Subtractable by Quantity[Rankines[1]] to Temperature =
      _ - _.value*9/5

    given subtractable3: Temperature is Subtractable by Temperature to Quantity[Kelvins[1]] =
      (left, right) => Quantity(left.kelvin - right.kelvin)

  extension [units <: Measure](quantity: Quantity[units])
    def underlying: Double = quantity

    inline def value: Double = quantity
    inline def amount[name <: Label]: Text = ${Quantitative.amount[units]}

  object MetricUnit:
    erased given underlying: [units <: Measure] => Underlying[MetricUnit[units], Double] = !!

    def apply[units <: Measure](value: Double): MetricUnit[units] = value

    @targetName("makeDerivedUnit")
    def apply[units <: Measure](value: Quantity[units]): MetricUnit[units] = value

  object Quantity:
    erased given underlying: [units <: Measure] => Underlying[Quantity[units], Double] = !!
    erased given canEqual: [units <: Measure] => CanEqual[Quantity[units], Quantity[units]] = !!

    given zeroic: [units <: Measure] => Quantity[units] is Zeroic:
      inline def zero: Quantity[units] = Quantity(0.0)

    given numeric: [units <: Measure] => Numeric[Quantity[units]] = summon[Numeric[Double]]

    given genericDuration: [units <: Measure: Normalizable to Seconds[1]]
          =>  Quantity[units] is GenericDuration =
      quantity => (quantity.normalize*1000.0).toLong


    given specificDuration: [units <: Measure: Normalizable to Seconds[1]]
          =>  Quantity[units] is SpecificDuration =
      long => Quantity[units](long*units.ratio()/1000.0)

    transparent inline given addable: [left <: Measure,
                                       quantity <: Quantity[left],
                                       right <: Measure,
                                       quantity2 <: Quantity[right]]
                             =>  quantity is Addable by quantity2 =
      ${Quantitative.addTypeclass[left, quantity, right, quantity2]}

    inline given checkable: [left <: Measure,
                             quantity <: Quantity[left],
                             right <: Measure,
                             quantity2 <: Quantity[right]]
                 =>  quantity is Checkable against quantity2 =
      ${Quantitative.checkable[left, quantity, right, quantity2]}

    transparent inline given subtractable: [left <: Measure,
                                            quantity <: Quantity[left],
                                            right <: Measure,
                                            quantity2 <: Quantity[right]]
                             =>  quantity is Subtractable by quantity2 =
      ${Quantitative.subTypeclass[left, quantity, right, quantity2]}

    transparent inline given multiplicable: [left <: Measure,
                                             multiplicand <: Quantity[left],
                                             right <: Measure,
                                             multiplier <: Quantity[right]]
                             =>  multiplicand is Multiplicable by multiplier =
      ${Quantitative.mulTypeclass[left, multiplicand, right, multiplier]}

    given negatable: [left <: Measure, operand <: Quantity[left]] => operand is Negatable:
      type Self = operand
      type Result = Quantity[left]
      def negate(operand: Self): Quantity[left] = -operand

    given multiplicable2: [left <: Measure, multiplicand <: Quantity[left]]
          =>  multiplicand is Multiplicable:
      type Self = multiplicand
      type Operand = Double
      type Result = Quantity[left]

      inline def multiply(left: multiplicand, right: Double): Quantity[left] = left*right

    given multiplicable3: [right <: Measure, multiplier <: Quantity[right]]
          =>  Double is Multiplicable:
      type Self = Double
      type Operand = multiplier
      type Result = Quantity[right]

      inline def multiply(left: Double, right: multiplier): Quantity[right] = left*right

    given multiplicable4: [right <: Measure, multiplier <: Quantity[right]] => Int is Multiplicable:
      type Self = Int
      type Operand = multiplier
      type Result = Quantity[right]

      inline def multiply(left: Int, right: multiplier): Quantity[right] = left*right

    given multiplicable5: [left <: Measure, multiplicand <: Quantity[left]]
          => multiplicand is Multiplicable:
      type Self = multiplicand
      type Operand = Int
      type Result = Quantity[left]

      inline def multiply(left: multiplicand, right: Int): Quantity[left] = left*right

    transparent inline given divisible: [left <: Measure,
                                         dividend <: Quantity[left],
                                         right <: Measure,
                                         divisor <: Quantity[right]]
                             =>  dividend is Divisible by divisor =
      ${Quantitative.divTypeclass[left, dividend, right, divisor]}

    transparent inline given divisible2: [right <: Measure, divisor <: Quantity[right]]
                             =>  Double is Divisible by divisor =
      ${Quantitative.divTypeclass2[right, divisor]}

    transparent inline given divisible3: [right <: Measure, divisor <: Quantity[right]]
                             =>  Int is Divisible by divisor =
      ${Quantitative.divTypeclass3[right, divisor]}

    given divisibleDouble: [left <: Measure, dividend <: Quantity[left]] => dividend is Divisible:
      type Self = dividend
      type Result = Quantity[left]
      type Operand = Double

      inline def divide(left: dividend, right: Double): Quantity[left] = left/right

    given divisibleInt: [left <: Measure, dividend <: Quantity[left]] => dividend is Divisible:
      type Self = dividend
      type Result = Quantity[left]
      type Operand = Int

      inline def divide(left: dividend, right: Int): Quantity[left] = left/right.toDouble

    transparent inline given squareRoot: [value <: Measure] =>  Quantity[value] is Rootable[2] =
      ${Quantitative.sqrtTypeclass[value]}

    transparent inline given cubeRoot: [value <: Measure] =>  Quantity[value] is Rootable[3] =
      ${Quantitative.cbrtTypeclass[value]}

    inline def apply[units <: Measure](value: Double): Quantity[units] = value

    given commensurable: [units <: Measure, units2 <: Measure] => Quantity[units] is Commensurable:
      type Operand = Quantity[units2]


      inline def compare
                  (inline left:        Quantity[units],
                   inline right:       Quantity[units2],
                   inline strict:      Boolean,
                   inline greaterThan: Boolean)
      : Boolean =

          ${Quantitative.greaterThan[units, units2]('left, 'right, 'strict, 'greaterThan)}


    class ShowableQuantity[units <: Measure](fn: Quantity[units] => Text)(using Decimalizer)
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
        if power == 1 then unit else
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

export Quantitative.{Quantity, MetricUnit, Temperature}
