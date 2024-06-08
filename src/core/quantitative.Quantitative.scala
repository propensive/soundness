/*
    Quantitative, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package quantitative

import gossamer.*
import rudiments.*
import anticipation.*
import hypotenuse.*
import symbolism.*
import spectacular.*

import scala.quoted.*

import language.implicitConversions
import language.experimental.captureChecking

object Quantitative extends Quantitative2:
  opaque type Quantity[UnitsType <: Measure] = Double
  opaque type MetricUnit[UnitsType <: Measure] <: Quantity[UnitsType] = Double

  extension [UnitsType <: Measure](quantity: Quantity[UnitsType])
    def underlying: Double = quantity

    inline def value: Double = compiletime.summonFrom:
      case unitsOffset: UnitsOffset[UnitsType] => quantity - unitsOffset.value()
      case _                                   => quantity

  object MetricUnit:
    erased given underlying[UnitsType <: Measure]: Underlying[MetricUnit[UnitsType], Double] = ###
    def apply[UnitsType <: Measure](value: Double): MetricUnit[UnitsType] = value

    @targetName("makeDerivedUnit")
    def apply[UnitsType <: Measure](value: Quantity[UnitsType]): MetricUnit[UnitsType] = value

  object Quantity:
    erased given underlying[UnitsType <: Measure]: Underlying[Quantity[UnitsType], Double] = ###
    erased given [UnitsType <: Measure]: CanEqual[Quantity[UnitsType], Quantity[UnitsType]] = ###

    given Quantity[Seconds[1]] is GenericDuration as genericDuration =
      quantity => (quantity*1000.0).toLong

    given Quantity[Seconds[1]] is SpecificDuration as specificDuration = long => Quantity(long/1000.0)

    transparent inline given [LeftType <: Measure, RightType <: Measure] => Quantity[LeftType] is Addable[Quantity[RightType]] as addable =
      ${Quantitative.addTypeclass[LeftType, RightType]}

    transparent inline given [LeftType <: Measure, RightType <: Measure] => Quantity[LeftType] is Subtractable[Quantity[RightType]] as subtractable =
      ${Quantitative.subTypeclass[LeftType, RightType]}

    transparent inline given [LeftType <: Measure, RightType <: Measure] => Quantity[LeftType] is Multiplicable[Quantity[RightType]] as multiplicable =
      ${Quantitative.mulTypeclass[LeftType, RightType]}

    given [LeftType <: Measure] => Quantity[LeftType] is Multiplicable[Double] as multiplicable2:
      type Result = Quantity[LeftType]
      inline def multiply(left: Quantity[LeftType], right: Double): Quantity[LeftType] = left*right

    transparent inline given [LeftType <: Measure, RightType <: Measure] => Quantity[LeftType] is Divisible[Quantity[RightType]] as divisible =
      ${Quantitative.divTypeclass[LeftType, RightType]}

    given [LeftType <: Measure] => Quantity[LeftType] is Divisible[Double]:
      type Result = Quantity[LeftType]
      inline def divide(left: Quantity[LeftType], right: Double): Quantity[LeftType] = left/right

    transparent inline given [ValueType <: Measure] => Quantity[ValueType] is Rootable[2] as squareRoot =
      ${Quantitative.sqrtTypeclass[ValueType]}

    transparent inline given [ValueType <: Measure] => Quantity[ValueType] is Rootable[3] as cubeRoot =
      ${Quantitative.cbrtTypeclass[ValueType]}

    inline def apply[UnitsType <: Measure](value: Double): Quantity[UnitsType] = value
    given convertDouble[UnitsType <: Measure]: Conversion[Double, Quantity[UnitsType]] = Quantity(_)

    given convertInt[UnitsType <: Measure]: Conversion[Int, Quantity[UnitsType]] =
      int => Quantity(int.toDouble)

    given inequality[UnitsType <: Measure, UnitsType2 <: Measure]
            : Inequality[Quantity[UnitsType], Quantity[UnitsType2]] with

      inline def compare
          (inline left:        Quantity[UnitsType],
           inline right:       Quantity[UnitsType2],
           inline strict:      Boolean,
           inline greaterThan: Boolean)
              : Boolean =

        ${Quantitative.greaterThan[UnitsType, UnitsType2]('left, 'right, 'strict, 'greaterThan)}


    inline given [UnitsType <: Measure](using Decimalizer) => Show[Quantity[UnitsType]] =
      new Show[Quantity[UnitsType]]:
        def text(value: Quantity[UnitsType]): Text = value.render

    inline given [UnitsType <: Measure](using Decimalizer) => Debug[Quantity[UnitsType]] =
      new Debug[Quantity[UnitsType]]:
        def text(value: Quantity[UnitsType]): Text = value.render

    def renderUnits(units: Map[Text, Int]): Text =
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
      .join(t"·")

export Quantitative.{Quantity, MetricUnit}
