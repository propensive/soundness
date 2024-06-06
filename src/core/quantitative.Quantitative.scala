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

    given genericDuration: GenericDuration with
      type Self = Quantity[Seconds[1]]
      def milliseconds(quantity: Quantity[Seconds[1]]): Long = (quantity*1000.0).toLong

    given specificDuration: SpecificDuration with
      type Self = Quantity[Seconds[1]]
      def duration(long: Long): Quantity[Seconds[1]] = Quantity(long/1000.0)

    transparent inline given add[LeftType <: Measure, RightType <: Measure]
            : AddOperator[Quantity[LeftType], Quantity[RightType]] =

      ${Quantitative.addTypeclass[LeftType, RightType]}

    transparent inline given sub[LeftType <: Measure, RightType <: Measure]
            : SubOperator[Quantity[LeftType], Quantity[RightType]] =

      ${Quantitative.subTypeclass[LeftType, RightType]}

    transparent inline given mul[LeftType <: Measure, RightType <: Measure]
            : MulOperator[Quantity[LeftType], Quantity[RightType]] =

      ${Quantitative.mulTypeclass[LeftType, RightType]}

    given mul2[LeftType <: Measure]: MulOperator[Quantity[LeftType], Double] with
      type Result = Quantity[LeftType]
      inline def mul(left: Quantity[LeftType], right: Double): Quantity[LeftType] = left*right

    transparent inline given div[LeftType <: Measure, RightType <: Measure]
            : DivOperator[Quantity[LeftType], Quantity[RightType]] =

      ${Quantitative.divTypeclass[LeftType, RightType]}

    given div2[LeftType <: Measure]: DivOperator[Quantity[LeftType], Double] with
      type Result = Quantity[LeftType]
      inline def div(left: Quantity[LeftType], right: Double): Quantity[LeftType] = left/right

    transparent inline given squareRoot[ValueType <: Measure]
            : RootOperator[2, Quantity[ValueType]] =
      ${Quantitative.sqrtTypeclass[ValueType]}

    transparent inline given cubeRoot[ValueType <: Measure]: RootOperator[3, Quantity[ValueType]] =
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


    inline given [UnitsType <: Measure](using Decimalizer): Show[Quantity[UnitsType]] =
      new Show[Quantity[UnitsType]]:
        def text(value: Quantity[UnitsType]): Text = value.render

    inline given [UnitsType <: Measure](using Decimalizer): Debug[Quantity[UnitsType]] =
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
