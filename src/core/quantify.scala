/*
    Quantify, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package quantify

import gossamer.*
import rudiments.*

import scala.quoted.*
import annotation.{targetName, allowConversions}

import language.implicitConversions

trait Dimension

erased trait Length extends Dimension
erased trait Mass extends Dimension
erased trait Time extends Dimension
erased trait Current extends Dimension
erased trait Luminosity extends Dimension
erased trait Temperature extends Dimension
erased trait AmountOfSubstance extends Dimension

sealed trait Measure
trait Units[PowerType <: Nat, DimensionType <: Dimension] extends Measure

erased trait Metres[Power <: Nat] extends Units[Power, Length]
erased trait Kilograms[Power <: Nat] extends Units[Power, Mass]
erased trait Candelas[Power <: Nat] extends Units[Power, Luminosity]
erased trait Moles[Power <: Nat] extends Units[Power, AmountOfSubstance]
erased trait Amperes[Power <: Nat] extends Units[Power, Current]
erased trait Kelvins[Power <: Nat] extends Units[Power, Temperature]
erased trait Seconds[Power <: Nat] extends Units[Power, Time]

trait UnitName[-ValueType]:
  def siPrefix: SiPrefix = NoPrefix
  def name(): Text
  def text: Text = t"${siPrefix.symbol}${name()}"

object UnitName:
  given UnitName[Metres[1]] = () => t"m"
  given UnitName[Candelas[1]] = () => t"cd"
  given UnitName[Moles[1]] = () => t"mol"
  given UnitName[Amperes[1]] = () => t"A"
  given UnitName[Kelvins[1]] = () => t"K"
  given UnitName[Seconds[1]] = () => t"s"

  given UnitName[Kilograms[1]] with
    override def siPrefix: SiPrefix = Kilo
    def name(): Text = t"g"

trait PrincipalUnit[DimensionType <: Dimension, UnitType <: Measure]()

object PrincipalUnit:
  given length: PrincipalUnit[Length, Metres[1]]()
  given mass: PrincipalUnit[Mass, Kilograms[1]]()
  given time: PrincipalUnit[Time, Seconds[1]]()
  given current: PrincipalUnit[Current, Amperes[1]]()
  given luminosity: PrincipalUnit[Luminosity, Candelas[1]]()
  given temperature: PrincipalUnit[Temperature, Kelvins[1]]()
  given amountOfSubstance: PrincipalUnit[AmountOfSubstance, Moles[1]]()

object QuantifyOpaques:
  opaque type Quantity[UnitsType <: Measure] = Double
  opaque type SiUnit[UnitsType <: Measure] <: Quantity[UnitsType] = Double

  extension [UnitsType <: Measure](quantity: Quantity[UnitsType])
    def value: Double = quantity

  object SiUnit:
    def apply[UnitsType <: Measure](value: Double): SiUnit[UnitsType] = value

    @targetName("makeDerivedUnit")
    def apply[UnitsType <: Measure](value: Quantity[UnitsType]): SiUnit[UnitsType] = value

  object Quantity:
    erased given [UnitsType <: Measure]: CanEqual[Quantity[UnitsType], Quantity[UnitsType]] =
      compiletime.erasedValue

    def apply[UnitsType <: Measure](value: Double): Quantity[UnitsType] = value
    
    given convertDouble[UnitsType <: Measure]: Conversion[Double, Quantity[UnitsType]] =
      Quantity(_)
    
    given convertInt[UnitsType <: Measure]: Conversion[Int, Quantity[UnitsType]] = int =>
      Quantity(int.toDouble)

    inline given [UnitsType <: Measure](using Decimalizer): Debug[Quantity[UnitsType]] =
      new Debug[Quantity[UnitsType]]:
        def show(value: Quantity[UnitsType]): Text = value.render
    
    inline given [UnitsType <: Measure](using Decimalizer): Show[Quantity[UnitsType]] =
      new Show[Quantity[UnitsType]]:
        def show(value: Quantity[UnitsType]): Text = value.render
  
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

export QuantifyOpaques.{Quantity, SiUnit}

val Metre: SiUnit[Metres[1]] = SiUnit(1)
val Gram: SiUnit[Kilograms[1]] = SiUnit(0.001)
val Candela: SiUnit[Candelas[1]] = SiUnit(1)
val Mole: SiUnit[Moles[1]] = SiUnit(1)
val Ampere: SiUnit[Amperes[1]] = SiUnit(1)
val Kelvin: SiUnit[Kelvins[1]] = SiUnit(1)
val Second: SiUnit[Seconds[1]] = SiUnit(1)

extension [UnitsType <: Measure](inline quantity: Quantity[UnitsType])
  @targetName("plus")
  transparent inline def +[UnitsType2 <: Measure](quantity2: Quantity[UnitsType2]): Any =
    ${QuantifyMacros.add[UnitsType, UnitsType2]('quantity, 'quantity2, false)}
  
  @targetName("minus")
  transparent inline def -[UnitsType2 <: Measure](quantity2: Quantity[UnitsType2]): Any =
    ${QuantifyMacros.add[UnitsType, UnitsType2]('quantity, 'quantity2, true)}

  transparent inline def invert: Any = Quantity[Measure](1.0)/quantity

  transparent inline def in[UnitsType2[power <: Nat] <: Units[power, ?]]: Any =
    ${QuantifyMacros.norm[UnitsType, UnitsType2]('quantity)}
  
  @targetName("times2")
  transparent inline def *
      [UnitsType2 <: Measure](@allowConversions inline quantity2: Quantity[UnitsType2]): Any =
    ${QuantifyMacros.multiply[UnitsType, UnitsType2]('quantity, 'quantity2, false)}
  
  @targetName("divide2")
  transparent inline def /
      [UnitsType2 <: Measure](@allowConversions inline quantity2: Quantity[UnitsType2]): Any =
    ${QuantifyMacros.multiply[UnitsType, UnitsType2]('quantity, 'quantity2, true)}

  inline def units: Map[Text, Int] = ${QuantifyMacros.collectUnits[UnitsType]}
  inline def render(using Decimalizer): Text = t"${quantity.value} ${Quantity.renderUnits(units)}"

  @targetName("greaterThan")
  inline def >[UnitsType2 <: Measure](that: Quantity[UnitsType2]): Boolean =
    ${QuantifyMacros.greaterThan[UnitsType, UnitsType2]('quantity, 'that, false)}
  
  @targetName("greaterThanOrEqualTo")
  inline def >=[UnitsType2 <: Measure](that: Quantity[UnitsType2]): Boolean =
    ${QuantifyMacros.greaterThan[UnitsType, UnitsType2]('quantity, 'that, true)}
  
  @targetName("lessThanOrEqualTo")
  inline def <=[UnitsType2 <: Measure](that: Quantity[UnitsType2]): Boolean =
    ${QuantifyMacros.greaterThan[UnitsType2, UnitsType]('that, 'quantity, true)}
  
  @targetName("lessThan")
  inline def <[UnitsType2 <: Measure](that: Quantity[UnitsType2]): Boolean =
    ${QuantifyMacros.greaterThan[UnitsType2, UnitsType]('that, 'quantity, false)}

extension (value: Double)
  @targetName("times")
  def *[UnitsType <: Measure](quantity: Quantity[UnitsType]): Quantity[UnitsType] = quantity*value
  
  @targetName("divide")
  transparent inline def /[UnitsType <: Measure](quantity: Quantity[UnitsType]): Any =
    ((1.0/value)*quantity).invert
  
