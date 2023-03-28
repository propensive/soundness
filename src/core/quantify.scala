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
erased trait TimeLength extends Dimension
erased trait Current extends Dimension
erased trait Luminosity extends Dimension
erased trait Temperature extends Dimension
erased trait AmountOfSubstance extends Dimension

trait Units[PowerType <: Int & Singleton, DimensionType <: Dimension]

erased trait Metre[Power <: Int & Singleton] extends Units[Power, Length]
erased trait Gram[Power <: Int & Singleton] extends Units[Power, Mass]
erased trait Candela[Power <: Int & Singleton] extends Units[Power, Luminosity]
erased trait Mole[Power <: Int & Singleton] extends Units[Power, AmountOfSubstance]
erased trait Ampere[Power <: Int & Singleton] extends Units[Power, Current]
erased trait Kelvin[Power <: Int & Singleton] extends Units[Power, Temperature]
erased trait Second[Power <: Int & Singleton] extends Units[Power, TimeLength]

trait UnitName[-ValueType]:
  def name(): Text

object UnitName:
  given UnitName[Metre[1]] = () => t"m"
  given UnitName[Gram[1]] = () => t"g"
  given UnitName[Candela[1]] = () => t"cd"
  given UnitName[Mole[1]] = () => t"mol"
  given UnitName[Ampere[1]] = () => t"A"
  given UnitName[Kelvin[1]] = () => t"K"
  given UnitName[Second[1]] = () => t"s"

trait PrincipalUnit
    [DimensionType <: Dimension, UnitType <: Units[1, DimensionType], PowerType <: Int & Singleton]
    ()

object PrincipalUnit:
  given PrincipalUnit[Length, Metre[1], 0]()
  given PrincipalUnit[Mass, Gram[1], 3]()
  given PrincipalUnit[TimeLength, Second[1], 0]()
  given PrincipalUnit[Current, Ampere[1], 0]()
  given PrincipalUnit[Luminosity, Candela[1], 0]()
  given PrincipalUnit[Temperature, Kelvin[1], 0]()
  given PrincipalUnit[AmountOfSubstance, Mole[1], 0]()

object QuantifyOpaques:
  opaque type Quantity[UnitsType <: Units[?, ?]] = Double
  opaque type SiUnit[UnitsType <: Units[?, ?]] <: Quantity[UnitsType] = Double

  extension [UnitsType <: Units[?, ?]](quantity: Quantity[UnitsType])
    def value: Double = quantity

  object SiUnit:
    def apply[UnitsType <: Units[?, ?]](value: Double): SiUnit[UnitsType] = value

  object Quantity:
    def apply[UnitsType <: Units[?, ?]](value: Double): Quantity[UnitsType] = value
    
    given convertDouble[UnitsType <: Units[?, ?]]: Conversion[Double, Quantity[UnitsType]] = Quantity(_)
    given convertInt[UnitsType <: Units[?, ?]]: Conversion[Int, Quantity[UnitsType]] = int => Quantity(int.toDouble)

    inline given [UnitsType <: Units[?, ?]](using DecimalFormat): Show[Quantity[UnitsType]] =
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
              case '-' => '⁻'
              case _   => ' '
          
          t"$unit$exponent"
      .join(t"·")

export QuantifyOpaques.{Quantity, SiUnit}

val Metre = SiUnit[Metre[1]](1)
val Gram = SiUnit[Gram[1]](1)
val Candela = SiUnit[Candela[1]](1)
val Mole = SiUnit[Mole[1]](1)
val Ampere = SiUnit[Ampere[1]](1)
val Kelvin = SiUnit[Kelvin[1]](1)
val Second = SiUnit[Second[1]](1)

// class Quantity[UnitsType <: Units[?, ?]](val value: Double):
//   quantity =>

extension [UnitsType <: Units[?, ?]](inline quantity: Quantity[UnitsType])
  @targetName("plus")
  inline def +(quantity2: Quantity[UnitsType]): Quantity[UnitsType] = Quantity(quantity.value + quantity2.value)
  
  @targetName("minus")
  inline def -(quantity2: Quantity[UnitsType]): Quantity[UnitsType] = Quantity(quantity.value - quantity2.value)
  
  @targetName("times2")
  transparent inline def *
      [UnitsType2 <: Units[?, ?]](@allowConversions inline quantity2: Quantity[UnitsType2]): Any =
    ${QuantifyMacros.multiply[UnitsType, UnitsType2]('quantity, 'quantity2, false)}
  
  @targetName("divide2")
  transparent inline def /
      [UnitsType2 <: Units[?, ?]](@allowConversions inline quantity2: Quantity[UnitsType2]): Any =
    ${QuantifyMacros.multiply[UnitsType, UnitsType2]('quantity, 'quantity2, true)}

  inline def units: Map[Text, Int] = ${QuantifyMacros.collectUnits[UnitsType]}
  inline def render(using DecimalFormat): Text = t"${quantity.value}${Quantity.renderUnits(units)}"

extension (value: Double)
  @targetName("times")
  def *[UnitsType <: Units[?, ?]](quantity: Quantity[UnitsType]): Quantity[UnitsType] = quantity*value
  
  // @tarhgetName("divide")
  // def /[UnitsType <: Units[?, ?]](quantity: Quantity[UnitsType]): Quantity[UnitsType] = quantity.invert*value
  