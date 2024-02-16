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

import rudiments.*
import gossamer.*
import anticipation.*
import symbolism.*

import language.experimental.captureChecking
import language.implicitConversions

trait Quantifiable[QuantityType, UnitsType <: Units[?, ?]]:
  extension (value: QuantityType) def quantify: Quantity[UnitsType]

extension (value: Double)
  @targetName("times")
  infix def * [UnitsType <: Measure](quantity: Quantity[UnitsType]): Quantity[UnitsType] = quantity*value
  
  @targetName("divide")
  transparent inline infix def / [UnitsType <: Measure](quantity: Quantity[UnitsType]): Any =
    ((1.0/value)*quantity).invert
  
extension [UnitsType <: Measure](inline quantity: Quantity[UnitsType])
  @targetName("plus")
  transparent inline infix def + [UnitsType2 <: Measure](quantity2: Quantity[UnitsType2]): Any =
    ${Quantitative.add[UnitsType, UnitsType2]('quantity, 'quantity2, '{false})}
  
  @targetName("minus")
  transparent inline infix def - [UnitsType2 <: Measure](quantity2: Quantity[UnitsType2]): Any =
    ${Quantitative.add[UnitsType, UnitsType2]('quantity, 'quantity2, '{true})}

  transparent inline def invert: Any = Quantity[Measure](1.0)/quantity

  transparent inline def in[UnitsType2[power <: Nat] <: Units[power, ?]]: Any =
    ${Quantitative.norm[UnitsType, UnitsType2]('quantity)}
  
  @targetName("times2")
  transparent inline infix def * [UnitsType2 <: Measure]
      (@convertible inline quantity2: Quantity[UnitsType2]): Any =
    ${Quantitative.multiply[UnitsType, UnitsType2]('quantity, 'quantity2, false)}
  
  @targetName("divide2")
  transparent inline infix def / [UnitsType2 <: Measure]
      (@convertible inline quantity2: Quantity[UnitsType2]): Any =
    ${Quantitative.multiply[UnitsType, UnitsType2]('quantity, 'quantity2, true)}

  transparent inline def sqrt(using sqrt: SquareRoot[Quantity[UnitsType]]): sqrt.Result = sqrt.sqrt(quantity)
  inline def units: Map[Text, Int] = ${Quantitative.collectUnits[UnitsType]}
  inline def render(using Decimalizer): Text = t"${quantity.value} ${Quantity.renderUnits(units)}"
  inline def dimension: Text = ${Quantitative.describe[UnitsType]}