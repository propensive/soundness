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
import anticipation.*
import spectacular.*

import scala.quoted.*

object CountQuaques:
  opaque type Count[UnitsType <: Tuple] = Long

  object Count:
    erased given underlying[UnitsType <: Tuple]: Underlying[Count[UnitsType], Long] = ###
    def fromLong[UnitsType <: Tuple](long: Long): Count[UnitsType] = long
    
    inline def apply[UnitsType <: Tuple](inline values: Int*): Count[UnitsType] =
      ${QuantitativeMacros.make[UnitsType]('values)}

    inline given [UnitsType <: Tuple]: Show[Count[UnitsType]] = new Show[Count[UnitsType]]:
      def apply(count: Count[UnitsType]): Text = Text:
        count.components.filter(_(1) != 0).map: (unit, count) =>
          Text(count.toString+unit)
        .mkString(" ")
    
  extension [UnitsType <: Tuple](count: Count[UnitsType])
    def longValue: Long = count
    
  extension [UnitsType <: Tuple](inline count: Count[UnitsType])
    @targetName("add")
    inline infix def + (right: Count[UnitsType]): Count[UnitsType] =
      ${QuantitativeMacros.addCount[UnitsType]('count, 'right)}

    inline def apply[UnitType[PowerType <: Nat] <: Units[PowerType, ? <: Dimension]]: Int =
      ${QuantitativeMacros.get[UnitsType, UnitType[1]]('count)}
    
    transparent inline def quantity: Any =
      ${QuantitativeMacros.toQuantity[UnitsType]('count)}
    
    inline def components: ListMap[Text, Long] =
      ${QuantitativeMacros.describeCount[UnitsType]('count)}
    
    @targetName("multiply")
    transparent inline infix def * (inline multiplier: Double): Any =
      ${QuantitativeMacros.multiplyCount('count, 'multiplier, false)}
    
    @targetName("divide")
    transparent inline infix def / (inline multiplier: Double): Any =
      ${QuantitativeMacros.multiplyCount('count, 'multiplier, true)}

export CountQuaques.Count

type TimeMinutes = (Hours[1], Minutes[1])
type TimeSeconds = (Hours[1], Minutes[1], Seconds[1])

