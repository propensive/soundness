/*
    Quantitative, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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
import spectacular.*

import scala.quoted.*

object TallyQuaques:
  opaque type Tally[UnitsType <: Tuple] = Long

  object Tally:
    def fromLong[UnitsType <: Tuple](long: Long): Tally[UnitsType] = long
    
    inline def apply[UnitsType <: Tuple](inline values: Int*): Tally[UnitsType] =
      ${QuantitativeMacros.make[UnitsType]('values)}

    inline given [UnitsType <: Tuple]: Show[Tally[UnitsType]] = new Show[Tally[UnitsType]]:
      def apply(tally: Tally[UnitsType]): Text = Text:
        tally.components.filter(_(1) != 0).map: (unit, count) =>
          Text(count.toString+unit)
        .mkString(" ")
    
  extension [UnitsType <: Tuple](tally: Tally[UnitsType])
    def longValue: Long = tally
    
  extension [UnitsType <: Tuple](inline tally: Tally[UnitsType])
    @targetName("add")
    inline def +(right: Tally[UnitsType]): Tally[UnitsType] =
      ${QuantitativeMacros.addTally[UnitsType]('tally, 'right)}

    inline def apply[UnitType[PowerType <: Nat] <: Units[PowerType, ? <: Dimension]]: Int =
      ${QuantitativeMacros.get[UnitsType, UnitType[1]]('tally)}
    
    transparent inline def quantity: Any =
      ${QuantitativeMacros.toQuantity[UnitsType]('tally)}
    
    inline def components: ListMap[Text, Long] =
      ${QuantitativeMacros.describeTally[UnitsType]('tally)}

export TallyQuaques.Tally

type TimeMinutes = (Hours[1], Minutes[1])
type TimeSeconds = (Hours[1], Minutes[1], Seconds[1])

