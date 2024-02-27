/*
    Abacist, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package abacist

import rudiments.*
import anticipation.*
import quantitative.*
import spectacular.*

import scala.quoted.*
import scala.compiletime.ops.int.*

object CountQuaques:
  opaque type Count[UnitsType <: Tuple] = Long

  object Count extends Count2:
    erased given underlying[UnitsType <: Tuple]: Underlying[Count[UnitsType], Long] = ###
    def fromLong[UnitsType <: Tuple](long: Long): Count[UnitsType] = long
    
    inline def apply[UnitsType <: Tuple](inline values: Int*): Count[UnitsType] =
      ${Abacist.make[UnitsType]('values)}

    inline given [UnitsType <: Tuple](using names: UnitsNames[UnitsType]): Show[Count[UnitsType]] =
      new Show[Count[UnitsType]]:
        def apply(count: Count[UnitsType]): Text =
          val nonzeroUnits = count.components.filter(_(1) != 0).map(_(1).toString.tt).to(List)
          val units = nonzeroUnits.head :: nonzeroUnits.tail.map(names.separator+_)
          units.interleave(names.units().takeRight(nonzeroUnits.length)).mkString.tt
    
  trait Count2:
    inline given [UnitsType <: Tuple]: Show[Count[UnitsType]] = new Show[Count[UnitsType]]:
      def apply(count: Count[UnitsType]): Text =
        count.components.filter(_(1) != 0).map { (unit, count) => count.toString+unit }.mkString(" ").tt
    
  extension [UnitsType <: Tuple](count: Count[UnitsType])
    def longValue: Long = count
    
  extension [UnitsType <: Tuple](inline count: Count[UnitsType])
    @targetName("add")
    inline infix def + (right: Count[UnitsType]): Count[UnitsType] = count + right
    
    @targetName("sub")
    inline infix def - (right: Count[UnitsType]): Count[UnitsType] = count - right

    @targetName("negate")
    inline def `unary_-`: Count[UnitsType] = -count

    inline def apply[UnitType[PowerType <: Nat] <: Units[PowerType, ? <: Dimension]]: Int =
      ${Abacist.get[UnitsType, UnitType[1]]('count)}
    
    transparent inline def quantity: Any =
      ${Abacist.toQuantity[UnitsType]('count)}
    
    inline def components: ListMap[Text, Long] =
      ${Abacist.describeCount[UnitsType]('count)}
    
    @targetName("multiply")
    transparent inline infix def * (inline multiplier: Double): Any =
      ${Abacist.multiplyCount('count, 'multiplier, false)}
    
    @targetName("divide")
    transparent inline infix def / (inline multiplier: Double): Any =
      ${Abacist.multiplyCount('count, 'multiplier, true)}

    transparent inline def collapse(length: Int)
        (using length.type < Tuple.Size[UnitsType] =:= true)
          : Count[Tuple.Drop[UnitsType, length.type]] =

      count

export CountQuaques.Count

type TimeMinutes = (Hours[1], Minutes[1])
type TimeSeconds = (Hours[1], Minutes[1], Seconds[1])

extension [UnitsType <: Measure](inline quantity: Quantity[UnitsType])
  inline def count[CountType <: Tuple]: Count[CountType] =
    ${Abacist.fromQuantity[UnitsType, CountType]('quantity)}

trait UnitsNames[UnitsType <: Tuple]:
  def prefix: Text = "".tt
  def separator: Text = " ".tt
  def units(): List[Text]