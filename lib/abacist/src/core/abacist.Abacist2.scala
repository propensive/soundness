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
package abacist

import anticipation.*
import quantitative.*
import proscenium.*
import rudiments.*
import spectacular.*
import symbolism.*

import scala.compiletime.*, ops.int.*

object Abacist2:
  opaque type Count[units <: Tuple] = Long

  object Count:
    erased given underlying: [units <: Tuple] => Underlying[Count[units], Long] = !!
    given zeroic: [units <: Tuple] => Count[units] is Zeroic:
      inline def zero(): Count[units] = 0L

    given typeable: [units <: Tuple] => Typeable[Count[units]]:
      def unapply(count: Any): Option[count.type & Count[units]] = count.asMatchable match
        case count: Long => Some(count)
        case _           => None


    def fromLong[units <: Tuple](long: Long): Count[units] = long
    given integral: [units <: Tuple] => Integral[Count[units]] = summon[Integral[Long]]

    inline def apply[units <: Tuple](inline values: Int*): Count[units] =
      ${Abacist.make[units]('values)}

    given addable: [units <: Tuple] => Count[units] is Addable:
      type Operand = Count[units]
      type Result = Count[units]

      def add(left: Count[units], right: Count[units]): Count[units] = left + right

    given subtractable: [units <: Tuple] => Count[units] is Subtractable:
      type Operand = Count[units]
      type Result = Count[units]

      def subtract(left: Count[units], right: Count[units]): Count[units] = left - right

    given multiplicable: [units <: Tuple] => Count[units] is Multiplicable:
      type Operand = Double
      type Result = Count[units]

      def multiply(left: Count[units], right: Double): Count[units] = left.multiply(right)

    given divisible: [units <: Tuple] => Count[units] is Divisible:
      type Operand = Double
      type Result = Count[units]

      def divide(left: Count[units], right: Double): Count[units] = left.divide(right)

    inline given showable: [units <: Tuple] => Count[units] is Showable = summonFrom:
      case names: UnitsNames[units] => count =>
        val nonzeroComponents = count.components.filter(_(1) != 0)
        val nonzeroUnits = nonzeroComponents.map(_(1).toString.tt).to(List)
        val units = nonzeroUnits.head :: nonzeroUnits.tail.map(names.separator+_)
        units.weave(names.units().takeRight(nonzeroUnits.length)).mkString.tt

      case _ => count =>
        val nonzeroComponents = count.components.filter(_(1) != 0)
        nonzeroComponents.map { (unit, count) => count.toString+unit }.mkString(" ").tt

  extension [units <: Tuple](count: Count[units])
    def longValue: Long = count

  extension [units <: Tuple](inline count: Count[units])
    @targetName("negate")
    inline def `unary_-`: Count[units] = -count

    inline def apply[unit[power <: Nat] <: Units[power, ? <: Dimension]]: Int =
      ${Abacist.get[units, unit[1]]('count)}

    transparent inline def quantity: Any = ${Abacist.toQuantity[units]('count)}
    inline def components: ListMap[Text, Long] = ${Abacist.describeCount[units]('count)}

    transparent inline def multiply(inline multiplier: Double): Any =
      ${Abacist.multiplyCount('count, 'multiplier, false)}

    transparent inline def divide(inline multiplier: Double): Any =
      ${Abacist.multiplyCount('count, 'multiplier, true)}

    transparent inline def collapse(length: Int)(using length.type < Tuple.Size[units] =:= true)
    :     Count[Tuple.Drop[units, length.type]] =

      count
