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
┃    Soundness, version 0.37.0.                                                                    ┃
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
  opaque type Quanta[units <: Tuple] = Long

  object Quanta:
    erased given underlying: [units <: Tuple] => Underlying[Quanta[units], Long] = !!
    given zeroic: [units <: Tuple] => Quanta[units] is Zeroic:
      inline def zero: Quanta[units] = 0L

    given typeable: [units <: Tuple] => Typeable[Quanta[units]]:
      def unapply(count: Any): Option[count.type & Quanta[units]] = count.asMatchable match
        case count: Long => Some(count)
        case _           => None


    def fromLong[units <: Tuple](long: Long): Quanta[units] = long
    given integral: [units <: Tuple] => Integral[Quanta[units]] = summon[Integral[Long]]

    inline def apply[units <: Tuple](inline values: Int*): Quanta[units] =
      ${Abacist.make[units]('values)}

    given addable: [units <: Tuple] => Quanta[units] is Addable:
      type Operand = Quanta[units]
      type Result = Quanta[units]

      def add(left: Quanta[units], right: Quanta[units]): Quanta[units] = left + right

    given subtractable: [units <: Tuple] => Quanta[units] is Subtractable:
      type Operand = Quanta[units]
      type Result = Quanta[units]

      def subtract(left: Quanta[units], right: Quanta[units]): Quanta[units] = left - right

    given multiplicable: [units <: Tuple] => Quanta[units] is Multiplicable:
      type Operand = Double
      type Result = Quanta[units]

      def multiply(left: Quanta[units], right: Double): Quanta[units] = left.multiply(right)

    given divisible: [units <: Tuple] => Quanta[units] is Divisible:
      type Operand = Double
      type Result = Quanta[units]

      def divide(left: Quanta[units], right: Double): Quanta[units] = left.divide(right)

    inline given showable: [units <: Tuple] => Quanta[units] is Showable = summonFrom:
      case names: UnitsNames[units] => count =>
        val nonzeroComponents = count.components.filter(_(1) != 0)
        val nonzeroUnits = nonzeroComponents.map(_(1).toString.tt).to(List)
        val units = nonzeroUnits.head :: nonzeroUnits.tail.map(names.separator+_)
        units.weave(names.units().takeRight(nonzeroUnits.length)).mkString.tt

      case _ => count =>
        val nonzeroComponents = count.components.filter(_(1) != 0)
        nonzeroComponents.map { (unit, count) => count.toString+unit }.mkString(" ").tt

  extension [units <: Tuple](count: Quanta[units])
    def longValue: Long = count

  extension [units <: Tuple](inline count: Quanta[units])
    @targetName("negate")
    inline def `unary_-`: Quanta[units] = -count

    inline def apply[unit[power <: Nat] <: Units[power, ? <: Dimension]]: Int =
      ${Abacist.get[units, unit[1]]('count)}

    transparent inline def quantity: Any = ${Abacist.toQuantity[units]('count)}
    inline def components: ListMap[Text, Long] = ${Abacist.describeQuanta[units]('count)}

    transparent inline def multiply(inline multiplier: Double): Any =
      ${Abacist.multiplyQuanta('count, 'multiplier, false)}

    transparent inline def divide(inline multiplier: Double): Any =
      ${Abacist.multiplyQuanta('count, 'multiplier, true)}


    transparent inline def collapse(length: Int)(using length.type < Tuple.Size[units] =:= true)
    : Quanta[Tuple.Drop[units, length.type]] =

        count
