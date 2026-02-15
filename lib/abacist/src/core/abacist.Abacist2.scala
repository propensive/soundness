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
┃    Soundness, version 0.54.0.                                                                    ┃
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
import gossamer.*
import hypotenuse.*
import quantitative.*
import prepositional.*
import proscenium.*
import rudiments.*
import spectacular.*
import symbolism.*

import scala.compiletime.*, ops.int.*

object Abacist2 extends Abacist3:
  opaque type Quanta[units <: Tuple] = Long

  object Quanta extends Quanta2:
    inline given underlying: [units <: Tuple] => Underlying[Quanta[units], Long] = !!
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

    given negatable: [units <: Tuple] => Quanta[units] is Negatable to Quanta[units] = -_

    inline given showable: [units <: Tuple] => Quanta[units] is Showable = summonFrom:
      case names: UnitsNames[units] => count =>
        val nonzeroComponents = count.components.filter(_(1) != 0)
        val nonzeroUnits = nonzeroComponents.map(_(1).toString.tt).to(List)
        val units = nonzeroUnits.head :: nonzeroUnits.tail.map(names.separator+_)
        units.weave(names.units().takeRight(nonzeroUnits.length)).mkString.tt

      case _ => count =>
        val nonzeroComponents = count.components.filter(_(1) != 0)
        nonzeroComponents.map { (unit, count) => count.toString+unit }.mkString(" ").tt

    inline given distributive2: [units <: Tuple] => Quanta[units] is Distributive by Long =
      distributive[units](_.components.map(_(1)).to(List)): (value, parts) =>
        parts.zip(value.components.map(_(0))).map: (number, units) =>
          t"$number $units"
        . join(t", ")

    def distributive[units <: Tuple]
      ( parts0: Quanta[units] => List[Long] )
      ( place0: (Quanta[units], List[Text]) => Text )
    : Quanta[units] is Distributive by Long =

        new Distributive:
          type Self = Quanta[units]
          type Operand = Long
          def parts(value: Quanta[units]): List[Long] = parts0(value)
          def place(value: Quanta[units], parts: List[Text]): Text = place0(value, parts)

  extension [units <: Tuple](count: Quanta[units])
    def long: Long = count

  extension [units <: Tuple](inline count: Quanta[units])
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
