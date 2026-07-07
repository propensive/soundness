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
┃    Soundness, version 0.63.0.                                                                    ┃
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

import scala.compiletime.*

import anticipation.*
import gossamer.*
import prepositional.*
import quantitative.*
import rudiments.*
import spectacular.*
import symbolism.*

object protointernal extends anteprotointernal:
  opaque type Quanta[base <: AnyUnit] = Long

  object Quanta extends Quanta2:
    inline given underlying: [base <: AnyUnit, form <: Divisions]
    =>  Underlying[Quanta[base] in form, Long] = !!

    inline given underlyingBare: [base <: AnyUnit] => Underlying[Quanta[base], Long] = !!

    // Two givens with structured `Self` types (rather than a single `quanta <: Quanta[base]`)
    // so that a `(? <: value) is Zeroic` search cannot collapse the wildcard to `Nothing`
    // (whose `zero` would throw on the cast in `fromLong`).
    given zeroic: [base <: AnyUnit] => Quanta[base] is Zeroic:
      inline def zero: Quanta[base] = fromLong(0L)

    given zeroic2: [base <: AnyUnit, form <: Divisions] => (Quanta[base] in form) is Zeroic:
      inline def zero: Quanta[base] in form = fromLong(0L)

    given typeable: [base <: AnyUnit, quanta <: Quanta[base]] => Typeable[quanta]:
      def unapply(count: Any): Option[count.type & quanta] = count.asMatchable match
        case count: Long => Some(count.asInstanceOf[count.type & quanta])
        case _           => None

    def fromLong[quanta](long: Long): quanta = long.asInstanceOf[quanta]

    given integral: [base <: AnyUnit, quanta <: Quanta[base]] => Integral[quanta] =
      summon[Integral[Long]].asInstanceOf[Integral[quanta]]

    // `base` and `form` are taken from the expected type, e.g. `val weight: Weight =
    // Quanta(5, 6)`, `date + Quanta(2)`, or — where no expected type is available — a type
    // ascription such as `(Quanta(5, 6): Weight)`.
    inline def apply[base <: AnyUnit, form <: Divisions](inline values: Int*)
    :   Quanta[base] { type Form = form } =

      ${abacist.internal.assembleParts[base, form]('values)}

    given addable: [base <: AnyUnit, quanta <: Quanta[base]] => quanta is Addable:
      type Operand = quanta
      type Result = quanta

      def add(left: quanta, right: quanta): quanta = fromLong(left.long + right.long)

    given subtractable: [base <: AnyUnit, quanta <: Quanta[base]] => quanta is Subtractable:
      type Operand = quanta
      type Result = quanta

      def subtract(left: quanta, right: quanta): quanta = fromLong(left.long - right.long)

    given multiplicable: [base <: AnyUnit, quanta <: Quanta[base]] => quanta is Multiplicable:
      type Operand = Double
      type Result = quanta

      def multiply(left: quanta, right: Double): quanta = fromLong((left.long*right + 0.5).toLong)

    given divisible: [base <: AnyUnit, quanta <: Quanta[base]] => quanta is Divisible:
      type Operand = Double
      type Result = quanta

      def divide(left: quanta, right: Double): quanta = fromLong((left.long/right + 0.5).toLong)

    given negatable: [base <: AnyUnit, quanta <: Quanta[base]] => quanta is Negatable to quanta =
      count => fromLong(-count.long)

    // `showable`/`distributive2` are provided as a pair each: one for refined `Quanta` types
    // (carrying a `Form`) and one for bare single-unit types. Their structured `Self` types let
    // `base`/`form` be unified exactly, rather than maximised to `Any` as a `quanta <: Quanta
    // [base]` bound would be in an inline given.
    inline def showQuanta[base <: AnyUnit, quanta <: Quanta[base]]: quanta is Showable = summonFrom:
      case names: UnitsNames[quanta] =>
        count =>
        val nonzeroComponents = count.components.filter(_(1) != 0)
        val nonzeroUnits = nonzeroComponents.map(_(1).toString.tt).to(List)
        val units = nonzeroUnits.head :: nonzeroUnits.tail.map(names.separator+_)
        units.weave(names.units().takeRight(nonzeroUnits.length)).mkString.tt

      case _ =>
        count =>
        val nonzeroComponents = count.components.filter(_(1) != 0)
        nonzeroComponents.map { (unit, count) => count.toString+unit }.mkString(" ").tt

    inline given showable: [base <: AnyUnit, form <: Divisions]
    =>  (Quanta[base] in form) is Showable =
      showQuanta[base, Quanta[base] in form]

    inline given showableBare: [base <: AnyUnit] => Quanta[base] is Showable =
      showQuanta[base, Quanta[base]]

    inline def distributiveQuanta[base <: AnyUnit, quanta <: Quanta[base]]
    :   quanta is Distributive by Long =

      distributive[quanta](_.components.map(_(1)).to(List)): (value, parts) =>
        parts.zip(value.components.map(_(0))).map: (number, units) =>
          t"$number $units"

        . join(t", ")

    inline given distributive2: [base <: AnyUnit, form <: Divisions]
    =>  (Quanta[base] in form) is Distributive by Long =
      distributiveQuanta[base, Quanta[base] in form]

    inline given distributive2Bare: [base <: AnyUnit] => Quanta[base] is Distributive by Long =
      distributiveQuanta[base, Quanta[base]]


    def distributive[quanta]
      ( parts0: quanta => List[Long] )
      ( place0: (quanta, List[Text]) => Text )
    :   quanta is Distributive by Long =

      new Distributive:
        type Self = quanta
        type Operand = Long
        def parts(value: quanta): List[Long] = parts0(value)
        def place(value: quanta, parts: List[Text]): Text = place0(value, parts)


  extension [base <: AnyUnit, quanta <: Quanta[base]](count: quanta)
    def long: Long = count


  extension [base <: AnyUnit, quanta <: Quanta[base]](inline count: quanta)
    inline def apply[unit[power <: Nat] <: Units[power, ? <: Dimension]]: Int =

      ${abacist.internal.get[quanta, unit[1]]('count)}

    transparent inline def quantity: Any = ${abacist.internal.toQuantity[quanta]('count)}
    inline def components: ListMap[Text, Long] = ${abacist.internal.describeQuanta[quanta]('count)}

    transparent inline def multiply(inline multiplier: Double): Any =
      ${abacist.internal.multiplyQuanta('count, 'multiplier, false)}

    transparent inline def divide(inline multiplier: Double): Any =
      ${abacist.internal.multiplyQuanta('count, 'multiplier, true)}


    transparent inline def collapse(inline length: Int): Any =
      ${abacist.internal.collapse[quanta]('count, 'length)}
