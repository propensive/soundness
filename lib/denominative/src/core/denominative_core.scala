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
package denominative

import anticipation.*
import prepositional.*

final val Prim: Ordinal = Ordinal.zerary(0)
final val Sec: Ordinal  = Ordinal.zerary(1)
final val Ter: Ordinal  = Ordinal.zerary(2)
final val Quat: Ordinal = Ordinal.zerary(3)
final val Quin: Ordinal = Ordinal.zerary(4)
final val Sen: Ordinal  = Ordinal.zerary(5)
final val Sept: Ordinal = Ordinal.zerary(6)

extension (inline cardinal: Int)
  inline def z: Ordinal = Ordinal.zerary(cardinal)
  inline def u: Ordinal = Ordinal.uniary(cardinal)

extension [populable: Populable](value: populable)
  inline def nil: Boolean = populable.nil(value)

extension [countable: Countable](value: countable)
  inline def gamut: Interval = Interval.initial(countable.size(value))

  inline def iterate(inline lambda: (Ordinal in value.type) => Unit): Unit =
    var index: Int = 0

    while index < countable.size(value) do
      lambda(Ordinal.zerary(index).asInstanceOf[Ordinal in value.type])
      index += 1

export denominative.internal.{Ordinal, Interval, Span}

infix type aka [subject, label <: Label] = denominative.protointernal.Tagged[subject, label]

extension (any: Any)
  inline def aka[label <: Label]: any.type aka label = denominative.protointernal.Tagged[label](any)

package ordinalTextualizables:
  given nominalOrdinal: Ordinal is Textualizable =
    case Prim    => "prim".tt
    case Sec     => "sec".tt
    case Ter     => "ter".tt
    case Quat    => "quat".tt
    case Quin    => "quin".tt
    case Sen     => "sen".tt
    case Sept    => "sept".tt
    case ordinal => (""+ordinal+".z").tt

  given uniaryOrdinal: Ordinal is Textualizable = ordinal => ""+ordinal.n1+"♭"
  given zeraryOrdinal: Ordinal is Textualizable = ordinal => ""+ordinal.n0+"♯"
  given unmarkedUniaryOrdinal: Ordinal is Textualizable = ordinal => ""+ordinal.n1
  given unmarkedZeraryOrdinal: Ordinal is Textualizable = ordinal => ""+ordinal.n0

  given intermediateOrdinal: Ordinal is Textualizable =
    ordinal => "⌞"+ordinal.n0+"⌟|⌞"+ordinal.n1+"⌟"

  given englishOrdinal: Ordinal is Textualizable = ordinal =>
    ordinal.n1%100 match
      case 11 | 12 | 13 => ordinal.n1.toString+"th"

      case _ =>
        (ordinal.n1%10) match
          case 1 => ordinal.n1.toString+"st"
          case 2 => ordinal.n1.toString+"nd"
          case 3 => ordinal.n1.toString+"rd"
          case _ => ordinal.n1.toString+"th"

  given englishSuperscriptOrdinal: Ordinal is Textualizable = ordinal =>
    ordinal.n1%100 match
      case 11 | 12 | 13 => ordinal.n1.toString+"ᵗʰ"

      case _ =>
        (ordinal.n1%10) match
          case 1 => ordinal.n1.toString+"ˢᵗ"
          case 2 => ordinal.n1.toString+"ⁿᵈ"
          case 3 => ordinal.n1.toString+"ʳᵈ"
          case _ => ordinal.n1.toString+"ᵗʰ"

  given frenchOrdinal: Ordinal is Textualizable = ordinal =>
    if ordinal.n1 == 1 then "1ᵉʳ" else s"${ordinal.n1}ᵉ"

  given italianOrdinal: Ordinal is Textualizable = ordinal => s"${ordinal.n1}ᵒ"
  given spanishOrdinal: Ordinal is Textualizable = ordinal => s"${ordinal.n1}.ᵒ"
  given russianOrdinal: Ordinal is Textualizable = ordinal => s"${ordinal.n1}-й"
