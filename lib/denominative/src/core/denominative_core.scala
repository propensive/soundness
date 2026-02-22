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
package denominative

import anticipation.*
import symbolism.*

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

  inline def from(ordinal: Ordinal): Interval = ordinal till (ordinal + cardinal)
  inline def upto(ordinal: Ordinal): Interval = (ordinal - cardinal) till ordinal

extension [countable: Countable](value: countable)
  inline def gamut: Interval = Interval(Prim, (countable.size(value) - 1).z)
  inline def nil: Boolean = countable.empty(value)

export Denominative.{Ordinal, Interval}

package ordinalShowables:
  given nominal: Ordinal is Textualizable =
    case Prim    => "prim".tt
    case Sec     => "sec".tt
    case Ter     => "ter".tt
    case Quat    => "quat".tt
    case Quin    => "quin".tt
    case Sen     => "sen".tt
    case Sept    => "sept".tt
    case ordinal => (""+ordinal+".z").tt

  given uniary: Ordinal is Textualizable = ordinal => ""+ordinal.n1+"♭"
  given zerary: Ordinal is Textualizable = ordinal => ""+ordinal.n0+"♯"
  given unmarkedUniary: Ordinal is Textualizable = ordinal => ""+ordinal.n1
  given unmarkedZerary: Ordinal is Textualizable = ordinal => ""+ordinal.n0
  given intermediate: Ordinal is Textualizable = ordinal => "⌞"+ordinal.n0+"⌟|⌞"+ordinal.n1+"⌟"

  given english: Ordinal is Textualizable = ordinal =>
    ordinal.n1%100 match
      case 11 | 12 | 13 => ordinal.n1.toString+"th"
      case _            => (ordinal.n1%10) match
        case 1 => ordinal.n1.toString+"st"
        case 2 => ordinal.n1.toString+"nd"
        case 3 => ordinal.n1.toString+"rd"
        case _ => ordinal.n1.toString+"th"

  given englishSuperscript: Ordinal is Textualizable = ordinal =>
    ordinal.n1%100 match
      case 11 | 12 | 13 => ordinal.n1.toString+"ᵗʰ"
      case _            => (ordinal.n1%10) match
        case 1 => ordinal.n1.toString+"ˢᵗ"
        case 2 => ordinal.n1.toString+"ⁿᵈ"
        case 3 => ordinal.n1.toString+"ʳᵈ"
        case _ => ordinal.n1.toString+"ᵗʰ"

  given french: Ordinal is Textualizable = ordinal =>
    if ordinal.n1 == 1 then "1ᵉʳ" else s"${ordinal}ᵉ"

  given italian: Ordinal is Textualizable = ordinal => s"${ordinal}ᵒ"
  given spanish: Ordinal is Textualizable = ordinal => s"$ordinal.ᵒ"
  given russian: Ordinal is Textualizable = ordinal => s"${ordinal}-й"
