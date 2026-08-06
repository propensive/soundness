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
┃    Soundness, version 0.64.0.                                                                    ┃
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
import vacuous.*

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

extension [populable: Vacuiscible](value: populable)
  inline def nil: Boolean = populable.nil(value)

extension [countable: Countable](value: countable)
  inline def gamut: Interval = Interval.initial(countable.size(value))

  inline def iterate(inline lambda: (Ordinal in value.type) => Unit): Unit =
    var index: Int = 0

    while index < countable.size(value) do
      lambda(Ordinal.zerary(index).asInstanceOf[Ordinal in value.type])
      index += 1

  // As `iterate`, over a branded sub-interval: a sub-interval of a valid extent is itself
  // valid, so its ordinals share the brand.
  inline def iterate(range: Interval in value.type)
    ( inline lambda: (Ordinal in value.type) => Unit )
  :   Unit =

    val interval: Interval = range
    var index: Int = interval.start.n0
    val end: Int = interval.limit.n0

    while index < end do
      lambda(Ordinal.zerary(index).asInstanceOf[Ordinal in value.type])
      index += 1

  // The first index satisfying the predicate, confined to this value, or `Unset`: the safe
  // form of a guarded forward scan whose caller consumes the stopping index.
  inline def spot(inline predicate: (Ordinal in value.type) => Boolean)
  :   Optional[Ordinal in value.type] =

    var index: Int = 0
    val size: Int = countable.size(value)
    var found: Int = -1

    while found < 0 && index < size do
      if predicate(Ordinal.zerary(index).asInstanceOf[Ordinal in value.type]) then found = index
      else index += 1

    if found < 0 then Unset else Ordinal.zerary(found).asInstanceOf[Ordinal in value.type]

  // The longest prefix whose every index satisfies the predicate, as a branded interval —
  // possibly empty, possibly the whole extent. Its `limit` is the stopping index, so
  // `value.lead(...)` replaces `while i < size && p(i) do i += 1` loops whose callers then
  // slice or resume from `i`.
  inline def lead(inline predicate: (Ordinal in value.type) => Boolean)
  :   Interval in value.type =

    var index: Int = 0
    val size: Int = countable.size(value)

    while index < size
      && predicate(Ordinal.zerary(index).asInstanceOf[Ordinal in value.type])
    do index += 1

    Interval.zerary(0, index).asInstanceOf[Interval in value.type]

  // The interval remaining after dropping the longest suffix whose indexes satisfy the
  // predicate, never shrinking below `floor` elements: the trailing-trim shape
  // (`while count > 1 && result(count - 1) == 0 do count -= 1` becomes `pare(1)(...)`).
  inline def pare(inline floor: Int)(inline predicate: (Ordinal in value.type) => Boolean)
  :   Interval in value.type =

    var count: Int = countable.size(value)
    val least: Int = floor.max(0)

    while count > least
      && predicate(Ordinal.zerary(count - 1).asInstanceOf[Ordinal in value.type])
    do count -= 1

    Interval.zerary(0, count).asInstanceOf[Interval in value.type]

  // As `iterate`, in reverse: from the last index down to the first.
  inline def retrace(inline lambda: (Ordinal in value.type) => Unit): Unit =
    var index: Int = countable.size(value) - 1

    while index >= 0 do
      lambda(Ordinal.zerary(index).asInstanceOf[Ordinal in value.type])
      index -= 1


// Brand-preserving narrowing: a sub-interval of a valid extent is itself valid, so clamping
// preserves the brand. `capped` keeps at most the first `count` indexes of the extent.
extension [form](range: Interval in form)
  inline def capped(count: Int): Interval in form =
    val interval: Interval = range
    Interval.sized(interval.start.n0, interval.size.min(count.max(0))).asInstanceOf[Interval in form]

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
