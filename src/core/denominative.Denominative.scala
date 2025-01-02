/*
    Denominative, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package denominative

import anticipation.*

import scala.annotation.targetName

object Denominative:
  opaque type Ordinal = Int
  opaque type Interval = Long

  extension (ordinal: Ordinal)
    @targetName("minus2")
    infix def - (right: Ordinal): Int = ordinal - right

  extension (inline ordinal: Ordinal)
    @targetName("plus")
    inline infix def + (inline cardinal: Int): Ordinal = ordinal + cardinal

    @targetName("invert")
    inline def `unary_~`: Countback = Countback(ordinal.n0)

    @targetName("minus")
    inline infix def - (inline cardinal: Int): Ordinal = ordinal - cardinal

    inline def le(inline right: Ordinal): Boolean = (ordinal: Int) <= (right: Int)
    inline def lt(inline right: Ordinal): Boolean = (ordinal: Int) < (right: Int)
    inline def ge(inline right: Ordinal): Boolean = (ordinal: Int) >= (right: Int)
    inline def gt(inline right: Ordinal): Boolean = (ordinal: Int) > (right: Int)
    inline def next: Ordinal = ordinal + 1
    inline def previous: Ordinal = (ordinal - 1).max(0)

    @targetName("to")
    inline infix def ~ (inline right: Ordinal): Interval = Interval(ordinal, right)

    @targetName("to2")
    inline infix def ~ (inline countback: Countback): Bounds = Bounds(ordinal, countback)

    inline def n0: Int = ordinal
    inline def n1: Int = ordinal + 1
    inline def subsequent(size: Int): Interval = Interval(ordinal + 1, ordinal + size + 1)
    inline def preceding(size: Int): Interval = Interval((ordinal - size).max(0), ordinal - 1)

  object Ordinal:
    inline def zerary(inline cardinal: Int): Ordinal = cardinal
    inline def natural(inline cardinal: Int): Ordinal = cardinal - 1

    given Ordinal is Textualizer =
      case Prim    => "prim".tt
      case Sec     => "sec".tt
      case Ter     => "ter".tt
      case Quat    => "quat".tt
      case Quin    => "quin".tt
      case Sen     => "sen".tt
      case Sept    => "sept".tt
      case Oct     => "oct".tt
      case Non     => "non".tt
      case Den     => "den".tt
      case ordinal => ("Ordinal.natural("+ordinal+")").tt

  extension (interval: Interval)
    inline def start: Ordinal = ((interval >> 32) & 0xffffffff).toInt
    inline def end: Ordinal = (interval & 0xffffffff).toInt
    inline def contains(ordinal: Ordinal): Boolean = start <= ordinal && ordinal <= end
    inline def size: Int = (end - start) max 0
    inline def next: Ordinal = end + 1
    inline def previous: Ordinal = start - 1
    inline def subsequent(size: Int): Interval = end.subsequent(size)
    inline def preceding(size: Int): Interval = start.preceding(size)

    inline def each(inline lambda: Ordinal => Unit): Unit =
      var i: Ordinal = start

      while i <= end do
        lambda(i)
        i = i.next

    inline def foldLeft[ValueType](inline initial: ValueType)
       (inline lambda: (ValueType, Ordinal) => ValueType)
            : ValueType =

      var i: Ordinal = start
      var acc: ValueType = initial

      while i <= end do
        acc = lambda(acc, i)
        i = i.next
      acc

    inline def empty: Boolean = end < start

  object Interval:
    inline def initial(size: Int): Interval = size.toLong
    inline def empty: Interval = 0L

    inline def apply(inline start: Ordinal, inline end: Ordinal): Interval =
      (start & 0xffffffffL) << 32 | (end + 1) & 0xffffffffL
