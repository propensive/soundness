/*
    Denominative, version 0.24.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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

import scala.annotation.targetName

object Denominative2:
  opaque type Countback = Int
  opaque type Bounds = Long

  object Bounds:
    inline def apply(inline start: Ordinal, inline end: Countback): Bounds =
      (start.n0 & 0xffffffffL) << 32 | end & 0xffffffffL

  extension (bounds: Bounds)
    inline def start: Ordinal = ((bounds >> 32) & 0xffffffff).toInt.z
    inline def end: Countback = (bounds & 0xffffffff).toInt
    inline def next: Countback = end - 1
    inline def previous: Ordinal = start - 1

    inline def of[ValueType: Countable](value: ValueType): Interval =
      Interval(bounds.start, Ult.of(value))

  extension (countback: Countback)
    @targetName("minus")
    infix def - (right: Countback): Int = right - countback

  object Countback:
    inline def apply(n: Int): Countback = n

  extension (inline countback: Countback)
    @targetName("plus2")
    inline infix def + (inline cardinal: Int): Countback = countback - cardinal

    @targetName("invert")
    inline def `unary_~`: Ordinal = countback.z

    @targetName("minus2")
    inline infix def - (inline cardinal: Int): Countback = countback + cardinal

    inline def le(inline right: Countback): Boolean = countback >= right
    inline def lt(inline right: Countback): Boolean = countback > right
    inline def ge(inline right: Countback): Boolean = countback <= right
    inline def gt(inline right: Countback): Boolean = countback < right
    inline def next: Countback = (countback - 1).max(0)
    inline def previous: Countback = countback + 1

    inline def of[ValueType: Countable](inline value: ValueType): Ordinal =
      Ordinal.natural(ValueType.size(value) - countback)
