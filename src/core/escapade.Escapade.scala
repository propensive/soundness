/*
    Escapade, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package escapade

import rudiments.*

import language.experimental.pureFunctions

object Escapade:
  opaque type CharSpan = Long

  object CharSpan:
    def apply(start: Int, end: Int): CharSpan = (start.toLong << 32) + (Int.MaxValue - end)
    given Ordering[CharSpan] = Ordering.Long.on[CharSpan](identity(_))
    val Nowhere: CharSpan = CharSpan(Int.MaxValue, Int.MaxValue)

  extension (span: CharSpan)
    def start: Int = (span >> 32).toInt
    def end: Int = Int.MaxValue - span.toInt
    def isEmpty: Boolean = start == end

    def trimLeft(n: Int): CharSpan =
      if n >= end then CharSpan.Nowhere else if n <= start then CharSpan(start - n, end - n)
      else CharSpan(0, end - n)

    def takeLeft(n: Int): CharSpan =
      if n <= start then CharSpan.Nowhere else if n >= end then span else CharSpan(start, n)

    def shift(n: Int): CharSpan = CharSpan(start + n, end + n)
