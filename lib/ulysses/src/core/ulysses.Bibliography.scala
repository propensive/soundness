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
package ulysses

import anticipation.*
import beneficence.*

object Bibliography:
  def apply(data: List[Data]): Bibliography =
    // Explicit element type: inference freshens the frozen inner arrays to `any.rd`. The
    // stdlib view feeds `Array.from`'s `IterableOnce` directly: the frozen element type does
    // not survive `to[Array]`'s capture set, so the generic conversion cannot serve here.
    new Bibliography(Array.from[Data](data.stdlib))

  // Unsigned lexicographic order of two byte strings, a shorter string preceding its extensions.
  private def ordered(left: Data, right: Data): Int =
    val shared = left.length.min(right.length)

    (0 until shared).find(index => left.readable(index) != right.readable(index)).fold
      (left.length - right.length)
      (index => (left.readable(index) & 0xff) - (right.readable(index) & 0xff))

  // The order of `hash` relative to every string beginning with `prefix`: zero when `hash` begins
  // with it, so that the hashes it matches form one run in the sorted index.
  private def prefixOrder(hash: Data, prefix: Data): Int =
    val shared = hash.length.min(prefix.length)

    (0 until shared).find(index => hash.readable(index) != prefix.readable(index)).fold
      (if hash.length < prefix.length then -1 else 0)
      (index => (hash.readable(index) & 0xff) - (prefix.readable(index) & 0xff))

  // The first index in `[low, high)` whose hash does not precede `prefix`.
  @tailrec
  private def lowerBound(sorted: scala.IArray[Data], prefix: Data, low: Int, high: Int): Int =
    if low >= high then low else
      val middle = (low + high) >>> 1

      if prefixOrder(sorted(middle), prefix) < 0 then lowerBound(sorted, prefix, middle + 1, high)
      else lowerBound(sorted, prefix, low, middle)

case class Bibliography(hashes: Array[Data]^{}) extends Findable:
  // The hashes in unsigned lexicographic byte order, so that every hash beginning with a given
  // prefix sits in one contiguous run, found by binary search. One ordering serves every prefix
  // length, so the `Cadence` (which sets the `k_i`- and `k_r`-byte prefix lengths the palimpsest
  // §5 decoder asks for) need not be known here. Built on the first lookup.
  private lazy val sorted: scala.IArray[Data] =
    hashes.readable.sortWith(Bibliography.ordered(_, _) < 0)

  // Return every library hash whose leading bytes equal `prefix`, in the index's byte order.
  def lookup(prefix: Data): Iterator[Data] =
    val first = Bibliography.lowerBound(sorted, prefix, 0, sorted.length)

    sorted.iterator.drop(first).takeWhile(Bibliography.prefixOrder(_, prefix) == 0)
