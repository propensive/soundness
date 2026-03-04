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

import scala.annotation.targetName

import prepositional.*
import symbolism.*

object internal:
  opaque type Ordinal = Int
  opaque type Interval = Long
  opaque type Tagged[+value, tag] = value


  object Tagged:
    inline def apply[tag](value: Any): Tagged[value.type, tag] = value

  extension [value, tag](tagged: Tagged[value, tag]) inline def apply(): value = tagged

  extension (ordinal: Ordinal)
    @targetName("minus2")
    infix def - (right: Ordinal): Int = ordinal - right

    inline infix def thru (right: Ordinal): Interval = Interval(ordinal, right)
    inline infix def till (right: Ordinal): Interval = Interval(ordinal, right - 1)
    inline infix def span (right: Int): Interval = Interval(ordinal, ordinal + right - 1)


  extension (inline ordinal: Ordinal)
    inline def le(inline right: Ordinal): Boolean = (ordinal: Int) <= (right: Int)

    inline def lt(inline right: Ordinal): Boolean = (ordinal: Int) < (right: Int)
    inline def ge(inline right: Ordinal): Boolean = (ordinal: Int) >= (right: Int)
    inline def gt(inline right: Ordinal): Boolean = (ordinal: Int) > (right: Int)
    inline def next: Ordinal = ordinal + 1
    inline def previous: Ordinal = (ordinal - 1).max(0)

    inline def n0: Int = ordinal
    inline def n1: Int = ordinal + 1
    inline def subsequent(size: Int): Interval = Interval(ordinal + 1, ordinal + size + 1)
    inline def preceding(size: Int): Interval = Interval((ordinal - size).max(0), ordinal - 1)


  object Ordinal:
    inline def zerary(inline cardinal: Int): Ordinal = cardinal
    inline def uniary(inline cardinal: Int): Ordinal = cardinal - 1

    given addable: Ordinal is Addable by Int to Ordinal = _ + _
    given subtractable: Ordinal is Subtractable by Ordinal to Int = _ - _
    given subtractable2: Ordinal is Subtractable by Int to Ordinal = _ - _
    given ordering: Ordering[Ordinal] = Ordering[Int]


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


    inline def fuse[value](inline initial: value)
      ( inline lambda: (value aka "state", Ordinal aka "next") ?=> value )
    :   value =

      var i: Ordinal = start
      var acc: value = initial

      while i <= end do
        acc = lambda(using acc, i)
        i = i.next
      acc


    inline def empty: Boolean = end < start


  object Interval:
    inline def initial(size: Int): Interval = size.toLong
    inline def empty: Interval = 0L

    inline def zerary(inline start: Int, inline end: Int): Interval =
      (start & 0xffffffffL) << 32 | end & 0xffffffffL

    inline def apply(inline start: Ordinal, inline end: Ordinal): Interval =
      (start & 0xffffffffL) << 32 | (end + 1) & 0xffffffffL
