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
package zephyrine

import soundness.*

import strategies.throwUnsafely

// A serializer lends each block it fills as a `Region`, valid only for the call; the region is
// a capability, so capture checking stops a consumer keeping it.
object CaptureTests extends Suite("streams-capture", m"Lending confinement tests"):
  def run(): Unit =
    // A lent region was an untracked value, which a consumer could keep after the call (#2565).
    test(m"a lent region cannot be kept in a collection"):
      demilitarize:
        var kept: List[zephyrine.Region[Data]] = Nil
        Producer.lendUtf8(region => interval => kept = region :: kept)(_.put(t"Exit"))
      . map(_.message)
    . assert(_.exists(_.contains("cannot flow into capture set")))

    test(m"a closure over a lent region cannot be kept"):
      demilitarize:
        var kept: () => Data = () => Data()
        Producer.lendUtf8(region => interval => kept = () => region.materialize(interval)):
          _.put(t"Exit")
    . assert(!_.nil)

    test(m"what a consumer copies from a lent region may be kept"):
      demilitarize:
        var kept: List[Data] = Nil
        Producer.lendUtf8(region => interval => kept = region.materialize(interval) :: kept):
          _.put(t"Exit")
    . assert(_ == Nil)

    // A mark is a position in a buffer which only a hold keeps: outside one, a mark which was
    // carried out of its hold cannot be taken up again (#2565).
    test(m"a mark carried out of its hold cannot be grabbed"):
      demilitarize:
        val cursor = Cursor(Iterator(t"hello", t"world"))
        val mark = cursor.hold(cursor.mark)
        cursor.grab(mark, mark)
    . assert(!_.nil)

    test(m"a mark carried out of its hold cannot be returned to"):
      demilitarize:
        val cursor = Cursor(Iterator(t"hello", t"world"))
        val mark = cursor.hold(cursor.mark)
        cursor.cue(mark)
    . assert(!_.nil)

    test(m"marks are grabbed within the hold which took them"):
      demilitarize:
        val cursor = Cursor(Iterator(t"hello", t"world"))
        val word: Text = cursor.hold:
          val start = cursor.mark
          cursor.next()
          cursor.grab(start, cursor.mark)
    . assert(_ == Nil)
