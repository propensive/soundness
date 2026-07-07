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
package stratiform

import soundness.*

import charEncoders.utf8Encoder
import strategies.throwUnsafely

object PositionTests extends Suite(m"Stratiform position-index tests"):

  // A compound's keyword position: 1-based line/column with the keyword's
  // character length (mirrors `jacinta.PositionTests.at`).
  private def at(line: Int, column: Int, length: Int): Tel.Position =
    Tel.Position(line, column, length = Optional(length))

  def run(): Unit =
    suite(m"Top-level compounds"):
      test(m"Locate the document root"):
        Tel.parseTracked(t"greeting hello\n").locate(TelPath(Nil))
      . assert(_ == at(1, 1, 0))

      test(m"Locate a top-level compound by keyword"):
        Tel.parseTracked(t"greeting hello\n").locate(TelPath(List(t"greeting")))
      . assert(_ == at(1, 1, 8))

      test(m"Locate the second of two top-level compounds"):
        Tel.parseTracked(t"first a\nsecond b\n").locate(TelPath(List(t"second")))
      . assert(_ == at(2, 1, 6))

      test(m"An unknown keyword returns Unset"):
        Tel.parseTracked(t"greeting hello\n").locate(TelPath(List(t"absent")))
      . assert(_ == Unset)

    suite(m"Nested compounds"):
      test(m"Locate a child compound"):
        Tel.parseTracked(t"person\n  name Alice\n  age 30\n")
        . locate(TelPath(List(t"person", t"name")))
      . assert(_ == at(2, 3, 4))

      test(m"Locate a sibling child on a later line"):
        Tel.parseTracked(t"person\n  name Alice\n  age 30\n")
        . locate(TelPath(List(t"person", t"age")))
      . assert(_ == at(3, 3, 3))

      test(m"Locate a grandchild compound"):
        Tel.parseTracked(t"a\n  b\n    c hello\n").locate(TelPath(List(t"a", t"b", t"c")))
      . assert(_ == at(3, 5, 1))

      test(m"A missing intermediate segment returns Unset"):
        Tel.parseTracked(t"person\n  name Alice\n").locate(TelPath(List(t"person", t"absent")))
      . assert(_ == Unset)

    suite(m"Column tracks indentation"):
      test(m"A top-level keyword is at column 1"):
        Tel.parseTracked(t"root\n  child value\n").locate(TelPath(List(t"root"))).let(_.column)
      . assert(_ == 1)

      test(m"A one-level-deep keyword is at column 3"):
        Tel.parseTracked(t"root\n  child value\n")
        . locate(TelPath(List(t"root", t"child"))).let(_.column)
      . assert(_ == 3)

      test(m"A one-level-deep keyword is on the second line"):
        Tel.parseTracked(t"root\n  child value\n")
        . locate(TelPath(List(t"root", t"child"))).let(_.line)
      . assert(_ == 2)

    suite(m"Tracking mode"):
      test(m"A tracked parse records a position index"):
        Tel.parseTracked(t"greeting hello\n").positionIndex.absent
      . assert(_ == false)

      test(m"A plain parse leaves the position index Unset"):
        t"greeting hello\n".read[Tel].positionIndex
      . assert(_ == Unset)

      test(m"Locating in a plain (untracked) document returns Unset"):
        t"greeting hello\n".read[Tel].locate(TelPath(List(t"greeting")))
      . assert(_ == Unset)

    suite(m"Span derivation"):
      test(m"a position's span carries its line as a 0-based ordinal"):
        at(2, 8, 3).span.startLine.vouch
      . assert(_ == 1.z)

      test(m"a position's span carries its column as a 0-based ordinal"):
        at(2, 8, 3).span.startColumn.vouch
      . assert(_ == 7.z)

      test(m"a position's span carries its length"):
        at(2, 8, 3).span.length.vouch
      . assert(_ == 3)
