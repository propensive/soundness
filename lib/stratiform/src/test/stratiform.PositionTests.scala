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
import parsing.trackPositions
import strategies.throwUnsafely

object PositionTests extends Suite(m"Stratiform position-index tests"):

  // A located region: 1-based line/column with its character length (mirrors
  // `jacinta.PositionTests.at`). `locateKey` gives a compound's keyword;
  // `locate` gives its value — the inline-atom run.
  private def at(line: Int, column: Int, length: Int): Tel.Position =
    Tel.Position(line, column, length = Optional(length))

  def run(): Unit =
    suite(m"Top-level compounds"):
      test(m"Locate the document root"):
        t"greeting hello\n".read[Tel].locate(Telp(Nil))
      . assert(_ == at(1, 1, 0))

      test(m"Locate a top-level compound's keyword"):
        t"greeting hello\n".read[Tel].locateKey(Telp(List(t"greeting")))
      . assert(_ == at(1, 1, 8))

      test(m"Locate the keyword of the second of two top-level compounds"):
        t"first a\nsecond b\n".read[Tel].locateKey(Telp(List(t"second")))
      . assert(_ == at(2, 1, 6))

      test(m"An unknown keyword returns Unset"):
        t"greeting hello\n".read[Tel].locate(Telp(List(t"absent")))
      . assert(_ == Unset)

    suite(m"Nested compounds"):
      test(m"Locate a child compound's keyword"):
        t"person\n  name Alice\n  age 30\n".read[Tel]
        . locateKey(Telp(List(t"person", t"name")))
      . assert(_ == at(2, 3, 4))

      test(m"Locate the keyword of a sibling child on a later line"):
        t"person\n  name Alice\n  age 30\n".read[Tel]
        . locateKey(Telp(List(t"person", t"age")))
      . assert(_ == at(3, 3, 3))

      test(m"Locate a grandchild compound's keyword"):
        t"a\n  b\n    c hello\n".read[Tel].locateKey(Telp(List(t"a", t"b", t"c")))
      . assert(_ == at(3, 5, 1))

      test(m"A missing intermediate segment returns Unset"):
        t"person\n  name Alice\n".read[Tel].locate(Telp(List(t"person", t"absent")))
      . assert(_ == Unset)

    suite(m"Column tracks indentation"):
      test(m"A top-level keyword is at column 1"):
        t"root\n  child value\n".read[Tel].locateKey(Telp(List(t"root"))).let(_.column)
      . assert(_ == 1)

      test(m"A one-level-deep keyword is at column 3"):
        t"root\n  child value\n".read[Tel]
        . locateKey(Telp(List(t"root", t"child"))).let(_.column)
      . assert(_ == 3)

      test(m"A one-level-deep keyword is on the second line"):
        t"root\n  child value\n".read[Tel]
        . locateKey(Telp(List(t"root", t"child"))).let(_.line)
      . assert(_ == 2)

    // `locate` points at the text a decode error is about — the compound's
    // inline atoms — so a diagnostic underlines the value its message names.
    suite(m"Value extents"):
      test(m"Locate a compound's value, not its keyword"):
        t"greeting hello\n".read[Tel].locate(Telp(List(t"greeting")))
      . assert(_ == at(1, 10, 5))

      test(m"Locate the value of an indented child"):
        t"item\n  unit-cost  banana\n".read[Tel]
        . locate(Telp(List(t"item", t"unit-cost")))
      . assert(_ == at(2, 14, 6))

      test(m"A value spans from the first atom to the last"):
        t"range 1 to 10\n".read[Tel].locate(Telp(List(t"range")))
      . assert(_ == at(1, 7, 7))

      test(m"A hard-space gap falls inside the value's span"):
        t"note a  b\n".read[Tel].locate(Telp(List(t"note")))
      . assert(_ == at(1, 6, 4))

      test(m"Value columns count characters, not bytes"):
        // `café` is four characters but five UTF-8 bytes: a byte-counting
        // implementation reports column 7 here.
        t"café x\n".read[Tel].locate(Telp(List(t"café")))
      . assert(_ == at(1, 6, 1))

      test(m"A compound with only children falls back to its keyword"):
        t"person\n  name Alice\n".read[Tel].locate(Telp(List(t"person")))
      . assert(_ == at(1, 1, 6))

      test(m"A remark is not a value, so the keyword is used"):
        t"item # just a remark\n".read[Tel].locate(Telp(List(t"item")))
      . assert(_ == at(1, 1, 4))

      test(m"A source atom's payload is not spanned, so the keyword is used"):
        t"name\n    Alice\n".read[Tel].locate(Telp(List(t"name")))
      . assert(_ == at(1, 1, 4))

      test(m"A literal atom's payload is not spanned, so the keyword is used"):
        t"name\n      ---\nAlice\n      ---\n".read[Tel].locate(Telp(List(t"name")))
      . assert(_ == at(1, 1, 4))

      test(m"The root has no value, so it falls back to its empty keyword"):
        t"greeting hello\n".read[Tel].locate(Telp(Nil))
      . assert(_ == at(1, 1, 0))

      test(m"locateKey at the root is Unset"):
        t"greeting hello\n".read[Tel].locateKey(Telp(Nil))
      . assert(_ == Unset)

      // If the parser's record stride and `buildIndex`'s fold ever disagree,
      // sibling descriptors garble; a deep, wide document catches that where a
      // single-compound one would not.
      test(m"Every path in a deep, wide document resolves to its own value"):
        val doc = t"a 1\nb\n  c 2\n  d\n    e 3\n    f 4\n  g 5\nh 6\n"
        val tel = doc.read[Tel]

        List
         ( tel.locate(Telp(List(t"a"))),
           tel.locate(Telp(List(t"b", t"c"))),
           tel.locate(Telp(List(t"b", t"d", t"e"))),
           tel.locate(Telp(List(t"b", t"d", t"f"))),
           tel.locate(Telp(List(t"b", t"g"))),
           tel.locate(Telp(List(t"h"))) )
      . assert(_ == List
                     ( at(1, 3, 1),
                       at(3, 5, 1),
                       at(5, 7, 1),
                       at(6, 7, 1),
                       at(7, 5, 1),
                       at(8, 3, 1) ))

    suite(m"Tracking mode"):
      test(m"`import parsing.trackPositions` records a position index"):
        t"greeting hello\n".read[Tel].positionIndex.absent
      . assert(_ == false)

      test(m"Without the import, the position index is Unset"):
        given PositionTracking = PositionTracking.Off
        t"greeting hello\n".read[Tel].positionIndex
      . assert(_ == Unset)

      test(m"Locating in an untracked document returns Unset"):
        given PositionTracking = PositionTracking.Off
        t"greeting hello\n".read[Tel].locate(Telp(List(t"greeting")))
      . assert(_ == Unset)

    suite(m"Span derivation"):
      test(m"a position's span carries its line as a 0-based ordinal"):
        at(2, 8, 3).span.startLine
      . assert(_ == 1.z)

      test(m"a position's span carries its column as a 0-based ordinal"):
        at(2, 8, 3).span.startColumn
      . assert(_ == 7.z)

      test(m"a position's span carries its length"):
        at(2, 8, 3).span.length
      . assert(_ == 3)

    // `enclosing` is the reverse lookup (#1463): a 1-indexed source coordinate
    // to the innermost compound containing it, with the keyword path that
    // reaches it. The highlighted region follows the coordinate: the
    // inline-atom run when the cursor is inside it, the keyword otherwise.
    suite(m"Enclosing-node lookup"):
      def enclosure(path: List[Text], position: Tel.Position): Tel.Enclosure =
        Tel.Enclosure(Telp(path), position)

      test(m"A coordinate on a top-level keyword encloses that compound"):
        t"person\n  name Alice\n  age 30\n".read[Tel].enclosing(1, 2)
      . assert(_ == enclosure(List(t"person"), at(1, 1, 6)))

      test(m"A coordinate on a child's keyword encloses the child"):
        t"person\n  name Alice\n  age 30\n".read[Tel].enclosing(2, 4)
      . assert(_ == enclosure(List(t"person", t"name"), at(2, 3, 4)))

      test(m"A coordinate inside the inline-atom run names the value region"):
        t"person\n  name Alice\n  age 30\n".read[Tel].enclosing(2, 9)
      . assert(_ == enclosure(List(t"person", t"name"), at(2, 8, 5)))

      test(m"A coordinate in a child line's indentation encloses that child"):
        t"person\n  name Alice\n".read[Tel].enclosing(2, 1)
      . assert(_ == enclosure(List(t"person", t"name"), at(2, 3, 4)))

      test(m"A coordinate on a later sibling encloses it, not the first"):
        t"person\n  name Alice\n  age 30\n".read[Tel].enclosing(3, 5)
      . assert(_ == enclosure(List(t"person", t"age"), at(3, 3, 3)))

      test(m"A grandchild coordinate builds the full keyword path"):
        t"a\n  b\n    c hello\n".read[Tel].enclosing(3, 5)
      . assert(_ == enclosure(List(t"a", t"b", t"c"), at(3, 5, 1)))

      test(m"A second top-level compound bounds its predecessor's extent"):
        t"first\n  child x\nsecond y\n".read[Tel].enclosing(3, 1)
      . assert(_ == enclosure(List(t"second"), at(3, 1, 6)))

      test(m"A blank line between siblings belongs to the preceding subtree"):
        t"first a\n\nsecond b\n".read[Tel].enclosing(2, 1)
      . assert(_ == enclosure(List(t"first"), at(1, 1, 5)))

      test(m"A coordinate after the last compound encloses its deepest node"):
        t"a\n  b c\n".read[Tel].enclosing(3, 1)
      . assert(_ == enclosure(List(t"a", t"b"), at(2, 3, 1)))

      test(m"A coordinate enclosed only by the document root yields Unset"):
        t"\nfirst a\n".read[Tel].enclosing(1, 1)
      . assert(_ == Unset)

      test(m"An untracked document yields Unset"):
        given PositionTracking = PositionTracking.Off
        t"greeting hello\n".read[Tel].enclosing(1, 1)
      . assert(_ == Unset)
