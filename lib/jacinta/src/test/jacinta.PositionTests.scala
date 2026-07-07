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
package jacinta

import soundness.*

import charEncoders.utf8Encoder
import parsing.trackPositions
import strategies.throwUnsafely

object PositionTests extends Suite(m"Jacinta position-index tests"):

  private def at(line: Int, column: Int, length: Int): Json.Ast.Position =
    Json.Ast.Position(line, column, length = length)

  def run(): Unit =
    suite(m"Single-line root primitives"):
      test(m"Locate a root number"):
        val json = t"42".read[Json]
        json.locate(JsonPointer())
      . assert(_ == at(1, 1, 2))

      test(m"Locate a root string"):
        val json = t""""hello"""".read[Json]
        json.locate(JsonPointer())
      . assert(_ == at(1, 1, 7))

      test(m"Locate a root boolean"):
        val json = t"true".read[Json]
        json.locate(JsonPointer())
      . assert(_ == at(1, 1, 4))

      test(m"Locate a root null"):
        val json = t"null".read[Json]
        json.locate(JsonPointer())
      . assert(_ == at(1, 1, 4))

      test(m"Locate a negative number includes the minus sign in length"):
        val json = t"-42".read[Json]
        json.locate(JsonPointer())
      . assert(_ == at(1, 1, 3))

    suite(m"Single-line objects"):
      test(m"Locate the root object"):
        val json = t"""{"a":1,"b":2}""".read[Json]
        json.locate(JsonPointer())
      . assert(_ == at(1, 1, 13))

      test(m"Locate the value at key 'a'"):
        val json = t"""{"a":1,"b":2}""".read[Json]
        json.locate(JsonPointer()(t"a"))
      . assert(_ == at(1, 6, 1))

      test(m"Locate the value at key 'b'"):
        val json = t"""{"a":1,"b":2}""".read[Json]
        json.locate(JsonPointer()(t"b"))
      . assert(_ == at(1, 12, 1))

      test(m"Locate the key 'a' itself"):
        val json = t"""{"a":1,"b":2}""".read[Json]
        json.locateKey(JsonPointer()(t"a"))
      . assert(_ == at(1, 2, 3))

      test(m"Locate the key 'b' itself"):
        val json = t"""{"a":1,"b":2}""".read[Json]
        json.locateKey(JsonPointer()(t"b"))
      . assert(_ == at(1, 8, 3))

      test(m"Missing key returns Unset"):
        val json = t"""{"a":1}""".read[Json]
        json.locate(JsonPointer()(t"missing"))
      . assert(_ == Unset)

    suite(m"Single-line arrays"):
      test(m"Locate the root array"):
        val json = t"[10,20,30]".read[Json]
        json.locate(JsonPointer())
      . assert(_ == at(1, 1, 10))

      test(m"Locate the first array element"):
        val json = t"[10,20,30]".read[Json]
        json.locate(JsonPointer()(Prim))
      . assert(_ == at(1, 2, 2))

      test(m"Locate the last array element"):
        val json = t"[10,20,30]".read[Json]
        json.locate(JsonPointer()(Ter))
      . assert(_ == at(1, 8, 2))

      test(m"Out-of-bounds index returns Unset"):
        val json = t"[10,20]".read[Json]
        json.locate(JsonPointer()(Ter))
      . assert(_ == Unset)

      test(m"Empty array locates only the root"):
        val json = t"[]".read[Json]
        json.locate(JsonPointer())
      . assert(_ == at(1, 1, 2))

    suite(m"Nested structures"):
      test(m"Locate a key in a nested object"):
        val json = t"""{"a":{"b":42}}""".read[Json]
        json.locate(JsonPointer()(t"a")(t"b"))
      . assert(_ == at(1, 11, 2))

      test(m"Locate an element of a nested array"):
        val json = t"""{"xs":[1,2,3]}""".read[Json]
        json.locate(JsonPointer()(t"xs")(Sec))
      . assert(_ == at(1, 10, 1))

      test(m"Locate an object inside an array"):
        val json = t"""[{"id":1},{"id":2}]""".read[Json]
        json.locate(JsonPointer()(Sec)(t"id"))
      . assert(_ == at(1, 17, 1))

    suite(m"Multi-line input"):
      test(m"Second-line key is on line 2"):
        val source = t"{\n  \"a\": 42\n}"
        val json = source.read[Json]
        json.locate(JsonPointer()(t"a")).let(_.line)
      . assert(_ == 2)

      test(m"Second-line value column is past the indent"):
        val source = t"{\n  \"a\": 42\n}"
        val json = source.read[Json]
        json.locate(JsonPointer()(t"a")).let(_.column)
      . assert(_ == 8)

      test(m"Third-line element is on line 3"):
        val source = t"[\n  1,\n  2,\n  3\n]"
        val json = source.read[Json]
        json.locate(JsonPointer()(Sec)).let(_.line)
      . assert(_ == 3)

    suite(m"Number-array specialisations"):
      test(m"Small-BCD array still has per-element positions"):
        val json = t"[1,2,3]".read[Json]
        json.locate(JsonPointer()(Sec))
      . assert(_ == at(1, 4, 1))

      test(m"BCD-Long array still has per-element positions"):
        val json = t"[12345678901234,1]".read[Json]
        json.locate(JsonPointer()(Prim)).let(_.length)
      . assert(_ == 14)

    suite(m"Subsequence property"):
      test(m"A nested object's descriptor slice has length == slot 0"):
        val json = t"""{"a":{"b":42}}""".read[Json]
        val data = json.positionIndex.vouch.ints
        val firstEntryOff = data(5)
        val valueDescOff = firstEntryOff + 3
        val valueSize = data(valueDescOff)
        val slice = data.slice(valueDescOff, valueDescOff + valueSize)
        slice.length == valueSize
      . assert(identity)

    suite(m"Non-tracking mode"):
      test(m"Without the import, positionIndex is Unset"):
        given PositionTracking = PositionTracking.Off
        t"42".read[Json].positionIndex
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
