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
┃    Soundness, version 0.27.0.                                                                    ┃
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
package inimitable

import anticipation.*
import contingency.*
import digression.*
import fulminate.*
import gossamer.*
import larceny.*
import probably.*
import rudiments.*

import errorDiagnostics.stackTraces

object Tests extends Suite(t"Inimitable Tests"):
  def run(): Unit =
    suite(t"UUID tests"):
      test(t"Construct a new UUID"):
        Uuid()
      .matches:
        case Uuid(a, b) => (a, b)

      test(t"Get bytes from UUID"):
        Uuid().bytes
      .assert(_.length == 16)

      test(t"Parse a UUID at compiletime"):
        uuid"a0cb16f0-d41e-4c28-862f-bd6164bbcc8c"
      .assert()

      test(t"Get the most significant bits from a UUID"):
        uuid"a0cb16f0-d41e-4c28-862f-bd6164bbcc8c".msb
      .assert(_ == -6860364383762101208L)

      test(t"Get the least significant bits from a UUID"):
        uuid"a0cb16f0-d41e-4c28-862f-bd6164bbcc8c".lsb
      .assert(_ == -8777588922722300788L)

      test(t"Get the Java UUID"):
        uuid"a0cb16f0-d41e-4c28-862f-bd6164bbcc8c".java
      .assert(_ == java.util.UUID.fromString("a0cb16f0-d41e-4c28-862f-bd6164bbcc8c"))

      test(t"Get the bytes from a UUID"):
        uuid"a0cb16f0-d41e-4c28-862f-bd6164bbcc8c".bytes.to(List)
      .assert(_ == List[Byte](-96, -53, 22, -16, -44, 30, 76, 40, -122, 47, -67, 97, 100, -69, -52, -116))

      test(t"Convert a UUID to Text"):
        uuid"a0cb16f0-d41e-4c28-862f-bd6164bbcc8c".text
      .assert(_ == t"a0cb16f0-d41e-4c28-862f-bd6164bbcc8c")

      test(t"Parse a UUID at runtime"):
        unsafely(Uuid.parse(t"a0cb16f0-d41e-4c28-862f-bd6164bbcc8c"))
      .assert(_ == Uuid(-6860364383762101208L, -8777588922722300788L))

      test(t"Parse a bad UUID at runtime"):
        unsafely(capture[UuidError](Uuid.parse(t"not-a-uuid")))
      .assert(_ == UuidError(t"not-a-uuid"))

      val uuid1 = Uuid()
      val uuid2 = Uuid()

      test(t"XOR two UUIDs"):
        uuid1 ^ uuid2
      .assert { uuid => uuid != uuid1 && uuid != uuid2 }

      test(t"Invert a UUID"):
        ~uuid1
      .assert(_ != uuid1)

      test(t"Parse a bad UUID at compiletime"):
        demilitarize:
          uuid"not-a-uuid"
        .map(_.message)
      .assert(_ == List(t"inimitable: not-a-uuid is not a valid UUID"))
