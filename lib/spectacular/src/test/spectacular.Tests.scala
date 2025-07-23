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
┃    Soundness, version 0.40.0.                                                                    ┃
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
package spectacular

import anticipation.*
import fulminate.*
import gossamer.*
import probably.*
import rudiments.*
import spectacular.*

case class Person(name: Text, age: Int)

object Tests extends Suite(m"Spectacular Tests"):
  def run(): Unit =
    suite(m"Debug tests"):
      test(m"serialize boring string"):
        t"Hello world!".inspect
      .assert(_ == t"t\"Hello world!\"")

      test(m"serialize string with newline"):
        t"Hello\nworld".inspect
      .assert(_ == t"t\"Hello\\nworld\"")

      test(m"serialize string with tab"):
        t"Hello\tworld".inspect
      .assert(_ == t"t\"Hello\\tworld\"")

      test(m"serialize string with apostrophe"):
        t"Hell' world".inspect
      .assert(_ == t"t\"Hell\\' world\"")

      test(m"serialize string with quote"):
        t"Hello \"world\"".inspect
      .assert(_ == t"t\"Hello \\\"world\\\"\"")

      test(m"serialize string with backslash"):
        t"Hello\\world".inspect
      .assert(_ == t"t\"Hello\\\\world\"")

      test(m"serialize string with linefeed"):
        t"Hello world\r".inspect
      .assert(_ == t"t\"Hello world\\r\"")

      test(m"serialize string with unicode escapes"):
        t"Hello мир".inspect
      .assert(_ == t"t\"Hello \\u043c\\u0438\\u0440\"")

      test(m"pattern match on Text"):
        var text = t"Hello"
        text match
          case t"Hello" => true
          case _        => false
      .assert(_ == true)

      test(m"pattern non-match on Text"):
        var text = t"Hello"
        text match
          case t"World" => true
          case _        => false
      .assert(_ == false)

      test(m"serialize double"):
        3.1.inspect
      .assert(_ == t"3.1")

      test(m"serialize float"):
        3.1f.inspect
      .assert(_ == t"3.1F")

      test(m"serialize long"):
        3L.inspect
      .assert(_ == t"3L")

      test(m"serialize int"):
        3.inspect
      .assert(_ == t"3")

      test(m"serialize short"):
        3.toShort.inspect
      .assert(_ == t"3.toShort")

      test(m"serialize +infinity"):
        (1.0/0.0).inspect
      .assert(_ == t"Double.PositiveInfinity")

      test(m"serialize -infinity"):
        (-1.0/0.0).inspect
      .assert(_ == t"Double.NegativeInfinity")

      test(m"serialize NaN"):
        (0.0/0.0).inspect
      .assert(_ == t"Double.NaN")

      test(m"serialize float +infinity"):
        (1.0F/0.0F).inspect
      .assert(_ == t"Float.PositiveInfinity")

      test(m"serialize float -infinity"):
        (-1.0F/0.0F).inspect
      .assert(_ == t"Float.NegativeInfinity")

      test(m"serialize float NaN"):
        (0.0F/0.0F).inspect
      .assert(_ == t"Float.NaN")

      test(m"serialize tab char"):
        '\t'.inspect
      .assert(_ == t"'\\t'")

      test(m"serialize backslash char"):
        '\\'.inspect
      .assert(_ == t"'\\\\'")

      test(m"serialize newline char"):
        '\n'.inspect
      .assert(_ == t"'\\n'")

      test(m"serialize backspace char"):
        '\b'.inspect
      .assert(_ == t"'\\b'")

      test(m"serialize unicode char"):
        '«'.inspect
      .assert(_ == t"'\\u00ab'")

      test(m"serialize apostrophe char"):
        '\''.inspect
      .assert(_ == t"'\\''")

      test(m"serialize quote char"):
        '\"'.inspect
      .assert(_ == t"'\\\"'")

      test(m"serialize case class"):
        Person(t"Simon", 72).inspect
      .assert(_ == t"Person(name:t\"Simon\" ╱ age:72)")

      test(m"serialize tuple"):
        (t"Simon", 72).inspect
      .assert(_ == t"(t\"Simon\" ╱ 72)")

      test(m"serialize list of strings"):
        List(t"one", t"two", t"three").inspect
      .assert(_ == t"""[t"one", t"two", t"three"]""")

      test(m"serialize set of strings"):
        Set(t"one", t"two", t"three").inspect
      .assert(_ == t"""{t"one", t"two", t"three"}""")

      test(m"serialize Array of strings"):
        Array(t"one", t"two", t"three").inspect
      .assert(_ == t"""⦋🅻₀t"one"∣₁t"two"∣₂t"three"⦌""")

      test(m"serialize Array of ints"):
        Array(1, 2, 3).inspect
      .assert(_ == t"""⦋🅸₀1∣₁2∣₂3⦌""")

      test(m"serialize Vector of shorts"):
        Vector(1.toShort, 2.toShort, 3.toShort).inspect
      .assert(_ == t"""⟨ 1.toShort 2.toShort 3.toShort ⟩""")

      test(m"serialize Array of Longs"):
        Array(1L, 2L, 3L).inspect
      .assert(_ == t"""⦋🅹₀1L∣₁2L∣₂3L⦌""")

      test(m"serialize IArray of booleans"):
        IArray(true, false, true).inspect
      .assert(_ == t"""🆉⁅₀true╱₁false╱₂true⁆""")

      test(m"serialize IArray of strings"):
        IArray(t"one", t"two", t"three").inspect
      .assert(_ == t"""🅻⁅₀t"one"╱₁t"two"╱₂t"three"⁆""")

    suite(m"Show tests"):
      test(m"Show a string"):
        t"Hello world".show
      .assert(_ == t"Hello world")

      test(m"Show an Int"):
        43.show
      .assert(_ == t"43")

      test(m"Show yes/no booleans"):
        import booleanStyles.yesNo
        t"${true} ${false}"
      .assert(_ == t"yes no")

      test(m"Show true/false booleans"):
        import booleanStyles.trueFalse
        t"${true} ${false}"
      .assert(_ == t"true false")

      test(m"Show on/off booleans"):
        import booleanStyles.onOff
        t"${true} ${false}"
      .assert(_ == t"on off")

      test(m"Show 1/0 booleans"):
        import booleanStyles.oneZero
        t"${true} ${false}"
      .assert(_ == t"1 0")

      test(m"Show a locally-declared showable"):
        given Exception is Showable = e => txt"<exception>"
        Exception("error message").inspect
      .assert(_ == t"<exception>")
