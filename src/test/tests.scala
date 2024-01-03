/*
    Spectacular, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package spectacular

import probably.*
import rudiments.*
import gossamer.*
import anticipation.*
import spectacular.*

case class Person(name: Text, age: Int)

object Tests extends Suite(t"Spectacular Tests"):
  def run(): Unit =
    suite(t"Debug tests"):
      test(t"serialize boring string"):
        t"Hello world!".debug
      .assert(_ == t"t\"Hello world!\"")

      test(t"serialize string with newline"):
        t"Hello\nworld".debug
      .assert(_ == t"t\"Hello\\nworld\"")
      
      test(t"serialize string with tab"):
        t"Hello\tworld".debug
      .assert(_ == t"t\"Hello\\tworld\"")
      
      test(t"serialize string with apostrophe"):
        t"Hell' world".debug
      .assert(_ == t"t\"Hell\\' world\"")
      
      test(t"serialize string with quote"):
        t"Hello \"world\"".debug
      .assert(_ == t"t\"Hello \\\"world\\\"\"")
      
      test(t"serialize string with backslash"):
        t"Hello\\world".debug
      .assert(_ == t"t\"Hello\\\\world\"")
      
      test(t"serialize string with linefeed"):
        t"Hello world\r".debug
      .assert(_ == t"t\"Hello world\\r\"")
      
      test(t"serialize string with unicode escapes"):
        t"Hello мир".debug
      .assert(_ == t"t\"Hello \\u043c\\u0438\\u0440\"")

      test(t"pattern match on Text"):
        var text = t"Hello"
        text match
          case t"Hello" => true
          case _        => false
      .assert(_ == true)
      
      test(t"pattern non-match on Text"):
        var text = t"Hello"
        text match
          case t"World" => true
          case _        => false
      .assert(_ == false)

      test(t"serialize double"):
        3.1.debug
      .assert(_ == t"3.1")
      
      test(t"serialize float"):
        3.1f.debug
      .assert(_ == t"3.1F")
      
      test(t"serialize long"):
        3L.debug
      .assert(_ == t"3L")
      
      test(t"serialize int"):
        3.debug
      .assert(_ == t"3")
      
      test(t"serialize short"):
        3.toShort.debug
      .assert(_ == t"3.toShort")
      
      test(t"serialize +infinity"):
        (1.0/0.0).debug
      .assert(_ == t"Double.PositiveInfinity")
      
      test(t"serialize -infinity"):
        (-1.0/0.0).debug
      .assert(_ == t"Double.NegativeInfinity")
      
      test(t"serialize NaN"):
        (0.0/0.0).debug
      .assert(_ == t"Double.NaN")
      
      test(t"serialize float +infinity"):
        (1.0F/0.0F).debug
      .assert(_ == t"Float.PositiveInfinity")
      
      test(t"serialize float -infinity"):
        (-1.0F/0.0F).debug
      .assert(_ == t"Float.NegativeInfinity")
      
      test(t"serialize float NaN"):
        (0.0F/0.0F).debug
      .assert(_ == t"Float.NaN")

      test(t"serialize tab char"):
        '\t'.debug
      .assert(_ == t"'\\t'")
      
      test(t"serialize backslash char"):
        '\\'.debug
      .assert(_ == t"'\\\\'")
      
      test(t"serialize newline char"):
        '\n'.debug
      .assert(_ == t"'\\n'")
      
      test(t"serialize backspace char"):
        '\b'.debug
      .assert(_ == t"'\\b'")
      
      test(t"serialize unicode char"):
        '«'.debug
      .assert(_ == t"'\\u00ab'")
      
      test(t"serialize apostrophe char"):
        '\''.debug
      .assert(_ == t"'\\''")
      
      test(t"serialize quote char"):
        '\"'.debug
      .assert(_ == t"'\\\"'")

      test(t"serialize case class"):
        Person(t"Simon", 72).debug
      .assert(_ == t"Person(name=t\"Simon\"·age=72)")
      
      test(t"serialize tuple"):
        (t"Simon", 72).debug
      .assert(_ == t"(t\"Simon\"·72)")
      
      test(t"serialize list of strings"):
        List(t"one", t"two", t"three").debug
      .assert(_ == t"""[t"one", t"two", t"three"]""")
      
      test(t"serialize set of strings"):
        Set(t"one", t"two", t"three").debug
      .assert(_ == t"""{t"one", t"two", t"three"}""")
      
      test(t"serialize Array of strings"):
        val text = Array(t"one", t"two", t"three").debug
        text.take(3)+text.drop(8) // need to remove part which is variable for different runs
      .assert(_ == t"""⦋𝕃¦₀t"one"∣₁t"two"∣₂t"three"⦌""")
      
      test(t"serialize Array of ints"):
        val text = Array(1, 2, 3).debug
        text.take(3)+text.drop(8) // need to remove part which is variable for different runs
      .assert(_ == t"""⦋𝕀¦₀1∣₁2∣₂3⦌""")
      
      test(t"serialize Vector of shorts"):
        Vector(1.toShort, 2.toShort, 3.toShort).debug
      .assert(_ == t"""⟨ 1.toShort 2.toShort 3.toShort ⟩""")
      
      test(t"serialize Array of Longs"):
        val text = Array(1L, 2L, 3L).debug
        text.take(3)+text.drop(8) // need to remove part which is variable for different runs
      .assert(_ == t"""⦋\ud835\udd41¦₀1L∣₁2L∣₂3L⦌""")
      
      test(t"serialize IArray of booleans"):
        val text = IArray(true, false, true).debug
        text.take(3)+text.drop(8) // need to remove part which is variable for different runs
      .assert(_ == t"""⁅ℤ¦₀true╱₁false╱₂true⁆""")
      
      test(t"serialize IArray of strings"):
        val text = IArray(t"one", t"two", t"three").debug
        text.take(3)+text.drop(8) // need to remove part which is variable for different runs
      .assert(_ == t"""⁅𝕃¦₀t"one"╱₁t"two"╱₂t"three"⁆""")

    suite(t"Show tests"):
      test(t"Show a string"):
        t"Hello world".show
      .assert(_ == t"Hello world")
      
      test(t"Show an Int"):
        43.show
      .assert(_ == t"43")

      test(t"Show yes/no booleans"):
        import booleanStyles.yesNo
        t"${true} ${false}"
      .assert(_ == t"yes no")
      
      test(t"Show true/false booleans"):
        import booleanStyles.trueFalse
        t"${true} ${false}"
      .assert(_ == t"true false")
      
      test(t"Show on/off booleans"):
        import booleanStyles.onOff
        t"${true} ${false}"
      .assert(_ == t"on off")
      
      test(t"Show 1/0 booleans"):
        import booleanStyles.oneZero
        t"${true} ${false}"
      .assert(_ == t"1 0")

      test(t"Show a locally-declared showable"):
        given Show[Exception] = e => txt"<exception>"
        Exception("error message").debug
      .assert(_ == t"<exception>")
      
