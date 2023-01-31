/*
    Dissonance, version 0.1.0. Copyright 2022-23 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package dissonance

import probably.*
import gossamer.*
import rudiments.*
import turbulence.*
import eucalyptus.*, logging.stdout

import unsafeExceptions.canThrowAny

import Change.*

object Tests extends Suite(t"Dissonance tests"):
  given Realm(t"tests")

  def run(): Unit =
    suite(t"Diff tests"):
      test(t"Empty lists"):
        diff(IArray[Char](), IArray[Char]())
      .assert(_ == Diff())
      
      test(t"One element, equal"):
        diff(IArray('a'), IArray('a'))
      .assert(_ == Diff(Keep(0, 0, 'a')))
      
      test(t"Two elements, equal"):
        diff(IArray('a', 'b'), IArray('a', 'b'))
      .assert(_ == Diff(Keep(0, 0, 'a'), Keep(1, 1, 'b')))
      
      test(t"Insertion to empty list"):
        diff(IArray[Char](), IArray('a'))
      .assert(_ == Diff(Ins(0, 'a')))
      
      test(t"Deletion to become empty list"):
        diff(t"a".chars, t"".chars)
      .assert(_ == Diff(Del(0, 'a')))
      
      test(t"Prefix to short list"):
        diff(t"BC".chars, t"ABC".chars)
      .assert(_ == Diff(Ins(0, 'A'), Keep(0, 1, 'B'), Keep(1, 2, 'C')))
      
      test(t"Suffix to short list"):
        diff(t"AB".chars, t"ABC".chars)
      .assert(_ == Diff(Keep(0, 0, 'A'), Keep(1, 1, 'B'), Ins(2, 'C')))
      
      test(t"Insertion in middle of short list"):
        diff(t"AC".chars, t"ABC".chars)
      .assert(_ == Diff(Keep(0, 0, 'A'), Ins(1, 'B'), Keep(1, 2, 'C')))
      
      test(t"Deletion from middle of short list"):
        diff(t"ABC".chars, t"AC".chars)
      .assert(_ == Diff(Keep(0, 0, 'A'), Del(1, 'B'), Keep(2, 1, 'C')))
      
      test(t"Deletion from start of short list"):
        diff(t"ABC".chars, t"BC".chars)
      .assert(_ == Diff(Del(0, 'A'), Keep(1, 0, 'B'), Keep(2, 1, 'C')))
      
      test(t"Deletion from end of short list"):
        diff(t"ABC".chars, t"AB".chars)
      .assert(_ == Diff(Keep(0, 0, 'A'), Keep(1, 1, 'B'), Del(2, 'C')))
      
      test(t"Multiple inner keeps"):
        import logging.stdout
        diff(t"BCD".chars, t"ABC".chars)
      .assert(_ == Diff(Ins(0, 'A'), Keep(0, 1, 'B'), Keep(1, 2, 'C'), Del(2, 'D')))
      
      test(t"Example from blog"):
        diff(t"ABCABBA".chars, t"CBABAC".chars)
      .assert(_ == Diff(Del(0, 'A'), Del(1, 'B'), Keep(2, 0, 'C'), Del(3, 'A'), Keep(4, 1, 'B'), Ins(2, 'A'), Keep(5, 3, 'B'), Keep(6, 4, 'A'), Ins(5, 'C')))
      
      test(t"Item swap"):
        diff(t"AB".chars, t"BA".chars)
      .assert(_ == Diff(Del(0, 'A'), Keep(1, 0, 'B'), Ins(1, 'A')))
      
      test(t"Item change"):
        diff(t"A".chars, t"C".chars)
      .assert(_ == Diff(Del(0, 'A'), Ins(0, 'C')))
      
      test(t"Item change between values"):
        diff(t"NAN".chars, t"NCN".chars)
      .assert(_ == Diff(Keep(0, 0, 'N'), Del(1, 'A'), Ins(1, 'C'), Keep(2, 2, 'N')))
      
      test(t"Item swap between values"):
        diff(t"NABN".chars, t"NBAN".chars)
      .assert(_ == Diff(Keep(0, 0, 'N'), Del(1, 'A'), Keep(2, 1, 'B'), Ins(2, 'A'), Keep(3, 3, 'N')))
      
      test(t"Item swap interspersed with values"):
        diff(t"AZB".chars, t"BZA".chars)
      .assert(_ == Diff(Del(0, 'A'), Del(1, 'Z'), Keep(2, 0, 'B'), Ins(1, 'Z'), Ins(2, 'A')))
      
      test(t"real-world example 1"):
        diff(IArray(t"A", t"B"), IArray(t"B", t"C", t"D"))
      .assert(_ == Diff(Del(0, t"A"), Keep(1, 0, t"B"), Ins(1, t"C"), Ins(2, t"D")))
      