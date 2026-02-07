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
┃    Soundness, version 0.53.0.                                                                    ┃
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
package dissonance

import anticipation.*
import contingency.*
import denominative.*
import eucalyptus.*
import fulminate.*
import gossamer.*
import probably.*
import proscenium.*
import rudiments.*
import turbulence.*

import proximities.levenshteinDistance
import caseSensitivity.sensitive

import strategies.throwUnsafely

object Tests extends Suite(m"Dissonance tests"):
  private given Realm = realm"tests"

  def run(): Unit =
    suite(m"Diff tests"):
      test(m"Empty lists"):
        diff(IArray[Char](), IArray[Char]())
      .assert(_ == Diff())

      test(m"One element, equal"):
        diff(IArray('a'), IArray('a'))
      .assert(_ == Diff(Par(0, 0, 'a')))

      test(m"Straight swap"):
        diff(IArray('a'), IArray('A'))
      .assert(_ == Diff(Del(0, 'a'), Ins(0, 'A')))

      test(m"Two elements, equal"):
        diff(IArray('a', 'b'), IArray('a', 'b'))
      .assert(_ == Diff(Par(0, 0, 'a'), Par(1, 1, 'b')))

      test(m"Insertion to empty list"):
        diff(IArray[Char](), IArray('a'))
      .assert(_ == Diff(Ins(0, 'a')))

      test(m"Deletion to become empty list"):
        diff(t"a".chars, t"".chars)
      .assert(_ == Diff(Del(0, 'a')))

      test(m"Prefix to short list"):
        diff(t"BC".chars, t"ABC".chars)
      .assert(_ == Diff(Ins(0, 'A'), Par(0, 1, 'B'), Par(1, 2, 'C')))

      test(m"Suffix to short list"):
        diff(t"AB".chars, t"ABC".chars)
      .assert(_ == Diff(Par(0, 0, 'A'), Par(1, 1, 'B'), Ins(2, 'C')))

      test(m"Insertion in middle of short list"):
        diff(t"AC".chars, t"ABC".chars)
      .assert(_ == Diff(Par(0, 0, 'A'), Ins(1, 'B'), Par(1, 2, 'C')))

      test(m"Deletion from middle of short list"):
        diff(t"ABC".chars, t"AC".chars)
      .assert(_ == Diff(Par(0, 0, 'A'), Del(1, 'B'), Par(2, 1, 'C')))

      test(m"Deletion from start of short list"):
        diff(t"ABC".chars, t"BC".chars).edits.to(List)
      .assert(_ == List(Del(0, 'A'), Par(1, 0, 'B'), Par(2, 1, 'C')))

      test(m"Deletion from end of short list"):
        diff(t"ABC".chars, t"AB".chars)
      .assert(_ == Diff(Par(0, 0, 'A'), Par(1, 1, 'B'), Del(2, 'C')))

      test(m"Multiple inner keeps"):
        diff(t"BCD".chars, t"ABC".chars)
      .assert(_ == Diff(Ins(0, 'A'), Par(0, 1, 'B'), Par(1, 2, 'C'), Del(2, 'D')))

      test(m"Example from blog"):
        diff(t"ABCABBA".chars, t"CBABAC".chars).edits.to(List)
      .assert(_ == List(Del(0, 'A'), Del(1, 'B'), Par(2, 0, 'C'), Ins(1, 'B'), Par(3, 2, 'A'),
          Par(4, 3, 'B'), Del(5, 'B'), Par(6, 4, 'A'), Ins(5, 'C')))

      test(m"Reversed example from blog"):
        diff(t"CBABAC".chars, t"ABCABBA".chars).edits.to(List)
      .assert(_ == List(Del(0, 'C'), Ins(0, 'A'), Par(1, 1, 'B'), Ins(2, 'C'), Par(2, 3, 'A'),
          Par(3, 4, 'B'), Ins(5, 'B'), Par(4, 6, 'A'), Del(5, 'C')))

      test(m"Item swap"):
        diff(t"AB".chars, t"BA".chars)
      .assert(_ == Diff(Del(0, 'A'), Par(1, 0, 'B'), Ins(1, 'A')))

      test(m"Item change"):
        diff(t"A".chars, t"C".chars)
      .assert(_ == Diff(Del(0, 'A'), Ins(0, 'C')))

      test(m"Item change between values"):
        diff(t"NAN".chars, t"NCN".chars)
      .assert(_ == Diff(Par(0, 0, 'N'), Del(1, 'A'), Ins(1, 'C'), Par(2, 2, 'N')))

      test(m"Item swap between values"):
        diff(t"NABN".chars, t"NBAN".chars)
      .assert(_ == Diff(Par(0, 0, 'N'), Del(1, 'A'), Par(2, 1, 'B'), Ins(2, 'A'), Par(3, 3, 'N')))

      test(m"Item swap interspersed with values"):
        diff(t"AZB".chars, t"BZA".chars)
      .assert(_ == Diff(Del(0, 'A'), Del(1, 'Z'), Par(2, 0, 'B'), Ins(1, 'Z'), Ins(2, 'A')))

      test(m"real-world example 1"):
        diff(IArray('a', 'b', 'c'), IArray('A', 'b', 'C')).edits.to(List)
      .assert(_ == Diff(Del(0, 'a'), Ins(0, 'A'), Par(1, 1, 'b'), Del(2, 'c'), Ins(2, 'C')).edits.to(List))

      test(m"real-world example 2"):
        diff(IArray(t"A", t"B"), IArray(t"B", t"C", t"D"))
      .assert(_ == Diff(Del(0, t"A"), Par(1, 0, t"B"), Ins(1, t"C"), Ins(2, t"D")))

    val start = Vector(t"foo", t"bar", t"baz")
    val end = Vector(t"foo", t"quux", t"bop", t"baz")

    suite(m"Diff parsing tests"):
      val diffStream = Stream(t"2c2,3", t"< bar", t"---", t"> quux", t"> bop")
      val reverseStream = Stream(t"2,3c2", t"< quux", t"< bop", t"---", t"> bar")

      test(m"Parse a simple diff file"):
        diffStream.read[Diff[Text]]
      .assert(_ == Diff(Par(0, 0), Del(1, t"bar"), Ins(1, t"quux"), Ins(2, t"bop")))

      test(m"Apply parsed diff to source to get result"):
        diffStream.read[Diff[Text]].patch(start)
      .assert(_ == end)

      test(m"Parse reverse diff file"):
        reverseStream.read[Diff[Text]]
      .assert(_ == Diff(Par(0, 0), Del(1, t"quux"), Del(2, t"bop"), Ins(1, t"bar")))

      test(m"Apply parsed reverse diff to get source"):
        reverseStream.read[Diff[Text]].patch(end)
      .assert(_ == start)

    suite(m"Diff serialization tests"):
      val changes = diff(start, end)
      val reverseChanges = diff(end, start)

      test(m"Serialize a trivial diff"):
        diff(Vector(), Vector(t"a")).serialize.to(List)
      .assert(_ == List(t"0a1", t"> a"))

      test(m"Serialize a trivial deletion diff"):
        diff(Vector(t"a", t"b", t"c", t"d"), Vector(t"a", t"d")).serialize.to(List)
      .assert(_ == List(t"2,3d1", t"< b", t"< c"))

      test(m"Serialize another trivial diff"):
        diff(Vector(t"a"), Vector()).serialize.to(List)
      .assert(_ == List(t"1d0", t"< a"))

      test(m"Serialize a simple diff"):
        changes.serialize.to(List)
      .assert(_ == List(t"2c2,3", t"< bar", t"---", t"> quux", t"> bop"))

      test(m"Serialize the reverse diff"):
        reverseChanges.serialize.to(List)
      .assert(_ == List(t"2,3c2", t"< quux", t"< bop", t"---", t"> bar"))

      test(m"Experimental diff"):
        diff(Vector(t"one"), Vector(t"two")).serialize.to(List)
      .assert(_ == List(t"1c1", t"< one", t"---", t"> two"))

      test(m"Experimental diff 2"):
        diff(Vector(t"zero", t"one"), Vector(t"two")).serialize.to(List)
      .assert(_ == List(t"1,2c1", t"< zero", t"< one", t"---", t"> two"))

      test(m"Experimental diff 3"):
        diff(Vector(t"zero", t"one"), Vector(t"zero", t"two")).serialize.to(List)
      .assert(_ == List(t"2c2", t"< one", t"---", t"> two"))

    val italian = Vector(t"zero", t"uno", t"due", t"tre", t"quattro", t"cinque", t"sei", t"sette")
    val spanish = Vector(t"cero", t"uno", t"dos", t"tres", t"cuatro", t"cinco", t"seis", t"siete")

    suite(m"Rdiff tests"):
      val italianToSpanish = test(m"Do a normal diff on Italian/Spanish numbers"):
        diff(italian, spanish)
      .check(_ == Diff(Del(0, t"zero"), Ins(0, t"cero"), Par(1,1, t"uno"), Del(2, t"due"),
          Del(3, t"tre"), Del(4, t"quattro"), Del(5, t"cinque"), Del(6, t"sei"), Del(7, t"sette"),
          Ins(2, t"dos"), Ins(3, t"tres"), Ins(4, t"cuatro"), Ins(5, t"cinco"), Ins(6, t"seis"),
          Ins(7, t"siete")))

      test(m"Align on Levenshtein distance < 4"):
        italianToSpanish.rdiff(_.proximity(_) < 4)
      .assert(_ == RDiff(Sub(0, 0, t"zero", t"cero"), Par(1, 1, t"uno"), Sub(2, 2, t"due", t"dos"),
          Sub(3, 3, t"tre", t"tres"), Sub(4, 4, t"quattro", t"cuatro"),
          Sub(5, 5, t"cinque", t"cinco"), Sub(6, 6, t"sei", t"seis"),
          Sub(7, 7, t"sette", t"siete")))

      test(m"Align on Levenshtein distance < 3"):
        italianToSpanish.rdiff(_.proximity(_) < 3)
      .assert(_ == RDiff(Sub(0, 0, t"zero", t"cero"), Par(1, 1, t"uno"), Sub(2, 2, t"due", t"dos"),
          Sub(3, 3, t"tre", t"tres"), Sub(4, 4, t"quattro", t"cuatro"), Del(5, t"cinque"),
          Ins(5, t"cinco"), Sub(6, 6, t"sei", t"seis"), Sub(7, 7, t"sette", t"siete")))

      test(m"Align on Levenshtein distance < 2"):
        italianToSpanish.rdiff(_.proximity(_) < 2)
      .assert(_ == RDiff(Sub(0, 0, t"zero", t"cero"), Par(1, 1, t"uno"), Del(2, t"due"),
          Ins(2, t"dos"), Sub(3, 3, t"tre", t"tres"), Del(4, t"quattro"), Del(5, t"cinque"),
          Ins(4, t"cuatro"), Ins(5, t"cinco"), Sub(6, 6, t"sei", t"seis"),
          Del(7, t"sette"), Ins(7, t"siete")))

    suite(m"Evolution tests"):
      test(m"Sample words"):
        val evolution =
          evolve(List(t"slain", t"stain", t"strange", t"star", t"rain", t"train").map(_.chars.to(List)))
        List(Prim, Sec, Ter, Quat, Quin, Sen).map(evolution(_).mkString)
      . assert(_ == List("slain", "stain", "strange", "star", "rain", "train"))

      test(m"dog/cat"):
        val evolution = evolve(List(t"dog", t"cat", t"dog").map(_.chars.to(List)))
        List(Prim, Sec, Ter).map(evolution(_).mkString)
      . assert(_ == List("dog", "cat", "dog"))

      test(m"dog/cat 2"):
        val evolution = evolve(List(t"dog", t"cat", t"dog", t"dog2").map(_.chars.to(List)))
        List(Prim, Sec, Ter, Quat).map(evolution(_).mkString)
      . assert(_ == List("dog", "cat", "dog", "dog2"))

      test(m"dog/cat 3"):
        val evolution = evolve(List(t"dog", t"cat", t"dog", t"do").map(_.chars.to(List)))
        List(Prim, Sec, Ter, Quat).map(evolution(_).mkString)
      . assert(_ == List("dog", "cat", "dog", "do"))

      test(m"Dogs and cats"):
        val evolution = evolve(List(t"dog", t"dog and cat", t"cat", t"cat and dog").map(_.chars.to(List)))
        List(Prim, Sec, Ter, Quat).map(evolution(_).mkString)
      . assert(_ == List("dog", "dog and cat", "cat", "cat and dog"))

      test(m"Jack and Jill"):
        val evolution = evolve(List(t"Jack and Jill", t"Jack with Jill", t"Jack und Jill").map(_.chars.to(List)))
        List(Prim, Sec, Ter).map(evolution(_).mkString)
      . assert(_ == List("Jack and Jill", "Jack with Jill", "Jack und Jill"))

    // suite(m"Casual diff tests"):
    //   test(m"Parse a simple casual diff"):
    //     import unsafeExceptions.canThrowAny
    //     CasualDiff.parse(t"- remove\n+ insert".cut(t"\n").to(Stream))
    //   .assert(_ == CasualDiff(List(Replace(Nil, List(t"remove"), List(t"insert")))))

    //   test(m"Parse a slightly longer casual diff"):
    //     import unsafeExceptions.canThrowAny
    //     CasualDiff.parse(t"- remove\n+ insert\n- removal".cut(t"\n").to(Stream))
    //   .assert(_ == CasualDiff(List(Replace(Nil, List(t"remove"), List(t"insert")), Replace(Nil, List(t"removal"), Nil))))

    //   test(m"Parse a longer casual diff"):
    //     import unsafeExceptions.canThrowAny
    //     CasualDiff.parse(t"- remove 1\n- remove 2\n+ insert 1\n+ insert 2\n- removal".cut(t"\n").to(Stream))
    //   .assert(_ == CasualDiff(List(Replace(Nil, List(t"remove 1", t"remove 2"), List(t"insert 1", t"insert 2")), Replace(Nil, List(t"removal"), Nil))))

    //   test(m"Fail to parse a problematic casual diff"):
    //     import unsafeExceptions.canThrowAny
    //     capture[CasualDiffError](CasualDiff.parse(t"- remove 1\n- remove 2\n insert 1\n+ insert 2\n- removal".cut(t"\n").to(Stream)))
    //   .assert(_ == CasualDiffError(CasualDiffError.Reason.BadLineStart(t" insert 1"), 3))

    // suite(m"Invariance tests"):
    //   val values = List(t"alpha", t"beta", t"gamma")

    //   def permutations(n: Int): List[List[Text]] =
    //     if n == 0 then List(Nil) else
    //       val last = permutations(n - 1)
    //       for value <- values; perm <- last yield value :: perm

    //   def allPermutations(n: Int): List[List[Text]] =
    //     if n == 0 then Nil else permutations(n) ++ allPermutations(n - 1)

    //   allPermutations(3).map(_.to(Vector)).each: perm1 =>
    //     allPermutations(3).map(_.to(Vector)).each: perm2 =>
    //       import unsafeExceptions.canThrowAny
    //       val d = diff(perm1, perm2).casual
    //       test(m"Check differences"):
    //         d.patch(perm1).to(Vector)
    //       .assert(_ == perm2)
