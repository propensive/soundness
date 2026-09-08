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
package dissonance

import soundness.*


import proximities.levenshteinProximity
import caseSensitivity.caseSensitive
import strategies.throwUnsafely
import errorDiagnostics.emptyDiagnostics

import Redraft.Directive as D

object Tests extends Suite(m"Dissonance tests"):
  def run(): Unit =
    suite(m"Diff tests"):
      test(m"Empty lists"):
        diff(Sequence[Char](), Sequence[Char]())
      . assert(_ == Diff())

      test(m"One element, equal"):
        diff(Sequence('a'), Sequence('a'))
      . assert(_ == Diff(Par(0, 0, 'a')))

      test(m"Straight swap"):
        diff(Sequence('a'), Sequence('A'))
      . assert(_ == Diff(Del(0, 'a'), Ins(0, 'A')))

      test(m"Two elements, equal"):
        diff(Sequence('a', 'b'), Sequence('a', 'b'))
      . assert(_ == Diff(Par(0, 0, 'a'), Par(1, 1, 'b')))

      test(m"Insertion to empty list"):
        diff(Sequence[Char](), Sequence('a'))
      . assert(_ == Diff(Ins(0, 'a')))

      test(m"Deletion to become empty list"):
        diff(Sequence.from("a".chars.readable), Sequence.from("".chars.readable))
      . assert(_ == Diff(Del(0, 'a')))

      test(m"Prefix to short list"):
        diff(Sequence.from("BC".chars.readable), Sequence.from("ABC".chars.readable))
      . assert(_ == Diff(Ins(0, 'A'), Par(0, 1, 'B'), Par(1, 2, 'C')))

      test(m"Suffix to short list"):
        diff(Sequence.from("AB".chars.readable), Sequence.from("ABC".chars.readable))
      . assert(_ == Diff(Par(0, 0, 'A'), Par(1, 1, 'B'), Ins(2, 'C')))

      test(m"Insertion in middle of short list"):
        diff(Sequence.from("AC".chars.readable), Sequence.from("ABC".chars.readable))
      . assert(_ == Diff(Par(0, 0, 'A'), Ins(1, 'B'), Par(1, 2, 'C')))

      test(m"Deletion from middle of short list"):
        diff(Sequence.from("ABC".chars.readable), Sequence.from("AC".chars.readable))
      . assert(_ == Diff(Par(0, 0, 'A'), Del(1, 'B'), Par(2, 1, 'C')))

      test(m"Deletion from start of short list"):
        diff(Sequence.from("ABC".chars.readable), Sequence.from("BC".chars.readable)).edits.to(List)
      . assert(_ == List(Del(0, 'A'), Par(1, 0, 'B'), Par(2, 1, 'C')))

      test(m"Deletion from end of short list"):
        diff(Sequence.from("ABC".chars.readable), Sequence.from("AB".chars.readable))
      . assert(_ == Diff(Par(0, 0, 'A'), Par(1, 1, 'B'), Del(2, 'C')))

      test(m"Multiple inner keeps"):
        diff(Sequence.from("BCD".chars.readable), Sequence.from("ABC".chars.readable))
      . assert(_ == Diff(Ins(0, 'A'), Par(0, 1, 'B'), Par(1, 2, 'C'), Del(2, 'D')))

      test(m"Example from blog"):
        diff(Sequence.from("ABCABBA".chars.readable), Sequence.from("CBABAC".chars.readable)).edits.to(List)
      . assert(_ == List(Del(0, 'A'), Del(1, 'B'), Par(2, 0, 'C'), Ins(1, 'B'), Par(3, 2, 'A'),
          Par(4, 3, 'B'), Del(5, 'B'), Par(6, 4, 'A'), Ins(5, 'C')))

      test(m"Reversed example from blog"):
        diff(Sequence.from("CBABAC".chars.readable), Sequence.from("ABCABBA".chars.readable)).edits.to(List)
      . assert(_ == List(Del(0, 'C'), Ins(0, 'A'), Par(1, 1, 'B'), Ins(2, 'C'), Par(2, 3, 'A'),
          Par(3, 4, 'B'), Ins(5, 'B'), Par(4, 6, 'A'), Del(5, 'C')))

      test(m"Item swap"):
        diff(Sequence.from("AB".chars.readable), Sequence.from("BA".chars.readable))
      . assert(_ == Diff(Del(0, 'A'), Par(1, 0, 'B'), Ins(1, 'A')))

      test(m"Item change"):
        diff(Sequence.from("A".chars.readable), Sequence.from("C".chars.readable))
      . assert(_ == Diff(Del(0, 'A'), Ins(0, 'C')))

      test(m"Item change between values"):
        diff(Sequence.from("NAN".chars.readable), Sequence.from("NCN".chars.readable))
      . assert(_ == Diff(Par(0, 0, 'N'), Del(1, 'A'), Ins(1, 'C'), Par(2, 2, 'N')))

      test(m"Item swap between values"):
        diff(Sequence.from("NABN".chars.readable), Sequence.from("NBAN".chars.readable))
      . assert(_ == Diff(Par(0, 0, 'N'), Del(1, 'A'), Par(2, 1, 'B'), Ins(2, 'A'), Par(3, 3, 'N')))

      test(m"Item swap interspersed with values"):
        diff(Sequence.from("AZB".chars.readable), Sequence.from("BZA".chars.readable))
      . assert(_ == Diff(Del(0, 'A'), Del(1, 'Z'), Par(2, 0, 'B'), Ins(1, 'Z'), Ins(2, 'A')))

      test(m"real-world example 1"):
        diff(Sequence('a', 'b', 'c'), Sequence('A', 'b', 'C')).edits.to(List)
      . assert(_ == Diff(Del(0, 'a'), Ins(0, 'A'), Par(1, 1, 'b'), Del(2, 'c'), Ins(2, 'C')).edits.to(List))

      test(m"real-world example 2"):
        diff(Sequence(t"A", t"B"), Sequence(t"B", t"C", t"D"))
      . assert(_ == Diff(Del(0, "A"), Par(1, 0, "B"), Ins(1, "C"), Ins(2, "D")))

    val start = proscenium.Sequence(t"foo", t"bar", t"baz")
    val end = proscenium.Sequence(t"foo", t"quux", t"bop", t"baz")

    suite(m"Diff parsing tests"):
      val diffStream = proscenium.Chain("2c2,3", "< bar", "---", "> quux", "> bop")
      val reverseStream = proscenium.Chain("2,3c2", "< quux", "< bop", "---", "> bar")

      test(m"Parse a simple diff file"):
        diffStream.read[Diff[Text]]
      . assert(_ == Diff(Par(0, 0), Del(1, "bar"), Ins(1, "quux"), Ins(2, "bop")))

      test(m"Apply parsed diff to source to get result"):
        diffStream.read[Diff[Text]].patch(List(t"foo", t"bar", t"baz"))
      . assert(_.stdlib.toList == end.stdlib.toList)

      test(m"Parse reverse diff file"):
        reverseStream.read[Diff[Text]]
      . assert(_ == Diff(Par(0, 0), Del(1, "quux"), Del(2, "bop"), Ins(1, "bar")))

      test(m"Apply parsed reverse diff to get source"):
        reverseStream.read[Diff[Text]].patch(List(t"foo", t"quux", t"bop", t"baz"))
      . assert(_.stdlib.toList == start.stdlib.toList)

    suite(m"Diff serialization tests"):
      val changes = diff(start, end)
      val reverseChanges = diff(end, start)

      test(m"Serialize a trivial diff"):
        diff(Sequence(), Sequence(t"a")).serialize.stdlib.to(List)
      . assert(_ == List(t"0a1", t"> a"))

      test(m"Serialize a trivial deletion diff"):
        diff(Sequence(t"a", t"b", t"c", t"d"), Sequence(t"a", t"d")).serialize.stdlib.to(List)
      . assert(_ == List(t"2,3d1", t"< b", t"< c"))

      test(m"Serialize another trivial diff"):
        diff(Sequence(t"a"), Sequence()).serialize.stdlib.to(List)
      . assert(_ == List(t"1d0", t"< a"))

      test(m"Serialize a simple diff"):
        changes.serialize.stdlib.to(List)
      . assert(_ == List(t"2c2,3", t"< bar", t"---", t"> quux", t"> bop"))

      test(m"Serialize the reverse diff"):
        reverseChanges.serialize.stdlib.to(List)
      . assert(_ == List(t"2,3c2", t"< quux", t"< bop", t"---", t"> bar"))

      test(m"Experimental diff"):
        diff(Sequence(t"one"), Sequence(t"two")).serialize.stdlib.to(List)
      . assert(_ == List(t"1c1", t"< one", t"---", t"> two"))

      test(m"Experimental diff 2"):
        diff(Sequence(t"zero", t"one"), Sequence(t"two")).serialize.stdlib.to(List)
      . assert(_ == List(t"1,2c1", t"< zero", t"< one", t"---", t"> two"))

      test(m"Experimental diff 3"):
        diff(Sequence(t"zero", t"one"), Sequence(t"zero", t"two")).serialize.stdlib.to(List)
      . assert(_ == List(t"2c2", t"< one", t"---", t"> two"))

    val italian = Sequence(t"zero", t"uno", t"due", t"tre", t"quattro", t"cinque", t"sei", t"sette")
    val spanish = Sequence(t"cero", t"uno", t"dos", t"tres", t"cuatro", t"cinco", t"seis", t"siete")

    suite(m"Rdiff tests"):
      val italianToSpanish = test(m"Do a normal diff on Italian/Spanish numbers"):
        diff(italian, spanish)
      .check(_ == Diff(Del(0, "zero"), Ins(0, "cero"), Par(1,1, "uno"), Del(2, "due"),
          Del(3, "tre"), Del(4, "quattro"), Del(5, "cinque"), Del(6, "sei"), Del(7, "sette"),
          Ins(2, "dos"), Ins(3, "tres"), Ins(4, "cuatro"), Ins(5, "cinco"), Ins(6, "seis"),
          Ins(7, "siete")))

      test(m"Align on Levenshtein distance < 4"):
        italianToSpanish.rdiff(_.proximity(_) < 4)
      . assert(_ == RDiff(Sub(0, 0, "zero", "cero"), Par(1, 1, "uno"), Sub(2, 2, "due", "dos"),
          Sub(3, 3, "tre", "tres"), Sub(4, 4, "quattro", "cuatro"),
          Sub(5, 5, "cinque", "cinco"), Sub(6, 6, "sei", "seis"),
          Sub(7, 7, "sette", "siete")))

      test(m"Align on Levenshtein distance < 3"):
        italianToSpanish.rdiff(_.proximity(_) < 3)
      . assert(_ == RDiff(Sub(0, 0, "zero", "cero"), Par(1, 1, "uno"), Sub(2, 2, "due", "dos"),
          Sub(3, 3, "tre", "tres"), Sub(4, 4, "quattro", "cuatro"), Del(5, "cinque"),
          Ins(5, "cinco"), Sub(6, 6, "sei", "seis"), Sub(7, 7, "sette", "siete")))

      test(m"Align on Levenshtein distance < 2"):
        italianToSpanish.rdiff(_.proximity(_) < 2)
      . assert(_ == RDiff(Sub(0, 0, "zero", "cero"), Par(1, 1, "uno"), Del(2, "due"),
          Ins(2, "dos"), Sub(3, 3, "tre", "tres"), Del(4, "quattro"), Del(5, "cinque"),
          Ins(4, "cuatro"), Ins(5, "cinco"), Sub(6, 6, "sei", "seis"),
          Del(7, "sette"), Ins(7, "siete")))

    suite(m"Evolution tests"):
      test(m"Sample words"):
        val evolution =
          evolve((scala.collection.immutable.List("slain", "stain", "strange", "star", "rain", "train").map { w => w.chars.to[List] }).to(List))
        List(Prim, Sec, Ter, Quat, Quin, Sen).map(evolution(_).stdlib.mkString)
      . assert(_ == List("slain", "stain", "strange", "star", "rain", "train"))

      test(m"dog/cat"):
        val evolution = evolve((scala.collection.immutable.List("dog", "cat", "dog").map { w => w.chars.to[List] }).to(List))
        List(Prim, Sec, Ter).map(evolution(_).stdlib.mkString)
      . assert(_ == List("dog", "cat", "dog"))

      test(m"dog/cat 2"):
        val evolution = evolve((scala.collection.immutable.List("dog", "cat", "dog", "dog2").map { w => w.chars.to[List] }).to(List))
        List(Prim, Sec, Ter, Quat).map(evolution(_).stdlib.mkString)
      . assert(_ == List("dog", "cat", "dog", "dog2"))

      test(m"dog/cat 3"):
        val evolution = evolve((scala.collection.immutable.List("dog", "cat", "dog", "do").map { w => w.chars.to[List] }).to(List))
        List(Prim, Sec, Ter, Quat).map(evolution(_).stdlib.mkString)
      . assert(_ == List("dog", "cat", "dog", "do"))

      test(m"Dogs and cats"):
        val evolution = evolve((scala.collection.immutable.List("dog", "dog and cat", "cat", "cat and dog").map { w => w.chars.to[List] }).to(List))
        List(Prim, Sec, Ter, Quat).map(evolution(_).stdlib.mkString)
      . assert(_ == List("dog", "dog and cat", "cat", "cat and dog"))

      test(m"Jack and Jill"):
        val evolution = evolve((scala.collection.immutable.List("Jack and Jill", "Jack with Jill", "Jack und Jill").map { w => w.chars.to[List] }).to(List))
        List(Prim, Sec, Ter).map(evolution(_).stdlib.mkString)
      . assert(_ == List("Jack and Jill", "Jack with Jill", "Jack und Jill"))

    val source = Sequence(t"line1", t"line2", t"line3")
    val dup = Sequence(t"a", t"b", t"a")
    val list = Sequence(t"- alpha", t"- beta", t"- gamma")

    suite(m"Redraft parsing tests"):
      val roundtrip =
        Chain("line1", "- line2", "+ new", "\\- escaped", "< forced", "> add")

      test(m"Parse a simple redraft"):
        Redraft.parse(Chain("line1", "- line2", "+ new line 2a", "line3"))
      . assert(_ == Redraft(D.Keep("line1"), D.Mark("line2", false), D.Mark("new line 2a", true),
          D.Keep("line3")))

      test(m"Parse forced and escaped directives"):
        Redraft.parse(Chain("< forced", "> add", "\\- escaped"))
      . assert(_ == Redraft(D.Cut("forced"), D.Add("add"), D.Keep("- escaped")))

      test(m"Serialize round-trips through parse"):
        Redraft.parse(roundtrip).serialize.stdlib.to(List)
      . assert(_ == roundtrip.to(List))

    suite(m"Redraft application tests"):
      test(m"Apply a simple redraft"):
        Redraft.parse(Chain("- line2", "+ new line 2a")).patch(source).pipe(prog => Sequence.from(prog.stdlib))
      . assert(_ == Sequence(t"line1", t"new line 2a", t"line3"))

      test(m"Omitted unchanged lines are skipped"):
        Redraft.parse(Chain("- line2")).patch(source).pipe(prog => Sequence.from(prog.stdlib))
      . assert(_ == Sequence(t"line1", t"line3"))

      test(m"Insert before the first line"):
        Redraft.parse(Chain("+ line0")).patch(source).pipe(prog => Sequence.from(prog.stdlib))
      . assert(_ == Sequence(t"line0", t"line1", t"line2", t"line3"))

      test(m"Forced insert with the alternate marker"):
        Redraft.parse(Chain("> line0")).patch(source).pipe(prog => Sequence.from(prog.stdlib))
      . assert(_ == Sequence(t"line0", t"line1", t"line2", t"line3"))

      test(m"Deletion anchored by context"):
        Redraft.parse(Chain("b", "- a")).patch(dup).pipe(prog => Sequence.from(prog.stdlib))
      . assert(_ == Sequence(t"a", t"b"))

      test(m"A verbatim marker line is kept as context"):
        Redraft.parse(Chain("- alpha", "< - beta", "- gamma")).patch(list).pipe(prog => Sequence.from(prog.stdlib))
      . assert(_ == Sequence(t"- alpha", t"- gamma"))

      test(m"Escaped context matches a literal marker line"):
        Redraft.parse(Chain("\\- alpha")).patch(list).pipe(prog => Sequence.from(prog.stdlib))
      . assert(_ == Sequence(t"- alpha", t"- beta", t"- gamma"))

      test(m"Delete a literal marker line with a doubled marker"):
        Redraft.parse(Chain("- alpha", "- - beta", "- gamma")).patch(list).pipe(prog => Sequence.from(prog.stdlib))
      . assert(_ == Sequence(t"- alpha", t"- gamma"))

    suite(m"Redraft ambiguity tests"):
      test(m"An under-anchored deletion is rejected"):
        capture[Redraft.Error](Redraft.parse(Chain("- a")).patch(dup))
      . assert(_.reason == Redraft.Error.Reason.Unanchored)

      test(m"verify reports the under-anchored line"):
        Redraft.parse(Chain("- a")).verify(dup)
      . assert(_ == List(Redraft.Anomaly(0, t"a", Redraft.Error.Reason.Unanchored)))

      test(m"A non-matching context line is rejected"):
        capture[Redraft.Error](Redraft.parse(Chain("absent", "- line2")).patch(source))
      . assert(_.reason == Redraft.Error.Reason.NoMatch)

    suite(m"Redraft rendering tests"):
      val target = Sequence(t"line1", t"new", t"line3")

      test(m"Render a minimal redraft, dropping all context"):
        diff(source, Sequence(t"line1", t"new line 2a", t"line3")).redraft().serialize.stdlib.to(List)
      . assert(_ == List(t"- line2", t"+ new line 2a"))

      test(m"Render minimal context to anchor an ambiguous deletion"):
        diff(dup, Sequence(t"a", t"b")).redraft().serialize.stdlib.to(List)
      . assert(_ == List(t"b", t"- a"))

      test(m"A rendered redraft reproduces the target when applied"):
        diff(source, target).redraft().patch(source).pipe(prog => Sequence.from(prog.stdlib))
      . assert(_ == target)

    // suite(m"Casual diff tests"):
    //   test(m"Parse a simple casual diff"):
    //     import unsafeExceptions.canThrowAny
    //     CasualDiff.parse(t"- remove\n+ insert".cut(t"\n").to[Chain])
    //   .assert(_ == CasualDiff(List(Replace(Nil, List(t"remove"), List(t"insert")))))

    //   test(m"Parse a slightly longer casual diff"):
    //     import unsafeExceptions.canThrowAny
    //     CasualDiff.parse(t"- remove\n+ insert\n- removal".cut(t"\n").to[Chain])
    //   .assert(_ == CasualDiff(List(Replace(Nil, List(t"remove"), List(t"insert")), Replace(Nil, List(t"removal"), Nil))))

    //   test(m"Parse a longer casual diff"):
    //     import unsafeExceptions.canThrowAny
    //     CasualDiff.parse(t"- remove 1\n- remove 2\n+ insert 1\n+ insert 2\n- removal".cut(t"\n").to[Chain])
    //   .assert(_ == CasualDiff(List(Replace(Nil, List(t"remove 1", t"remove 2"), List(t"insert 1", t"insert 2")), Replace(Nil, List(t"removal"), Nil))))

    //   test(m"Fail to parse a problematic casual diff"):
    //     import unsafeExceptions.canThrowAny
    //     capture[CasualDiffError](CasualDiff.parse(t"- remove 1\n- remove 2\n insert 1\n+ insert 2\n- removal".cut(t"\n").to[Chain]))
    //   .assert(_ == CasualDiffError(CasualDiffError.Reason.BadLineStart(t" insert 1"), 3))

    // suite(m"Invariance tests"):
    //   val values = List(t"alpha", t"beta", t"gamma")

    //   def permutations(n: Int): List[List[Text]] =
    //     if n == 0 then List(Nil) else
    //       val last = permutations(n - 1)
    //       for value <- values; perm <- last yield value :: perm

    //   def allPermutations(n: Int): List[List[Text]] =
    //     if n == 0 then Nil else permutations(n) ++ allPermutations(n - 1)

    //   allPermutations(3).map(_.pipe(prog => Sequence.from(prog.stdlib))).each: perm1 =>
    //     allPermutations(3).map(_.pipe(prog => Sequence.from(prog.stdlib))).each: perm2 =>
    //       import unsafeExceptions.canThrowAny
    //       val d = diff(perm1, perm2).casual
    //       test(m"Check differences"):
    //         d.patch(perm1).pipe(prog => Sequence.from(prog.stdlib))
    //       . assert(_ == perm2)
