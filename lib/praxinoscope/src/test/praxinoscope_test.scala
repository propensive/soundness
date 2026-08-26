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
package praxinoscope

import soundness.*

import strategies.throwUnsafely
import errorDiagnostics.stackTracesDiagnostics

object Tests extends Suite(m"Praxinoscope tests"):
  def run(): Unit =
    import Motif.Node, Node.*, Node.Anchor.*
    import Motif.Error.Reason

    suite(m"Parsing"):
      test(m"Parse a single literal")(Motif.parse(t"a").node)
      . assert(_ == Literal('a'.toInt))

      test(m"Parse a sequence of literals")(Motif.parse(t"abc").node)
      . assert(_ == Sequence(List(Literal('a'.toInt), Literal('b'.toInt), Literal('c'.toInt))))

      test(m"Parse the empty pattern")(Motif.parse(t"").node)
      . assert(_ == Empty)

      test(m"Parse a literal containing a space")(Motif.parse(t"a b").node)
      . assert(_ == Sequence(List(Literal('a'.toInt), Literal(' '.toInt), Literal('b'.toInt))))

      test(m"Parse an astral-plane literal")(Motif.parse(t"🦆").node)
      . assert(_ == Literal(0x1f986))

      test(m"Parse an alternation")(Motif.parse(t"a|b").node)
      . assert(_ == Alternation(List(Literal('a'.toInt), Literal('b'.toInt))))

      test(m"Parse a three-way alternation")(Motif.parse(t"a|b|c").node)
      . assert(_ == Alternation(List(Literal('a'.toInt), Literal('b'.toInt), Literal('c'.toInt))))

      test(m"Parse an alternation with an empty option")(Motif.parse(t"a|").node)
      . assert(_ == Alternation(List(Literal('a'.toInt), Empty)))

      test(m"Parse a dot")(Motif.parse(t".").node)
      . assert(_ == Klass(Ranges.any))

      test(m"Parse a capturing group")(Motif.parse(t"(a)").node)
      . assert(_ == Group(Literal('a'.toInt), 1))

      test(m"Parse a non-capturing group")(Motif.parse(t"(?:a)").node)
      . assert(_ == Group(Literal('a'.toInt), Unset))

      test(m"Number capturing groups in opening order")(Motif.parse(t"((a)(b))").captures)
      . assert(_ == 3)

      test(m"Parse nested groups")(Motif.parse(t"((a))").node)
      . assert(_ == Group(Group(Literal('a'.toInt), 2), 1))

      test(m"Parse a group in context")(Motif.parse(t"a(b|c)d").node)
      . assert:
          _ == Sequence
                 ( List
                     ( Literal('a'.toInt),
                       Group(Alternation(List(Literal('b'.toInt), Literal('c'.toInt))), 1),
                       Literal('d'.toInt) ) )

      test(m"Parse a starred literal")(Motif.parse(t"a*").node)
      . assert(_ == Repeat(Literal('a'.toInt), 0, Unset, false))

      test(m"Parse a reluctant star")(Motif.parse(t"a*?").node)
      . assert(_ == Repeat(Literal('a'.toInt), 0, Unset, true))

      test(m"Parse a plus")(Motif.parse(t"a+").node)
      . assert(_ == Repeat(Literal('a'.toInt), 1, Unset, false))

      test(m"Parse a question mark")(Motif.parse(t"a?").node)
      . assert(_ == Repeat(Literal('a'.toInt), 0, 1, false))

      test(m"Parse an exact repetition")(Motif.parse(t"a{3}").node)
      . assert(_ == Repeat(Literal('a'.toInt), 3, 3, false))

      test(m"Parse an at-least repetition")(Motif.parse(t"a{3,}").node)
      . assert(_ == Repeat(Literal('a'.toInt), 3, Unset, false))

      test(m"Parse a bounded repetition")(Motif.parse(t"a{3,5}").node)
      . assert(_ == Repeat(Literal('a'.toInt), 3, 5, false))

      test(m"Parse a repeated group")(Motif.parse(t"(ab)+").node)
      . assert:
          _ == Repeat(Group(Sequence(List(Literal('a'.toInt), Literal('b'.toInt))), 1), 1, Unset,
                 false)

      test(m"A brace without digits is a literal")(Motif.parse(t"a{x}").node)
      . assert:
          _ == Sequence
                 ( List
                     ( Literal('a'.toInt),
                       Literal('{'.toInt),
                       Literal('x'.toInt),
                       Literal('}'.toInt) ) )

      test(m"Parse a character class")(Motif.parse(t"[a-c]").node)
      . assert(_ == Klass(Ranges('a'.toInt, 'c'.toInt)))

      test(m"Parse a multi-part character class")(Motif.parse(t"[a-cx]").node)
      . assert(_ == Klass(Ranges('a'.toInt, 'c'.toInt).union(Ranges.point('x'.toInt))))

      test(m"Coalesce adjacent class ranges")(Motif.parse(t"[a-cd-f]").node)
      . assert(_ == Klass(Ranges('a'.toInt, 'f'.toInt)))

      test(m"Parse a negated character class")(Motif.parse(t"[^a]").node)
      . assert(_ == Klass(Ranges.point('a'.toInt).negate()))

      test(m"Parse a class with a leading hyphen")(Motif.parse(t"[-a]").node)
      . assert(_ == Klass(Ranges.point('-'.toInt).union(Ranges.point('a'.toInt))))

      test(m"Parse a class with a trailing hyphen")(Motif.parse(t"[a-]").node)
      . assert(_ == Klass(Ranges.point('-'.toInt).union(Ranges.point('a'.toInt))))

      test(m"Parse a perl class in a character class")(Motif.parse(t"[\\d.]").node)
      . assert(_ == Klass(Ranges.digit.union(Ranges.point('.'.toInt))))

      test(m"Parse a digit class")(Motif.parse(t"\\d").node)
      . assert(_ == Klass(Ranges.digit))

      test(m"Parse a negated digit class")(Motif.parse(t"\\D").node)
      . assert(_ == Klass(Ranges.digit.negate()))

      test(m"Parse a word class")(Motif.parse(t"\\w").node)
      . assert(_ == Klass(Ranges.word))

      test(m"Parse a space class")(Motif.parse(t"\\s").node)
      . assert(_ == Klass(Ranges.space))

      test(m"Parse anchors")(Motif.parse(t"^a$$").node)
      . assert(_ == Sequence(List(Boundary(Start), Literal('a'.toInt), Boundary(End))))

      test(m"Parse word boundaries")(Motif.parse(t"\\ba\\B").node)
      . assert:
          _ == Sequence(List(Boundary(WordBoundary), Literal('a'.toInt),
                 Boundary(NonWordBoundary)))

      test(m"Parse absolute anchors")(Motif.parse(t"\\Aa\\z").node)
      . assert(_ == Sequence(List(Boundary(Start), Literal('a'.toInt), Boundary(End))))

      test(m"Parse an escaped dot")(Motif.parse(t"\\.").node)
      . assert(_ == Literal('.'.toInt))

      test(m"Parse an escaped backslash")(Motif.parse(t"\\\\").node)
      . assert(_ == Literal('\\'.toInt))

      test(m"Parse a newline escape")(Motif.parse(t"\\n").node)
      . assert(_ == Literal('\n'.toInt))

      test(m"Parse a two-digit hex escape")(Motif.parse(t"\\x41").node)
      . assert(_ == Literal('A'.toInt))

      test(m"Parse a braced hex escape")(Motif.parse(t"\\x{1f986}").node)
      . assert(_ == Literal(0x1f986))

    suite(m"Range algebra"):
      test(m"Union of overlapping ranges coalesces")
        (Ranges(0, 10).union(Ranges(5, 15)).spans)
      . assert(_ == List(0, 15))

      test(m"Union of adjacent ranges coalesces")(Ranges(0, 4).union(Ranges(5, 9)).spans)
      . assert(_ == List(0, 9))

      test(m"Union of disjoint ranges keeps both")(Ranges(0, 1).union(Ranges(5, 9)).spans)
      . assert(_ == List(0, 1, 5, 9))

      test(m"Negation of a range")(Ranges(5, 9).negate().spans)
      . assert(_ == List(0, 4, 10, Ranges.maxSymbol))

      test(m"Double negation is identity")(Ranges(5, 9).negate().negate().spans)
      . assert(_ == List(5, 9))

      test(m"Negation of everything is empty")(Ranges(0, Ranges.maxSymbol).negate().spans)
      . assert(_ == List())

      test(m"Intersection of overlapping ranges")(Ranges(0, 10).intersect(Ranges(5, 15)).spans)
      . assert(_ == List(5, 10))

      test(m"Intersection of disjoint ranges is empty")(Ranges(0, 4).intersect(Ranges(6, 9)).vacant)
      . assert(_ == true)

      test(m"Containment inside a range")(Ranges(5, 9).contains(7))
      . assert(_ == true)

      test(m"Containment outside a range")(Ranges(5, 9).contains(10))
      . assert(_ == false)

    suite(m"Parsing failures"):
      test(m"Reject an unclosed group")(capture[Motif.Error](Motif.parse(t"(ab")))
      . assert(_.reason == Reason.UnclosedGroup)

      test(m"Reject a stray closing parenthesis")(capture[Motif.Error](Motif.parse(t"ab)c")))
      . assert(_ == Motif.Error(2, Reason.NotInGroup))

      test(m"Reject a dangling quantifier")(capture[Motif.Error](Motif.parse(t"*a")))
      . assert(_ == Motif.Error(0, Reason.UnexpectedChar))

      test(m"Reject a double quantifier")(capture[Motif.Error](Motif.parse(t"a**")))
      . assert(_ == Motif.Error(2, Reason.UnexpectedChar))

      test(m"Reject an inverted repetition")(capture[Motif.Error](Motif.parse(t"a{3,1}")))
      . assert(_.reason == Reason.BadRepetition)

      test(m"Reject an unclosed repetition")(capture[Motif.Error](Motif.parse(t"a{3")))
      . assert(_.reason == Reason.IncompleteRepetition)

      test(m"Reject an oversized repetition")(capture[Motif.Error](Motif.parse(t"a{1001}")))
      . assert(_.reason == Reason.RepetitionTooLarge)

      test(m"Reject an empty character class")(capture[Motif.Error](Motif.parse(t"a[]b")))
      . assert(_ == Motif.Error(2, Reason.EmptyCharClass))

      test(m"Reject an unclosed character class")(capture[Motif.Error](Motif.parse(t"[ab")))
      . assert(_.reason == Reason.UnclosedClass)

      test(m"Reject an inverted class range")(capture[Motif.Error](Motif.parse(t"[z-a]")))
      . assert(_.reason == Reason.InvertedRange)

      test(m"Reject a trailing escape")(capture[Motif.Error](Motif.parse(t"ab\\")))
      . assert(_.reason == Reason.UnclosedEscape)

      test(m"Reject an unknown letter escape")(capture[Motif.Error](Motif.parse(t"\\q")))
      . assert(_.reason == Reason.InvalidEscape)

      test(m"Reject a malformed hex escape")(capture[Motif.Error](Motif.parse(t"\\xg1")))
      . assert(_.reason == Reason.InvalidEscape)

      test(m"Reject an oversized braced hex escape"):
        capture[Motif.Error](Motif.parse(t"\\x{110000}"))
      . assert(_.reason == Reason.InvalidEscape)

      test(m"Reject a backreference")(capture[Motif.Error](Motif.parse(t"(a)\\1")))
      . assert(_.reason == Reason.Backreference)

      test(m"Reject a named backreference")(capture[Motif.Error](Motif.parse(t"(a)\\k<x>")))
      . assert(_.reason == Reason.Backreference)

      test(m"Reject lookahead")(capture[Motif.Error](Motif.parse(t"(?=a)")))
      . assert(_.reason == Reason.Lookaround)

      test(m"Reject negative lookahead")(capture[Motif.Error](Motif.parse(t"(?!a)")))
      . assert(_.reason == Reason.Lookaround)

      test(m"Reject lookbehind")(capture[Motif.Error](Motif.parse(t"(?<=a)")))
      . assert(_.reason == Reason.Lookaround)

      test(m"Reject a named group")(capture[Motif.Error](Motif.parse(t"(?<name>a)")))
      . assert(_.reason == Reason.UnsupportedGroup)

      test(m"Reject a P-style named group")(capture[Motif.Error](Motif.parse(t"(?P<name>a)")))
      . assert(_.reason == Reason.UnsupportedGroup)

      test(m"Reject an atomic group")(capture[Motif.Error](Motif.parse(t"(?>a)")))
      . assert(_.reason == Reason.AtomicGroup)

      test(m"Reject a possessive quantifier")(capture[Motif.Error](Motif.parse(t"a*+")))
      . assert(_.reason == Reason.PossessiveQuantifier)

      test(m"Reject an inline flag")(capture[Motif.Error](Motif.parse(t"(?i)a")))
      . assert(_.reason == Reason.Flag)

      test(m"Reject a unicode property escape")(capture[Motif.Error](Motif.parse(t"\\p{L}")))
      . assert(_.reason == Reason.InvalidEscape)

    suite(m"Matching"):
      def motif(pattern: Text): Motif = Motif.parse(pattern)

      test(m"Match a literal")(motif(t"abc").matches(t"abc")).assert(_ == true)
      test(m"Reject a wrong literal")(motif(t"abc").matches(t"abd")).assert(_ == false)
      test(m"Matching is whole-input")(motif(t"abc").matches(t"xabcx")).assert(_ == false)
      test(m"Match an alternation")(motif(t"cat|dog").matches(t"dog")).assert(_ == true)
      test(m"Match a star")(motif(t"ab*c").matches(t"abbbc")).assert(_ == true)
      test(m"Match a star zero times")(motif(t"ab*c").matches(t"ac")).assert(_ == true)
      test(m"A plus requires one")(motif(t"ab+c").matches(t"ac")).assert(_ == false)
      test(m"Match a bounded repetition")(motif(t"a{2,3}").matches(t"aaa")).assert(_ == true)
      test(m"Reject an excessive repetition")(motif(t"a{2,3}").matches(t"aaaa")).assert(_ == false)
      test(m"Reject a deficient repetition")(motif(t"a{2,3}").matches(t"a")).assert(_ == false)
      test(m"Match a character class")(motif(t"[a-c]+").matches(t"abcba")).assert(_ == true)
      test(m"Match a negated class")(motif(t"[^a]+").matches(t"bcd")).assert(_ == true)
      test(m"A negated class excludes")(motif(t"[^a]+").matches(t"bad")).assert(_ == false)
      test(m"Dot excludes newline")(motif(t".").matches(t"\n")).assert(_ == false)
      test(m"Match the empty pattern")(motif(t"").matches(t"")).assert(_ == true)
      test(m"Match an astral literal")(motif(t"🦆+").matches(t"🦆🦆")).assert(_ == true)
      test(m"Match a perl class")(motif(t"\\d+").matches(t"0123")).assert(_ == true)
      test(m"Anchors are consistent inside matches")(motif(t"^abc$$").matches(t"abc"))
      . assert(_ == true)

      test(m"Empty-loop star terminates")(motif(t"(a*)*").matches(t"aaa")).assert(_ == true)
      test(m"Empty-loop star matches nothing")(motif(t"(a*)*").matches(t"")).assert(_ == true)

      test(m"Seek finds the leftmost match")(motif(t"o").seek(t"foo"))
      . assert(_ == Interval.zerary(1, 2))

      test(m"Seek respects word boundaries")(motif(t"\\bfoo\\b").seek(t"afoo foo"))
      . assert(_ == Interval.zerary(5, 8))

      test(m"Seek misses absent patterns")(motif(t"z").seek(t"foo")).assert(_ == Unset)

      test(m"Search finds every occurrence")(motif(t"a").search(t"banana"))
      . assert(_ == Chain(Interval.zerary(1, 2), Interval.zerary(3, 4), Interval.zerary(5, 6)))

      test(m"Search survives empty matches")(motif(t"a*").search(t"ab"))
      . assert:
          _ == Chain(Interval.zerary(0, 1), Interval.zerary(1, 1), Interval.zerary(2, 2))

    suite(m"Seek acceleration"):
      def motif(pattern: Text): Motif = Motif.parse(pattern)
      val filler = ("lorem ipsum dolor sit amet ".repeat(64).nn).tt
      val haystack = (filler.s + "ERROR overload").tt

      test(m"A literal-prefix pattern is found deep in the input"):
        motif(t"ERROR [a-z]+").seek(haystack)
      . assert(_ == Interval.zerary(filler.s.length, haystack.s.length))

      test(m"A class-prefix pattern is found deep in the input"):
        motif(t"[0-9]+x").seek((filler.s + "42x").tt)
      . assert(_ == Interval.zerary(filler.s.length, filler.s.length + 3))

      test(m"An absent literal prefix yields no match"):
        motif(t"ERROR [a-z]+").seek(filler)
      . assert(_ == Unset)

      test(m"An anchored pattern still only matches at the start"):
        motif(t"^lorem").seek(haystack).present && motif(t"^ERROR").seek(haystack).absent
      . assert(_ == true)

      test(m"A nullable pattern still matches emptily at the start"):
        motif(t"(?:ERROR)*").seek(haystack)
      . assert(_ == Interval.zerary(0, 0))

      test(m"Skipping does not confuse repeated search"):
        motif(t"ERROR").search((filler.s + "ERROR ERROR").tt)
      . assert(_ == Chain(Interval.zerary(filler.s.length, filler.s.length + 5),
          Interval.zerary(filler.s.length + 6, filler.s.length + 11)))

    suite(m"Captures"):
      def motif(pattern: Text): Motif = Motif.parse(pattern)

      test(m"Capture two groups")(motif(t"(a+)(b+)").groups(t"aabb"))
      . assert(_ == List(Interval.zerary(0, 2), Interval.zerary(2, 4)))

      test(m"Greedy star captures maximally")(motif(t"(a*)a*").groups(t"aaa"))
      . assert(_ == List(Interval.zerary(0, 3)))

      test(m"Reluctant star captures minimally")(motif(t"(a*?)a*").groups(t"aaa"))
      . assert(_ == List(Interval.zerary(0, 0)))

      test(m"An unmatched optional group is unset")(motif(t"(a)?b").groups(t"b"))
      . assert(_ == List[Optional[Interval]](Unset))

      test(m"A repeated group records its last iteration")(motif(t"(a|b)+").groups(t"ab"))
      . assert(_ == List(Interval.zerary(1, 2)))

      test(m"Nested groups capture independently")(motif(t"((a)b)").groups(t"ab"))
      . assert(_ == List(Interval.zerary(0, 2), Interval.zerary(0, 1)))

      test(m"No match yields no groups")(motif(t"(a)").groups(t"b")).assert(_ == Unset)

    suite(m"Differential against java.util.regex"):
      test(m"Agree on a corpus of patterns and inputs"):
        val patterns = List
          ( t"a*b", t"(a|b)*c", t"[a-m]+", t"a{2,4}", t"(ab)+", t"a?b?c?", t"\\d+", t"\\w+",
            t"[^x]*x", t"a(b|c)d", t"(?:ab|cd)+", t"x[0-9]{2}y", t"a+b+c+", t"(a?)(a?)aa" )

        val inputs = List
          ( t"", t"a", t"b", t"ab", t"abc", t"aab", t"aaab", t"abab", t"abababc", t"aabb",
            t"123", t"x42y", t"xyz", t"aaaa", t"abcd", t"cdab", t"wordy1", t"mix", t"aax" )

        var failures: List[Text] = Nil

        patterns.each: pattern =>
          val motif = Motif.parse(pattern)
          val rival = java.util.regex.Pattern.compile(pattern.s).nn

          inputs.each: input =>
            if motif.matches(input) != rival.matcher(input.s).nn.matches
            then failures = (pattern.s+" on "+input.s).tt :: failures

        failures

      . assert(_ == Nil)

    suite(m"Linearity"):
      test(m"A pathological pattern completes in linear time"):
        Motif.parse(t"(?:a+)+b").matches(("a".repeat(200).nn).tt)
      . assert(_ == false)

      test(m"A pathological alternation completes")
        (Motif.parse(t"(?:a|a)*c").matches(("a".repeat(200).nn).tt))
      . assert(_ == false)

    suite(m"Containment"):
      def motif(pattern: Text): Motif = Motif.parse(pattern)

      test(m"A star subsumes a repetition")(motif(t"a*").subsumes(motif(t"aa")))
      . assert(_ == true)

      test(m"A repetition does not subsume a star")(motif(t"aa").subsumes(motif(t"a*")))
      . assert(_ == false)

      test(m"A class subsumes an alternation")(motif(t"[a-z]+").subsumes(motif(t"abc|def")))
      . assert(_ == true)

      test(m"An alternation does not subsume a class")(motif(t"abc|def").subsumes(motif(t"[a-z]+")))
      . assert(_ == false)

      test(m"Everything subsumes itself")(motif(t"(a|b)*c").subsumes(motif(t"(a|b)*c")))
      . assert(_ == true)

      test(m"Equivalent forms subsume each other"):
        motif(t"(a|b)*").subsumes(motif(t"[ab]*")) && motif(t"[ab]*").subsumes(motif(t"(a|b)*"))
      . assert(_ == true)

      test(m"A redundant anchor changes nothing")(motif(t"a").subsumes(motif(t"^a$$")))
      . assert(_ == true)

      test(m"The empty pattern is subsumed by a star")(motif(t"a*").subsumes(motif(t"")))
      . assert(_ == true)

      test(m"A star does not subsume a different letter")(motif(t"a*").subsumes(motif(t"b")))
      . assert(_ == false)

      test(m"Dot does not subsume newline")(motif(t".").subsumes(motif(t"\\n")))
      . assert(_ == false)

      test(m"Overlapping classes intersect")(motif(t"a+").intersects(motif(t"[ab]+")))
      . assert(_ == true)

      test(m"Distinct literals do not intersect")(motif(t"abc").intersects(motif(t"abd")))
      . assert(_ == false)

      test(m"Disjoint classes do not intersect")(motif(t"[a-m]+").intersects(motif(t"[n-z]+")))
      . assert(_ == false)

      test(m"Nullable patterns intersect on the empty input")
        (motif(t"a*").intersects(motif(t"b*")))
      . assert(_ == true)

      test(m"Word boundaries are unverifiable"):
        capture[Motif.Error](motif(t"\\bfoo\\b").subsumes(motif(t"foo")))
      . assert(_.reason == Reason.Unverifiable)

    suite(m"Containment of an intersection"):
      def motif(pattern: Text): Motif = Motif.parse(pattern)

      // One conjunct also admits two digits and the other also admits six, so neither alone is
      // contained in the cover; only their intersection is. This is the case a pairwise-only
      // decision procedure gets wrong.
      test(m"An intersection narrower than either conjunct is contained"):
        motif(t"[A-Z]{2}-[0-9]{4}").subsumes
         (List(motif(t"[A-Z]{2}-[0-9]{4}|[A-Z]{2}-[0-9]{2}"),
               motif(t"[A-Z]{2}-[0-9]{4}|[A-Z]{2}-[0-9]{6}")))
      . assert(_ == true)

      test(m"Neither conjunct alone is contained"):
        motif(t"[A-Z]{2}-[0-9]{4}").subsumes(motif(t"[A-Z]{2}-[0-9]{4}|[A-Z]{2}-[0-9]{2}"))
      . assert(_ == false)

      // Both conjuncts admit `AB-X-1234`, which the cover does not: a genuine widening that
      // survives intersection.
      test(m"An intersection wider than the cover is not contained"):
        motif(t"[A-Z]{2}-[0-9]{4}")
        . subsumes(List(motif(t"[A-Z]{2}-.*"), motif(t".*-[0-9]{4}")))
      . assert(_ == false)

      test(m"A singleton intersection agrees with the pairwise form"):
        motif(t"[a-z]+").subsumes(List(motif(t"[a-z]{2,}")))
      . assert(_ == true)

      test(m"A widening intersection is not contained"):
        motif(t"(EU|UK)-[0-9]{4}").subsumes(List(motif(t"[A-Z]{2}-[0-9]{4}")))
      . assert(_ == false)

      test(m"An unsatisfiable intersection is contained in anything"):
        motif(t"zzz").subsumes(List(motif(t"[a-m]+"), motif(t"[n-z]+")))
      . assert(_ == true)

      test(m"A nullable intersection is contained in a nullable cover"):
        motif(t"a*").subsumes(List(motif(t"a*"), motif(t"[ab]*")))
      . assert(_ == true)

      test(m"An empty input escaping the cover is not contained"):
        motif(t"a+").subsumes(List(motif(t"a*"), motif(t"[ab]*")))
      . assert(_ == false)

      test(m"A word boundary in any conjunct is unverifiable"):
        capture[Motif.Error](motif(t"foo").subsumes(List(motif(t"foo"), motif(t"\\bfoo\\b"))))
      . assert(_.reason == Reason.Unverifiable)
