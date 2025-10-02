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
┃    Soundness, version 0.41.0.                                                                    ┃
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
package kaleidoscope

import soundness.*

import strategies.throwUnsafely
import errorDiagnostics.stackTraces

object Tests extends Suite(m"Kaleidoscope tests"):
  def run(): Unit =
    suite(m"Regex tests"):
      import Regex.Group, Regex.Quantifier.*, Regex.Greed.*

      suite(m"Standard parsing"):
        test(m"Parse aaa")(Regex.parse(List(t"aaa")))
        . assert(_ == Regex(t"aaa", Nil))

        test(m"Parse (aaa)")(Regex.parse(List(t"(aaa)")))
        . assert:
          _ == Regex(t"(aaa)", List(Group(1, 4, 5)))

        test(m"Parse (aa)bb")(Regex.parse(List(t"(aa)bb")))
        . assert:
          _ == Regex(t"(aa)bb", List(Group(1, 3, 4)))

        test(m"Parse aa(bb)")(Regex.parse(List(t"aa(bb)")))
        . assert(_ == Regex(t"aa(bb)", List(Group(3, 5, 6))))

        test(m"Parse aa(bb)cc")(Regex.parse(List(t"aa(bb)cc")))
        . assert(_ == Regex(t"aa(bb)cc", List(Group(3, 5, 6))))

        test(m"Parse aa(bb)+?cc")(Regex.parse(List(t"aa(bb)+?cc")))
        . assert(_ == Regex(t"aa(bb)+?cc", List(Group(3, 5, 8, Nil, AtLeast(1), Reluctant))))

        test(m"Parse aa(bb)++cc")(Regex.parse(List(t"aa(bb)++cc")))
        . assert(_ == Regex(t"aa(bb)++cc", List(Group(3, 5, 8, Nil, AtLeast(1), Possessive))))

        test(m"Parse aa(bb)cc(dd)ee")(Regex.parse(List(t"aa(bb)cc(dd)ee")))
        . assert(_ == Regex(t"aa(bb)cc(dd)ee", List(Group(3, 5, 6), Group(9, 11, 12))))

        test(m"Parse aa(bb(cc)dd)ee")(Regex.parse(List(t"aa(bb(cc)dd)ee")))
        . assert(_ == Regex(t"aa(bb(cc)dd)ee", List(Group(3, 11, 12, List(Group(6, 8, 9))))))

        test(m"Parse aa(bb)*cc(dd)ee")(Regex.parse(List(t"aa(bb)*cc(dd)ee")))
        . assert(_ == Regex(t"aa(bb)*cc(dd)ee", List(Group(3, 5, 7, Nil, AtLeast(0)), Group(10, 12, 13))))

        test(m"Parse aa(bb(cc)*dd)ee"):
          Regex.parse(List(t"aa(bb(cc)*dd)ee"))

        . assert:
          _ == Regex(t"aa(bb(cc)*dd)ee", List(Group(3, 12, 13, List(Group(6, 8, 10, Nil, AtLeast(0))))))

        test(m"Parse aa(bb)+cc(dd)ee"):
          Regex.parse(List(t"aa(bb)+cc(dd)ee"))

        . assert:
          _ == Regex(t"aa(bb)+cc(dd)ee", List(Group(3, 5, 7, Nil, AtLeast(1)), Group(10, 12, 13)))

        test(m"Parse aa(bb){4}cc(dd)ee"):
          Regex.parse(List(t"aa(bb){4}cc(dd)ee"))

        . assert:
          _ == Regex(t"aa(bb){4}cc(dd)ee", List(Group(3, 5, 9, Nil, Exactly(4)), Group(12, 14, 15)))

        test(m"Parse aa(bb){4,}cc(dd)ee"):
          Regex.parse(List(t"aa(bb){4,}cc(dd)ee"))

        . assert:
          _ == Regex(t"aa(bb){4,}cc(dd)ee", List(Group(3, 5, 10, Nil, AtLeast(4)), Group(13, 15, 16)))

        test(m"Parse aa(bb){4,6}cc(dd)ee"):
          Regex.parse(List(t"aa(bb){4,6}cc(dd)ee"))

        . assert:
          _ == Regex(t"aa(bb){4,6}cc(dd)ee", List(Group(3, 5, 11, Nil, Between(4, 6)), Group(14, 16, 17)))

        test(m"Parse aa(bb){14,16}ccddee"):
          Regex.parse(List(t"aa(bb){14,16}ccddee"))

        . assert(_ == Regex(t"aa(bb){14,16}ccddee", List(Group(3, 5, 13, Nil, Between(14, 16)))))

        test(m"Capture character class"):
          Regex.parse(List(t"w[aeiou]rld"))

        .assert(_ == Regex(t"w[aeiou]rld", List(Group(2, 7, 8, Nil, Exactly(1), Greedy, false, true))))

      suite(m"Parsing failures"):
        test(m"Fail to parse aa(bb){14,16ccddee"):
          capture(Regex.parse(List(t"aa(bb){14,16ccddee")))

        . assert(_ == RegexError(12, RegexError.Reason.UnexpectedChar))

        test(m"Fail to parse aa(bb){14!}ccddee"):
          capture(Regex.parse(List(t"aa(bb){14!}ccddee")))

        . assert(_ == RegexError(9, RegexError.Reason.UnexpectedChar))

        test(m"Fail to parse aa(bb{14}ccddee"):
          capture(Regex.parse(List(t"aa(bb{14}ccddee")))

        . assert(_ == RegexError(15, RegexError.Reason.UnclosedGroup))

        test(m"Fail to parse aa(bb){2,1}c"):
          capture(Regex.parse(List(t"aa(bb){2,1}c")))

        . assert(_ == RegexError(9, RegexError.Reason.BadRepetition))

        test(m"Fail to parse aabb){2,1}c"):
          capture(Regex.parse(List(t"aabb){2,1}c")))

        . assert(_ == RegexError(4, RegexError.Reason.NotInGroup))

        test(m"Fail to parse aa(bb){2,,1}c"):
          capture(Regex.parse(List(t"aabb){2,,1}c")))

        . assert(_ == RegexError(4, RegexError.Reason.NotInGroup))

        test(m"Fail to parse aabb){2,,1}c"):
          capture(Regex.parse(List(t"aabb){2,,1}c")))

        . assert(_ == RegexError(4, RegexError.Reason.NotInGroup))

        test(m"Fail to parse aabb){,2}c"):
          capture(Regex.parse(List(t"aabb){,2}c")))

        . assert(_ == RegexError(4, RegexError.Reason.NotInGroup))

        test(m"Fail to parse aa(bb){2,,1}c"):
          capture(Regex.parse(List(t"aa(bb){2,,1}c")))

        . assert(_ == RegexError(9, RegexError.Reason.UnexpectedChar))

        test(m"Fail to parse aa(bb){"):
          capture(Regex.parse(List(t"aa(bb){")))

        . assert(_ == RegexError(7, RegexError.Reason.IncompleteRepetition))

      suite(m"Test captures"):
        test(m"Capture without parens should fail"):
          capture(Regex.parse(List(t"a", t"a(bb)")))

        . assert(_ == RegexError(0, RegexError.Reason.ExpectedGroup))

        test(m"Check simple group is captured"):
          Regex.parse(List(t"aa", t"(bb)cc"))

        . assert(_ == Regex(t"aa(bb)cc", List(Group(3, 5, 6, Nil, Exactly(1), Greedy, true))))

        test(m"Check simple group at start is captured"):
          Regex.parse(List(t"", t"(bb)cc"))

        . assert(_ == Regex(t"(bb)cc", List(Group(1, 3, 4, Nil, Exactly(1), Greedy, true))))

        test(m"Check nested group is captured"):
          Regex.parse(List(t"a(a", t"(bb)c)c"))

        . assert(_ == Regex(t"a(a(bb)c)c", List(Group(2, 8, 9, List(Group(4, 6, 7, Nil, Exactly(1), Greedy, true))))))

        test(m"Check that capture in repeated group is forbidden"):
          capture(Regex.parse(List(t"a(a", t"(bb)c)*c")))

        . assert(_ == RegexError(3, RegexError.Reason.Uncapturable))

        test(m"Check that capture in another repeated group is forbidden"):
          capture(Regex.parse(List(t"a(a", t"(bb)c){2}c")))

        . assert(_ == RegexError(3, RegexError.Reason.Uncapturable))

      suite(m"Capturing patterns"):
        test(m"Show plain capturing pattern"):
          Regex.parse(List(t"abc")).capturePattern

        . assert(_ == t"abc")

        test(m"Show simple capturing pattern"):
          Regex.parse(List(t"a", t"(bc)")).capturePattern

        . assert(_ == t"a(?<g0>bc)")

        test(m"Capturing groups are numbered correctly"):
          Regex.parse(List(t"(hello) ", t"(world)")).capturePattern

        . assert(_ == t"(hello) (?<g0>world)")

        test(m"Show double capturing pattern"):
          Regex.parse(List(t"a", t"(bc)d", t"(ef)")).capturePattern

        . assert(_ == t"a(?<g0>bc)d(?<g1>ef)")

        test(m"Show repeating capturing pattern"):
          Regex.parse(List(t"a", t"(bc)*")).capturePattern

        . assert(_ == t"a(?<g0>(bc)*)")

        test(m"Show exact repeating capturing pattern"):
          Regex.parse(List(t"a", t"(bc){3}")).capturePattern

        . assert(_ == t"a(?<g0>(bc){3})")

      suite(m"Scanner patterns"):
        test(m"Simple capture"):
          Regex.parse(List(t"foo", t"(bar)")).matchGroups(t"foobar")
          . map(_.to(List))

        . assert(_ == Some(List(t"bar")))

        test(m"Two captures"):
          Regex.parse(List(t"foo", t"(bar)", t"(baz)")).matchGroups(t"foobarbaz")
          . map(_.to(List))

        . assert(_ == Some(List(t"bar", t"baz")))

        test(m"Two captures, one repeating"):
          Regex.parse(List(t"foo", t"(bar)", t"(baz)*")).matchGroups(t"foobarbazbaz")
          . map(_.to(List))

        . assert(_ == Some(List(t"bar", List(t"baz", t"baz"))))

        test(m"Two captures, both repeating"):
          Regex.parse(List(t"foo", t"(bar){4}", t"(baz)*")).matchGroups(t"foobarbarbarbarbazbaz")
          . map(_.to(List))

        . assert(_ == Some(List(List(t"bar", t"bar", t"bar", t"bar"), List(t"baz", t"baz"))))

        test(m"Two captures, one optional and absent, one repeating"):
          Regex.parse(List(t"foo", t"(bar)+", t"(baz)?")).matchGroups(t"foobarbar")
          . map(_.to(List))

        . assert(_ == Some(List(List(t"bar", t"bar"), Unset)))

        test(m"Two captures, one optional and present, one repeating"):
          Regex.parse(List(t"foo", t"(b.r)+", t"(baz)?")).matchGroups(t"fooberbirbaz")
          . map(_.to(List))

        . assert(_ == Some(List(List(t"ber", t"bir"), t"baz")))

        test(m"Nested captures, one optional and present, one repeating"):
          Regex.parse(List(t"f(oo", t"(b.r)+", t"(baz)?)")).matchGroups(t"fooberbirbaz")
          . map(_.to(List))

        . assert(_ == Some(List(List(t"ber", t"bir"), t"baz")))

    suite(m"Match tests"):
      test(m"simple match"):
        t"hello world".absolve match
          case r"hello world" => 1

      . assert(_ == 1)

      test(m"basic extractor"):
        t"hello world".absolve match
          case r"(hello world)" => 2

      . assert(_ == 2)

      test(m"extract one word"):
        t"hello world".absolve match
          case r"$first(hello) world" => first.show

      . check(_ == t"hello")

      test(m"extract a nested capture group"):
        t"hello world".absolve match
          case r"(($first(hello)) world)" => first.show

      . assert(_ == t"hello")

      test(m"extract words"):
        t"hello world".absolve match
          case r"$first(hello) $second(world)" => List(first, second)

      . assert(_ == List(t"hello", t"world"))

      test(m"skipped capture group"):
        t"hello world".absolve match
          case r"(hello) $second(world)" => second.show

      . assert(_ == t"world")

      test(m"skipped capture group 2"):
        t"1 2 3 4 5".absolve match
          case r"1 $two(2) 3 4 5" => two.show

      . assert(_ == t"2")

      test(m"nested unbound capture group"):
        t"anyval".absolve match
          case r"$x(any(val))" => x.show
      . assert(_ == t"anyval")

      test(m"email regex"):
        val r"^$prefix([a-z0-9._%+-]+)@$domain([a-z0-9.-]+)\.$tld([a-z]{2,6})$$" =
            t"test@example.com": @unchecked

        List(prefix, domain, tld)

      . assert(_ == List(t"test", t"example", t"com"))

      suite(m"Character match tests"):
        test(m"Match a character"):
          t"hello" match
            case r"h$vowel[aeiou]llo" => vowel
            case _ => Nil

        . assert(_ == 'e')

        test(m"Match several characters"):
          t"favourite" match
            case r"fav$vowels[aeiou]+rite" => vowels
            case _ => Nil

        . assert(_ == List('o', 'u'))

        test(m"Match zero characters"):
          t"favourite" match
            case r"favou$misc[cxm]*rite" => misc
            case _ => Nil

        . assert(_ == Nil)

        test(m"Match maybe one character; preset"):
          t"favourite" match
            case r"favo$vowel[aeiou]?rite" => vowel
            case _ => Nil

        . assert(_ == 'u')

        test(m"Match maybe one character; absent"):
          t"favourite" match
            case r"favou$vowel[aeiou]?rite" => vowel
            case _ => Nil

        . assert(_ == Unset)

        test(m"Match characters in subgroup"):
          t"favourite" match
            case r"fav($vowels[ou]*)rite" => vowels
            case _ => Nil

        . assert(_ == List('o', 'u'))


        test(m"Match characters in subgroup"):
          t"favourite" match
            case r"fav($vowels[ou]*)rite" => vowels
            case _ => Nil

        . assert(_ == List('o', 'u'))

    suite(m"Glob tests"):
      test(m"Parse a plain glob"):
        Glob.parse(t"hello world").regex

      . assert(_ == t"hello world")

      test(m"Parse a plain glob with some symbols"):
        Glob.parse(t"hello-world!").regex

      . assert(_ == t"hello\\-world\\!")

      test(m"Parse a glob with a star"):
        Glob.parse(t"hello*world").regex

      . assert(_ == t"hello[^/\\\\]*world")

      test(m"Parse a glob with a question mark"):
        Glob.parse(t"hello?world").regex

      . assert(_ == t"hello[^/\\\\]world")

      test(m"Parse a glob with a range"):
        Glob.parse(t"hello[a-z]world").regex

      . assert(_ == t"hello[a-z]world")

      test(m"Parse a glob with a specific set of characters"):
        Glob.parse(t"hello[aeiou]world").regex

      . assert(_ == t"hello[aeiou]world")

      test(m"Parse a glob excluding a specific set of characters"):
        Glob.parse(t"hello[!aeiou]world").regex

      . assert(_ == t"hello[^aeiou]world")

      test(m"Parse a glob excluding a range of characters"):
        Glob.parse(t"hello[!a-z]world").regex

      . assert(_ == t"hello[^a-z]world")

      test(m"Extract from a glob"):
        t"/home/work/docs" match
          case g"/$home/work/docs" => home
          case _ => Nil

      . assert(_ == t"home")

      test(m"Extract from a glob with a star"):
        t"/home/work/docs" match
          case g"/$home/*/docs" => home
          case _ => Nil

      . assert(_ == t"home")

      test(m"Extract from a glob with question marks"):
        t"/home/work/docs" match
          case g"/$home/????/docs" => home
          case _ => Nil

      . assert(_ == t"home")

      test(m"Extract from a glob with two extractions"):
        t"/home/work/docs" match
          case g"/$home/$work/docs" => (home, work)
          case _ => Nil

      . assert(_ == (t"home", t"work"))

      test(m"Extract from a glob with globstar"):
        t"/home/work/docs" match
          case g"/$home/**" => home
          case _ => Nil

      . assert(_ == t"home")

    suite(m"Compilation tests"):
      test(m"brackets must be matched"):
        demilitarize:
          t"" match
            case r"hello(world" =>

        . head
        . message

      . assert(_.contains("kaleidoscope: the regular expression could not be parsed because a capturing group was not closed at 11"))

      test(m"variable must be bound"):
        demilitarize:
          t"" match
            case r"hello${space}world" =>

        . head
        . message

      . assert(_.contains("kaleidoscope: the regular expression could not be parsed because a capturing group was expected immediately following an extractor at 0"))
