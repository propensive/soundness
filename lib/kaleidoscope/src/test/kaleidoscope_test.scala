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
package kaleidoscope

import soundness.*

import strategies.throwUnsafely
import errorDiagnostics.stackTracesDiagnostics

object Tests extends Suite(m"Kaleidoscope tests"):
  def run(): Unit =
    suite(m"Regex tests"):
      import Regex.Group, Regex.Quantifier.*, Regex.Greed.*

      suite(m"Standard parsing"):
        test(m"Parse aaa")(Regex.parse(List(t"aaa")))
        . assert(_ == Regex("aaa", Nil))

        test(m"Parse (aaa)")(Regex.parse(List(t"(aaa)")))
        . assert:
            _ == Regex("(aaa)", List(Group(1, 4, 5)))

        test(m"Parse (aa)bb")(Regex.parse(List(t"(aa)bb")))
        . assert:
            _ == Regex("(aa)bb", List(Group(1, 3, 4)))

        test(m"Parse aa(bb)")(Regex.parse(List(t"aa(bb)")))
        . assert(_ == Regex("aa(bb)", List(Group(3, 5, 6))))

        test(m"Parse aa(bb)cc")(Regex.parse(List(t"aa(bb)cc")))
        . assert(_ == Regex("aa(bb)cc", List(Group(3, 5, 6))))

        test(m"Parse aa(bb)+?cc")(Regex.parse(List(t"aa(bb)+?cc")))
        . assert(_ == Regex("aa(bb)+?cc", List(Group(3, 5, 8, Nil, AtLeast(1), Reluctant))))

        test(m"Parse aa(bb)++cc")(Regex.parse(List(t"aa(bb)++cc")))
        . assert(_ == Regex("aa(bb)++cc", List(Group(3, 5, 8, Nil, AtLeast(1), Possessive))))

        test(m"Parse aa(bb)cc(dd)ee")(Regex.parse(List(t"aa(bb)cc(dd)ee")))
        . assert(_ == Regex("aa(bb)cc(dd)ee", List(Group(3, 5, 6), Group(9, 11, 12))))

        test(m"Parse aa(bb(cc)dd)ee")(Regex.parse(List(t"aa(bb(cc)dd)ee")))
        . assert(_ == Regex("aa(bb(cc)dd)ee", List(Group(3, 11, 12, List(Group(6, 8, 9))))))

        test(m"Parse aa(bb)*cc(dd)ee")(Regex.parse(List(t"aa(bb)*cc(dd)ee")))
        . assert(_ == Regex("aa(bb)*cc(dd)ee", List(Group(3, 5, 7, Nil, AtLeast(0)), Group(10, 12, 13))))

        test(m"Parse aa(bb(cc)*dd)ee"):
          Regex.parse(List(t"aa(bb(cc)*dd)ee"))

        . assert:
            _ == Regex("aa(bb(cc)*dd)ee", List(Group(3, 12, 13, List(Group(6, 8, 10, Nil, AtLeast(0))))))

        test(m"Parse aa(bb)+cc(dd)ee"):
          Regex.parse(List(t"aa(bb)+cc(dd)ee"))

        . assert:
            _ == Regex("aa(bb)+cc(dd)ee", List(Group(3, 5, 7, Nil, AtLeast(1)), Group(10, 12, 13)))

        test(m"Parse aa(bb){4}cc(dd)ee"):
          Regex.parse(List(t"aa(bb){4}cc(dd)ee"))

        . assert:
            _ == Regex("aa(bb){4}cc(dd)ee", List(Group(3, 5, 9, Nil, Exactly(4)), Group(12, 14, 15)))

        test(m"Parse aa(bb){4,}cc(dd)ee"):
          Regex.parse(List(t"aa(bb){4,}cc(dd)ee"))

        . assert:
            _ == Regex("aa(bb){4,}cc(dd)ee", List(Group(3, 5, 10, Nil, AtLeast(4)), Group(13, 15, 16)))

        test(m"Parse aa(bb){4,6}cc(dd)ee"):
          Regex.parse(List(t"aa(bb){4,6}cc(dd)ee"))

        . assert:
            _ == Regex("aa(bb){4,6}cc(dd)ee", List(Group(3, 5, 11, Nil, Between(4, 6)), Group(14, 16, 17)))

        test(m"Parse aa(bb){14,16}ccddee"):
          Regex.parse(List(t"aa(bb){14,16}ccddee"))

        . assert(_ == Regex("aa(bb){14,16}ccddee", List(Group(3, 5, 13, Nil, Between(14, 16)))))

        test(m"Capture character class"):
          Regex.parse(List(t"w[aeiou]rld"))

        . assert(_ == Regex("w[aeiou]rld", List(Group(2, 7, 8, Nil, Exactly(1), Greedy, false, true))))

      suite(m"Parsing failures"):
        test(m"Fail to parse aa(bb){14,16ccddee"):
          capture(Regex.parse(List(t"aa(bb){14,16ccddee")))

        . assert(_ == Regex.Error(12, Regex.Error.Reason.UnexpectedChar))

        test(m"Fail to parse aa(bb){14!}ccddee"):
          capture(Regex.parse(List(t"aa(bb){14!}ccddee")))

        . assert(_ == Regex.Error(9, Regex.Error.Reason.UnexpectedChar))

        test(m"Fail to parse aa(bb{14}ccddee"):
          capture(Regex.parse(List(t"aa(bb{14}ccddee")))

        . assert(_ == Regex.Error(15, Regex.Error.Reason.UnclosedGroup))

        test(m"Fail to parse aa(bb){2,1}c"):
          capture(Regex.parse(List(t"aa(bb){2,1}c")))

        . assert(_ == Regex.Error(9, Regex.Error.Reason.BadRepetition))

        test(m"Fail to parse aabb){2,1}c"):
          capture(Regex.parse(List(t"aabb){2,1}c")))

        . assert(_ == Regex.Error(4, Regex.Error.Reason.NotInGroup))

        test(m"Fail to parse aabb){2,,1}c"):
          capture(Regex.parse(List(t"aabb){2,,1}c")))

        . assert(_ == Regex.Error(4, Regex.Error.Reason.NotInGroup))

        test(m"Fail to parse aabb){,2}c"):
          capture(Regex.parse(List(t"aabb){,2}c")))

        . assert(_ == Regex.Error(4, Regex.Error.Reason.NotInGroup))

        test(m"Fail to parse aa(bb){2,,1}c"):
          capture(Regex.parse(List(t"aa(bb){2,,1}c")))

        . assert(_ == Regex.Error(9, Regex.Error.Reason.UnexpectedChar))

        test(m"Fail to parse aa(bb){"):
          capture(Regex.parse(List(t"aa(bb){")))

        . assert(_ == Regex.Error(7, Regex.Error.Reason.IncompleteRepetition))

      suite(m"Test captures"):
        test(m"Capture without parens should fail"):
          capture(Regex.parse(List(t"a", t"a(bb)")))

        . assert(_ == Regex.Error(0, Regex.Error.Reason.ExpectedGroup))

        test(m"Check simple group is captured"):
          Regex.parse(List(t"aa", t"(bb)cc"))

        . assert(_ == Regex("aa(bb)cc", List(Group(3, 5, 6, Nil, Exactly(1), Greedy, true))))

        test(m"Check simple group at start is captured"):
          Regex.parse(List(t"", t"(bb)cc"))

        . assert(_ == Regex("(bb)cc", List(Group(1, 3, 4, Nil, Exactly(1), Greedy, true))))

        test(m"Check nested group is captured"):
          Regex.parse(List(t"a(a", t"(bb)c)c"))

        . assert(_ == Regex("a(a(bb)c)c", List(Group(2, 8, 9, List(Group(4, 6, 7, Nil, Exactly(1), Greedy, true))))))

        test(m"Check that capture in repeated group is forbidden"):
          capture(Regex.parse(List(t"a(a", t"(bb)c)*c")))

        . assert(_ == Regex.Error(3, Regex.Error.Reason.Uncapturable))

        test(m"Check that capture in another repeated group is forbidden"):
          capture(Regex.parse(List(t"a(a", t"(bb)c){2}c")))

        . assert(_ == Regex.Error(3, Regex.Error.Reason.Uncapturable))

      suite(m"Capturing patterns"):
        test(m"Show plain capturing pattern"):
          Regex.parse(List(t"abc")).capturePattern

        . assert(_ == "abc")

        test(m"Show simple capturing pattern"):
          Regex.parse(List(t"a", t"(bc)")).capturePattern

        . assert(_ == "a(?<g0>bc)")

        test(m"Capturing groups are numbered correctly"):
          Regex.parse(List(t"(hello) ", t"(world)")).capturePattern

        . assert(_ == "(hello) (?<g0>world)")

        test(m"Show double capturing pattern"):
          Regex.parse(List(t"a", t"(bc)d", t"(ef)")).capturePattern

        . assert(_ == "a(?<g0>bc)d(?<g1>ef)")

        test(m"Show repeating capturing pattern"):
          Regex.parse(List(t"a", t"(bc)*")).capturePattern

        . assert(_ == "a(?<g0>(bc)*)")

        test(m"Show exact repeating capturing pattern"):
          Regex.parse(List(t"a", t"(bc){3}")).capturePattern

        . assert(_ == "a(?<g0>(bc){3})")

      suite(m"Scanner patterns"):
        test(m"Simple capture"):
          JavaBaseRegex.engine.matchGroups(Regex.parse(List(t"foo", t"(bar)")), "foobar")
          . map { (g: Array[List[Text | Char] | Optional[Text | Char]]^{}) =>
              g.readable.toList.to(proscenium.List) } // explicit: inference boxes the element captures

        . assert(_ == Some(List(t"bar")))

        test(m"Two captures"):
          JavaBaseRegex.engine.matchGroups(Regex.parse(List(t"foo", t"(bar)", t"(baz)")), "foobarbaz")
          . map { (g: Array[List[Text | Char] | Optional[Text | Char]]^{}) =>
              g.readable.toList.to(proscenium.List) } // explicit: inference boxes the element captures

        . assert(_ == Some(List(t"bar", t"baz")))

        test(m"Two captures, one repeating"):
          JavaBaseRegex.engine.matchGroups(Regex.parse(List(t"foo", t"(bar)", t"(baz)*")), "foobarbazbaz")
          . map { (g: Array[List[Text | Char] | Optional[Text | Char]]^{}) =>
              g.readable.toList.to(proscenium.List) } // explicit: inference boxes the element captures

        . assert(_ == Some(List(t"bar", List(t"baz", t"baz"))))

        test(m"Two captures, both repeating"):
          JavaBaseRegex.engine.matchGroups(Regex.parse(List(t"foo", t"(bar){4}", t"(baz)*")), "foobarbarbarbarbazbaz")
          . map { (g: Array[List[Text | Char] | Optional[Text | Char]]^{}) =>
              g.readable.toList.to(proscenium.List) } // explicit: inference boxes the element captures

        . assert(_ == Some(List(List(t"bar", t"bar", t"bar", t"bar"), List(t"baz", t"baz"))))

        test(m"Two captures, one optional and absent, one repeating"):
          JavaBaseRegex.engine.matchGroups(Regex.parse(List(t"foo", t"(bar)+", t"(baz)?")), "foobarbar")
          . map { (g: Array[List[Text | Char] | Optional[Text | Char]]^{}) =>
              g.readable.toList.to(proscenium.List) } // explicit: inference boxes the element captures

        . assert(_ == Some(List(List(t"bar", t"bar"), Unset)))

        test(m"Two captures, one optional and present, one repeating"):
          JavaBaseRegex.engine.matchGroups(Regex.parse(List(t"foo", t"(b.r)+", t"(baz)?")), "fooberbirbaz")
          . map { (g: Array[List[Text | Char] | Optional[Text | Char]]^{}) =>
              g.readable.toList.to(proscenium.List) } // explicit: inference boxes the element captures

        . assert(_ == Some(List(List(t"ber", t"bir"), t"baz")))

        test(m"Nested captures, one optional and present, one repeating"):
          JavaBaseRegex.engine.matchGroups(Regex.parse(List(t"f(oo", t"(b.r)+", t"(baz)?)")), "fooberbirbaz")
          . map { (g: Array[List[Text | Char] | Optional[Text | Char]]^{}) =>
              g.readable.toList.to(proscenium.List) } // explicit: inference boxes the element captures

        . assert(_ == Some(List(List(t"ber", t"bir"), t"baz")))

    suite(m"Match tests"):
      test(m"simple match"):
        "hello world".absolve match
          case r"hello world" => 1

      . assert(_ == 1)

      test(m"basic extractor"):
        "hello world".absolve match
          case r"(hello world)" => 2

      . assert(_ == 2)

      test(m"extract one word"):
        "hello world".absolve match
          case r"$first(hello) world" => first.show

      . check(_ == "hello")

      test(m"extract a nested capture group"):
        "hello world".absolve match
          case r"(($first(hello)) world)" => first.show

      . assert(_ == "hello")

      test(m"extract words"):
        "hello world".absolve match
          case r"$first(hello) $second(world)" => List(first, second)

      . assert(_ == List(t"hello", t"world"))

      test(m"skipped capture group"):
        "hello world".absolve match
          case r"(hello) $second(world)" => second.show

      . assert(_ == "world")

      test(m"skipped capture group 2"):
        "1 2 3 4 5".absolve match
          case r"1 $two(2) 3 4 5" => two.show

      . assert(_ == "2")

      test(m"nested unbound capture group"):
        "anyval".absolve match
          case r"$x(any(val))" => x.show
      . assert(_ == "anyval")

      test(m"email regex"):
        val r"^$prefix([a-z0-9._%+-]+)@$domain([a-z0-9.-]+)\.$tld([a-z]{2,6})$$" =
          "test@example.com": @unchecked

        List(prefix, domain, tld)

      . assert(_ == List(t"test", t"example", t"com"))

      suite(m"Character match tests"):
        test(m"Match a character"):
          "hello" match
            case r"h$vowel[aeiou]llo" => vowel
            case _                    => Nil

        . assert(_ == 'e')

        test(m"Match several characters"):
          "favourite" match
            case r"fav$vowels[aeiou]+rite" => vowels
            case _                         => Nil

        . assert(_ == List('o', 'u'))

        test(m"Match zero characters"):
          "favourite" match
            case r"favou$misc[cxm]*rite" => misc
            case _                       => Nil

        . assert(_ == Nil)

        test(m"Match maybe one character; preset"):
          "favourite" match
            case r"favo$vowel[aeiou]?rite" => vowel
            case _                         => Nil

        . assert(_ == 'u')

        test(m"Match maybe one character; absent"):
          "favourite" match
            case r"favou$vowel[aeiou]?rite" => vowel
            case _                          => Nil

        . assert(_ == Unset)

        test(m"Match characters in subgroup"):
          "favourite" match
            case r"fav($vowels[ou]*)rite" => vowels
            case _                        => Nil

        . assert(_ == List('o', 'u'))

      suite(m"Single-character matcher tests"):
        test(m"Match any character"):
          "foo!bar" match
            case r"foo$ch.bar" => ch
            case _             => Nil

        . assert(_ == '!')

        test(m"Match a digit"):
          "foo5bar" match
            case r"foo$d\dbar" => d
            case _             => Nil

        . assert(_ == '5')

        test(m"Match a non-digit"):
          "foo!bar" match
            case r"foo$c\Dbar" => c
            case _             => Nil

        . assert(_ == '!')

        test(m"Match a word character"):
          "abcdef" match
            case r"$c\wbcdef" => c
            case _            => Nil

        . assert(_ == 'a')

        test(m"Match a non-word character"):
          "abc def" match
            case r"abc$c\Wdef" => c
            case _             => Nil

        . assert(_ == ' ')

        test(m"Match a whitespace character"):
          "hello world" match
            case r"hello$ws\sworld" => ws
            case _                  => Nil

        . assert(_ == ' ')

        test(m"Match a non-whitespace character"):
          " x " match
            case r" $c\S " => c
            case _         => Nil

        . assert(_ == 'x')

        test(m"Match one-or-more digits"):
          "foo123bar" match
            case r"foo$ds\d+bar" => ds
            case _               => Nil

        . assert(_ == List('1', '2', '3'))

        test(m"Match zero-or-more digits; absent"):
          "foobar" match
            case r"foo$ds\d*bar" => ds
            case _               => Nil

        . assert(_ == Nil)

        test(m"Match optional digit; present"):
          "foo5bar" match
            case r"foo$d\d?bar" => d
            case _              => Unset

        . assert(_ == '5')

        test(m"Match optional digit; absent"):
          "foobar" match
            case r"foo$d\d?bar" => d
            case _              => Unset

        . assert(_ == Unset)

        test(m"Match exact number of digits"):
          "foo123bar" match
            case r"foo$ds\d{3}bar" => ds
            case _                 => Nil

        . assert(_ == List('1', '2', '3'))

        test(m"Match a bounded run of any characters"):
          "foo!?#bar" match
            case r"foo$cs.{2,4}bar" => cs
            case _                  => Nil

        . assert(_ == List('!', '?', '#'))

        test(m"Match two single-character shorthands"):
          "a 9z" match
            case r"$letter\w $digit\d$tail." => (letter, digit, tail)
            case _                           => Nil

        . assert(_ == ('a', '9', 'z'))

    suite(m"Glob tests"):
      test(m"Parse a plain glob"):
        Glob.parse("hello world").regex

      . assert(_ == "hello world")

      test(m"Parse a plain glob with some symbols"):
        Glob.parse("hello-world!").regex

      . assert(_ == "hello\\-world\\!")

      test(m"Parse a glob with a star"):
        Glob.parse("hello*world").regex

      . assert(_ == "hello[^/\\\\]*world")

      test(m"Parse a glob with a question mark"):
        Glob.parse("hello?world").regex

      . assert(_ == "hello[^/\\\\]world")

      test(m"Parse a glob with a range"):
        Glob.parse("hello[a-z]world").regex

      . assert(_ == "hello[a-z]world")

      test(m"Parse a glob with a specific set of characters"):
        Glob.parse("hello[aeiou]world").regex

      . assert(_ == "hello[aeiou]world")

      test(m"Parse a glob excluding a specific set of characters"):
        Glob.parse("hello[!aeiou]world").regex

      . assert(_ == "hello[^aeiou]world")

      test(m"Parse a glob excluding a range of characters"):
        Glob.parse("hello[!a-z]world").regex

      . assert(_ == "hello[^a-z]world")

      test(m"A star glob matches at runtime"):
        Glob.parse("*.jar").matches("foo.jar")

      . assert(_ == true)

      test(m"A star does not cross a slash at runtime"):
        Glob.parse("*.jar").matches("dir/foo.jar")

      . assert(_ == false)

      test(m"A globstar crosses slashes at runtime"):
        Glob.parse("**/*.jar").matches("dir/deeper/foo.jar")

      . assert(_ == true)

      test(m"A question mark matches exactly one character at runtime"):
        (Glob.parse("a?c").matches("abc"), Glob.parse("a?c").matches("abbc"))

      . assert(_ == (true, false))

      test(m"Ranges and negated ranges match at runtime"):
        (Glob.parse("[a-c]").matches("b"), Glob.parse("[!a-c]").matches("b"))

      . assert(_ == (true, false))

      test(m"A glob decodes from text"):
        "h?llo*.jar".as[Glob]

      . assert(_ == Glob.parse("h?llo*.jar"))

      test(m"Extract from a glob"):
        "/home/work/docs" match
          case g"/$home/work/docs" => home
          case _                   => Nil

      . assert(_ == t"home")

      test(m"Extract from a glob with a star"):
        "/home/work/docs" match
          case g"/$home/*/docs" => home
          case _                => Nil

      . assert(_ == t"home")

      test(m"Extract from a glob with question marks"):
        "/home/work/docs" match
          case g"/$home/????/docs" => home
          case _                   => Nil

      . assert(_ == t"home")

      test(m"Extract from a glob with two extractions"):
        "/home/work/docs" match
          case g"/$home/$work/docs" => (home, work)
          case _                    => Nil

      . assert(_ == ("home", "work"))

      test(m"Extract from a glob with globstar"):
        "/home/work/docs" match
          case g"/$home/**" => home
          case _            => Nil

      . assert(_ == t"home")

    suite(m"Compilation tests"):
      test(m"brackets must be matched"):
        demilitarize:
          "" match
            case r"hello(world" =>

        . head
        . message

      . assert(_.contains("[↯SN-397.1] the regular expression could not be parsed because a capturing group was not closed at 11"))

      test(m"variable must be bound"):
        demilitarize:
          "" match
            case r"hello${space}world" =>

        . head
        . message

      . assert(_.contains("[↯SN-397.2] the regular expression could not be parsed because a capturing group was expected immediately following an extractor at 0"))

      test(m"invalid quantifier focus is the offending character"):
        demilitarize:
          r"ab{3,1}c"
        . map(_.focus)
      . assert(_ == List("}"))

      test(m"unclosed group focus falls on the last character"):
        demilitarize:
          r"hello (world"
        . map(_.focus)
      . assert(_ == List("d"))

    suite(m"Re2 backend"):
      import regexBackends.re2

      test(m"Boolean match"):
        "hello world" match
          case r"hello world" => 1
          case _              => 2

      . assert(_ == 1)

      test(m"Failed match"):
        "hello" match
          case r"world" => 1
          case _        => 2

      . assert(_ == 2)

      test(m"Extract one word"):
        "hello world".absolve match
          case r"$first(hello) world" => first.show

      . check(_ == "hello")

      test(m"Extract two words"):
        "hello world".absolve match
          case r"$first(hello) $second(world)" => List(first, second)

      . assert(_ == List(t"hello", t"world"))

      test(m"Extract a character class"):
        "hello" match
          case r"h$vowel[aeiou]llo" => vowel
          case _                    => Nil

      . assert(_ == 'e')

      test(m"Extract an optional group"):
        "hello" match
          case r"hell${vowel}([aeiou])?" => vowel
          case _                         => Unset

      . assert(_ == "o")

      test(m"Extract a missing optional group"):
        "hell" match
          case r"hell${vowel}([aeiou])?" => vowel
          case _                         => "unmatched"

      . assert(_ == Unset)

      test(m"Extract a repeated group"):
        "ababab".absolve match
          case r"$xs(ab)+" => xs

      . assert(_ == List(t"ab", t"ab", t"ab"))

      test(m"Extract repeated characters"):
        "aeiou" match
          case r"$vowels[aeiou]*" => vowels
          case _                  => Nil

      . assert(_ == List('a', 'e', 'i', 'o', 'u'))

      test(m"A literal in expression position is tagged Re2"):
        val regex: Regex in Re2 = r"ab?c"
        regex.matches("ac")

      . assert(_ == true)

      test(m"The static matcher handles astral codepoints"):
        "a🦆z" match
          case r"a.z" => 1
          case _      => 2

      . assert(_ == 1)

      test(m"The static matcher respects anchors"):
        "abc" match
          case r"^a[b-d]c$$" => 1
          case _             => 2

      . assert(_ == 1)

      test(m"The static matcher rejects non-matches"):
        "abd" match
          case r"a[xy]?d" => 1
          case _          => 2

      . assert(_ == 2)

      test(m"A glob uses the selected backend"):
        "file.txt" match
          case g"*.txt" => 1
          case _        => 2

      . assert(_ == 1)

      test(m"Subsumption of literals by a star"):
        r"a*".subsumes(r"aa")
      . assert(_ == true)

      test(m"No subsumption of a star by literals"):
        r"aa".subsumes(r"a*")
      . assert(_ == false)

      test(m"Intersection of overlapping patterns"):
        r"[a-m]+".intersects(r"[k-z]+")
      . assert(_ == true)

      test(m"No intersection of disjoint patterns"):
        r"[a-m]+".intersects(r"[n-z]+")
      . assert(_ == false)

    suite(m"Engine conformance"):
      val patterns = List
        ( t"a*b", t"(a|b)*c", t"[a-m]+", t"a{2,4}", t"(ab)+", t"a?b?c?", t"x[0-9]{2}y",
          t"a(b|c)d", t"a+b+c+" )

      val inputs = List
        ( t"", t"a", t"b", t"ab", t"abc", t"aab", t"aaab", t"abab", t"abababc", t"aabb",
          t"x42y", t"xyz", t"aaaa", t"abcd", t"abd", t"acd" )

      test(m"JavaBaseRegex and Re2 engines agree on matches, seek and search"):
        var failures: List[Text] = Nil

        patterns.each: pattern =>
          val jvmRegex = Regex.parse(List(pattern))
          val re2Regex = jvmRegex.to[Re2]

          inputs.each: input =>
            given Scanner = Scanner(Unset)

            if jvmRegex.matches(input) != re2Regex.matches(input)
            || jvmRegex.seek(input) != re2Regex.seek(input)
            || jvmRegex.search(input) != re2Regex.search(input)
            then failures = (pattern.s+s" on ${input.s}").tt :: failures

        failures

      . assert(_ == Nil)

      test(m"JavaBaseRegex and Re2 engines agree on capture groups"):
        var failures: List[Text] = Nil

        def strip(result: Option[Array[List[Text | Char] | Optional[Text | Char]]^{}])
        :   Option[List[Any]] =
          result.map: (groups: Array[List[Text | Char] | Optional[Text | Char]]^{}) =>
            proscenium.List.from(groups.readable.toList)

        patterns.each: pattern =>
          val regex = Regex.parse(List(pattern))

          inputs.each: input =>
            val jvmResult = strip(JavaBaseRegex.engine.matchGroups(regex, input)(using Scanner(Unset)))

            val re2Result =
              strip(Regex.Engine.re2.matchGroups(regex, input)(using Scanner(Unset)))

            if jvmResult != re2Result
            then failures = (pattern.s+s" on ${input.s}").tt :: failures

        failures

      . assert(_ == Nil)
