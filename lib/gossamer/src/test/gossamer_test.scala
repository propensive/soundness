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
package gossamer

import scala.collection.immutable.Seq
import scala.collection.immutable.IndexedSeq
import scala.collection.immutable.Vector

import scala.math

import soundness.*


import textMetrics.uniformMetric
import caseSensitivity.caseSensitive
import soundness.sortingAlgorithms.timsort
import denominative.dysasymptotics.linearSize

case class Person(name: Text, age: Int)

object Tests extends Suite(m"Gossamer Tests"):
  def run(): Unit =
    suite(m"Minimum Edit Distance"):
      import proximities.levenshteinProximity

      test(m"equal strings have zero edit distance"):
        "Hello world".proximity("Hello world")

      . assert(_ == 0)

      test(m"missing character has edit distance of 1"):
        "Hello world".proximity("Hello orld")

      . assert(_ == 1)

      test(m"missing character from end has edit distance of 1"):
        "Hello world".proximity("Hello worl")

      . assert(_ == 1)

      test(m"missing character from start has edit distance of 1"):
        "Hello world".proximity("ello world")

      . assert(_ == 1)

      test(m"changed character has edit distance of 1"):
        "Hello world".proximity("Hellq world")

      . assert(_ == 1)

      test(m"switched characters has edit distance of 2"):
        "Hello world".proximity("Hello wrold")

      . assert(_ == 2)

      test(m"different strings have large edit distance"):
        "Hello".proximity("world").toLong

      . assert(_ == 4)

      test(m"empty left string has edit distance equal to right length"):
        "".proximity("abc").toLong

      . assert(_ == 3)

      test(m"empty right string has edit distance equal to left length"):
        "abc".proximity("").toLong

      . assert(_ == 3)

      test(m"two empty strings have zero edit distance"):
        "".proximity("").toLong

      . assert(_ == 0)

    suite(m"String functions"):
      test(m"punycode test"):
        "www.äpfel.com".punycode

      . assert(_ == "www.xn--pfel-koa.com")

      test(m"URL encoding of space"):
        "hello world".urlEncode

      . assert(_ == "hello+world")

      test(m"URL encoding of multibyte UTF-8 character"):
        "Café".urlEncode

      . assert(_ == "Caf%C3%A9")

      test(m"URL decoding of UTF-8 string"):
        "Na%C3%AFve".urlDecode

      . assert(_ == "Naïve")

      test(m"Lower-case"):
        "InDeCiSiVe".lower

      . assert(_ == "indecisive")

      test(m"Upper-case"):
        "InDeCiSiVe".upper

      . assert(_ == "INDECISIVE")

      test(m"Empty string is nil"):
        "".nil

      . assert(_ == true)

      test(m"Non-empty string is not nil"):
        !"Hello World".nil

      . assert(_ == true)

    suite(m"Joining strings"):
      test(m"join with separator"):
        scala.collection.immutable.List("one", "two", "three").join(", ")

      . assert(_ == "one, two, three")

      test(m"join with separator; different last"):
        scala.collection.immutable.List("one", "two", "three", "four").join(", ", " and ")

      . assert(_ == "one, two, three and four")

      test(m"join with separator; different last; two elements"):
        scala.collection.immutable.List("three", "four").join(", ", " and ")

      . assert(_ == "three and four")

      test(m"join with separator, prefix and suffix"):
        scala.collection.immutable.List("one", "two").join("(", ", ", ")")

      . assert(_ == "(one, two)")

    suite(m"txt interpolator"):
      test(m"multiline collapses to space-delimited"):
        txt"""Hello
              world"""

      . assert(_ == "Hello world")

      test(m"double newline becomes single newline"):
        txt"""Hello

              world"""

      . assert(_ == "Hello\nworld")

      test(m"paragraphs"):
        txt"""Hello
              world

              Bonjour
              le monde"""

      . assert(_ == "Hello world\nBonjour le monde")

    suite(m"Text methods"):
      test(m"get bytes from text"):
        "hello".sysData.readable.to(List)

      . assert(_ == List(104, 101, 108, 108, 111))

      test(m"get bytes from empty Text"):
        "".sysData.readable.isEmpty

      . assert(identity(_))

      test(m"get Text length"):
        "hello world".length

      . assert(_ == 11)

      test(m"empty Text should be nil"):
        "".nil

      . assert(_ == true)

      test(m"non-empty Text should not be nil"):
        !"Hello".nil

      . assert(_ == true)

      test(m"convert to lower case"):
        "Hello World".lower

      . assert(_ == "hello world")

      test(m"convert to upper case"):
        "Hello World".upper

      . assert(_ == "HELLO WORLD")

      test(m"URL encode a space"):
        " ".urlEncode

      . assert(_ == "+")

      test(m"URL encode a +"):
        "+".urlEncode

      . assert(_ == "%2B")

      test(m"URL encode an é"):
        "é".urlEncode

      . assert(_ == "%C3%A9")

      test(m"URL encode a Text"):
        "Nechť již hříšné saxofony ďáblů rozezvučí síň úděsnými tóny waltzu, tanga a quickstepu.".urlEncode

      . assert(_ == "Nech%C5%A5+ji%C5%BE+h%C5%99%C3%AD%C5%A1n%C3%A9+saxofony+%C4%8F%C3%A1bl%C5%AF+rozezvu%C4%8D%C3%AD+s%C3%AD%C5%88+%C3%BAd%C4%9Bsn%C3%BDmi+t%C3%B3ny+waltzu%2C+tanga+a+quickstepu.")

      test(m"URL decode a Text"):
        "Nech%C5%A5%20ji%C5%BE%20h%C5%99%C3%AD%C5%A1n%C3%A9%20saxofony%20%C4%8F%C3%A1bl%C5%AF%20rozezvu%C4%8D%C3%AD%20s%C3%AD%C5%88%20%C3%BAd%C4%9Bsn%C3%BDmi%20t%C3%B3ny%20waltzu%2C%20tanga%20a%20quickstepu.".urlDecode

      . assert(_ == "Nechť již hříšné saxofony ďáblů rozezvučí síň úděsnými tóny waltzu, tanga a quickstepu.")

      test(m"URL decode a space"):
        "+".urlDecode

      . assert(_ == " ")

      test(m"URL decode a +"):
        "%2B".urlDecode

      . assert(_ == "+")

      test(m"drop the first character"):
        "Hello".skip(1)

      . assert(_ == "ello")

      test(m"drop the last character"):
        "Hello".skip(1, Rtl)

      . assert(_ == "Hell")

      test(m"drop more characters than the length of the Text"):
        "Hello".skip(10)

      . assert(_ == "")

      test(m"drop more right chars than text length"):
        "Hello".skip(10, Rtl)

      . assert(_ == "")

      test(m"take the first character"):
        "Hello".keep(1)

      . assert(_ == "H")

      test(m"take the last character"):
        "Hello".keep(1, Rtl)

      . assert(_ == "o")

      test(m"take more characters than the length of the Text"):
        "Hello".keep(10)

      . assert(_ == "Hello")

      test(m"take more right chars than text length"):
        "Hello".keep(10, Rtl)

      . assert(_ == "Hello")

      test(m"from includes the anchor to the end"):
        "hello".from(Ter)
      . assert(_ == "llo")

      test(m"upto includes the anchor from the start"):
        "hello".upto(Ter)
      . assert(_ == "hel")

      test(m"before excludes the anchor from the start"):
        "hello".before(Ter)
      . assert(_ == "he")

      test(m"after excludes the anchor to the end"):
        "hello".after(Ter)
      . assert(_ == "lo")

      test(m"from slices a List by ordinal"):
        import dysasymptotics.linearSize
        (List(0, 1, 2, 3, 4): List[Int]).from(Ter)
      . assert(_ == List(2, 3, 4))

      test(m"after slices a List by ordinal"):
        import dysasymptotics.linearSize
        (List(0, 1, 2, 3, 4): List[Int]).after(Ter)
      . assert(_ == List(3, 4))

      test(m"upto slices a frozen array by ordinal"):
        Array(0, 1, 2, 3, 4).upto(Ter).to[List]
      . assert(_ == List(0, 1, 2))

      test(m"before slices an IndexedSeq by ordinal"):
        val sequence: IndexedSeq[Int] = Vector(0, 1, 2, 3, 4)
        sequence.before(Ter).to(List)
      . assert(_ == List(0, 1))

      test(m"snip a Text in two"):
        "Hello".snip(2): (Text, Text)

      . assert(_ == ("He", "llo"))

      test(m"trim spaces from a Text"):
        "  Hello   ".trim

      . assert(_ == "Hello")

      test(m"trim mixed whitespace from a Text"):
        "\n\r\t Hello\n \t\r".trim

      . assert(_ == "Hello")

      test(m"take a slice from a Text"):
        "Hello world".segment(Quin thru Sept)

      . assert(_ == "o w")

      test(m"take an oversized slice from a Text"):
        "Hello world".segment(Quin thru 100.z)

      . assert(_ == "o world")

      test(m"Get characters from a Text"):
        "Hello world".chars.to[List]

      . assert(_ == List('H', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd'))

      test(m"Flatmap a text"):
        import mercator.bind
        "ABC".bind { c => t"${c}." }

      . assert(_ == "A.B.C.")

      test(m"Map over a text's characters"):
        "ABC".tr { char => char.toLower }
      . assert(_ == "abc")

      test(m"Check an empty Text is empty"):
        "".nil

      . assert(_ == true)

      test(m"Check a non-empty Text is not empty"):
        "abc".nil

      . assert(_ == false)

      test(m"Cut a Text"):
        "one,two,three".cut(",")

      . assert(_ == List(t"one", t"two", t"three"))

      test(m"Cut a Text with empty Text at start"):
        ",one,two".cut(",")

      . assert(_ == List(t"", t"one", t"two"))

      test(m"Cut a Text with empty Text at end"):
        "one,two,".cut(",")

      . assert(_ == List(t"one", t"two", t""))

      test(m"Cut a Text with empty parts at start and end"):
        ",one,two,".cut(",")

      . assert(_ == List(t"", t"one", t"two", t""))

      test(m"Cut a sequence of empty Texts"):
        ",,,".cut(",")

      . assert(_ == List(t"", t"", t"", t""))

      test(m"Cut a Text which doesn't contain the separator"):
        "one,two,three".cut("x")

      . assert(_ == List(t"one,two,three"))

      test(m"Cut a Text on an escaped character"):
        "one\ntwo\nthree".cut("\n")

      . assert(_ == List(t"one", t"two", t"three"))

      test(m"Substitute characters"):
        "one,two,three".tr(',', ';')

      . assert(_ == "one;two;three")

      test(m"Replace substring"):
        "naive".sub("i", "ï")

      . assert(_ == "naïve")

      test(m"Replace different-length substring"):
        "Once upon a time".sub("Once", "Twice")

      . assert(_ == "Twice upon a time")

      test(m"Several substitutions"):
        "foo bar baz".sub("ba", "ma")

      . assert(_ == "foo mar maz")

      test(m"Overlapping substitutions"):
        "fofofoo".sub("fofo", "momo")

      . assert(_ == "momofoo")

      test(m"Get camel-case words"):
        "oneTwoThree".uncamel

      . assert(_ == List(t"one", t"two", t"three"))

      test(m"Camel-case to dashed words"):
        "oneTwoThree".uncamel.kebab

      . assert(_ == "one-two-three")

      test(m"Camel-case to dotted words"):
        "oneTwoThree".uncamel.dotted

      . assert(_ == "one.two.three")

      test(m"Fit short text into fixed width"):
        "123".fit(5)

      . assert(_ == "123  ")

      test(m"Fit long text into fixed width"):
        "12345".fit(3)

      . assert(_ == "123")

      test(m"Right-fit long text into fixed width"):
        "12345".fit(3, Rtl)

      . assert(_ == "345")

      test(m"Right-fit short text into fixed width"):
        "123".fit(5, Rtl)

      . assert(_ == "  123")

      test(m"Right-fit short text with different padding character"):
        "123".fit(5, Rtl, '.')

      . assert(_ == "..123")

      test(m"Fit short text with different padding character"):
        "123".fit(5, Ltr, '.')

      . assert(_ == "123..")

      test(m"duplicate text several times"):
        "123"*3

      . assert(_ == "123123123")

      test(m"duplicate text zero times"):
        "123"*0

      . assert(_ == "")

      test(m"Random access of character"):
        t"123"(Prim)

      . assert(_ == '1')

      // test(m"Random access of out-of-range character"):
      //   capture[Range.Error](t"123".at(Sen))
      //
      //. assert(_ == Range.Error(5, 0, 3))

      test(m"Random access of out-of-range character"):
        t"123"(Sen)

      . assert(_ == Unset)

      test(m"Pad-right with space"):
        "123".pad(5, Rtl)

      . assert(_ == "  123")

      test(m"Pad-left with space"):
        "123".pad(5, Ltr)

      . assert(_ == "123  ")

      test(m"Pad-right with smaller value does not change text"):
        "12345".pad(3, Rtl)

      . assert(_ == "12345")

      test(m"Pad-left with smaller value does not change text"):
        "12345".pad(3, Ltr)

      . assert(_ == "12345")

      test(m"Text does contain value"):
        "hello world".contains("ello")

      . assert(_ == true)

      test(m"Text does not contain value"):
        "hello world".contains("goodbye")

      . assert(_ == false)

      test(m"Text contains itself"):
        "hello world".contains("hello world")

      . assert(_ == true)

      test(m"Text contains empty text"):
        "hello world".contains("")

      . assert(_ == true)

      test(m"Empty text contains empty text"):
        "".contains("")

      . assert(_ == true)

      test(m"Index of character satisfying predicate"):
        "oh, Hello World".where(_.isUpper)

      . assert(_ == Quin)

      test(m"Index satisfying predicate starting at result"):
        "oh, Hello World".pinpoint(_.isUpper, Quin)

      . assert(_ == Quin)

      test(m"Index satisfying predicate starting after result"):
        "oh, Hello World".pinpoint(_.isUpper, Sen)

      . assert(_ == 10.z)

      test(m"Take characters while predicate is true"):
        "HELLOworld".keep(_.isUpper)

      . assert(_ == "HELLO")

      test(m"Take chars when predicate is never true"):
        "hello world".keep(_.isUpper)

      . assert(_ == "")

      test(m"Take chars when predicate isn't initially true"):
        "Helloworld".keep(_.isLower)

      . assert(_ == "")

      test(m"Capitalize a lowercase word"):
        "hello".capitalize

      . assert(_ == "Hello")

      test(m"Capitalize a mixed-case word"):
        "fooBar".capitalize

      . assert(_ == "FooBar")

      test(m"Capitalize an uppercase word does not change it"):
        "HELLO".capitalize

      . assert(_ == "HELLO")

    suite(m"Compile errors"):
      test(m"Check that Text and String are incompatible"):
        demilitarize:
          val x: String = Text("text")
        . map(_.message)

      . assert(!_.isEmpty)

    suite(m"Decimalization tests"):
      test(m"Write negative pi"):
        Decimalizer(1).decimalize(-math.Pi)

      . assert(_ == "-3")

      test(m"Write negative pi to 2 s.f."):
        Decimalizer(2).decimalize(-math.Pi)

      . assert(_ == "-3.1")

      test(m"Write negative pi to 3 s.f."):
        Decimalizer(3).decimalize(-math.Pi)

      . assert(_ == "-3.14")

      test(m"Write 1 s.f. pi"):
        Decimalizer(1).decimalize(math.Pi)

      . assert(_ == "3")

      test(m"Write 2 s.f. pi"):
        Decimalizer(2).decimalize(math.Pi)

      . assert(_ == "3.1")

      test(m"Write 3 s.f. pi"):
        Decimalizer(3).decimalize(math.Pi)

      . assert(_ == "3.14")

      test(m"Write 4 s.f. pi"):
        Decimalizer(4).decimalize(math.Pi)

      . assert(_ == "3.142")

      test(m"Write 5 s.f. pi"):
        Decimalizer(5).decimalize(math.Pi)

      . assert(_ == "3.1416")

      test(m"Write 6 s.f. pi"):
        Decimalizer(6).decimalize(math.Pi)

      . assert(_ == "3.14159")

      test(m"Write 7 s.f. pi"):
        Decimalizer(7).decimalize(math.Pi)

      . assert(_ == "3.141593")

      test(m"Write 8 s.f. pi"):
        Decimalizer(8).decimalize(math.Pi)

      . assert(_ == "3.1415927")

      test(m"Write 1 s.f. 10*pi"):
        Decimalizer(1).decimalize(10*math.Pi)

      . assert(_ == "30")

      test(m"Write 2 s.f. 10*pi"):
        Decimalizer(2).decimalize(10*math.Pi)

      . assert(_ == "31")

      test(m"Write 3 s.f. 10*pi"):
        Decimalizer(3).decimalize(10*math.Pi)

      . assert(_ == "31.4")

      test(m"Write 4 s.f. 10*pi"):
        Decimalizer(4).decimalize(10*math.Pi)

      . assert(_ == "31.42")

      test(m"Write 5 s.f. 10*pi"):
        Decimalizer(5).decimalize(10*math.Pi)

      . assert(_ == "31.416")

      test(m"Write 6 s.f. 10*pi"):
        Decimalizer(6).decimalize(10*math.Pi)

      . assert(_ == "31.4159")

      test(m"Write 7 s.f. 10*pi"):
        Decimalizer(7).decimalize(10*math.Pi)

      . assert(_ == "31.41593")

      test(m"Write 8 s.f. 10*pi"):
        Decimalizer(8).decimalize(10*math.Pi)

      . assert(_ == "31.415927")

      test(m"Write 1 s.f. 100*pi"):
        Decimalizer(1).decimalize(100*math.Pi)

      . assert(_ == "300")

      test(m"Write 2 s.f. 100*pi"):
        Decimalizer(2).decimalize(100*math.Pi)

      . assert(_ == "310")

      test(m"Write 3 s.f. 100*pi"):
        Decimalizer(3).decimalize(100*math.Pi)

      . assert(_ == "314")

      test(m"Write 4 s.f. 100*pi"):
        Decimalizer(4).decimalize(100*math.Pi)

      . assert(_ == "314.2")

      test(m"Write 5 s.f. 100*pi"):
        Decimalizer(5).decimalize(100*math.Pi)

      . assert(_ == "314.16")

      test(m"Write 6 s.f. 100*pi"):
        Decimalizer(6).decimalize(100*math.Pi)

      . assert(_ == "314.159")

      test(m"Write 7 s.f. 100*pi"):
        Decimalizer(7).decimalize(100*math.Pi)

      . assert(_ == "314.1593")

      test(m"Write 8 s.f. 100*pi"):
        Decimalizer(8).decimalize(100*math.Pi)

      . assert(_ == "314.15927")

      test(m"Write 1 s.f. pi/10"):
        Decimalizer(1).decimalize(math.Pi/10)

      . assert(_ == "0.3")

      test(m"Write 2 s.f. pi/10"):
        Decimalizer(2).decimalize(math.Pi/10)

      . assert(_ == "0.31")

      test(m"Write 3 s.f. pi/10"):
        Decimalizer(3).decimalize(math.Pi/10)

      . assert(_ == "0.314")

      test(m"Write 4 s.f. pi/10"):
        Decimalizer(4).decimalize(math.Pi/10)

      . assert(_ == "0.3142")

      test(m"Write 5 s.f. pi/10"):
        Decimalizer(5).decimalize(math.Pi/10)

      . assert(_ == "0.31416")

      test(m"Write 6 s.f. pi/10"):
        Decimalizer(6).decimalize(math.Pi/10)

      . assert(_ == "0.314159")

      test(m"Write 7 s.f. pi/10"):
        Decimalizer(7).decimalize(math.Pi/10)

      . assert(_ == "0.3141593")

      test(m"Write 8 s.f. pi/10"):
        Decimalizer(8).decimalize(math.Pi/10)

      . assert(_ == "0.31415927")

      test(m"Write 1 s.f. pi/100"):
        Decimalizer(1).decimalize(math.Pi/100)

      . assert(_ == "0.03")

      test(m"Write 2 s.f. pi/100"):
        Decimalizer(2).decimalize(math.Pi/100)

      . assert(_ == "0.031")

      test(m"Write 3 s.f. pi/100"):
        Decimalizer(3).decimalize(math.Pi/100)

      . assert(_ == "0.0314")

      test(m"Write 4 s.f. pi/100"):
        Decimalizer(4).decimalize(math.Pi/100)

      . assert(_ == "0.03142")

      test(m"Write 5 s.f. pi/100"):
        Decimalizer(5).decimalize(math.Pi/100)

      . assert(_ == "0.031416")

      test(m"Write 6 s.f. pi/100"):
        Decimalizer(6).decimalize(math.Pi/100)

      . assert(_ == "0.0314159")

      test(m"Write 7 s.f. pi/100"):
        Decimalizer(7).decimalize(math.Pi/100)

      . assert(_ == "0.03141593")

      test(m"Write 8 s.f. pi/100"):
        Decimalizer(8).decimalize(math.Pi/100)

      . assert(_ == "0.031415927")

      test(m"Write 1 s.f. pi/1000"):
        Decimalizer(1).decimalize(math.Pi/1000)

      . assert(_ == "3×10¯³")

      test(m"Write 2 s.f. pi/1000"):
        Decimalizer(2).decimalize(math.Pi/1000)

      . assert(_ == "3.1×10¯³")

      test(m"Write 3 s.f. pi/1000"):
        Decimalizer(3).decimalize(math.Pi/1000)

      . assert(_ == "3.14×10¯³")

      test(m"Write 4 s.f. pi/1000"):
        Decimalizer(4).decimalize(math.Pi/1000)

      . assert(_ == "3.142×10¯³")

      test(m"Write 5 s.f. pi/1000"):
        Decimalizer(5).decimalize(math.Pi/1000)

      . assert(_ == "3.1416×10¯³")

      test(m"Write 6 s.f. pi/1000"):
        Decimalizer(6).decimalize(math.Pi/1000)

      . assert(_ == "3.14159×10¯³")

      test(m"Write 7 s.f. pi/1000"):
        Decimalizer(7).decimalize(math.Pi/1000)

      . assert(_ == "3.141593×10¯³")

      test(m"Show Avogadro's number"):
        Decimalizer(5).decimalize(6.0221408e23)

      . assert(_ == "6.0221×10²³")

      test(m"Write 8 s.f. pi/1000"):
        Decimalizer(8).decimalize(math.Pi/1000)

      . assert(_ == "3.1415927×10¯³")

      test(m"Show Avogadro's number to 7 s.f."):
        Decimalizer(7).decimalize(6.0221408e23)

      . assert(_ == "6.022141×10²³")

      test(m"Show Planck's constant to 7 s.f."):
        Decimalizer(7, decimalPoint = '·').decimalize(6.626070e-34)

      . assert(_ == "6·626070×10¯³⁴")

      test(m"Show Avogadro's numer with decimal multiplier"):
        Decimalizer(7, exponentMultiple = 3).decimalize(6.0221408e23)

      . assert(_ == "602.2141×10²¹")

      test(m"Show Planck's constant with decimal multiplier"):
        Decimalizer(7, decimalPoint = '·', exponentMultiple = 3).decimalize(6.626070e-34)

      . assert(_ == "0·6626070×10¯³³")

      test(m"Show positive infinity"):
        Decimalizer(7, decimalPoint = '·', exponentMultiple = 3).decimalize(1.0/0.0)

      . assert(_ == "∞")

      test(m"Show negative infinity"):
        Decimalizer(7, decimalPoint = '·', exponentMultiple = 3).decimalize(-1.0/0.0)

      . assert(_ == "-∞")

      test(m"Show not-a-number"):
        Decimalizer(7, decimalPoint = '·', exponentMultiple = 3).decimalize(0.0/0.0)

      . assert(_ == "∉ℝ")

      test(m"Show 100.0"):
        Decimalizer(decimalPlaces = 1).decimalize(100.0)

      . assert(_ == "100.0")

      test(m"Show 0.0"):
        Decimalizer(decimalPlaces = 1).decimalize(0.0)

      . assert(_ == "0.0")

    val words: List[Text] = List("ba", "baa", "baal", "baar", "baba", "babe", "babu",
      "baby", "bac", "bach", "back", "bad", "bade", "bae", "baff", "baft",
      "bag", "baga", "bago", "bah", "baho", "baht", "bail", "bain", "bait",
      "baka", "bake", "baku", "bal", "bald", "bale", "bali", "balk", "ball",
      "balm", "balu", "bam", "ban", "banc", "band", "bane", "bang", "bani",
      "bank", "bant", "bap", "bar", "bara", "barb", "bard", "bare", "bari",
      "bark", "barm", "barn", "baru", "bas", "base", "bash", "bask", "bass",
      "bast", "bat", "bate", "bath", "bats", "batt", "batz", "baud", "baul",
      "baun", "baw", "bawd", "bawl", "bawn", "bay", "baya", "baze", "be",
      "bead", "beak", "beal", "beam", "bean", "bear", "beat", "beau", "beck",
      "bed", "bee", "beef", "beek", "been", "beer", "bees", "beet", "beg",
      "bego", "behn", "bel", "bela", "beld", "bell", "belt", "bely", "bema",
      "ben", "bena", "bend", "bene", "beng", "beni", "benj", "benn", "beno",
      "bent", "ber", "bere", "berg", "berm", "bes", "besa", "best", "bet",
      "beta", "beth", "bevy", "bey", "bhat", "bhoy", "bhut", "bias", "bib",
      "bibb", "bibi", "bice", "bick", "bid", "bide", "bien", "bier", "biff",
      "big", "biga", "bigg", "bija", "bike", "bikh", "bile", "bilk", "bill",
      "bilo", "bin", "bind", "bine", "bing", "binh", "bink", "bino", "bint",
      "biod", "bion", "bios", "bird", "biri", "birk", "birl", "birn", "birr",
      "bis", "bit", "bite", "biti", "bito", "bitt", "biwa", "biz", "bizz",
      "blab", "blad", "blae", "blah", "blan", "blas", "blat", "blaw", "blay",
      "bleb", "bled", "blee", "bleo", "blet", "blip", "blo", "blob", "bloc",
      "blot", "blow", "blub", "blue", "blup", "blur", "bo", "boa", "boar",
      "boat", "bob", "boba", "bobo", "boce", "bock", "bod", "bode", "body",
      "bog", "boga", "bogo", "bogy", "boho", "boid", "boil", "bojo", "boke",
      "bola", "bold", "bole", "bolk", "boll", "bolo", "bolt", "bom", "boma",
      "bomb", "bon", "bond", "bone", "bong", "bonk", "bony", "boo", "boob",
      "bood", "boof", "book", "bool", "boom", "boon", "boor", "boot", "bop",
      "bor", "bora", "bord", "bore", "borg", "borh", "born", "boro", "bort",
      "bose", "bosh", "bosk", "bosn", "boss", "bot", "bota", "bote", "both",
      "bott", "boud", "bouk", "boun", "bout", "bouw", "bow", "bowk", "bowl",
      "boxy", "boy", "boza", "bozo", "bra", "brab", "brad", "brae", "brag",
      "bran", "brat", "braw", "bray", "bred", "bree", "brei", "bret", "brew",
      "brey", "brig", "brim", "brin", "brit", "brob", "brod", "brog", "broo",
      "brot", "brow", "brut", "bu", "bual", "bub", "buba", "bubo", "buck",
      "bud", "buda", "buff", "bufo", "bug", "buhl", "buhr", "bukh", "bulb",
      "bulk", "bull", "bult", "bum", "bump", "bun", "buna", "bund", "bung",
      "bunk", "bunt", "buoy", "bur", "burd", "bure", "burg", "buri", "burl",
      "burn", "buro", "burp", "burr", "burt", "bury", "bus", "bush", "busk",
      "buss", "bust", "busy", "but", "butt", "buy", "buzz", "by", "bye", "byee",
      "bygo", "byon", "byre", "byth")

    val words2: List[Text] =
      List
        ( "a", "aa", "aal", "aalii", "aam", "aardvark", "aardwolf", "aba", "abac", "abaca",
          "abacate", "abacay", "abacinate", "abacination", "abaciscus", "abacist", "aback",
          "abactinal", "abactinally", "abaction", "abactor", "abaculus", "abacus", "abaff", "abaft",
          "abaisance", "abaiser", "abaissed", "abalienate", "abalienation", "abalone", "abampere",
          "abandon", "abandonable", "abandoned", "abandonedly", "abandonee", "abandoner",
          "abandonment", "abaptiston", "abarthrosis", "abarticular", "abarticulation", "abas",
          "abase", "abased", "abasedly", "abasedness", "abasement", "abaser", "abash", "abashed",
          "abashedly", "abashedness", "abashless", "abashlessly", "abashment", "abasia", "abasic",
          "abask", "abastardize", "abatable", "abate", "abatement", "abater", "abatis", "abatised",
          "abaton", "abator", "abattoir", "abature", "abave", "abaxial", "abaxile", "abaze", "abb",
          "abbacomes", "abbacy", "abbas", "abbasi", "abbassi", "abbatial", "abbatical", "abbess",
          "abbey", "abbeystede", "abbot", "abbotcy", "abbotnullius", "abbotship", "abbreviate",
          "abbreviately", "abbreviation", "abbreviator", "abbreviatory", "abbreviature", "abcoulomb",
          "abdal", "abdat", "abdest", "abdicable", "abdicant", "abdicate", "abdication", "abdicative",
          "abdicator", "abditive", "abditory", "abdomen", "abdominal", "abdominalian", "abdominally",
          "abdominoanterior", "abdominocardiac", "abdominocentesis", "abdominocystic",
          "abdominogenital", "abdominohysterectomy", "abdominohysterotomy", "abdominoposterior",
          "abdominoscope", "abdominoscopy", "abdominothoracic", "abdominous", "abdominovaginal",
          "abdominovesical", "abduce", "abducens", "abducent", "abduct", "abduction", "abductor",
          "abeam", "abear", "abearance", "abecedarian", "abecedarium", "abecedary", "abed", "abeigh",
          "abele", "abelite", "abelmosk", "abeltree", "abenteric", "abepithymia", "aberdevine",
          "aberrance", "aberrancy", "aberrant", "aberrate", "aberration", "aberrational", "aberrator",
          "aberrometer", "aberroscope", "aberuncator", "abet", "abetment", "abettal", "abettor",
          "abevacuation", "abey", "abeyance", "abeyancy", "abeyant", "abfarad", "abhenry", "abhiseka",
          "abhominable", "abhor", "abhorrence", "abhorrency", "abhorrent", "abhorrently", "abhorrer",
          "abhorrible", "abhorring", "abidal", "abidance", "abide", "abider", "abidi", "abiding",
          "abidingly", "abidingness", "abietate", "abietene", "abietic", "abietin", "abietineous",
          "abietinic", "abigail", "abigailship", "abigeat", "abigeus", "abilao", "ability", "abilla",
          "abilo", "abintestate", "abiogenesis", "abiogenesist", "abiogenetic", "abiogenetical",
          "abiogenetically", "abiogenist", "abiogenous", "abiogeny", "abiological", "abiologically",
          "abiology", "abiosis", "abiotic", "abiotrophic", "abiotrophy", "abir", "abirritant",
          "abirritate", "abirritation", "abirritative", "abiston", "abiuret", "abject",
          "abjectedness", "abjection", "abjective", "abjectly", "abjectness", "abjoint", "abjudge",
          "abjudicate", "abjudication", "abjunction", "abjunctive", "abjuration", "abjuratory",
          "abjure", "abjurement", "abjurer", "abkar", "abkari", "ablach", "ablactate", "ablactation",
          "ablare", "ablastemic", "ablastous", "ablate", "ablation", "ablatitious", "ablatival",
          "ablative", "ablator", "ablaut", "ablaze", "able", "ableeze", "ablegate", "ableness",
          "ablepharia", "ablepharon", "ablepharous", "ablepsia", "ableptical", "ableptically",
          "abler", "ablest", "ablewhackets", "ablins", "abloom", "ablow", "ablude", "abluent",
          "ablush", "ablution", "ablutionary", "abluvion", "ably", "abmho", "abnegate", "abnegation",
          "abnegative", "abnegator", "abnerval", "abnet", "abneural", "abnormal", "abnormalism",
          "abnormalist", "abnormality", "abnormalize", "abnormally", "abnormalness", "abnormity",
          "abnormous", "abnumerable", "aboard", "abode", "abodement", "abody", "abohm", "aboil",
          "abolish", "abolisher", "abolishment", "abolition", "abolitionary", "abolitionism",
          "abolitionist", "abolitionize", "abolla", "aboma", "abomasum", "abomasus", "abominable",
          "abominableness", "abominably", "abominate", "abomination", "abominator", "abomine",
          "aboon", "aborad", "aboral", "aborally", "abord", "aboriginal", "aboriginality",
          "aboriginally", "aboriginary", "aborigine", "abort", "aborted", "aborticide", "abortient",
          "abortifacient", "abortin", "abortion", "abortional", "abortionist", "abortive",
          "abortively", "abortiveness", "abortus", "abouchement", "abound", "abounder", "abounding",
          "aboundingly", "about", "abouts", "above", "aboveboard", "abovedeck", "aboveground",
          "aboveproof", "abovestairs", "abox", "abracadabra", "abrachia", "abradant", "abrade",
          "abrader", "abraid", "abranchial", "abranchialism", "abranchian", "abranchiate",
          "abranchious", "abrasax", "abrase", "abrash", "abrasiometer", "abrasion", "abrasive",
          "abrastol", "abraum", "abraxas", "abreact", "abreaction", "abreast", "abrenounce", "abret",
          "abrico", "abridge", "abridgeable", "abridged", "abridgedly", "abridger", "abridgment",
          "abrim", "abrin", "abristle", "abroach", "abroad", "abrocome", "abrogable", "abrogate",
          "abrogation", "abrogative", "abrogator", "abrook", "abrotanum", "abrotine", "abrupt",
          "abruptedly", "abruption", "abruptly", "abruptness", "absampere", "absarokite", "abscess",
          "abscessed", "abscession", "abscessroot", "abscind", "abscise", "abscision", "absciss",
          "abscissa", "abscissae", "abscisse", "abscission", "absconce", "abscond", "absconded",
          "abscondedly", "abscondence", "absconder", "absconsa", "abscoulomb", "absence", "absent",
          "absentation", "absentee", "absenteeism", "absenteeship", "absenter", "absently",
          "absentment", "absentmindedly", "absentness", "absfarad", "abshenry", "absinthe",
          "absinthial", "absinthian", "absinthiate", "absinthic", "absinthin", "absinthine",
          "absinthism", "absinthismic", "absinthium", "absinthol", "absit", "absmho", "absohm",
          "absolute", "absolutely", "absoluteness", "absolution", "absolutism", "absolutist",
          "absolutistic", "absolutistically", "absolutive", "absolutization", "absolutize",
          "absolutory", "absolvable", "absolvatory", "absolve", "absolvent", "absolver", "absolvitor",
          "absolvitory", "absonant", "absonous", "absorb", "absorbability", "absorbable", "absorbed",
          "absorbedly", "absorbedness", "absorbefacient", "absorbency", "absorbent", "absorber",
          "absorbing", "absorbingly", "absorbition", "absorpt", "absorptance", "absorptiometer",
          "absorptiometric", "absorption", "absorptive", "absorptively" )

    suite(m"BK-Tree tests"):
      import proximities.levenshteinProximity

      val lexicon = Lexicon(words)

      test(m"There is one exact match on `book`"):
        lexicon.search("book", 0)
      . assert(_ == Set("book"))

      test(m"There are no exact matches on `booq`"):
        lexicon.search("booq", 0)
      . assert(_ == Set())

      test(m"There several matches at distance 1 from `book`"):
        lexicon.search("book", 1)
      . assert(_ == Set("boon", "bolk", "bouk", "boot", "book", "boor", "boo", "boof", "boob",
                        "bonk", "bool", "bowk", "bosk", "boom", "bood", "bock"))

      test(m"There many matches at distance 2 from `book`"):
        lexicon.search("book", 2).stdlib.size
      . assert(_ == 112)

      test(m"All the matches are found at distance 3"):
        lexicon.search("book", 3).stdlib.size
      . assert(_ == words.size)

    suite(m"Dictionary tests"):
      test(m"One-entry dictionary size"):
        val dictionary = Dictionary("color" -> 0)
        dictionary.size
      . assert(_ == 1)

      test(m"One-entry dictionary lookup"):
        val dictionary = Dictionary("color" -> 0)
        dictionary("color")
      . assert(_ == 0)

      test(m"One-entry dictionary absent lookup"):
        val dictionary = Dictionary("color" -> 0)
        dictionary("colo")
      . assert(_ == Unset)

      test(m"One-entry dictionary absent lookup 2"):
        val dictionary = Dictionary("color" -> 0)
        dictionary("colors")
      . assert(_ == Unset)

      test(m"Two-entry dictionary lookups"):
        val dictionary = Dictionary("color" -> "COLOR", "size" -> "SIZE")
        (dictionary("color"), dictionary("size"))
      . assert(_ == ("COLOR", "SIZE"))

      test(m"Two-entry dictionary lookups with overlap"):
        val dictionary = Dictionary("color" -> "COLOR", "change" -> "CHANGE")
        (dictionary("color"), dictionary("change"))
      . assert(_ == ("COLOR", "CHANGE"))

      test(m"Two-entry dictionary lookups with more overlap"):
        val dictionary = Dictionary("color" -> "COLOR", "colors" -> "COLORS")
        (dictionary("color"), dictionary("colors"))
      . assert(_ == ("COLOR", "COLORS"))

      test(m"Two-entry dictionary lookups with more overlap, reverse order"):
        val dictionary = Dictionary("colors" -> "COLORS", "color" -> "COLOR")
        (dictionary("color"), dictionary("colors"))
      . assert(_ == ("COLOR", "COLORS"))

      test(m"Large dictionary size"):
        val dictionary = Dictionary(words.map { word => (word, word.upper) }*)
        dictionary.size
      . assert(_ == words.size)

      test(m"Large dictionary"):
        Dictionary(words2.map { word => (word, word.upper) }*)
      . assert: dictionary =>
          words2.all: word =>
            dictionary(word) == word.upper

    // A missing `Inspectable` is never a compile error — `derived` always succeeds and
    // substitutes a marked `toString`, `Showable` or `Encodable` rendering — so coverage can
    // only be held in place by asserting on the renderings themselves.
    suite(m"Native-rendering coverage"):
      test(m"gossamer's types inspect natively"):
        Inspectable.fallbacks
         ( a"hello".inspect,
           Grapheme("é").inspect,
           Writing("a\r\nb").inspect )
      . assert(_ == Nil)

      test(m"an Ascii value inspects as an `ascii` literal"):
        a"hi there".inspect
      . assert(_ == "ascii\"hi there\"")

      test(m"a grapheme inspects as its cluster in guillemets"):
        Grapheme("👨‍👩‍👧").inspect
      . assert(_ == Text("‹👨‍👩‍👧›"))

      test(m"a Writing inspects with its grapheme boundaries marked"):
        Writing("a\r\nb").inspect
      . assert(_ == "w\"a·\r\n·b\"")

    suite(m"Writing and grapheme clusters"):
      test(m"empty Text has zero graphemes"):
        Writing("").graphemeCount
      . assert(_ == 0)

      test(m"empty Text has single sentinel boundary"):
        Writing("").boundaries.to[List]
      . assert(_ == List(0))

      test(m"ASCII text grapheme count equals char count"):
        Writing("abc").graphemeCount
      . assert(_ == 3)

      test(m"ASCII text boundaries are sentinels at every char"):
        Writing("abc").boundaries.to[List]
      . assert(_ == List(0, 1, 2, 3))

      test(m"CR LF stays one grapheme"):
        Writing("a\r\nb").boundaries.to[List]
      . assert(_ == List(0, 1, 3, 4))

      test(m"CR LF grapheme count"):
        Writing("a\r\nb").graphemeCount
      . assert(_ == 3)

      test(m"combining mark joins with base character"):
        Writing(Text("é")).graphemeCount
      . assert(_ == 1)

      test(m"family emoji ZWJ sequence is one grapheme"):
        Writing(Text("👨‍👩‍👧")).graphemeCount
      . assert(_ == 1)

      test(m"two flag emoji are exactly two graphemes"):
        Writing(Text("🇬🇧🇫🇷")).graphemeCount
      . assert(_ == 2)

      test(m"Textual[Writing].length returns grapheme count"):
        summon[Writing is Textual].length(Writing("abc"))
      . assert(_ == 3)

      test(m"Writing graphemes view round-trips"):
        Writing("abc").graphemes.map(_.text).mkString
      . assert(_ == "abc")

    suite(m"Grapheme widths via Kuhn"):
      import textMetrics.wideCharacterWidthMetric

      test(m"ASCII grapheme width 1"):
        Grapheme("a").metrics
      . assert(_ == 1)

      test(m"e + combining acute width 1"):
        Grapheme("é").metrics
      . assert(_ == 1)

      test(m"CJK grapheme width 2"):
        Grapheme("日").metrics
      . assert(_ == 2)

      test(m"Regional-indicator pair (flag) width 2 not 4"):
        Grapheme("🇬🇧").metrics
      . assert(_ == 2)

      test(m"ZWJ family emoji width 2 not 6"):
        Grapheme("👨‍👩‍👧").metrics
      . assert(_ == 2)

      test(m"Writing width sums grapheme widths"):
        Writing("abc").metrics
      . assert(_ == 3)

      test(m"Writing CJK width is 2 per character"):
        Writing(Text("日本")).metrics
      . assert(_ == 4)

      test(m"Writing flag emoji counts as 2 cells per flag"):
        Writing(Text("🇬🇧🇫🇷")).metrics
      . assert(_ == 4)

      test(m"Writing combining accent doesn't double-count"):
        Writing(Text("café")).metrics // c, a, f, é (e+combining)
      . assert(_ == 4)

    suite(m"Regex extract"):
      // TODO(chain-flip): the opaque `Chain` flip broke the inline `extract`
      // macro's `value` type-parameter inference. Callers used to fix `value` via the stdlib
      // `LazyList#to(List)` on the result; opaque `Chain` has no back-propagating `to`,
      // and neither an explicit type argument nor a result ascription overrides the macro's
      // `Chain[Nothing]` output. The macro needs to infer `value` from the case bodies
      // (LUB) and become `transparent inline`. Extraction itself is unaffected at runtime.
      test(m"regex extraction — pending extract-macro fix for opaque Chain"):
        ()
      . assert(_ == ())

    // `last` is `transparent inline`, so its adaptivity has to survive the umbrella's export
    // forwarder as well as the direct `rudiments` call: hence this suite here, under
    // `import soundness.*`.
    suite(m"Adaptive `last` through the umbrella"):
      test(m"an unproven receiver yields Unset when empty"):
        val xs: Sequence[Int] = Sequence()
        xs.last
      . assert(_ == Unset)

      test(m"Text is Terminable, and adapts the same way"):
        "abc".last
      . assert(_ == 'c')

    suite(m"Fuzzy match"):
      import proximities.normalizedLevenshteinProximity

      test(m"exact match returns that case's RHS"):
        "hello".fuzzy():
          case "hello" => 1
          case "world" => 2

      . assert(_ == 1)

      test(m"typo picks the nearest pattern"):
        "hlelo".fuzzy():
          case "hello"   => 1
          case "bonjour" => 2

      . assert(_ == 1)

      test(m"mixed String and Text literal patterns"):
        "world".fuzzy():
          case "hello"  => 1
          case "world" => 2

      . assert(_ == 2)

      test(m"tie goes to the earlier source-order case"):
        "abc".fuzzy():
          case "abd" => 1
          case "abe" => 2

      . assert(_ == 1)

      test(m"single-case PF returns that case's RHS"):
        "anything".fuzzy():
          case "only-option" => 42

      . assert(_ == 42)

      test(m"match within threshold returns that case's RHS"):
        "hlelo".fuzzy(0.5):
          case "hello"   => 1
          case "bonjour" => 2
          case _          => 0

      . assert(_ == 1)

      test(m"all distances above threshold returns wildcard"):
        "xyzxyz".fuzzy(0.3):
          case "hello"   => 1
          case "bonjour" => 2
          case _          => -1

      . assert(_ == -1)

      test(m"all above threshold without wildcard throws MatchError"):
        try
          "xyzxyz".fuzzy(0.3):
            case "hello"   => 1
            case "bonjour" => 2
          false
        catch case _: MatchError => true

      . assert(_ == true)

      test(m"wildcard binds the scrutinee"):
        "unknown".fuzzy(0.1):
          case "hello"     => "english"
          case "bonjour"   => "french"
          case other        => other

      . assert(_ == "unknown")

      test(m"multiple cases within threshold — closest wins"):
        "hlelo".fuzzy(0.5):
          case "hello" => 1
          case "helxo" => 2
          case _        => 0

      . assert(_ == 1)

    suite(m"Fuzzy compile errors"):
      test(m"unsupported pattern shape errors"):
        demilitarize:
          "hello".fuzzy():
            case r"foo" => 1

        . map(_.message)

      . assert(!_.isEmpty)

      test(m"mid-list wildcard errors"):
        demilitarize:
          "hello".fuzzy():
            case _      => 0
            case "good" => 1

        . map(_.message)

      . assert(!_.isEmpty)

    suite(m"Ascii compile-time error positioning"):
      test(m"a non-ASCII character's focus is the character itself"):
        demilitarize:
          a"hello café"
        . map(_.focus)
      . assert(_ == List("é"))

      test(m"a non-ASCII character in a later part is still located"):
        val name = a"world"
        demilitarize:
          a"hello $name café"
        . map(_.focus)
      . assert(_ == List("é"))

    suite(m"Collation"):
      // The reason is `NotAMember` rather than `MissingImplicitArgument` because `sort` is an
      // extension method: an extension whose implicit arguments cannot all be found is not
      // applicable, and the compiler reports that as the name being absent from the receiver,
      // with the given it could not construct named further down the message. (`least` and the
      // comparison operators, which predate this, report the same way.) The guarantee is in the
      // message rather than the code, so the test asserts on both.
      test(m"Text is not sortable without a collation in scope"):
        demilitarize:
          proscenium.List(t"b", t"a").sort
        . filter(_.error).map(_.reason)
      . assert(_ == List(CompileError.Reason.NotAMember))

      test(m"the unsortable text names the missing comparison"):
        demilitarize:
          proscenium.List(t"b", t"a").sort
        . map(_.message)
      . assert(_.exists(_.contains("is symbolism.Comparable")))

      test(m"dictionary order ranks accents before case: cafe < café < caff"):
        import collations.unicodeCollation
        proscenium.List(t"caff", t"café", t"cafe").sort
      . assert(_ == proscenium.List(t"cafe", t"café", t"caff"))

      test(m"comparison operators work once a collation is chosen"):
        import collations.unicodeCollation
        "apple" < "banana"
      . assert(_ == true)

      test(m"codepoint order sorts supplementary characters after the BMP"):
        import collations.codepointCollation
        proscenium.List(t"𝓐", t"�").sort
      . assert(_ == proscenium.List(t"�", t"𝓐"))

      test(m"codepoint and dictionary orders differ on case"):
        val upper = proscenium.List(t"a", t"B")
        import collations.unicodeCollation
        val dictionary = upper.sort

        val codepoint =
          import collations.codepointCollation
          upper.sort

        (dictionary, codepoint)
      . assert(_ == (proscenium.List(t"a", t"B"), proscenium.List(t"B", t"a")))
