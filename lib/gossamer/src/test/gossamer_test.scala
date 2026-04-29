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
┃    Soundness, version 0.54.0.                                                                    ┃
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

import soundness.*

import textMetrics.uniform
import caseSensitivity.sensitive

case class Person(name: Text, age: Int)

object Tests extends Suite(m"Gossamer Tests"):
  def run(): Unit =
    suite(m"Minimum Edit Distance"):
      import proximities.levenshteinDistance

      test(m"equal strings have zero edit distance"):
        t"Hello world".proximity(t"Hello world")

      . assert(_ == 0)

      test(m"missing character has edit distance of 1"):
        t"Hello world".proximity(t"Hello orld")

      . assert(_ == 1)

      test(m"missing character from end has edit distance of 1"):
        t"Hello world".proximity(t"Hello worl")

      . assert(_ == 1)

      test(m"missing character from start has edit distance of 1"):
        t"Hello world".proximity(t"ello world")

      . assert(_ == 1)

      test(m"changed character has edit distance of 1"):
        t"Hello world".proximity(t"Hellq world")

      . assert(_ == 1)

      test(m"switched characters has edit distance of 2"):
        t"Hello world".proximity(t"Hello wrold")

      . assert(_ == 2)

      test(m"different strings have large edit distance"):
        t"Hello".proximity(t"world").toLong

      . assert(_ == 4)

    suite(m"String functions"):
      test(m"punycode test"):
        t"www.äpfel.com".punycode

      . assert(_ == t"www.xn--pfel-koa.com")

      test(m"URL encoding of space"):
        t"hello world".urlEncode

      . assert(_ == t"hello+world")

      test(m"URL encoding of multibyte UTF-8 character"):
        t"Café".urlEncode

      . assert(_ == t"Caf%C3%A9")

      test(m"URL decoding of UTF-8 string"):
        t"Na%C3%AFve".urlDecode

      . assert(_ == t"Naïve")

      test(m"Lower-case"):
        t"InDeCiSiVe".lower

      . assert(_ == t"indecisive")

      test(m"Upper-case"):
        t"InDeCiSiVe".upper

      . assert(_ == t"INDECISIVE")

      test(m"Empty string not occupied"):
        t"".occupied

      . assert(_ == Unset)

      test(m"Non-empty string occupied"):
        t"Hello World".occupied

      . assert(_ == t"Hello World")

    suite(m"Joining strings"):
      test(m"join with separator"):
        List(t"one", t"two", t"three").join(t", ")

      . assert(_ == t"one, two, three")

      test(m"join with separator; different last"):
        List(t"one", t"two", t"three", t"four").join(t", ", t" and ")

      . assert(_ == t"one, two, three and four")

      test(m"join with separator; different last; two elements"):
        List(t"three", t"four").join(t", ", t" and ")

      . assert(_ == t"three and four")

      test(m"join with separator, prefix and suffix"):
        List(t"one", t"two").join(t"(", t", ", t")")

      . assert(_ == t"(one, two)")

    suite(m"txt interpolator"):
      test(m"multiline collapses to space-delimited"):
        txt"""Hello
              world"""

      . assert(_ == t"Hello world")

      test(m"double newline becomes single newline"):
        txt"""Hello

              world"""

      . assert(_ == t"Hello\nworld")

      test(m"paragraphs"):
        txt"""Hello
              world

              Bonjour
              le monde"""

      . assert(_ == t"Hello world\nBonjour le monde")

    suite(m"Text methods"):
      test(m"get bytes from text"):
        t"hello".sysData.to(List)

      . assert(_ == List(104, 101, 108, 108, 111))

      test(m"get bytes from empty Text"):
        t"".sysData.to(List)

      . assert(_.nil)

      test(m"get Text length"):
        t"hello world".length

      . assert(_ == 11)

      test(m"empty Text should not be occupied"):
        t"".occupied

      . assert(_ == Unset)

      test(m"non-empty Text should be occupied"):
        t"Hello".occupied

      . assert(_ == t"Hello")

      test(m"convert to lower case"):
        t"Hello World".lower

      . assert(_ == t"hello world")

      test(m"convert to upper case"):
        t"Hello World".upper

      . assert(_ == t"HELLO WORLD")

      test(m"URL encode a space"):
        t" ".urlEncode

      . assert(_ == t"+")

      test(m"URL encode a +"):
        t"+".urlEncode

      . assert(_ == t"%2B")

      test(m"URL encode an é"):
        t"é".urlEncode

      . assert(_ == t"%C3%A9")

      test(m"URL encode a Text"):
        t"Nechť již hříšné saxofony ďáblů rozezvučí síň úděsnými tóny waltzu, tanga a quickstepu.".urlEncode

      . assert(_ == t"Nech%C5%A5+ji%C5%BE+h%C5%99%C3%AD%C5%A1n%C3%A9+saxofony+%C4%8F%C3%A1bl%C5%AF+rozezvu%C4%8D%C3%AD+s%C3%AD%C5%88+%C3%BAd%C4%9Bsn%C3%BDmi+t%C3%B3ny+waltzu%2C+tanga+a+quickstepu.")

      test(m"URL decode a Text"):
        t"Nech%C5%A5%20ji%C5%BE%20h%C5%99%C3%AD%C5%A1n%C3%A9%20saxofony%20%C4%8F%C3%A1bl%C5%AF%20rozezvu%C4%8D%C3%AD%20s%C3%AD%C5%88%20%C3%BAd%C4%9Bsn%C3%BDmi%20t%C3%B3ny%20waltzu%2C%20tanga%20a%20quickstepu.".urlDecode

      . assert(_ == t"Nechť již hříšné saxofony ďáblů rozezvučí síň úděsnými tóny waltzu, tanga a quickstepu.")

      test(m"URL decode a space"):
        t"+".urlDecode

      . assert(_ == t" ")

      test(m"URL decode a +"):
        t"%2B".urlDecode

      . assert(_ == t"+")

      test(m"drop the first character"):
        t"Hello".skip(1)

      . assert(_ == t"ello")

      test(m"drop the last character"):
        t"Hello".skip(1, Rtl)

      . assert(_ == t"Hell")

      test(m"drop more characters than the length of the Text"):
        t"Hello".skip(10)

      . assert(_ == t"")

      test(m"drop more right chars than text length"):
        t"Hello".skip(10, Rtl)

      . assert(_ == t"")

      test(m"take the first character"):
        t"Hello".keep(1)

      . assert(_ == t"H")

      test(m"take the last character"):
        t"Hello".keep(1, Rtl)

      . assert(_ == t"o")

      test(m"take more characters than the length of the Text"):
        t"Hello".keep(10)

      . assert(_ == t"Hello")

      test(m"take more right chars than text length"):
        t"Hello".keep(10, Rtl)

      . assert(_ == t"Hello")

      test(m"snip a Text in two"):
        t"Hello".snip(2): (Text, Text)

      . assert(_ == (t"He", t"llo"))

      test(m"trim spaces from a Text"):
        t"  Hello   ".trim

      . assert(_ == t"Hello")

      test(m"trim mixed whitespace from a Text"):
        t"\n\r\t Hello\n \t\r".trim

      . assert(_ == t"Hello")

      test(m"take a slice from a Text"):
        t"Hello world".segment(Quin thru Sept)

      . assert(_ == t"o w")

      test(m"take an oversized slice from a Text"):
        t"Hello world".segment(Quin thru 100.z)

      . assert(_ == t"o world")

      test(m"Get characters from a Text"):
        t"Hello world".chars.to(List)

      . assert(_ == List('H', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd'))

      test(m"Flatmap a text"):
        t"ABC".bind { c => t"${c}." }

      . assert(_ == t"A.B.C.")

      test(m"Map over a text's characters"):
        t"ABC".translate { char => char.toLower }
      . assert(_ == t"abc")

      test(m"Check an empty Text is empty"):
        t"".nil

      . assert(_ == true)

      test(m"Check a non-empty Text is not empty"):
        t"abc".nil

      . assert(_ == false)

      test(m"Cut a Text"):
        t"one,two,three".cut(t",").to(List)

      . assert(_ == List(t"one", t"two", t"three"))

      test(m"Cut a Text with empty Text at start"):
        t",one,two".cut(t",").to(List)

      . assert(_ == List(t"", t"one", t"two"))

      test(m"Cut a Text with empty Text at end"):
        t"one,two,".cut(t",").to(List)

      . assert(_ == List(t"one", t"two", t""))

      test(m"Cut a Text with empty parts at start and end"):
        t",one,two,".cut(t",").to(List)

      . assert(_ == List(t"", t"one", t"two", t""))

      test(m"Cut a series of empty Texts"):
        t",,,".cut(t",").to(List)

      . assert(_ == List(t"", t"", t"", t""))

      test(m"Cut a Text which doesn't contain the separator"):
        t"one,two,three".cut(t"x").to(List)

      . assert(_ == List(t"one,two,three"))

      test(m"Cut a Text on an escaped character"):
        t"one\ntwo\nthree".cut(t"\n").to(List)

      . assert(_ == List(t"one", t"two", t"three"))

      test(m"Substitute characters"):
        t"one,two,three".tr(',', ';')

      . assert(_ == t"one;two;three")

      test(m"Replace substring"):
        t"naive".sub(t"i", t"ï")

      . assert(_ == t"naïve")

      test(m"Replace different-length substring"):
        t"Once upon a time".sub(t"Once", t"Twice")

      . assert(_ == t"Twice upon a time")

      test(m"Several substitutions"):
        t"foo bar baz".sub(t"ba", t"ma")

      . assert(_ == t"foo mar maz")

      test(m"Overlapping substitutions"):
        t"fofofoo".sub(t"fofo", t"momo")

      . assert(_ == t"momofoo")

      test(m"Get camel-case words"):
        t"oneTwoThree".uncamel

      . assert(_ == Seq(t"one", "two", "three"))

      test(m"Camel-case to dashed words"):
        t"oneTwoThree".uncamel.kebab

      . assert(_ == t"one-two-three")

      test(m"Fit short text into fixed width"):
        t"123".fit(5)

      . assert(_ == t"123  ")

      test(m"Fit long text into fixed width"):
        t"12345".fit(3)

      . assert(_ == t"123")

      test(m"Right-fit long text into fixed width"):
        t"12345".fit(3, Rtl)

      . assert(_ == t"345")

      test(m"Right-fit short text into fixed width"):
        t"123".fit(5, Rtl)

      . assert(_ == t"  123")

      test(m"Right-fit short text with different padding character"):
        t"123".fit(5, Rtl, '.')

      . assert(_ == t"..123")

      test(m"Fit short text with different padding character"):
        t"123".fit(5, Ltr, '.')

      . assert(_ == t"123..")

      test(m"duplicate text several times"):
        t"123"*3

      . assert(_ == t"123123123")

      test(m"duplicate text zero times"):
        t"123"*0

      . assert(_ == t"")

      test(m"Random access of character"):
        t"123".at(Prim)

      . assert(_ == '1')

      // test(m"Random access of out-of-range character"):
      //   capture[RangeError](t"123".at(Sen))
      //
      //. assert(_ == RangeError(5, 0, 3))

      test(m"Random access of out-of-range character"):
        t"123".at(Sen)

      . assert(_ == Unset)

      test(m"Pad-right with space"):
        t"123".pad(5, Rtl)

      . assert(_ == t"  123")

      test(m"Pad-left with space"):
        t"123".pad(5, Ltr)

      . assert(_ == t"123  ")

      test(m"Pad-right with smaller value does not change text"):
        t"12345".pad(3, Rtl)

      . assert(_ == t"12345")

      test(m"Pad-left with smaller value does not change text"):
        t"12345".pad(3, Ltr)

      . assert(_ == t"12345")

      test(m"Text does contain value"):
        t"hello world".contains(t"ello")

      . assert(_ == true)

      test(m"Text does not contain value"):
        t"hello world".contains(t"goodbye")

      . assert(_ == false)

      test(m"Text contains itself"):
        t"hello world".contains(t"hello world")

      . assert(_ == true)

      test(m"Text contains empty text"):
        t"hello world".contains(t"")

      . assert(_ == true)

      test(m"Empty text contains empty text"):
        t"".contains(t"")

      . assert(_ == true)

      test(m"Index of character satisfying predicate"):
        t"oh, Hello World".where(_.isUpper)

      . assert(_ == Quin)

      test(m"Index satisfying predicate starting at result"):
        t"oh, Hello World".where(_.isUpper, Quin)

      . assert(_ == Quin)

      test(m"Index satisfying predicate starting after result"):
        t"oh, Hello World".where(_.isUpper, Sen)

      . assert(_ == 10.z)

      test(m"Take characters while predicate is true"):
        t"HELLOworld".whilst(_.isUpper)

      . assert(_ == t"HELLO")

      test(m"Take chars when predicate is never true"):
        t"hello world".whilst(_.isUpper)

      . assert(_ == t"")

      test(m"Take chars when predicate isn't initially true"):
        t"Helloworld".whilst(_.isLower)

      . assert(_ == t"")

      test(m"Capitalize a lowercase word"):
        t"hello".capitalize

      . assert(_ == t"Hello")

      test(m"Capitalize a mixed-case word"):
        t"fooBar".capitalize

      . assert(_ == t"FooBar")

      test(m"Capitalize an uppercase word does not change it"):
        t"HELLO".capitalize

      . assert(_ == t"HELLO")

    suite(m"Compile errors"):
      test(m"Check that Text and String are incompatible"):
        demilitarize:
          val x: String = Text("text")
        . map(_.message)

      . assert(!_.nil)

    suite(m"Decimalization tests"):
      test(m"Write negative pi"):
        Decimalizer(1).decimalize(-math.Pi)

      . assert(_ == t"-3")

      test(m"Write negative pi to 2 s.f."):
        Decimalizer(2).decimalize(-math.Pi)

      . assert(_ == t"-3.1")

      test(m"Write negative pi to 3 s.f."):
        Decimalizer(3).decimalize(-math.Pi)

      . assert(_ == t"-3.14")

      test(m"Write 1 s.f. pi"):
        Decimalizer(1).decimalize(math.Pi)

      . assert(_ == t"3")

      test(m"Write 2 s.f. pi"):
        Decimalizer(2).decimalize(math.Pi)

      . assert(_ == t"3.1")

      test(m"Write 3 s.f. pi"):
        Decimalizer(3).decimalize(math.Pi)

      . assert(_ == t"3.14")

      test(m"Write 4 s.f. pi"):
        Decimalizer(4).decimalize(math.Pi)

      . assert(_ == t"3.142")

      test(m"Write 5 s.f. pi"):
        Decimalizer(5).decimalize(math.Pi)

      . assert(_ == t"3.1416")

      test(m"Write 6 s.f. pi"):
        Decimalizer(6).decimalize(math.Pi)

      . assert(_ == t"3.14159")

      test(m"Write 7 s.f. pi"):
        Decimalizer(7).decimalize(math.Pi)

      . assert(_ == t"3.141593")

      test(m"Write 8 s.f. pi"):
        Decimalizer(8).decimalize(math.Pi)

      . assert(_ == t"3.1415927")

      test(m"Write 1 s.f. 10*pi"):
        Decimalizer(1).decimalize(10*math.Pi)

      . assert(_ == t"30")

      test(m"Write 2 s.f. 10*pi"):
        Decimalizer(2).decimalize(10*math.Pi)

      . assert(_ == t"31")

      test(m"Write 3 s.f. 10*pi"):
        Decimalizer(3).decimalize(10*math.Pi)

      . assert(_ == t"31.4")

      test(m"Write 4 s.f. 10*pi"):
        Decimalizer(4).decimalize(10*math.Pi)

      . assert(_ == t"31.42")

      test(m"Write 5 s.f. 10*pi"):
        Decimalizer(5).decimalize(10*math.Pi)

      . assert(_ == t"31.416")

      test(m"Write 6 s.f. 10*pi"):
        Decimalizer(6).decimalize(10*math.Pi)

      . assert(_ == t"31.4159")

      test(m"Write 7 s.f. 10*pi"):
        Decimalizer(7).decimalize(10*math.Pi)

      . assert(_ == t"31.41593")

      test(m"Write 8 s.f. 10*pi"):
        Decimalizer(8).decimalize(10*math.Pi)

      . assert(_ == t"31.415927")

      test(m"Write 1 s.f. 100*pi"):
        Decimalizer(1).decimalize(100*math.Pi)

      . assert(_ == t"300")

      test(m"Write 2 s.f. 100*pi"):
        Decimalizer(2).decimalize(100*math.Pi)

      . assert(_ == t"310")

      test(m"Write 3 s.f. 100*pi"):
        Decimalizer(3).decimalize(100*math.Pi)

      . assert(_ == t"314")

      test(m"Write 4 s.f. 100*pi"):
        Decimalizer(4).decimalize(100*math.Pi)

      . assert(_ == t"314.2")

      test(m"Write 5 s.f. 100*pi"):
        Decimalizer(5).decimalize(100*math.Pi)

      . assert(_ == t"314.16")

      test(m"Write 6 s.f. 100*pi"):
        Decimalizer(6).decimalize(100*math.Pi)

      . assert(_ == t"314.159")

      test(m"Write 7 s.f. 100*pi"):
        Decimalizer(7).decimalize(100*math.Pi)

      . assert(_ == t"314.1593")

      test(m"Write 8 s.f. 100*pi"):
        Decimalizer(8).decimalize(100*math.Pi)

      . assert(_ == t"314.15927")

      test(m"Write 1 s.f. pi/10"):
        Decimalizer(1).decimalize(math.Pi/10)

      . assert(_ == t"0.3")

      test(m"Write 2 s.f. pi/10"):
        Decimalizer(2).decimalize(math.Pi/10)

      . assert(_ == t"0.31")

      test(m"Write 3 s.f. pi/10"):
        Decimalizer(3).decimalize(math.Pi/10)

      . assert(_ == t"0.314")

      test(m"Write 4 s.f. pi/10"):
        Decimalizer(4).decimalize(math.Pi/10)

      . assert(_ == t"0.3142")

      test(m"Write 5 s.f. pi/10"):
        Decimalizer(5).decimalize(math.Pi/10)

      . assert(_ == t"0.31416")

      test(m"Write 6 s.f. pi/10"):
        Decimalizer(6).decimalize(math.Pi/10)

      . assert(_ == t"0.314159")

      test(m"Write 7 s.f. pi/10"):
        Decimalizer(7).decimalize(math.Pi/10)

      . assert(_ == t"0.3141593")

      test(m"Write 8 s.f. pi/10"):
        Decimalizer(8).decimalize(math.Pi/10)

      . assert(_ == t"0.31415927")

      test(m"Write 1 s.f. pi/100"):
        Decimalizer(1).decimalize(math.Pi/100)

      . assert(_ == t"0.03")

      test(m"Write 2 s.f. pi/100"):
        Decimalizer(2).decimalize(math.Pi/100)

      . assert(_ == t"0.031")

      test(m"Write 3 s.f. pi/100"):
        Decimalizer(3).decimalize(math.Pi/100)

      . assert(_ == t"0.0314")

      test(m"Write 4 s.f. pi/100"):
        Decimalizer(4).decimalize(math.Pi/100)

      . assert(_ == t"0.03142")

      test(m"Write 5 s.f. pi/100"):
        Decimalizer(5).decimalize(math.Pi/100)

      . assert(_ == t"0.031416")

      test(m"Write 6 s.f. pi/100"):
        Decimalizer(6).decimalize(math.Pi/100)

      . assert(_ == t"0.0314159")

      test(m"Write 7 s.f. pi/100"):
        Decimalizer(7).decimalize(math.Pi/100)

      . assert(_ == t"0.03141593")

      test(m"Write 8 s.f. pi/100"):
        Decimalizer(8).decimalize(math.Pi/100)

      . assert(_ == t"0.031415927")

      test(m"Write 1 s.f. pi/1000"):
        Decimalizer(1).decimalize(math.Pi/1000)

      . assert(_ == t"3×10¯³")

      test(m"Write 2 s.f. pi/1000"):
        Decimalizer(2).decimalize(math.Pi/1000)

      . assert(_ == t"3.1×10¯³")

      test(m"Write 3 s.f. pi/1000"):
        Decimalizer(3).decimalize(math.Pi/1000)

      . assert(_ == t"3.14×10¯³")

      test(m"Write 4 s.f. pi/1000"):
        Decimalizer(4).decimalize(math.Pi/1000)

      . assert(_ == t"3.142×10¯³")

      test(m"Write 5 s.f. pi/1000"):
        Decimalizer(5).decimalize(math.Pi/1000)

      . assert(_ == t"3.1416×10¯³")

      test(m"Write 6 s.f. pi/1000"):
        Decimalizer(6).decimalize(math.Pi/1000)

      . assert(_ == t"3.14159×10¯³")

      test(m"Write 7 s.f. pi/1000"):
        Decimalizer(7).decimalize(math.Pi/1000)

      . assert(_ == t"3.141593×10¯³")

      test(m"Show Avogadro's number"):
        Decimalizer(5).decimalize(6.0221408e23)

      . assert(_ == t"6.0221×10²³")

      test(m"Write 8 s.f. pi/1000"):
        Decimalizer(8).decimalize(math.Pi/1000)

      . assert(_ == t"3.1415927×10¯³")

      test(m"Show Avogadro's number to 7 s.f."):
        Decimalizer(7).decimalize(6.0221408e23)

      . assert(_ == t"6.022141×10²³")

      test(m"Show Planck's constant to 7 s.f."):
        Decimalizer(7, decimalPoint = '·').decimalize(6.626070e-34)

      . assert(_ == t"6·626070×10¯³⁴")

      test(m"Show Avogadro's numer with decimal multiplier"):
        Decimalizer(7, exponentMultiple = 3).decimalize(6.0221408e23)

      . assert(_ == t"602.2141×10²¹")

      test(m"Show Planck's constant with decimal multiplier"):
        Decimalizer(7, decimalPoint = '·', exponentMultiple = 3).decimalize(6.626070e-34)

      . assert(_ == t"0·6626070×10¯³³")

      test(m"Show positive infinity"):
        Decimalizer(7, decimalPoint = '·', exponentMultiple = 3).decimalize(1.0/0.0)

      . assert(_ == t"∞")

      test(m"Show negative infinity"):
        Decimalizer(7, decimalPoint = '·', exponentMultiple = 3).decimalize(-1.0/0.0)

      . assert(_ == t"-∞")

      test(m"Show not-a-number"):
        Decimalizer(7, decimalPoint = '·', exponentMultiple = 3).decimalize(0.0/0.0)

      . assert(_ == t"∉ℝ")

      test(m"Show 100.0"):
        Decimalizer(decimalPlaces = 1).decimalize(100.0)

      . assert(_ == t"100.0")

      test(m"Show 0.0"):
        Decimalizer(decimalPlaces = 1).decimalize(0.0)

      . assert(_ == t"0.0")

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
      import proximities.levenshteinDistance

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
        lexicon.search("book", 2).size
      . assert(_ == 112)

      test(m"All the matches are found at distance 3"):
        lexicon.search("book", 3).size
      . assert(_ == words.size)

    suite(m"Dictionary tests"):
      test(m"One-entry dictionary size"):
        val dictionary = Dictionary(t"color" -> 0)
        dictionary.size
      . assert(_ == 1)

      test(m"One-entry dictionary lookup"):
        val dictionary = Dictionary(t"color" -> 0)
        dictionary(t"color")
      . assert(_ == 0)

      test(m"One-entry dictionary absent lookup"):
        val dictionary = Dictionary(t"color" -> 0)
        dictionary(t"colo")
      . assert(_ == Unset)

      test(m"One-entry dictionary absent lookup 2"):
        val dictionary = Dictionary(t"color" -> 0)
        dictionary(t"colors")
      . assert(_ == Unset)

      test(m"Two-entry dictionary lookups"):
        val dictionary = Dictionary(t"color" -> "COLOR", t"size" -> "SIZE")
        (dictionary(t"color"), dictionary(t"size"))
      . assert(_ == ("COLOR", "SIZE"))

      test(m"Two-entry dictionary lookups with overlap"):
        val dictionary = Dictionary(t"color" -> "COLOR", t"change" -> "CHANGE")
        (dictionary(t"color"), dictionary(t"change"))
      . assert(_ == ("COLOR", "CHANGE"))

      test(m"Two-entry dictionary lookups with more overlap"):
        val dictionary = Dictionary(t"color" -> "COLOR", t"colors" -> "COLORS")
        (dictionary(t"color"), dictionary(t"colors"))
      . assert(_ == ("COLOR", "COLORS"))

      test(m"Two-entry dictionary lookups with more overlap, reverse order"):
        val dictionary = Dictionary(t"colors" -> "COLORS", t"color" -> "COLOR")
        (dictionary(t"color"), dictionary(t"colors"))
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
