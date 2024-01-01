/*
    Gossamer, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package gossamer

import probably.*
import rudiments.*
import hieroglyph.*, charEncoders.utf8, textWidthCalculation.uniform
import spectacular.*
import perforate.*
import anticipation.*
import symbolism.*
import larceny.*

import unsafeExceptions.canThrowAny
import errorHandlers.throwUnsafely

case class Person(name: Text, age: Int)

object Tests extends Suite(t"Gossamer Tests"):
  def run(): Unit =
    suite(t"Minimum Edit Distance"):
      test(t"equal strings have zero edit distance"):
        t"Hello world".lev(t"Hello world")
      .assert(_ == 0)
      
      test(t"missing character has edit distance of 1"):
        t"Hello world".lev(t"Hello orld")
      .assert(_ == 1)
      
      test(t"missing character from end has edit distance of 1"):
        t"Hello world".lev(t"Hello worl")
      .assert(_ == 1)
      
      test(t"missing character from start has edit distance of 1"):
        t"Hello world".lev(t"ello world")
      .assert(_ == 1)
      
      test(t"changed character has edit distance of 1"):
        t"Hello world".lev(t"Hellq world")
      .assert(_ == 1)
      
      test(t"switched characters has edit distance of 2"):
        t"Hello world".lev(t"Hello wrold")
      .assert(_ == 2)

      test(t"different strings have large edit distance"):
        t"Hello".lev(t"world").toLong
      .assert(_ == 4)

    suite(t"String functions"):
      test(t"punycode test"):
        t"www.äpfel.com".punycode
      .assert(_ == t"www.xn--pfel-koa.com")

      test(t"URL encoding of space"):
        t"hello world".urlEncode
      .assert(_ == t"hello+world")
      
      test(t"URL encoding of multibyte UTF-8 character"):
        t"Café".urlEncode
      .assert(_ == t"Caf%C3%A9")

      test(t"URL decoding of UTF-8 string"):
        t"Na%C3%AFve".urlDecode
      .assert(_ == t"Naïve")

      test(t"Lower-case"):
        t"InDeCiSiVe".lower
      .assert(_ == t"indecisive")
      
      test(t"Upper-case"):
        t"InDeCiSiVe".upper
      .assert(_ == t"INDECISIVE")
      
      test(t"Empty string not populated"):
        t"".populated
      .assert(_ == Unset)
      
      test(t"Non-empty string populated"):
        t"Hello World".populated
      .assert(_ == t"Hello World")

    suite(t"Joining strings"):
      test(t"join with separator"):
        List(t"one", t"two", t"three").join(t", ")
      .assert(_ == t"one, two, three")
      
      test(t"join with separator; different last"):
        List(t"one", t"two", t"three", t"four").join(t", ", t" and ")
      .assert(_ == t"one, two, three and four")
      
      test(t"join with separator; different last; two elements"):
        List(t"three", t"four").join(t", ", t" and ")
      .assert(_ == t"three and four")
      
      test(t"join with separator, prefix and suffix"):
        List(t"one", t"two").join(t"(", t", ", t")")
      .assert(_ == t"(one, two)")

    suite(t"txt interpolator"):
      test(t"multiline collapses to space-delimited"):
        txt"""Hello
              world"""
      .assert(_ == t"Hello world")
      
      test(t"double newline becomes single newline"):
        txt"""Hello
              
              world"""
      .assert(_ == t"Hello\nworld")
      
      test(t"paragraphs"):
        txt"""Hello
              world
              
              Bonjour
              le monde"""
      .assert(_ == t"Hello world\nBonjour le monde")

    suite(t"Text methods"):
      test(t"get bytes from text"):
        t"hello".bytes.to(List)
      .assert(_ == List(104, 101, 108, 108, 111))
      
      test(t"get bytes from empty Text"):
        t"".bytes.to(List)
      .assert(_ == List())

      test(t"get Text length"):
        t"hello world".length
      .assert(_ == 11)

      test(t"empty Text should not be populated"):
        t"".populated
      .assert(_ == Unset)

      test(t"non-empty Text should be populated"):
        t"Hello".populated
      .assert(_ == t"Hello")

      test(t"convert to lower case"):
        t"Hello World".lower
      .assert(_ == t"hello world")

      test(t"convert to upper case"):
        t"Hello World".upper
      .assert(_ == t"HELLO WORLD")

      test(t"URL encode a space"):
        t" ".urlEncode
      .assert(_ == t"+")

      test(t"URL encode a +"):
        t"+".urlEncode
      .assert(_ == t"%2B")

      test(t"URL encode an é"):
        t"é".urlEncode
      .assert(_ == t"%C3%A9")

      test(t"URL encode a Text"):
        t"Nechť již hříšné saxofony ďáblů rozezvučí síň úděsnými tóny waltzu, tanga a quickstepu.".urlEncode
      .assert(_ == t"Nech%C5%A5+ji%C5%BE+h%C5%99%C3%AD%C5%A1n%C3%A9+saxofony+%C4%8F%C3%A1bl%C5%AF+rozezvu%C4%8D%C3%AD+s%C3%AD%C5%88+%C3%BAd%C4%9Bsn%C3%BDmi+t%C3%B3ny+waltzu%2C+tanga+a+quickstepu.")
      
      test(t"URL decode a Text"):
        t"Nech%C5%A5%20ji%C5%BE%20h%C5%99%C3%AD%C5%A1n%C3%A9%20saxofony%20%C4%8F%C3%A1bl%C5%AF%20rozezvu%C4%8D%C3%AD%20s%C3%AD%C5%88%20%C3%BAd%C4%9Bsn%C3%BDmi%20t%C3%B3ny%20waltzu%2C%20tanga%20a%20quickstepu.".urlDecode
      .assert(_ == t"Nechť již hříšné saxofony ďáblů rozezvučí síň úděsnými tóny waltzu, tanga a quickstepu.")

      test(t"URL decode a space"):
        t"+".urlDecode
      .assert(_ == t" ")

      test(t"URL decode a +"):
        t"%2B".urlDecode
      .assert(_ == t"+")

      test(t"drop the first character"):
        t"Hello".drop(1)
      .assert(_ == t"ello")

      test(t"drop the last character"):
        t"Hello".drop(1, Rtl)
      .assert(_ == t"Hell")

      test(t"drop more characters than the length of the Text"):
        t"Hello".drop(10)
      .assert(_ == t"")

      test(t"drop more characters from the right than the length of the Text"):
        t"Hello".drop(10, Rtl)
      .assert(_ == t"")

      test(t"take the first character"):
        t"Hello".take(1)
      .assert(_ == t"H")

      test(t"take the last character"):
        t"Hello".take(1, Rtl)
      .assert(_ == t"o")

      test(t"take more characters than the length of the Text"):
        t"Hello".take(10)
      .assert(_ == t"Hello")

      test(t"take more characters from the right than the length of the Text"):
        t"Hello".take(10, Rtl)
      .assert(_ == t"Hello")

      test(t"snip a Text in two"):
        t"Hello".snip(2): (Text, Text)
      .assert(_ == (t"He", t"llo"))

      test(t"trim spaces from a Text"):
        t"  Hello   ".trim
      .assert(_ == t"Hello")

      test(t"trim mixed whitespace from a Text"):
        t"\n\r\t Hello\n \t\r".trim
      .assert(_ == t"Hello")

      test(t"take a slice from a Text"):
        t"Hello world".slice(4, 7)
      .assert(_ == t"o w")

      test(t"take an oversized slice from a Text"):
        t"Hello world".slice(4, 100)
      .assert(_ == t"o world")

      test(t"Get characters from a Text"):
        t"Hello world".chars.to(List)
      .assert(_ == List('H', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd'))

      test(t"Flatmap a text"):
        t"ABC".flatMap { c => t"${c}." }
      .assert(_ == t"A.B.C.")

      test(t"Map over a text's characters"):
        t"ABC".mapChars { char => char.toLower }
      .assert(_ == t"abc")

      test(t"Check an empty Text is empty"):
        t"".empty
      .assert(_ == true)

      test(t"Check a non-empty Text is not empty"):
        t"abc".empty
      .assert(_ == false)

      test(t"Cut a Text"):
        t"one,two,three".cut(t",")
      .assert(_ == List(t"one", t"two", t"three"))

      test(t"Cut a Text with empty Text at start"):
        t",one,two".cut(t",")
      .assert(_ == List(t"", t"one", t"two"))

      test(t"Cut a Text with empty Text at end"):
        t"one,two,".cut(t",")
      .assert(_ == List(t"one", t"two", t""))
      
      test(t"Cut a Text with empty parts at start and end"):
        t",one,two,".cut(t",")
      .assert(_ == List(t"", t"one", t"two", t""))

      test(t"Cut a series of empty Texts"):
        t",,,".cut(t",")
      .assert(_ == List(t"", t"", t"", t""))

      test(t"Cut a Text which doesn't contain the separator"):
        t"one,two,three".cut(t"x")
      .assert(_ == List(t"one,two,three"))

      test(t"Cut a Text on an escaped character"):
        t"one\ntwo\nthree".cut(t"\n")
      .assert(_ == List(t"one", t"two", t"three"))

      test(t"Substitute characters"):
        t"one,two,three".tr(',', ';')
      .assert(_ == t"one;two;three")

      test(t"Replace substring"):
        t"naive".sub(t"i", t"ï")
      .assert(_ == t"naïve")

      test(t"Replace different-length substring"):
        t"Once upon a time".sub(t"Once", t"Twice")
      .assert(_ == t"Twice upon a time")

      test(t"Several substitutions"):
        t"foo bar baz".sub(t"ba", t"ma")
      .assert(_ == t"foo mar maz")

      test(t"Overlapping substitutions"):
        t"fofofoo".sub(t"fofo", t"momo")
      .assert(_ == t"momofoo")

      test(t"Get camel-case words"):
        t"oneTwoThree".uncamel
      .assert(_ == List(t"one", "two", "three"))
      
      test(t"Camel-case to dashed words"):
        t"oneTwoThree".uncamel.kebab
      .assert(_ == t"one-two-three")
    
      test(t"Fit short text into fixed width"):
        t"123".fit(5)
      .assert(_ == t"123  ")

      test(t"Fit long text into fixed width"):
        t"12345".fit(3)
      .assert(_ == t"123")

      test(t"Right-fit long text into fixed width"):
        t"12345".fit(3, Rtl)
      .assert(_ == t"345")

      test(t"Right-fit short text into fixed width"):
        t"123".fit(5, Rtl)
      .assert(_ == t"  123")

      test(t"Right-fit short text with different padding character"):
        t"123".fit(5, Rtl, '.')
      .assert(_ == t"..123")
      
      test(t"Fit short text with different padding character"):
        t"123".fit(5, Ltr, '.')
      .assert(_ == t"123..")

      test(t"duplicate text several times"):
        t"123"*3
      .assert(_ == t"123123123")

      test(t"duplicate text zero times"):
        t"123"*0
      .assert(_ == t"")

      test(t"Random access of character"):
        t"123"(0)
      .assert(_ == '1')

      test(t"Random access of out-of-range character"):
        capture[OutOfRangeError](t"123"(5))
      .assert(_ == OutOfRangeError(5, 0, 3))

      test(t"Pad-right with space"):
        t"123".pad(5, Rtl)
      .assert(_ == t"  123")

      test(t"Pad-left with space"):
        t"123".pad(5, Ltr)
      .assert(_ == t"123  ")

      test(t"Pad-right with smaller value does not change text"):
        t"12345".pad(3, Rtl)
      .assert(_ == t"12345")

      test(t"Pad-left with smaller value does not change text"):
        t"12345".pad(3, Ltr)
      .assert(_ == t"12345")

      test(t"Text does contain value"):
        t"hello world".contains(t"ello")
      .assert(_ == true)

      test(t"Text does not contain value"):
        t"hello world".contains(t"goodbye")
      .assert(_ == false)

      test(t"Text contains itself"):
        t"hello world".contains(t"hello world")
      .assert(_ == true)

      test(t"Text contains empty text"):
        t"hello world".contains(t"")
      .assert(_ == true)
    
      test(t"Empty text contains empty text"):
        t"".contains(t"")
      .assert(_ == true)

      test(t"Index of character satisfying predicate"):
        t"oh, Hello World".where(_.isUpper)
      .assert(_ == 4)

      test(t"Index of character satisfying predicate with start point at result index"):
        t"oh, Hello World".where(_.isUpper, 4)
      .assert(_ == 4)
      
      test(t"Index of character satisfying predicate with start point after first result"):
        t"oh, Hello World".where(_.isUpper, 5)
      .assert(_ == 10)

      test(t"Take characters while predicate is true"):
        t"HELLOworld".whilst(_.isUpper)
      .assert(_ == t"HELLO")
      
      test(t"Take characters when predicate is never true returns empty text"):
        t"hello world".whilst(_.isUpper)
      .assert(_ == t"")
      
      test(t"Take characters when predicate isn't initially true returns empty text"):
        t"Helloworld".whilst(_.isLower)
      .assert(_ == t"")
    
      test(t"Capitalize a lowercase word"):
        t"hello".capitalize
      .assert(_ == t"Hello")

      test(t"Capitalize a mixed-case word"):
        t"fooBar".capitalize
      .assert(_ == t"FooBar")

      test(t"Capitalize an uppercase word does not change it"):
        t"HELLO".capitalize
      .assert(_ == t"HELLO")

    suite(t"Compile errors"):
      test(t"Check that Text and String are incompatible"):
        demilitarize:
          val x: String = Text("text")
      .assert(_.head.errorId == ErrorId.TypeMismatchID)

    suite(t"Decimalization tests"):
      test(t"Write negative pi"):
        Decimalizer(1).decimalize(-math.Pi)
      .assert(_ == t"-3")
      
      test(t"Write negative pi to 2 s.f."):
        Decimalizer(2).decimalize(-math.Pi)
      .assert(_ == t"-3.1")
      
      test(t"Write negative pi to 3 s.f."):
        Decimalizer(3).decimalize(-math.Pi)
      .assert(_ == t"-3.14")
      
      test(t"Write 1 s.f. pi"):
        Decimalizer(1).decimalize(math.Pi)
      .assert(_ == t"3")
      
      test(t"Write 2 s.f. pi"):
        Decimalizer(2).decimalize(math.Pi)
      .assert(_ == t"3.1")
      
      test(t"Write 3 s.f. pi"):
        Decimalizer(3).decimalize(math.Pi)
      .assert(_ == t"3.14")
      
      test(t"Write 4 s.f. pi"):
        Decimalizer(4).decimalize(math.Pi)
      .assert(_ == t"3.142")
      
      test(t"Write 5 s.f. pi"):
        Decimalizer(5).decimalize(math.Pi)
      .assert(_ == t"3.1416")
      
      test(t"Write 6 s.f. pi"):
        Decimalizer(6).decimalize(math.Pi)
      .assert(_ == t"3.14159")
      
      test(t"Write 7 s.f. pi"):
        Decimalizer(7).decimalize(math.Pi)
      .assert(_ == t"3.141593")
      
      test(t"Write 8 s.f. pi"):
        Decimalizer(8).decimalize(math.Pi)
      .assert(_ == t"3.1415927")
      
      test(t"Write 1 s.f. 10*pi"):
        Decimalizer(1).decimalize(10*math.Pi)
      .assert(_ == t"30")
      
      test(t"Write 2 s.f. 10*pi"):
        Decimalizer(2).decimalize(10*math.Pi)
      .assert(_ == t"31")
      
      test(t"Write 3 s.f. 10*pi"):
        Decimalizer(3).decimalize(10*math.Pi)
      .assert(_ == t"31.4")
      
      test(t"Write 4 s.f. 10*pi"):
        Decimalizer(4).decimalize(10*math.Pi)
      .assert(_ == t"31.42")
      
      test(t"Write 5 s.f. 10*pi"):
        Decimalizer(5).decimalize(10*math.Pi)
      .assert(_ == t"31.416")
      
      test(t"Write 6 s.f. 10*pi"):
        Decimalizer(6).decimalize(10*math.Pi)
      .assert(_ == t"31.4159")
      
      test(t"Write 7 s.f. 10*pi"):
        Decimalizer(7).decimalize(10*math.Pi)
      .assert(_ == t"31.41593")
      
      test(t"Write 8 s.f. 10*pi"):
        Decimalizer(8).decimalize(10*math.Pi)
      .assert(_ == t"31.415927")
      
      test(t"Write 1 s.f. 100*pi"):
        Decimalizer(1).decimalize(100*math.Pi)
      .assert(_ == t"300")
      
      test(t"Write 2 s.f. 100*pi"):
        Decimalizer(2).decimalize(100*math.Pi)
      .assert(_ == t"310")
      
      test(t"Write 3 s.f. 100*pi"):
        Decimalizer(3).decimalize(100*math.Pi)
      .assert(_ == t"314")
      
      test(t"Write 4 s.f. 100*pi"):
        Decimalizer(4).decimalize(100*math.Pi)
      .assert(_ == t"314.2")
      
      test(t"Write 5 s.f. 100*pi"):
        Decimalizer(5).decimalize(100*math.Pi)
      .assert(_ == t"314.16")
      
      test(t"Write 6 s.f. 100*pi"):
        Decimalizer(6).decimalize(100*math.Pi)
      .assert(_ == t"314.159")
      
      test(t"Write 7 s.f. 100*pi"):
        Decimalizer(7).decimalize(100*math.Pi)
      .assert(_ == t"314.1593")
      
      test(t"Write 8 s.f. 100*pi"):
        Decimalizer(8).decimalize(100*math.Pi)
      .assert(_ == t"314.15927")
      
      test(t"Write 1 s.f. pi/10"):
        Decimalizer(1).decimalize(math.Pi/10)
      .assert(_ == t"0.3")
      
      test(t"Write 2 s.f. pi/10"):
        Decimalizer(2).decimalize(math.Pi/10)
      .assert(_ == t"0.31")
      
      test(t"Write 3 s.f. pi/10"):
        Decimalizer(3).decimalize(math.Pi/10)
      .assert(_ == t"0.314")
      
      test(t"Write 4 s.f. pi/10"):
        Decimalizer(4).decimalize(math.Pi/10)
      .assert(_ == t"0.3142")
      
      test(t"Write 5 s.f. pi/10"):
        Decimalizer(5).decimalize(math.Pi/10)
      .assert(_ == t"0.31416")
      
      test(t"Write 6 s.f. pi/10"):
        Decimalizer(6).decimalize(math.Pi/10)
      .assert(_ == t"0.314159")
      
      test(t"Write 7 s.f. pi/10"):
        Decimalizer(7).decimalize(math.Pi/10)
      .assert(_ == t"0.3141593")
      
      test(t"Write 8 s.f. pi/10"):
        Decimalizer(8).decimalize(math.Pi/10)
      .assert(_ == t"0.31415927")
      
      test(t"Write 1 s.f. pi/100"):
        Decimalizer(1).decimalize(math.Pi/100)
      .assert(_ == t"0.03")
      
      test(t"Write 2 s.f. pi/100"):
        Decimalizer(2).decimalize(math.Pi/100)
      .assert(_ == t"0.031")
      
      test(t"Write 3 s.f. pi/100"):
        Decimalizer(3).decimalize(math.Pi/100)
      .assert(_ == t"0.0314")
      
      test(t"Write 4 s.f. pi/100"):
        Decimalizer(4).decimalize(math.Pi/100)
      .assert(_ == t"0.03142")
      
      test(t"Write 5 s.f. pi/100"):
        Decimalizer(5).decimalize(math.Pi/100)
      .assert(_ == t"0.031416")
      
      test(t"Write 6 s.f. pi/100"):
        Decimalizer(6).decimalize(math.Pi/100)
      .assert(_ == t"0.0314159")
      
      test(t"Write 7 s.f. pi/100"):
        Decimalizer(7).decimalize(math.Pi/100)
      .assert(_ == t"0.03141593")
      
      test(t"Write 8 s.f. pi/100"):
        Decimalizer(8).decimalize(math.Pi/100)
      .assert(_ == t"0.031415927")
      
      test(t"Write 1 s.f. pi/1000"):
        Decimalizer(1).decimalize(math.Pi/1000)
      .assert(_ == t"3×10¯³")
      
      test(t"Write 2 s.f. pi/1000"):
        Decimalizer(2).decimalize(math.Pi/1000)
      .assert(_ == t"3.1×10¯³")
      
      test(t"Write 3 s.f. pi/1000"):
        Decimalizer(3).decimalize(math.Pi/1000)
      .assert(_ == t"3.14×10¯³")
      
      test(t"Write 4 s.f. pi/1000"):
        Decimalizer(4).decimalize(math.Pi/1000)
      .assert(_ == t"3.142×10¯³")
      
      test(t"Write 5 s.f. pi/1000"):
        Decimalizer(5).decimalize(math.Pi/1000)
      .assert(_ == t"3.1416×10¯³")
      
      test(t"Write 6 s.f. pi/1000"):
        Decimalizer(6).decimalize(math.Pi/1000)
      .assert(_ == t"3.14159×10¯³")
      
      test(t"Write 7 s.f. pi/1000"):
        Decimalizer(7).decimalize(math.Pi/1000)
      .assert(_ == t"3.141593×10¯³")
      
      test(t"Show Avogadro's number"):
        Decimalizer(5).decimalize(6.0221408e23)
      .assert(_ == t"6.0221×10²³")
      
      test(t"Write 8 s.f. pi/1000"):
        Decimalizer(8).decimalize(math.Pi/1000)
      .assert(_ == t"3.1415927×10¯³")
    
      test(t"Show Avogadro's number to 7 s.f."):
        Decimalizer(7).decimalize(6.0221408e23)
      .assert(_ == t"6.022141×10²³")
      
      test(t"Show Planck's constant to 7 s.f."):
        Decimalizer(7, decimalPoint = '·').decimalize(6.626070e-34)
      .assert(_ == t"6·626070×10¯³⁴")
      
      test(t"Show Avogadro's numer with decimal multiplier"):
        Decimalizer(7, exponentMultiple = 3).decimalize(6.0221408e23)
      .assert(_ == t"602.2141×10²¹")
      
      test(t"Show Planck's constant with decimal multiplier"):
        Decimalizer(7, decimalPoint = '·', exponentMultiple = 3).decimalize(6.626070e-34)
      .assert(_ == t"0·6626070×10¯³³")
      
      test(t"Show positive infinity"):
        Decimalizer(7, decimalPoint = '·', exponentMultiple = 3).decimalize(1.0/0.0)
      .assert(_ == t"∞")
      
      test(t"Show negative infinity"):
        Decimalizer(7, decimalPoint = '·', exponentMultiple = 3).decimalize(-1.0/0.0)
      .assert(_ == t"-∞")
      
      test(t"Show not-a-number"):
        Decimalizer(7, decimalPoint = '·', exponentMultiple = 3).decimalize(0.0/0.0)
      .assert(_ == t"∉ℝ")

      test(t"Show 100.0"):
        Decimalizer(decimalPlaces = 1).decimalize(100.0)
      .assert(_ == t"100.0")
      
      test(t"Show 0.0"):
        Decimalizer(decimalPlaces = 1).decimalize(0.0)
      .assert(_ == t"0.0")

