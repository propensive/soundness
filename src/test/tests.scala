/*
    Gossamer, version 0.5.0. Copyright 2021-21 Jon Pretty, Propensive OÜ.

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
import eucalyptus.*

import unsafeExceptions.canThrowAny

given Log(Everything |-> Stdout)

case class Person(name: Txt, age: Int)

object Tests extends Suite(str"Gossamer Tests"):
  def run(using Runner): Unit =
    suite(str"DebugString tests") {
      test(str"serialize boring string") {
        str"Hello world!".debug
      }.check(_ == str""""Hello world!"""")

      test(str"serialize string with newline") {
        str"Hello\nworld".debug
      }.check(_ == str"""\"Hello\\nworld\"""")
      
      test(str"serialize string with tab") {
        str"Hello\tworld".debug
      }.check(_ == str"""\"Hello\\tworld\"""")
      
      test(str"serialize string with apostrophe") {
        str"Hell' world".debug
      }.check(_ == str"""\"Hell\\' world\"""")
      
      test(str"serialize string with quote") {
        str"Hello \"world\"".debug
      }.check(_ == str"""\"Hello \\\"world\\\"\"""")
      
      test(str"serialize string with backslash") {
        str"Hello\\world".debug
      }.check(_ == str"""\"Hello\\\\world\"""")
      
      test(str"serialize string with linefeed") {
        str"Hello world\r".debug
      }.check(_ == str"""\"Hello world\\r\"""")
      
      test(str"serialize string with unicode escapes") {
        str"Hello мир".debug
      }.check(_ == str"""\"Hello \\u043c\\u0438\\u0440\"""")

      test(str"serialize double") {
        3.1.debug
      }.check(_ == str"3.1")
      
      test(str"serialize float") {
        3.1f.debug
      }.check(_ == str"3.1F")
      
      test(str"serialize long") {
        3L.debug
      }.check(_ == str"3L")
      
      test(str"serialize int") {
        3.debug
      }.check(_ == str"3")
      
      test(str"serialize short") {
        3.toShort.debug
      }.check(_ == str"3.toShort")
      
      test(str"serialize +infinity") {
        (1.0/0.0).debug
      }.check(_ == str"Double.PositiveInfinity")
      
      test(str"serialize -infinity") {
        (-1.0/0.0).debug
      }.check(_ == str"Double.NegativeInfinity")
      
      test(str"serialize NaN") {
        (0.0/0.0).debug
      }.check(_ == str"Double.NaN")
      
      test(str"serialize float +infinity") {
        (1.0F/0.0F).debug
      }.check(_ == str"Float.PositiveInfinity")
      
      test(str"serialize float -infinity") {
        (-1.0F/0.0F).debug
      }.check(_ == str"Float.NegativeInfinity")
      
      test(str"serialize float NaN") {
        (0.0F/0.0F).debug
      }.check(_ == str"Float.NaN")

      test(str"serialize tab char") {
        '\t'.debug
      }.check(_ == str"'\\t'")
      
      test(str"serialize backslash char") {
        '\\'.debug
      }.check(_ == str"'\\\\'")
      
      test(str"serialize newline char") {
        '\n'.debug
      }.check(_ == str"'\\n'")
      
      test(str"serialize backspace char") {
        '\b'.debug
      }.check(_ == str"'\\b'")
      
      test(str"serialize unicode char") {
        '«'.debug
      }.check(_ == str"'\\u00ab'")
      
      test(str"serialize apostrophe char") {
        '\''.debug
      }.check(_ == str"'\\''")
      
      test(str"serialize quote char") {
        '\"'.debug
      }.check(_ == str"'\\\"'")

      test(str"serialize case class") {
        Person(str"Simon", 72).debug
      }.check(_ == str"Person(name = \"Simon\", age = 72)")
      
      test(str"directly-derived serialize case class") {
        DebugString.derived[Person].show(Person(str"Simon", 72))
      }.check(_ == str"Person(name = \"Simon\", age = 72)")
    
      test(str"serialize list of strings") {
        List(str"one", str"two", str"three").debug
      }.check(_ == str"""Seq("one", "two", "three")""")
    }

    suite(str"Minimum Edit Distance") {
      test(str"equal strings have zero edit distance") {
        str"Hello world".lev(str"Hello world")
      }.assert(_ == 0)
      
      test(str"missing character has edit distance of 1") {
        str"Hello world".lev(str"Hello orld")
      }.assert(_ == 1)
      
      test(str"missing character from end has edit distance of 1") {
        str"Hello world".lev(str"Hello worl")
      }.assert(_ == 1)
      
      test(str"missing character from start has edit distance of 1") {
        str"Hello world".lev(str"ello world")
      }.assert(_ == 1)
      
      test(str"changed character has edit distance of 1") {
        str"Hello world".lev(str"Hellq world")
      }.assert(_ == 1)
      
      test(str"switched characters has edit distance of 2") {
        str"Hello world".lev(str"Hello wrold")
      }.assert(_ == 2)

      test(str"different strings have large edit distance") {
        str"Hello".lev(str"world").toLong
      }.assert(_ == 4)
    }

    suite(str"String functions") {
      test(str"punycode test") {
        str"www.äpfel.com".punycode: Txt
      }.check(_ == str"www.xn--pfel-koa.com")

      test(str"URL encoding of space") {
        str"hello world".urlEncode: Txt
      }.check(_ == str"hello+world")
      
      test(str"URL encoding of multibyte UTF-8 character") {
        str"Café".urlEncode: Txt
      }.check(_ == str"Caf%C3%A9")

      test(str"URL decoding of UTF-8 string") {
        str"Na%C3%AFve".urlDecode: Txt
      }.check(_ == str"Naïve")

      test(str"Lower-case") {
        str"InDeCiSiVe".lower: Txt
      }.check(_ == str"indecisive")
      
      test(str"Upper-case") {
        str"InDeCiSiVe".upper: Txt
      }.check(_ == str"INDECISIVE")
      
      test(str"Empty string not populated") {
        "".populated
      }.assert(_ == None)
      
      test(str"Non-empty string populated") {
        str"Hello World".populated
      }.assert(_ == Some("Hello World"))
    }

    suite(str"Joining strings") {
      test(str"join with separator") {
        List(str"one", str"two", str"three").join(str", ")
      }.check(_ == str"one, two, three")
      
      test(str"join with separator; different last") {
        List(str"one", str"two", str"three", str"four").join(str", ", str" and ")
      }.check(_ == str"one, two, three and four")
      
      test(str"join with separator; different last; two elements") {
        List(str"three", str"four").join(str", ", str" and ")
      }.check(_ == str"three and four")
      
      test(str"join with separator, prefix and suffix") {
        List(str"one", str"two").join(str"(", str", ", str")")
      }.check(_ == str"(one, two)")
    }

    suite(str"txt interpolator") {
      test(str"multiline collapses to space-delimited") {
        txt"""Hello
              world"""
      }.check(_ == str"Hello world")
      
      test(str"double newline becomes single newline") {
        txt"""Hello
              
              world"""
      }.check(_ == str"Hello\nworld")
      
      test(str"paragraphs") {
        txt"""Hello
              world
              
              Bonjour
              le monde"""
      }.check(_ == str"Hello world\nBonjour le monde")
    }

    suite(str"Show tests") {
      test(str"Show a string") {
        str"Hello world".show
      }.check(_ == str"Hello world")
      
      test(str"Show an Int") {
        43.show
      }.check(_ == str"43")
      
      test(str"Show a locally-declared showable") {
        given Show[Exception] = e => txt"<exception>"
        Exception("").show
      }.check(_ == str"<exception>")
    
      // test(str"Show a Double") {
      //   3.1415926.show
      // }.check(_ == "3.1415926")
    }
