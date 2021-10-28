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

case class Person(name: Text, age: Int)

object Tests extends Suite(t"Gossamer Tests"):
  def run(using Runner): Unit =
    suite(t"DebugString tests") {
      test(t"serialize boring string") {
        t"Hello world!".debug
      }.check(_ == t""""Hello world!"""")

      test(t"serialize string with newline") {
        t"Hello\nworld".debug
      }.check(_ == t"""\"Hello\\nworld\"""")
      
      test(t"serialize string with tab") {
        t"Hello\tworld".debug
      }.check(_ == t"""\"Hello\\tworld\"""")
      
      test(t"serialize string with apostrophe") {
        t"Hell' world".debug
      }.check(_ == t"""\"Hell\\' world\"""")
      
      test(t"serialize string with quote") {
        t"Hello \"world\"".debug
      }.check(_ == t"""\"Hello \\\"world\\\"\"""")
      
      test(t"serialize string with backslash") {
        t"Hello\\world".debug
      }.check(_ == t"""\"Hello\\\\world\"""")
      
      test(t"serialize string with linefeed") {
        t"Hello world\r".debug
      }.check(_ == t"""\"Hello world\\r\"""")
      
      test(t"serialize string with unicode escapes") {
        t"Hello мир".debug
      }.check(_ == t"""\"Hello \\u043c\\u0438\\u0440\"""")

      test(t"serialize double") {
        3.1.debug
      }.check(_ == t"3.1")
      
      test(t"serialize float") {
        3.1f.debug
      }.check(_ == t"3.1F")
      
      test(t"serialize long") {
        3L.debug
      }.check(_ == t"3L")
      
      test(t"serialize int") {
        3.debug
      }.check(_ == t"3")
      
      test(t"serialize short") {
        3.toShort.debug
      }.check(_ == t"3.toShort")
      
      test(t"serialize +infinity") {
        (1.0/0.0).debug
      }.check(_ == t"Double.PositiveInfinity")
      
      test(t"serialize -infinity") {
        (-1.0/0.0).debug
      }.check(_ == t"Double.NegativeInfinity")
      
      test(t"serialize NaN") {
        (0.0/0.0).debug
      }.check(_ == t"Double.NaN")
      
      test(t"serialize float +infinity") {
        (1.0F/0.0F).debug
      }.check(_ == t"Float.PositiveInfinity")
      
      test(t"serialize float -infinity") {
        (-1.0F/0.0F).debug
      }.check(_ == t"Float.NegativeInfinity")
      
      test(t"serialize float NaN") {
        (0.0F/0.0F).debug
      }.check(_ == t"Float.NaN")

      test(t"serialize tab char") {
        '\t'.debug
      }.check(_ == t"'\\t'")
      
      test(t"serialize backslash char") {
        '\\'.debug
      }.check(_ == t"'\\\\'")
      
      test(t"serialize newline char") {
        '\n'.debug
      }.check(_ == t"'\\n'")
      
      test(t"serialize backspace char") {
        '\b'.debug
      }.check(_ == t"'\\b'")
      
      test(t"serialize unicode char") {
        '«'.debug
      }.check(_ == t"'\\u00ab'")
      
      test(t"serialize apostrophe char") {
        '\''.debug
      }.check(_ == t"'\\''")
      
      test(t"serialize quote char") {
        '\"'.debug
      }.check(_ == t"'\\\"'")

      test(t"serialize case class") {
        Person(t"Simon", 72).debug
      }.check(_ == t"Person(name = \"Simon\", age = 72)")
      
      test(t"directly-derived serialize case class") {
        DebugString.derived[Person].show(Person(t"Simon", 72))
      }.check(_ == t"Person(name = \"Simon\", age = 72)")
    
      test(t"serialize list of strings") {
        List(t"one", t"two", t"three").debug
      }.check(_ == t"""Seq("one", "two", "three")""")
    }

    suite(t"Minimum Edit Distance") {
      test(t"equal strings have zero edit distance") {
        t"Hello world".lev(t"Hello world")
      }.assert(_ == 0)
      
      test(t"missing character has edit distance of 1") {
        t"Hello world".lev(t"Hello orld")
      }.assert(_ == 1)
      
      test(t"missing character from end has edit distance of 1") {
        t"Hello world".lev(t"Hello worl")
      }.assert(_ == 1)
      
      test(t"missing character from start has edit distance of 1") {
        t"Hello world".lev(t"ello world")
      }.assert(_ == 1)
      
      test(t"changed character has edit distance of 1") {
        t"Hello world".lev(t"Hellq world")
      }.assert(_ == 1)
      
      test(t"switched characters has edit distance of 2") {
        t"Hello world".lev(t"Hello wrold")
      }.assert(_ == 2)

      test(t"different strings have large edit distance") {
        t"Hello".lev(t"world").toLong
      }.assert(_ == 4)
    }

    suite(t"String functions") {
      test(t"punycode test") {
        t"www.äpfel.com".punycode: Text
      }.check(_ == t"www.xn--pfel-koa.com")

      test(t"URL encoding of space") {
        t"hello world".urlEncode: Text
      }.check(_ == t"hello+world")
      
      test(t"URL encoding of multibyte UTF-8 character") {
        t"Café".urlEncode: Text
      }.check(_ == t"Caf%C3%A9")

      test(t"URL decoding of UTF-8 string") {
        t"Na%C3%AFve".urlDecode: Text
      }.check(_ == t"Naïve")

      test(t"Lower-case") {
        t"InDeCiSiVe".lower: Text
      }.check(_ == t"indecisive")
      
      test(t"Upper-case") {
        t"InDeCiSiVe".upper: Text
      }.check(_ == t"INDECISIVE")
      
      test(t"Empty string not populated") {
        "".populated
      }.assert(_ == None)
      
      test(t"Non-empty string populated") {
        t"Hello World".populated
      }.assert(_ == Some("Hello World"))
    }

    suite(t"Joining strings") {
      test(t"join with separator") {
        List(t"one", t"two", t"three").join(t", ")
      }.check(_ == t"one, two, three")
      
      test(t"join with separator; different last") {
        List(t"one", t"two", t"three", t"four").join(t", ", t" and ")
      }.check(_ == t"one, two, three and four")
      
      test(t"join with separator; different last; two elements") {
        List(t"three", t"four").join(t", ", t" and ")
      }.check(_ == t"three and four")
      
      test(t"join with separator, prefix and suffix") {
        List(t"one", t"two").join(t"(", t", ", t")")
      }.check(_ == t"(one, two)")
    }

    suite(t"txt interpolator") {
      test(t"multiline collapses to space-delimited") {
        txt"""Hello
              world"""
      }.check(_ == t"Hello world")
      
      test(t"double newline becomes single newline") {
        txt"""Hello
              
              world"""
      }.check(_ == t"Hello\nworld")
      
      test(t"paragraphs") {
        txt"""Hello
              world
              
              Bonjour
              le monde"""
      }.check(_ == t"Hello world\nBonjour le monde")
    }

    suite(t"Show tests") {
      test(t"Show a string") {
        t"Hello world".show
      }.check(_ == t"Hello world")
      
      test(t"Show an Int") {
        43.show
      }.check(_ == t"43")
      
      test(t"Show a locally-declared showable") {
        given Show[Exception] = e => txt"<exception>"
        Exception("").show
      }.check(_ == t"<exception>")
    
      // test(t"Show a Double") {
      //   3.1415926.show
      // }.check(_ == "3.1415926")
    }
