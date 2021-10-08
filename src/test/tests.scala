/*
    Gossamer, version 0.5.0. Copyright 2020-21 Jon Pretty, Propensive OÜ.

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

import unsafeExceptions.canThrowAny

case class Person(name: String, age: Int)

object Tests extends Suite("Gossamer Tests"):


  def run(using Runner): Unit =
    suite("DebugString tests") {
      test("serialize boring string") {
        "Hello world!".debug
      }.assert(_ == """"Hello world!"""")

      test("serialize string with newline") {
        "Hello\nworld".debug
      }.assert(_ == """"Hello\nworld"""")
      
      test("serialize string with tab") {
        "Hello\tworld".debug
      }.assert(_ == """"Hello\tworld"""")
      
      test("serialize string with apostrophe") {
        "Hell' world".debug
      }.assert(_ == """"Hell\' world"""")
      
      test("serialize string with quote") {
        "Hello \"world\"".debug
      }.assert(_ == """"Hello \"world\""""")
      
      test("serialize string with backslash") {
        "Hello\\world".debug
      }.assert(_ == """"Hello\\world"""")
      
      test("serialize string with linefeed") {
        "Hello world\r".debug
      }.assert(_ == """"Hello world\r"""")
      
      test("serialize string with unicode escapes") {
        "Hello мир".debug
      }.assert(_ == """"Hello \u043c\u0438\u0440"""")

      test("serialize double") {
        3.1.debug
      }.assert(_ == "3.1")
      
      test("serialize float") {
        3.1f.debug
      }.assert(_ == "3.1F")
      
      test("serialize long") {
        3L.debug
      }.assert(_ == "3L")
      
      test("serialize int") {
        3.debug
      }.assert(_ == "3")
      
      test("serialize short") {
        3.toShort.debug
      }.assert(_ == "3.toShort")
      
      test("serialize +infinity") {
        (1.0/0.0).debug
      }.assert(_ == "Double.PositiveInfinity")
      
      test("serialize -infinity") {
        (-1.0/0.0).debug
      }.assert(_ == "Double.NegativeInfinity")
      
      test("serialize NaN") {
        (0.0/0.0).debug
      }.assert(_ == "Double.NaN")
      
      test("serialize float +infinity") {
        (1.0F/0.0F).debug
      }.assert(_ == "Float.PositiveInfinity")
      
      test("serialize float -infinity") {
        (-1.0F/0.0F).debug
      }.assert(_ == "Float.NegativeInfinity")
      
      test("serialize float NaN") {
        (0.0F/0.0F).debug
      }.assert(_ == "Float.NaN")

      test("serialize tab char") {
        '\t'.debug
      }.assert(_ == "'\\t'")
      
      test("serialize backslash char") {
        '\\'.debug
      }.assert(_ == "'\\\\'")
      
      test("serialize newline char") {
        '\n'.debug
      }.assert(_ == "'\\n'")
      
      test("serialize backspace char") {
        '\b'.debug
      }.assert(_ == "'\\b'")
      
      test("serialize unicode char") {
        '«'.debug
      }.assert(_ == "'\\u00ab'")
      
      test("serialize apostrophe char") {
        '\''.debug
      }.assert(_ == "'\\''")
      
      test("serialize quote char") {
        '\"'.debug
      }.assert(_ == "'\\\"'")

      test("serialize case class") {
        summon[DebugString[String]]
        summon[DebugString[Int]]
        summon[Show[Person]]
        Person("Simon", 72).debug
      }.assert(_ == "Person(name = \"Simon\", age = 72)")
      
      test("directly-derived serialize case class") {
        DebugString.derived[Person].show(Person("Simon", 72))
      }.assert(_ == "Person(name = \"Simon\", age = 72)")
    
      test("serialize list of strings") {
        List("one", "two", "three").debug
      }.assert(_ == """Seq("one", "two", "three")""")
    }

    suite("Minimum Edit Distance") {
      test("equal strings have zero edit distance") {
        "Hello world".editDistanceTo("Hello world")
      }.assert(_ == 0)
      
      test("missing character has edit distance of 1") {
        "Hello world".editDistanceTo("Hello orld")
      }.assert(_ == 1)
      
      test("missing character from end has edit distance of 1") {
        "Hello world".editDistanceTo("Hello worl")
      }.assert(_ == 1)
      
      test("missing character from start has edit distance of 1") {
        "Hello world".editDistanceTo("ello world")
      }.assert(_ == 1)
      
      test("changed character has edit distance of 1") {
        "Hello world".editDistanceTo("Hellq world")
      }.assert(_ == 1)
      
      test("switched characters has edit distance of 2") {
        "Hello world".editDistanceTo("Hello wrold")
      }.assert(_ == 2)

      val x: Long = 4

      test("different strings have large edit distance") {
        "Hello".editDistanceTo("world").toLong
      }.assert(_ == x)
    }

    suite("String functions") {
      test("punycode test") {
        "www.äpfel.com".punycode
      }.assert(_ == "www.xn--pfel-koa.com")

      test("URL encoding of space") {
        "hello world".urlEncode
      }.assert(_ == "hello+world")
      
      test("URL encoding of multibyte UTF-8 character") {
        "Café".urlEncode
      }.assert(_ == "Caf%C3%A9")

      test("URL decoding of UTF-8 string") {
        "Na%C3%AFve".urlDecode
      }.assert(_ == "Naïve")

      test("Lower-case") {
        "InDeCiSiVe".lower
      }.assert(_ == "indecisive")
      
      test("Upper-case") {
        "InDeCiSiVe".upper
      }.assert(_ == "INDECISIVE")
      
      test("Empty string not populated") {
        "".populated
      }.assert(_ == None)
      
      test("Non-empty string populated") {
        "Hello World".populated
      }.assert(_ == Some("Hello World"))
    }

    suite("Joining strings") {
      test("join with separator") {
        List("one", "two", "three").join(", ")
      }.assert(_ == "one, two, three")
      
      test("join with separator; different last") {
        List("one", "two", "three", "four").join(", ", " and ")
      }.assert(_ == "one, two, three and four")
      
      test("join with separator; different last; two elements") {
        List("three", "four").join(", ", " and ")
      }.assert(_ == "three and four")
      
      test("join with separator, prefix and suffix") {
        List("one", "two").join("(", ", ", ")")
      }.assert(_ == "(one, two)")
    }

    suite("txt interpolator") {
      test("multiline collapses to space-delimited") {
        txt"""Hello
              world""".s
      }.assert(_ == "Hello world")
      
      test("double newline becomes single newline") {
        txt"""Hello
              
              world""".s
      }.assert(_ == "Hello\nworld")
      
      test("paragraphs") {
        txt"""Hello
              world
              
              Bonjour
              le monde""".s
      }.assert(_ == "Hello world\nBonjour le monde")
    }

    suite("Show tests") {
      test("Show a string") {
        "Hello world".show.s
      }.assert(_ == "Hello world")
      
      test("Show an Int") {
        43.show.s
      }.assert(_ == "43")
      
      test("Show a locally-declared showable") {
        given Show[Exception] = e => txt"<exception>"
        Exception("").show.s
      }.assert(_ == "<exception>")
    
      // test("Show a Double") {
      //   3.1415926.show
      // }.assert(_ == "3.1415926")
    }
