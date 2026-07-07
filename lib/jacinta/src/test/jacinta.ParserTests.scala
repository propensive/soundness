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
┃    Soundness, version 0.63.0.                                                                    ┃
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
package jacinta

import soundness.*

import interfaces.paths.pathOnLinux
import strategies.throwUnsafely
import logging.silentLogging
import charEncoders.utf8Encoder
import gitCommands.environmentDefaultGitCommand
import workingDirectories.javaWorkingDirectory
import internetAccess.online
import errorDiagnostics.stackTracesDiagnostics

import scala.compiletime.*

import filesystemOptions.readAccess.enabled
import filesystemOptions.writeAccess.enabled
import filesystemOptions.dereferenceSymlinks.enabled
import filesystemOptions.createNonexistent.enabled
import filesystemOptions.createNonexistentParents.enabled

object ParserTests extends Suite(m"Jacinta JSON parser tests"):
  def run(): Unit =
    val work: Path on Linux = workingDirectory
    val jsonSuite: Path on Linux = work/"tests"/"JSONTestSuite"

    if !(jsonSuite/"test_parsing").exists() then
      Git.clone(url"https://github.com/nst/JSONTestSuite", jsonSuite).complete()

    val tests: Path on Linux = jsonSuite/"test_parsing"

    val deeplyNested: Set[Text] =
      Set(t"n_structure_100000_opening_arrays.json", t"n_structure_open_array_object.json")

    val positiveCases: List[(Text, Data)] =
      tests.children
        . filter(_.name.starts(t"y_"))
        . map: file =>
            (file.name, file.open(_.read[Data]))
        . to(List)

    val negativeCases: List[(Text, Data)] =
      tests.children
        . filter(_.name.starts(t"n_"))
        . filter { file => !deeplyNested.contains(file.name) }
        . map: file =>
            (file.name, file.open(_.read[Data]))
        . to(List)

    suite(m"Positive tests"):
      positiveCases.each: (name, data) =>
        test(Message(name.skip(5, Rtl))):
          Json.Ast.parse(data)
        .check()

    suite(m"Negative tests"):
      negativeCases.each: (name, data) =>
        test(Message(name.skip(5, Rtl))):
          capture[ParseError](Json.Ast.parse(data))
        .matches:
          case ParseError(_, _, _) => true

    suite(m"Number tests"):
      test(m"Parse 0e+1"):
        t"0e+1".read[Json.Ast]
      . assert(_ == Json.Ast(0L))

      test(m"Parse 0e1"):
        t"0e1".read[Json.Ast]
      . assert(_ == Json.Ast(0L))

      test(m"Parse ' 4'"):
        t" 4".read[Json.Ast]
      . assert(_ == Json.Ast(4L))

      test(m"Parse small negative number"):
        // Number exceeds 15 nibbles, so the parser hands back a `Bcd`
        // (high-precision representation) rather than a `Double`. The Bcd
        // round-trips back to the same `Double` the old fallback path used
        // to return.
        val raw =
          t"-0.000000000000000000000000000000000000000000000000000000000000000000000000000001"
          . read[Json.Ast]
        raw.asInstanceOf[Bcd].toDouble
      . assert(_ == -1.0e-78)

      test(m"Parse 20e1"):
        t"20e1".read[Json.Ast]
      . assert(_ == Json.Ast(200L))

      test(m"Parse 123e65"):
        t"123e65".read[Json.Ast]
      . assert(_ == Json.Ast(1.23e67))

      test(m"Parse -0"):
        t"-0".read[Json.Ast]
      . assert(_ == Json.Ast(-0.0))

      test(m"Parse -123"):
        t"-123".read[Json.Ast]
      . assert(_ == Json.Ast(-123L))

      test(m"Parse -1"):
        t"-1".read[Json.Ast]
      . assert(_ == Json.Ast(-1L))

      test(m"Parse 1E22"):
        t"1E22".read[Json.Ast]
      . assert(_ == Json.Ast(1.0E22))

      test(m"Parse 1E-2"):
        t"1E-2".read[Json.Ast]
      . assert(_ == Json.Ast(1.0E-2))

      test(m"Parse 1E+2"):
        t"1E+2".read[Json.Ast]
      . assert(_ == Json.Ast(1.0E2))

      test(m"Parse 123e45"):
        t"123e45".read[Json.Ast]
      . assert(_ == Json.Ast(1.23E47))

      test(m"Parse 123.456e78"):
        t"123.456e78".read[Json.Ast]
      . assert(_ == Json.Ast(1.23456E80))

      test(m"Parse 1e-2"):
        t"1e-2".read[Json.Ast]
      . assert(_ == Json.Ast(1.0E-2))

      test(m"Parse 1e+2"):
        t"1e+2".read[Json.Ast]
      . assert(_ == Json.Ast(1.0E2))

      test(m"Parse 123"):
        t"123".read[Json.Ast]
      . assert(_ == Json.Ast(123L))

      test(m"Parse 123.456789"):
        t"123.456789".read[Json.Ast]
      . assert(_ == Json.Ast(123.456789))

      test(m"Parse \"Hello World\""):
        t"\"Hello World\"".read[Json.Ast]
      . assert(_ == Json.Ast("Hello World"))

      test(m"Parse \"\""):
        t"\"\"".read[Json.Ast]
      . assert(_ == Json.Ast(""))

    suite(m"Streaming over multi-block input"):
      def chunks(text: Text, sizes: Int*): Iterator[Data] =
        val bytes = text.s.getBytes("UTF-8").nn
        var offset = 0

        sizes.iterator.flatMap: size =>
          if offset >= bytes.length then None else
            val end = math.min(offset + size, bytes.length)
            val slice: Data = IArray.from(bytes.slice(offset, end))
            offset = end
            Some(slice)

      test(m"Number split across two blocks"):
        Json.Ast.parse(chunks(t"123456", 3, 3))
      . assert(_ == Json.Ast(123456L))

      test(m"String split mid-content across two blocks"):
        Json.Ast.parse(chunks(t"\"hello world\"", 4, 9))
      . assert(_ == Json.Ast("hello world"))

      test(m"String split mid-escape across two blocks"):
        Json.Ast.parse(chunks(t"\"a\\nb\"", 2, 4))
      . assert(_ == Json.Ast("a\nb"))

      test(m"Keyword true split across blocks"):
        Json.Ast.parse(chunks(t"true", 1, 1, 1, 1))
      . assert(_ == Json.Ast(true))

      test(m"Keyword false split across blocks"):
        Json.Ast.parse(chunks(t"false", 2, 3))
      . assert(_ == Json.Ast(false))

      test(m"Keyword null split across blocks"):
        Json.Ast.parse(chunks(t"null", 1, 1, 2))
      . assert(_ == Json.Ast(Json.JsonNull))

      test(m"Decimal number split at decimal point"):
        Json.Ast.parse(chunks(t"123.456", 3, 4))
      . assert(_ == Json.Ast(123.456))

      test(m"Whitespace then value across blocks"):
        Json.Ast.parse(chunks(t"   42", 2, 3))
      . assert(_ == Json.Ast(42L))

    suite(m"Hole-mode parsing"):
      def bytes(text: Text): Data = IArray.from(text.s.getBytes("UTF-8").nn)

      def shape(node: Any): Any = node.asMatchable match
        case nums: Array[Double] @unchecked =>
          // Number-only array: recover Long for whole values, Double for the rest.
          nums.toList.map: d =>
            if d.isWhole && d >= Long.MinValue.toDouble && d <= Long.MaxValue.toDouble
            then d.toLong
            else d
        case arr: IArray[?] @unchecked =>
          val raw = arr.toList
          if (raw.length & 1) == 0 then
            // Object: alternating key/value
            val keys = (0 until raw.length/2).toList.map(i => raw(i*2).asInstanceOf[String])
            val values = (0 until raw.length/2).toList.map(i => shape(raw(i*2 + 1)))
            (keys, values)
          else
            // Array: strip sentinel pad if present
            val elems =
              if raw.nonEmpty && raw.last.asInstanceOf[AnyRef] == Json.Ast.arrayPad
              then raw.init else raw
            elems.map(shape)
        case other => other

      test(m"Hole as a top-level value"):
        shape(Json.Ast.parse(bytes(t" "), holes = true))
      . assert(_ == Unset)

      test(m"Hole as an array element"):
        shape(Json.Ast.parse(bytes(t"[ ]"), holes = true))
      . assert(_ == List(Unset))

      test(m"Hole as an object value"):
        shape(Json.Ast.parse(bytes(t"""{"a":   }"""), holes = true))
      . assert(_ == (List("a"), List(Unset)))

      test(m"Hole as an object rest, no other entries"):
        shape(Json.Ast.parse(bytes(t"{ }"), holes = true))
      . assert(_ == (List("\u0000"), List(Unset)))

      test(m"Hole as an object rest after literal entry"):
        shape(Json.Ast.parse(bytes(t"""{"a": 1,   }"""), holes = true))
      . assert(_ == (List("a", "\u0000"), List(1L, Unset)))

      test(m"Hole inside a string is preserved"):
        shape(Json.Ast.parse(bytes(t""" "x y" """), holes = true))
      . assert(_ == "x\u0000y")

      test(m"Plain mode rejects a value-position hole"):
        capture[ParseError](Json.Ast.parse(bytes(t" ")))
      . matches:
          case ParseError(_, _, _) => true

      test(m"Plain mode rejects a hole inside a string"):
        capture[ParseError](Json.Ast.parse(bytes(t""" "a b" """)))
      . matches:
          case ParseError(_, _, _) => true

    suite(m"Position ranges"):
      def asBytes(text: Text): Data = IArray.from(text.s.getBytes("UTF-8").nn)
      def position(input: Text): Json.Ast.Position =
        capture[ParseError](Json.Ast.parse(asBytes(input)))
        . position.asInstanceOf[Json.Ast.Position]

      // Bad escape \q after the opening quote (raw to avoid Text-interpolator escaping).
      val badEscape = t""" "abc${"\\"}q" """

      test(m"Bad escape in string carries a non-empty range"):
        position(badEscape).length
      . assert(_ != Unset)

      test(m"Bad escape range covers the string token start"):
        position(badEscape).offset
      . assert(_ == (1: Int))

    suite(m"Number-only arrays"):
      def parseRaw(text: Text): Any = Json.Ast.parse(IArray.from(text.s.getBytes("UTF-8").nn))

      test(m"Pure integer array uses the unboxed Array[Double] form"):
        parseRaw(t"[1, 2, 3]").getClass.getName
      . assert(_ == "[D")

      test(m"Pure integer array decodes back to the original numbers"):
        t"[1, 2, 3]".read[Json].as[List[Int]]
      . assert(_ == List(1, 2, 3))

      test(m"Decimal-only array uses the unboxed form and round-trips"):
        given Json.Formatting = Json.Formatting(Unset, false)
        val raw = parseRaw(t"[1.5, 2.25, 3.125]")
        raw.getClass.getName == "[D"
        && raw.asInstanceOf[Json.Ast].show == t"[1.5,2.25,3.125]"
      . assert(identity)

      test(m"Exponent-bearing numbers stay in the unboxed form"):
        val raw = parseRaw(t"[1e2, 2.5e-3]")
        raw.getClass.getName
      . assert(_ == "[D")

      test(m"Negative numbers stay in the unboxed form and round-trip"):
        t"[-1, -2, -3]".read[Json].as[List[Int]]
      . assert(_ == List(-1, -2, -3))

      test(m"Empty array uses the parity-padded boxed form, not Array[Double]"):
        val raw = parseRaw(t"[]")
        raw.asInstanceOf[Json.Ast].isArray && !raw.asInstanceOf[Json.Ast].isNumberArray
      . assert(identity)

      test(m"Single-element number array uses the unboxed form"):
        parseRaw(t"[42]").getClass.getName
      . assert(_ == "[D")

      test(m"Non-number after numbers triggers fallback to the boxed form"):
        val raw = parseRaw(t"""[1, 2, "three"]""")
        raw.asInstanceOf[Json.Ast].isArray && !raw.asInstanceOf[Json.Ast].isNumberArray
      . assert(identity)

      test(m"Fallback array preserves the leading numbers as Long values"):
        t"""[1, 2, "three"]""".read[Json].root.arrayElement(0).asMatchable
      . assert:
          case l: Long => l == 1L
          case _       => false

      test(m"Bcd-overflow number triggers fallback to the boxed form"):
        // 16-digit value overflows the 15-nibble in-Long fast path and
        // forces a Bcd fallback, which doesn't fit in Array[Double].
        val raw = parseRaw(t"[1, 1234567890123456]")
        raw.asInstanceOf[Json.Ast].isArray && !raw.asInstanceOf[Json.Ast].isNumberArray
      . assert(identity)

      test(m"Fallback retains the Bcd-overflow number as Bcd"):
        val raw = parseRaw(t"[1, 1234567890123456]")
        raw.asInstanceOf[Json.Ast].arrayElement(1).isBcd
      . assert(identity)

      test(m"15-nibble integer (parser fast path) stays in the unboxed form"):
        // 15 digits = 15 nibbles, fits in the in-Long fast path and in
        // Double exactly (< 2^50).
        val raw = parseRaw(t"[1, 123456789012345]")
        raw.getClass.getName
      . assert(_ == "[D")

      test(m"Non-number first element keeps the boxed buffer"):
        val raw = parseRaw(t"""["a", 1, 2]""")
        raw.asInstanceOf[Json.Ast].isArray && !raw.asInstanceOf[Json.Ast].isNumberArray
      . assert(identity)

      test(m"Hand-built boxed array of Longs equals a parsed number array"):
        val elements: IArray[Any] = IArray(Json.Ast(1L), Json.Ast(2L), Json.Ast(3L))
        val handBuilt = Json.ast(Json.Ast.arr(elements))
        handBuilt == t"[1, 2, 3]".read[Json]
      . assert(identity)

    suite(m"Boxed-array storage"):
      def parseRaw(text: Text): Any = Json.Ast.parse(IArray.from(text.s.getBytes("UTF-8").nn))

      test(m"Empty array round-trips via the printer"):
        given Json.Formatting = Json.Formatting(Unset, false)
        parseRaw(t"[]").asInstanceOf[Json.Ast].show
      . assert(_ == t"[]")

      test(m"Even-length mixed array reports its user-visible length"):
        // [1, "x"] mixes a number and a string, so it must use the boxed
        // (parity-padded) form. The user-visible length is 2 even though
        // the underlying IArray carries a sentinel pad to keep the length
        // odd (and so distinguishable from an object).
        parseRaw(t"""[1, "x"]""").asInstanceOf[Json.Ast].arrayLength
      . assert(_ == 2)

      test(m"Object equality is preserved"):
        t"""{"a": 1, "b": 2}""".read[Json] == t"""{"b": 2, "a": 1}""".read[Json]
      . assert(identity)



