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
package jacinta

import scala.compiletime.*

import anticipation.*
import denominative.*
import gossamer.*
import parasite.*
import zephyrine.*

object JsonPrinter:
  // 64 spaces, sliced for indentation.
  private val spaces: Text =
    t"                                                                "

  def print(json: Json.Ast, indentation: Boolean): Text =
    Producer.collect[Text](): producer =>
      write(producer, json, indentation)

  def emit(json: Json.Ast, indentation: Boolean)(using Monitor, Probate): Iterator[Text] =
    val producer = Producer[Text](4096)

    async:
      write(producer, json, indentation)
      producer.finish()

    producer.iterator

  private def unicode(char: Char): Text =
    val hex = Integer.toHexString(char.toInt).nn
    if hex.length == 1 then t"\\u000$hex" else t"\\u00$hex"

  private def write(producer: Producer[Text], json: Json.Ast, indentation: Boolean): Unit =
    def indent(count: Int): Unit =
      var remaining = count

      while remaining > 0 do
        val chunk = remaining.min(64)
        producer.put(spaces, Prim, chunk)
        remaining -= chunk

    // JSON string escaping (RFC 8259): the quote and backslash, the named control escapes, and any
    // other U+0000-001F control character as a `\uXXXX` reference.
    def writeString(string: String): Unit =
      producer.put("\"")
      val length = string.length
      var start = 0
      var index = 0

      inline def escape(entity: Text): Unit =
        if index > start then producer.put(string.tt, start.z, index - start)
        producer.put(entity)
        start = index + 1

      while index < length do
        string.charAt(index) match
          case '"'          => escape(t"\\\"")
          case '\\'         => escape(t"\\\\")
          case '\b'         => escape(t"\\b")
          case '\f'         => escape(t"\\f")
          case '\n'         => escape(t"\\n")
          case '\r'         => escape(t"\\r")
          case '\t'         => escape(t"\\t")
          case c if c < ' ' => escape(unicode(c))
          case _            => ()

        index += 1

      if length > start then producer.put(string.tt, start.z, length - start)
      producer.put("\"")

    def writeObject(node: IArray[Any], level: Int): Unit =
      val n = node.length/2
      producer.put("{")
      val last = n - 1
      var index = 0

      while index < n do
        if indentation then
          producer.put("\n")
          indent(level*2)

        writeString(node(index*2).asInstanceOf[String])
        producer.put(":")
        if indentation then producer.put(" ")
        recur(node(index*2 + 1).asInstanceOf[Json.Ast], level + 1)

        if index < last then producer.put(",")
        index += 1

      if indentation then
        producer.put("\n")
        indent(level*2 - 2)

      producer.put("}")

    def writeArray(elements: IArray[Any], level: Int): Unit =
      // Strip the sentinel pad if present (parity-padded heterogeneous arrays
      // carry one for empty/even-length cases).
      val raw = elements.length

      val n =
        if raw > 0 && (elements(raw - 1).asInstanceOf[AnyRef] eq Json.Ast.arrayPad)
        then raw - 1
        else raw

      producer.put("[")
      val last = n - 1
      var index = 0

      while index < n do
        if indentation then
          producer.put("\n")
          indent(level*2)

        recur(elements(index).asInstanceOf[Json.Ast], level + 1)
        if index < last then producer.put(",")
        index += 1

      if indentation then
        producer.put("\n")
        indent(level*2 - 2)

      producer.put("]")

    def writeBcdLongArray(bcds: Array[Long]): Unit =
      val n = bcds.length
      producer.put("[")
      val last = n - 1
      var index = 0

      while index < n do
        producer.put(Bcd.bcdLongText(bcds(index)).tt)
        if index < last then producer.put(",")
        index += 1

      producer.put("]")

    def writeSmallBcdArray(smalls: Array[Int]): Unit =
      val n = smalls.length
      producer.put("[")
      val last = n - 1
      var index = 0

      while index < n do
        producer.put(Bcd.bcdIntText(smalls(index)).tt)
        if index < last then producer.put(",")
        index += 1

      producer.put("]")

    def recur(json: Json.Ast, level: Int): Unit = json.asMatchable match
      case bcds: Array[Long] @unchecked =>
        writeBcdLongArray(bcds)

      case smalls: Array[Int] @unchecked =>
        writeSmallBcdArray(smalls)

      case bcd: Array[Double] @unchecked =>
        // High-precision number — emit the canonical JSON-number text from the
        // BCD nibble stream directly; this preserves all digits the parser saw,
        // in contrast to a `Double.toString` round-trip.
        producer.put(bcd.asInstanceOf[Bcd].text.tt)

      case smallBcd: Int =>
        // Small-BCD number — at most 7 nibbles packed into one Int.
        producer.put(Bcd.bcdIntText(smallBcd).tt)

      case arr: IArray[Any] @unchecked =>
        // Heterogeneous array or object, distinguished by length parity: even =
        // object (alternating key/value); odd = array (with optional sentinel
        // pad on the end).
        if (arr.length & 1) == 0 then writeObject(arr, level) else writeArray(arr, level)

      case long: Long =>
        producer.put(long.toString.tt)

      case double: Double =>
        producer.put(double.toString.tt)

      case string: String =>
        writeString(string)

      case boolean: Boolean =>
        producer.put(boolean.toString.tt)

      case _ =>
        producer.put("null")

    recur(json, 1)

trait JsonPrinter:
  def print(json: Json.Ast): Text
