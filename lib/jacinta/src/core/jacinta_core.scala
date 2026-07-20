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

import scala.language.dynamics
import scala.language.experimental.pureFunctions

import scala.compiletime.*

import anticipation.*
import contextual.*
import denominative.*
import gossamer.*
import prepositional.*
import spectacular.*
import vacuous.*
import wisteria.*
import zephyrine.*

export jacinta.internal.Bcd

// Renders a `Json.Ast` node to its serialized text. The whole serialization fold lives in this
// instance so that `ast.show` is the single route to JSON text; the producer is driven
// synchronously and the result collected into one `Text`. Number nodes are emitted from their
// BCD representation directly (preserving every digit the parser saw), and objects/heterogeneous
// arrays are distinguished by the length parity of their boxed `IArray[Any]` backing.
given showable: (formatting: Json.Formatting) => Json.Ast is Showable = ast =>
  Producer.collect[Text](): producer =>
    def newlineIndent(level: Int): Unit = formatting.indent.let: unit =>
      producer.put("\n")
      var i = 0

      while i < level do
        producer.put(unit)
        i += 1

    def unicode(char: Char): Text =
      val hex = Integer.toHexString(char.toInt).nn
      if hex.length == 1 then t"\\u000$hex" else t"\\u00$hex"

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
        newlineIndent(level)

        writeString(node(index*2).asInstanceOf[String])
        producer.put(":")
        if formatting.indent.present then producer.put(" ")
        recur(node(index*2 + 1).asInstanceOf[Json.Ast], level + 1)

        if index < last then producer.put(",")
        index += 1

      newlineIndent(level - 1)

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
        newlineIndent(level)

        recur(elements(index).asInstanceOf[Json.Ast], level + 1)
        if index < last then producer.put(",")
        index += 1

      newlineIndent(level - 1)

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

    recur(ast, 1)
    if formatting.trailingNewline then producer.put("\n")


extension (inline context: StringContext)
  transparent inline def j: Interpolation = interpolation[Json](context)
  transparent inline def jp: Interpolation = interpolation[JsonPointer](context)

package formatting:
  given indentedJsonFormatting: Json.Formatting =
    Json.Formatting(Text("  "), trailingNewline = false)

  given compactJsonFormatting: Json.Formatting = Json.Formatting(Unset, trailingNewline = false)

package discriminables:
  given jsonByTypeDiscriminable: [value] => value is Discriminable in Json =
    Json.discriminatedUnion[value]("type")

  given jsonByKindDiscriminable: [value] => value is Discriminable in Json =
    Json.discriminatedUnion[value]("kind")

  // `{"Circle": {"radius": 1.0}}` — the streaming-friendliest shape: the
  // tag is always the first token of the value.
  given jsonWrapperDiscriminable: [value] => value is Discriminable in Json =
    Json.DiscriminantWrapper[value]()

  // `{"type": "Circle", "value": {"radius": 1.0}}`
  given jsonEnvelopeDiscriminable: [value] => value is Discriminable in Json =
    Json.DiscriminantEnvelope[value]("type", "value")


package numberModes:
  given fullNumberMode:   NumberMode = NumberMode.Full
  given bcdNumberMode:    NumberMode = NumberMode.Bcd
  given doubleNumberMode: NumberMode = NumberMode.Double
