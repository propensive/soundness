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

import language.dynamics
import language.experimental.pureFunctions

import scala.compiletime.*

import anticipation.*
import contextual.*
import contingency.*
import denominative.*
import gossamer.*
import prepositional.*
import rudiments.*
import spectacular.*
import vacuous.*
import wisteria.*
import zephyrine.*

import JsonError.Reason

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

extension (json: Json.Ast)
  inline def isNumber: Boolean = isDouble || isLong || isBcd || isSmallBcd
  inline def isAbsent: Boolean = json == Unset
  inline def isLong: Boolean = json.isInstanceOf[Long]
  inline def isDouble: Boolean = json.isInstanceOf[Double]

  // Small-BCD number node — a single number with up to 7 nibbles packed
  // into one `Int` (see `Bcd.packBcdInt`). The boxed runtime class is
  // `java.lang.Integer`, distinct from `java.lang.Long` (`isLong`), so
  // the type test reliably separates the two.
  inline def isSmallBcd: Boolean = json.isInstanceOf[Int]

  // High-precision number node. `Bcd` is `Array[Double]` (`[D`) at
  // runtime, with each Double's raw bits packing 13 nibbles in its
  // mantissa — distinct from `Array[Int]` (`[I`, arrays of small BCDs),
  // `Array[Long]` (`[J`, arrays of larger BCDs), and `Array[AnyRef]`
  // (`[Ljava/lang/Object;`).
  inline def isBcd: Boolean = json.isInstanceOf[Array[Double]]
  inline def isString: Boolean = json.isInstanceOf[String]
  inline def isBoolean: Boolean = json.isInstanceOf[Boolean]
  inline def isNull: Boolean = json.asInstanceOf[AnyRef] eq Json.JsonNull

  // Objects and heterogeneous arrays share a runtime representation
  // (`IArray[Any]` = `[Ljava/lang/Object;`) and are distinguished by
  // length parity: even = object (alternating `key, value, …`), odd =
  // array (with sentinel padding when the logical element count is even).
  // Number-only arrays are stored unboxed as `Array[Double]` (`[D`),
  // a distinct runtime class.
  inline def isObject: Boolean =
    json.isInstanceOf[Array[AnyRef]] &&
      (json.asInstanceOf[Array[?]].length & 1) == 0

  inline def isArray: Boolean =
    json.isInstanceOf[Array[Long]] ||
      json.isInstanceOf[Array[Int]] ||
      (json.isInstanceOf[Array[AnyRef]] &&
        (json.asInstanceOf[Array[?]].length & 1) == 1)

  // True when the array is in either unboxed number-only form (BCD-packed).
  inline def isNumberArray: Boolean =
    json.isInstanceOf[Array[Long]] || json.isInstanceOf[Array[Int]]

  // True when the array is in the small-BCD `Array[Int]` form.
  inline def isBcdIntArray:  Boolean = json.isInstanceOf[Array[Int]]

  // True when the array is in the larger-BCD `Array[Long]` form.
  inline def isBcdLongArray: Boolean = json.isInstanceOf[Array[Long]]

  private def expected(jsonPrimitive: JsonPrimitive): Unit raises JsonError =
    raise(JsonError(if isAbsent then Reason.Absent else Reason.NotType(primitive, jsonPrimitive)))

  // Returns the user-visible element count of an array node (excludes the
  // sentinel pad if present).
  inline def arrayLength: Int = Json.Ast.arrayLength(json)

  // Materialise an array element as a `Json.Ast` node. Three cases:
  //   - `Array[Long]`: a single-Long BCD-packed number — decoded back to
  //     `Long` if it represents an exact integer that fits, else to
  //     `Double` via the canonical text form.
  //   - `Array[Int]`: a single-Int small BCD — the raw `Int` *is* a
  //     small-BCD `JsonNumber`, so it surfaces directly via `Json.Ast(_)`.
  //   - boxed `IArray[Any]`: direct indexed lookup.
  def arrayElement(index: Int): Json.Ast = (json: @unchecked) match
    case bcds: Array[Long] @unchecked =>
      val v = bcds(index)
      val text = Bcd.bcdLongText(v)

      try Json.Ast(java.lang.Long.parseLong(text))
      catch case _: NumberFormatException => Json.Ast(java.lang.Double.parseDouble(text))

    case smalls: Array[Int] @unchecked =>
      Json.Ast(smalls(index))

    case _ =>
      json.asInstanceOf[IArray[Json.Ast]](index)

  // Returns the number of key/value pairs in an object node.
  inline def objectSize: Int = Json.Ast.objectSize(json)

  inline def objectKey(index: Int): String =
    json.asInstanceOf[IArray[Any]](index*2).asInstanceOf[String]

  inline def objectValue(index: Int): Json.Ast =
    json.asInstanceOf[IArray[Any]](index*2 + 1).asInstanceOf[Json.Ast]

  // Linear scan for a key. Returns the value index (in pair units) or -1.
  def objectIndexOf(key: String): Int =
    val arr = json.asInstanceOf[IArray[Any]]
    val len = arr.length
    var i = 0

    while i < len do
      if arr(i) == key then return i/2
      i += 2
    -1

  def array: IArray[Json.Ast] raises JsonError = (json: @unchecked) match
    case bcds: Array[Long] @unchecked =>
      IArray.tabulate(bcds.length)(json.arrayElement(_))

    case smalls: Array[Int] @unchecked =>
      IArray.tabulate(smalls.length)(json.arrayElement(_))

    case _ =>
      if isArray then
        val full = json.asInstanceOf[IArray[Json.Ast]]
        val n = Json.Ast.arrayLength(json)

        if n == full.length then full
        else IArray.tabulate(n)(full(_))
      else
        expected(JsonPrimitive.Array) yet IArray[Json.Ast]()

  def double: Double raises JsonError = json.asMatchable match
    case value: Double                   => value
    case value: Long                     => value.toDouble
    case value: Int                      => Bcd.bcdIntToDouble(value)
    case value: Array[Double] @unchecked => value.asInstanceOf[Bcd].toDouble
    case _                               => expected(JsonPrimitive.Number) yet 0.0

  def bcd: Bcd raises JsonError = json.asMatchable match
    case value: Array[Double] @unchecked => value.asInstanceOf[Bcd]
    case value: Long                     => Bcd(BigDecimal(value))
    case value: Double                   => Bcd(BigDecimal(value))

    case value: Int =>
      Bcd.fromString(Bcd.bcdIntText(value).stripPrefix("-"), value < 0)

    case _ =>
      expected(JsonPrimitive.Number) yet Bcd(BigDecimal(0L))

  def long: Long raises JsonError = json.asMatchable match
    case value: Long                     => value
    case value: Double                   => value.toLong
    case value: Int                      => Bcd.bcdIntToDouble(value).toLong
    case value: Array[Double] @unchecked => value.asInstanceOf[Bcd].toLong.or(0L)
    case _                               => expected(JsonPrimitive.Number) yet 0L

  def primitive: JsonPrimitive =
    if isNumber then JsonPrimitive.Number
    else if isBoolean then JsonPrimitive.Boolean
    else if isString then JsonPrimitive.String
    else if isObject then JsonPrimitive.Object
    else if isArray then JsonPrimitive.Array
    else JsonPrimitive.Null

  def string: Text raises JsonError =
    if isString then json.asInstanceOf[Text]
    else expected(JsonPrimitive.String) yet "".tt

  def boolean: Boolean raises JsonError =
    if isBoolean then json.asInstanceOf[Boolean]
    else expected(JsonPrimitive.Boolean) yet false

  // Returns a (keys, values) view over an object node. This *materialises*
  // two new IArrays from the flat alternating layout, so prefer
  // `objectKey`/`objectValue` when you only need a few entries.
  def obj: (IArray[String], IArray[Json.Ast]) raises JsonError =
    if !isObject then expected(JsonPrimitive.Object) yet (IArray[String]() -> IArray[Json.Ast]())
    else
      val arr = json.asInstanceOf[IArray[Any]]
      val n = arr.length/2
      val keys = new Array[String](n)
      val values = new Array[Json.Ast](n)
      var i = 0

      while i < n do
        keys(i) = arr(i*2).asInstanceOf[String]
        values(i) = arr(i*2 + 1).asInstanceOf[Json.Ast]
        i += 1

      (keys.asInstanceOf[IArray[String]], values.asInstanceOf[IArray[Json.Ast]])

  def number: Long | Double | Bcd raises JsonError =
    if isLong then long
    else if isDouble then double
    else if isBcd then bcd
    else if isSmallBcd then long
    else expected(JsonPrimitive.Number) yet 0L

extension [entity: Encodable in Json](value: entity) def json: Json = value.encode

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


package numberModes:
  given fullNumberMode:   NumberMode = NumberMode.Full
  given bcdNumberMode:    NumberMode = NumberMode.Bcd
  given doubleNumberMode: NumberMode = NumberMode.Double
