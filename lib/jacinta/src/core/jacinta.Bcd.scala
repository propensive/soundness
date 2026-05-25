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

import rudiments.*
import vacuous.*

// `Bcd` is the high-precision-number representation in `Json.Ast`. It encodes a
// JSON number as a sign bit plus a sequence of nibbles drawn from the same
// alphabet used by `JsonParser`'s in-Long fast path:
//
//   0x0–0x9 : decimal digits, in left-to-right order
//   0xA     : decimal point
//   0xB     : exponent marker `e`
//   0xC     : exponent marker `e-` (negative exponent — single nibble)
//
// Storage is an `Array[Short]` whose first element is a header word and
// whose remaining elements pack 4 nibbles each. Nibbles within a `Short`
// are LSB-first / least-recent-at-the-bottom — i.e., for a fully-packed
// `Short` the oldest nibble is at bits 12–15 and the newest is at bits
// 0–3, matching the `(content << 4) | n` accumulator in the parser. The
// trailing data `Short` may be partially filled with K < 4 nibbles
// right-justified in bits 0..K*4-1; the count in the header tells
// readers where the partial fill ends.
//
// `Array[Short]` (`[S` at runtime) keeps `Bcd` distinct from the
// number-array variants `Array[Int]` (`[I`, arrays of single-Int small
// BCDs), `Array[Long]` (`[J`, arrays of single-Long larger BCDs), and
// `Array[Double]` (`[D`, double-valued arrays).
//
// Header word layout:
//   bit 15        : sign (0 = non-negative, 1 = negative)
//   bits 0–14     : total nibble count (up to 32767)
opaque type Bcd = Array[Short]

object Bcd:
  private inline val SignBit   = 0x8000
  private inline val CountMask = 0x7FFF
  inline val NibblesPerShort = 4

  // Internal: wrap a freshly-built header+data array as a `Bcd`.
  private[jacinta] inline def wrap(arr: Array[Short]): Bcd = arr

  // Single-Long BCD encoding for arrays of numbers — see `Array[Long]` as
  // a `Json.Ast` array variant. One number per Long:
  //   bit 63        : sign (0 = non-negative, 1 = negative)
  //   bits 56–62    : nibble count (7 bits; max 14)
  //   bits 0–55     : up to 14 nibbles, oldest at the highest used position
  //                   (bit 52–55 when count = 14), newest at bits 0–3 —
  //                   matching the parser's `(content << 4) | n` order.
  // A value of `0L` is reserved for "uninitialised slot" since a valid
  // BCD-Long always has count >= 1 (the digit `0` is encoded as count = 1,
  // sign = 0, nibble 0x0 → header byte 0x01).
  inline val MaxBcdLongNibbles = 14
  private inline val BcdLongSignBit    = 0x8000_0000_0000_0000L
  private inline val BcdLongCountMask  = 0x7F00_0000_0000_0000L   // bits 56–62
  private inline val BcdLongNibbleMask = 0x00FF_FFFF_FFFF_FFFFL   // bits 0–55

  // Pack the parser's in-Long fast-path accumulator (`content` with
  // `nibbles` digits, plus sign) into the single-Long BCD format. The
  // caller is responsible for checking `nibbles <= MaxBcdLongNibbles`.
  inline def packBcdLong(content: Long, nibbles: Int, negative: Boolean): Long =
    val sign  = if negative then BcdLongSignBit else 0L
    val count = (nibbles.toLong & 0x7FL) << 56
    sign | count | (content & BcdLongNibbleMask)

  // Decode a single-Long BCD value's nibble count (1..14 for valid values).
  inline def bcdLongNibbleCount(value: Long): Int =
    ((value >>> 56) & 0x7FL).toInt

  // Decode a single-Long BCD value's sign.
  inline def bcdLongNegative(value: Long): Boolean = value < 0L

  // Decode a single-Long BCD value to its canonical JSON-number text. The
  // walk mirrors `Bcd.each` but operates on a single Long.
  def bcdLongText(value: Long): String =
    val count = bcdLongNibbleCount(value)
    if count == 0 then "0"
    else
      val sb = new java.lang.StringBuilder(count + 1)
      if bcdLongNegative(value) then sb.append('-')
      var j = count - 1

      while j >= 0 do
        val n = ((value >>> (j*4)) & 0xFL).toInt
        if      n <= 9     then sb.append(('0' + n).toChar)
        else if n == 0xA   then sb.append('.')
        else if n == 0xB   then sb.append('e')
        else if n == 0xC   then sb.append("e-")
        j -= 1

      sb.toString

  // Convert a single-Long BCD value to `Double` via its text form.
  inline def bcdLongToDouble(value: Long): Double =
    java.lang.Double.parseDouble(bcdLongText(value))

  // Single-Int BCD encoding for small numbers — a JSON number with up to
  // 7 nibbles fits in a single `Int`, with the same per-Long bit-layout
  // pattern but compressed:
  //   bit 31        : sign (0 = non-negative, 1 = negative)
  //   bits 28–30    : nibble count (3 bits; max 7)
  //   bits 0–27     : up to 7 nibbles, oldest at the highest used position
  //                   (bit 24–27 when count = 7), newest at bits 0–3.
  // Stored as the `Int` variant of `JsonNumber` for numbers that fit; the
  // boxed `Integer` runtime class is distinct from `Long` (`Long`) so the
  // pattern match in `Json.Ast` extensions can distinguish them.
  inline val MaxBcdIntNibbles = 7
  private inline val BcdIntSignBit    = 0x8000_0000
  private inline val BcdIntCountMask  = 0x7000_0000   // bits 28–30
  private inline val BcdIntNibbleMask = 0x0FFF_FFFF   // bits 0–27

  // Pack the parser's in-Long fast-path accumulator into the single-Int
  // BCD format. The caller is responsible for `nibbles <= MaxBcdIntNibbles`.
  inline def packBcdInt(content: Long, nibbles: Int, negative: Boolean): Int =
    val sign  = if negative then BcdIntSignBit else 0
    val count = (nibbles & 0x7) << 28
    sign | count | (content.toInt & BcdIntNibbleMask)

  inline def bcdIntNibbleCount(value: Int): Int = (value >>> 28) & 0x7
  inline def bcdIntNegative(value: Int): Boolean = value < 0

  def bcdIntText(value: Int): String =
    val count = bcdIntNibbleCount(value)
    if count == 0 then "0"
    else
      val sb = new java.lang.StringBuilder(count + 1)
      if bcdIntNegative(value) then sb.append('-')
      var j = count - 1

      while j >= 0 do
        val n = (value >>> (j*4)) & 0xF
        if      n <= 9     then sb.append(('0' + n).toChar)
        else if n == 0xA   then sb.append('.')
        else if n == 0xB   then sb.append('e')
        else if n == 0xC   then sb.append("e-")
        j -= 1

      sb.toString

  inline def bcdIntToDouble(value: Int): Double =
    java.lang.Double.parseDouble(bcdIntText(value))

  // Build a `Bcd` from a `BigDecimal`. Goes via `toPlainString` so the result
  // matches the in-AST representation a parser would produce for the same
  // textual JSON number.
  def apply(value: BigDecimal): Bcd =
    val s: String = value.bigDecimal.toPlainString.nn
    val negative = s.startsWith("-")
    fromString(if negative then s.substring(1).nn else s, negative)

  // Public construction from a positive-magnitude JSON-number string. The
  // caller is responsible for stripping any leading `-`. The string may
  // contain digits, a single `.`, and an optional `e[+-]?\d+` suffix.
  def fromString(text: String, negative: Boolean): Bcd =
    val builder = new Builder
    var i = 0
    val n = text.length
    var prevWasE = false

    while i < n do
      val char = text.charAt(i)

      (char: @scala.annotation.switch) match
        case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
          builder.add(char - '0')
          prevWasE = false

        case '.' =>
          builder.add(0xA)
          prevWasE = false

        case 'e' | 'E' =>
          builder.add(0xB)
          prevWasE = true

        case '+' if prevWasE =>
          prevWasE = false

        case '-' if prevWasE =>
          builder.overwriteLast(0xC)
          prevWasE = false

        case _ => ()

      i += 1

    builder.finish(negative)

  // Incremental builder used by `JsonParser` to assemble a `Bcd` one nibble
  // at a time as it overflows the in-Long fast path. Keeps a growing
  // `Array[Short]` of completed words and a current word being filled in.
  final class Builder:
    private var data: Array[Short] = new Array[Short](2)
    private var wordIdx: Int = 0
    private var word: Int = 0   // intermediate; final word stored as Short
    private var inWord: Int = 0
    private var nibbles: Int = 0

    // Append one nibble (0x0–0xC) to the in-progress word. When the word
    // fills (4 nibbles), it is committed to the `data` array.
    def add(nibble: Int): Unit =
      word = ((word << 4) | (nibble & 0xF)) & 0xFFFF
      inWord += 1
      nibbles += 1

      if inWord == NibblesPerShort then
        ensureCapacity(wordIdx + 1)
        data(wordIdx) = word.toShort
        wordIdx += 1
        word = 0
        inWord = 0

    // Replace the most recently-added nibble. Used by the parser to rewrite
    // an emitted `e` (0xB) as `e-` (0xC) when a `-` follows.
    def overwriteLast(nibble: Int): Unit =
      val mask = 0xF
      val v = nibble & mask

      if inWord > 0 then word = (word & ~mask) | v
      else if wordIdx > 0 then
        val prev = data(wordIdx - 1).toInt & 0xFFFF
        data(wordIdx - 1) = ((prev & ~mask) | v).toShort

    // Snapshot the in-Long fast-path accumulator (15 nibbles) into the
    // builder, so the parser can hand off mid-number without losing state.
    def seedFromLong(content: Long, count: Int): Unit =
      var i = count - 1

      while i >= 0 do
        add(((content >>> (i * 4)) & 0xFL).toInt)
        i -= 1

    def nibbleCount: Int = nibbles

    // Produce the final `Bcd`. The trailing partial word (if any) is
    // committed right-justified in its data slot.
    def finish(negative: Boolean): Bcd =
      val totalDataShorts = if inWord > 0 then wordIdx + 1 else wordIdx
      val arr = new Array[Short](1 + totalDataShorts)
      arr(0) = ((if negative then SignBit else 0) | (nibbles & CountMask)).toShort
      System.arraycopy(data, 0, arr, 1, wordIdx)
      if inWord > 0 then arr(1 + wordIdx) = word.toShort
      arr

    private def ensureCapacity(needed: Int): Unit =
      if needed > data.length then
        val newSize = (data.length * 2).max(needed)
        val newData = new Array[Short](newSize)
        System.arraycopy(data, 0, newData, 0, wordIdx)
        data = newData

  extension (bcd: Bcd)
    def negative:    Boolean = (bcd(0).toInt & SignBit) != 0
    def nibbleCount: Int     = bcd(0).toInt & CountMask

    // Iterate nibbles in left-to-right (oldest-first) order, invoking the
    // action for each. Used by the printer to emit a JSON-number string and
    // by conversion routines (`toBigDecimal`, `toDouble`, `toLong`).
    inline def each(inline action: Int => Unit): Unit =
      val total = bcd.nibbleCount
      val fullShorts = total/NibblesPerShort
      val partial = total - fullShorts*NibblesPerShort
      var i = 0

      while i < fullShorts do
        val w = bcd(1 + i).toInt & 0xFFFF
        var j = NibblesPerShort - 1

        while j >= 0 do
          action((w >>> (j*4)) & 0xF)
          j -= 1

        i += 1

      if partial > 0 then
        val w = bcd(1 + fullShorts).toInt & 0xFFFF
        var j = partial - 1

        while j >= 0 do
          action((w >>> (j*4)) & 0xF)
          j -= 1

    // Render as a JSON number string (canonical form: digits, optional `.`
    // and fraction, optional `e[-]?\d+`).
    def text: String =
      val sb = new java.lang.StringBuilder(bcd.nibbleCount + 1)
      if bcd.negative then sb.append('-')

      bcd.each: nibble =>
        if nibble <= 9 then sb.append(('0' + nibble).toChar)
        else if nibble == 0xA then sb.append('.')
        else if nibble == 0xB then sb.append('e')
        else if nibble == 0xC then sb.append("e-")

      sb.toString

    def toBigDecimal: BigDecimal = BigDecimal(bcd.text)
    def toDouble:     Double     = java.lang.Double.parseDouble(bcd.text)

    // Returns the value as a `Long` if it represents an exact integer that
    // fits in `Long`'s range; `Unset` otherwise. Lightweight: a non-fitting
    // value is detected by `Long.parseLong`'s `NumberFormatException`.
    def toLong: Optional[Long] =
      val s = bcd.text
      try java.lang.Long.parseLong(s) catch case _: NumberFormatException => Unset
