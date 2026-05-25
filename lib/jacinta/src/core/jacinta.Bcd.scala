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
// Storage is an `Array[Double]` whose first element is a header word and
// whose remaining elements pack 16 nibbles each into the Double's raw
// bit pattern. Each data word uses the full 64-bit Long-bit view of the
// Double — we round-trip via `Double.doubleToRawLongBits` and
// `Double.longBitsToDouble`, never doing any arithmetic on the doubles,
// so the Double's IEEE-754 interpretation is irrelevant. Safety comes
// from the BCD alphabet: nibbles are 0x0–0xC (digits, decimal point,
// `e`, `e-`), so the nibble at bits 52–55 of any data word is ≤ 0xC and
// therefore not 0xF, which means bits 52–62 can never all be 1 —
// no data word is ever a NaN bit pattern, regardless of platform or JIT
// NaN-payload handling.
//
// `Array[Double]` (`[D` at runtime) keeps `Bcd` distinct from the
// number-array variants `Array[Int]` (`[I`, arrays of single-Int small
// BCDs) and `Array[Long]` (`[J`, arrays of single-Long larger BCDs).
//
// Header word layout (raw bits):
//   bit 63        : sign (0 = non-negative, 1 = negative)
//   bits 52–62    : zero (so the header is a small denormal — also not NaN)
//   bits 0–51     : total nibble count
// Data word layout (raw bits):
//   bits 0–63     : 16 nibbles, oldest at bits 60–63, newest at bits 0–3,
//                   matching the parser's `(content << 4) | n` accumulator.
opaque type Bcd = Array[Double]

object Bcd:
  // Header masks for the raw-bits layout. Data words use all 64 bits.
  private inline val SignBit      = 0x8000_0000_0000_0000L  // bit 63
  private inline val MantissaMask = 0x000F_FFFF_FFFF_FFFFL  // bits 0–51
  inline val NibblesPerDouble = 16

  private inline def toRawBits(d: Double): Long =
    java.lang.Double.doubleToRawLongBits(d)

  private inline def fromRawBits(l: Long): Double =
    java.lang.Double.longBitsToDouble(l)

  // Encode 16 nibbles (already packed in `nibbles`) as a storage word.
  // BCD nibbles ≤ 0xC guarantee the bit pattern is never NaN, so the
  // raw 64-bit value is stored verbatim.
  private inline def packDataDouble(nibbles: Long): Double = fromRawBits(nibbles)

  // Encode the Bcd header. Sign goes to bit 63 (the double's own sign
  // bit); count lands in the mantissa (low 52 bits, plenty).
  private inline def packHeaderDouble(negative: Boolean, count: Int): Double =
    val sign = if negative then SignBit else 0L
    fromRawBits(sign | (count.toLong & MantissaMask))

  // Internal: wrap a freshly-built header+data array as a `Bcd`.
  private[jacinta] inline def wrap(arr: Array[Double]): Bcd = arr

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
  // walk mirrors `Bcd.each` but operates on a single Long. The parser
  // omits the redundant leading "0" for "0.xxx" inputs, so this re-
  // inserts a "0" when the oldest nibble is "." (`0xA`) to keep the
  // output a valid JSON number.
  def bcdLongText(value: Long): String =
    val count = bcdLongNibbleCount(value)
    if count == 0 then "0"
    else
      val sb = new java.lang.StringBuilder(count + 2)
      if bcdLongNegative(value) then sb.append('-')

      val firstNibble = ((value >>> ((count - 1)*4)) & 0xFL).toInt
      if firstNibble == 0xA then sb.append('0')

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
      val sb = new java.lang.StringBuilder(count + 2)
      if bcdIntNegative(value) then sb.append('-')

      val firstNibble = (value >>> ((count - 1)*4)) & 0xF
      if firstNibble == 0xA then sb.append('0')

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

  // Re-encode a single-Int small-BCD value into the single-Long BCD form.
  // Used by the array parser's `bcdInt` → `bcdLong` migration step.
  inline def repackBcdIntAsLong(value: Int): Long =
    val sign     = if value < 0 then BcdLongSignBit else 0L
    val count    = (((value >>> 28) & 0x7).toLong) << 56
    val nibbles  = (value & 0x0FFF_FFFF).toLong
    sign | count | nibbles

  // Re-encode a single-Long BCD value as a single-Int small BCD. The
  // caller is responsible for ensuring the value has ≤ 7 nibbles (so the
  // nibble bits fit in the low 28 bits of the Int). Cheap bit-shuffle —
  // sign moves from bit 63 to bit 31; count moves from bits 56–62 to
  // bits 28–30; nibbles in bits 0–27 are unchanged.
  inline def packBcdIntFromLong(value: Long): Int =
    val sign     = if (value & BcdLongSignBit) != 0L then BcdIntSignBit else 0
    val count    = (((value >>> 56) & 0x7L).toInt) << 28
    val nibbles  = (value & 0x0FFF_FFFFL).toInt
    sign | count | nibbles

  // Build a `Bcd` directly from the parser's in-Long fast-path
  // accumulator when it filled to capacity (15 nibbles) and the caller
  // wants a `Bcd` instead of a single-Long packing. Skips the
  // `Bcd.Builder` allocation and the `seedFromLong` re-walk; the layout
  // is fixed (1 header + 1 data word).
  private[jacinta] inline def fromContent15(content: Long, negative: Boolean): Bcd =
    val arr = new Array[Double](2)
    arr(0) = packHeaderDouble(negative, 15)
    arr(1) = packDataDouble(content)
    arr

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
  // Skips a leading `0` if it's followed by `.` — matches the parser's
  // convention (the printer reinserts the `0` on emit).
  def fromString(text: String, negative: Boolean): Bcd =
    val builder = new Builder
    var i = if text.length >= 2 && text.charAt(0) == '0' && text.charAt(1) == '.' then 1 else 0
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
  // `Array[Double]` of completed words and a current 52-bit nibble buffer.
  final class Builder:
    private var data: Array[Double] = new Array[Double](2)
    private var wordIdx: Int = 0
    private var word: Long = 0L   // raw nibble buffer; packed into a Double on commit
    private var inWord: Int = 0
    private var nibbles: Int = 0

    // Append one nibble (0x0–0xC) to the in-progress word. When the word
    // fills (13 nibbles), it is committed to the `data` array.
    def add(nibble: Int): Unit =
      word = (word << 4) | (nibble & 0xFL)
      inWord += 1
      nibbles += 1

      if inWord == NibblesPerDouble then
        ensureCapacity(wordIdx + 1)
        data(wordIdx) = packDataDouble(word)
        wordIdx += 1
        word = 0L
        inWord = 0

    // Replace the most recently-added nibble. Used by the parser to rewrite
    // an emitted `e` (0xB) as `e-` (0xC) when a `-` follows.
    def overwriteLast(nibble: Int): Unit =
      val mask = 0xFL
      val v = nibble & mask

      if inWord > 0 then word = (word & ~mask) | v
      else if wordIdx > 0 then
        val prev = toRawBits(data(wordIdx - 1))
        data(wordIdx - 1) = packDataDouble((prev & ~mask) | v)

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
      val totalDataDoubles = if inWord > 0 then wordIdx + 1 else wordIdx
      val arr = new Array[Double](1 + totalDataDoubles)
      arr(0) = packHeaderDouble(negative, nibbles)
      System.arraycopy(data, 0, arr, 1, wordIdx)
      if inWord > 0 then arr(1 + wordIdx) = packDataDouble(word)
      arr

    private def ensureCapacity(needed: Int): Unit =
      if needed > data.length then
        val newSize = (data.length * 2).max(needed)
        val newData = new Array[Double](newSize)
        System.arraycopy(data, 0, newData, 0, wordIdx)
        data = newData

  extension (bcd: Bcd)
    def negative:    Boolean = (toRawBits(bcd(0)) & SignBit) != 0L
    def nibbleCount: Int     = (toRawBits(bcd(0)) & MantissaMask).toInt

    // Iterate nibbles in left-to-right (oldest-first) order, invoking the
    // action for each. Used by the printer to emit a JSON-number string and
    // by conversion routines (`toBigDecimal`, `toDouble`, `toLong`).
    inline def each(inline action: Int => Unit): Unit =
      val total = bcd.nibbleCount
      val fullDoubles = total/NibblesPerDouble
      val partial = total - fullDoubles*NibblesPerDouble
      var i = 0

      while i < fullDoubles do
        val w = toRawBits(bcd(1 + i))
        var j = NibblesPerDouble - 1

        while j >= 0 do
          action(((w >>> (j*4)) & 0xFL).toInt)
          j -= 1

        i += 1

      if partial > 0 then
        val w = toRawBits(bcd(1 + fullDoubles))
        var j = partial - 1

        while j >= 0 do
          action(((w >>> (j*4)) & 0xFL).toInt)
          j -= 1

    // Render as a JSON number string (canonical form: digits, optional `.`
    // and fraction, optional `e[-]?\d+`). Re-inserts a leading "0" when
    // the oldest nibble is "." — the parser and `fromString` omit it for
    // "0.xxx" inputs, but JSON requires it.
    def text: String =
      if bcd.nibbleCount == 0 then "0"
      else
        val sb = new java.lang.StringBuilder(bcd.nibbleCount + 2)
        if bcd.negative then sb.append('-')
        var first = true

        bcd.each: nibble =>
          if first then
            first = false
            if nibble == 0xA then sb.append('0')

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
