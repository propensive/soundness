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
package merino

import vacuous.*

// `Bcd` is the high-precision-number representation in `JsonAst`. It encodes a
// JSON number as a sign bit plus a sequence of nibbles drawn from the same
// alphabet used by `JsonParser`'s in-Long fast path:
//
//   0x0–0x9 : decimal digits, in left-to-right order
//   0xA     : decimal point
//   0xB     : exponent marker `e`
//   0xC     : exponent marker `e-` (negative exponent — single nibble)
//
// Storage is an `Array[Long]` whose first element is a header word and whose
// remaining elements pack 16 nibbles each. Nibbles within a Long are
// LSB-first / least-recent-at-the-bottom — i.e., for a fully-packed Long the
// oldest nibble is at bits 60–63 and the newest is at bits 0–3, matching the
// `(content << 4) | n` accumulator in the parser. The trailing data Long may
// be partially filled with K < 16 nibbles right-justified in bits 0..K*4-1;
// the count in the header tells readers where the partial fill ends.
//
// Header word layout:
//   bit 63        : sign (0 = non-negative, 1 = negative)
//   bits 0–31     : total nibble count
//   bits 32–62    : reserved (always 0)
opaque type Bcd = Array[Long]

object Bcd:
  private inline val SignBit       = 0x8000000000000000L
  private inline val CountMask     = 0xFFFFFFFFL
  inline val NibblesPerLong = 16

  // Internal: wrap a freshly-built header+data array as a `Bcd`.
  private[merino] inline def wrap(arr: Array[Long]): Bcd = arr

  // Build a `Bcd` from a `BigDecimal`. Goes via `toPlainString` so the result
  // matches the in-AST representation a parser would produce for the same
  // textual JSON number.
  def apply(value: BigDecimal): Bcd =
    val s: String = value.bigDecimal.nn.toPlainString.nn
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
      val c = text.charAt(i)
      (c: @scala.annotation.switch) match
        case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
          builder.add(c - '0'); prevWasE = false
        case '.' => builder.add(0xA); prevWasE = false
        case 'e' | 'E' => builder.add(0xB); prevWasE = true
        case '+' if prevWasE => prevWasE = false
        case '-' if prevWasE =>
          builder.overwriteLast(0xC); prevWasE = false
        case _ => () // ignore unexpected characters
      i += 1
    builder.finish(negative)

  // Incremental builder used by `JsonParser` to assemble a `Bcd` one nibble
  // at a time as it overflows the in-Long fast path. Keeps a growing
  // `Array[Long]` of completed words and a current word being filled in.
  final class Builder:
    private var data: Array[Long] = new Array[Long](2)
    private var wordIdx: Int = 0
    private var word: Long = 0L
    private var inWord: Int = 0
    private var nibbles: Int = 0

    // Append one nibble (0x0–0xC) to the in-progress word. When the word
    // fills (16 nibbles), it is committed to the `data` array.
    def add(nibble: Int): Unit =
      word = (word << 4) | (nibble & 0xFL)
      inWord += 1
      nibbles += 1
      if inWord == NibblesPerLong then
        ensureCapacity(wordIdx + 1)
        data(wordIdx) = word
        wordIdx += 1
        word = 0L
        inWord = 0

    // Replace the most recently-added nibble. Used by the parser to rewrite
    // an emitted `e` (0xB) as `e-` (0xC) when a `-` follows.
    def overwriteLast(nibble: Int): Unit =
      val mask = 0xFL
      val v = nibble & mask
      if inWord > 0 then word = (word & ~mask) | v
      else if wordIdx > 0 then data(wordIdx - 1) = (data(wordIdx - 1) & ~mask) | v

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
      val totalDataLongs = if inWord > 0 then wordIdx + 1 else wordIdx
      val arr = new Array[Long](1 + totalDataLongs)
      arr(0) = (if negative then SignBit else 0L) | (nibbles.toLong & CountMask)
      System.arraycopy(data, 0, arr, 1, wordIdx)
      if inWord > 0 then arr(1 + wordIdx) = word
      arr

    private def ensureCapacity(needed: Int): Unit =
      if needed > data.length then
        val newSize = (data.length * 2).max(needed)
        val newData = new Array[Long](newSize)
        System.arraycopy(data, 0, newData, 0, wordIdx)
        data = newData

  extension (bcd: Bcd)
    def negative:    Boolean = (bcd(0) & SignBit) != 0L
    def nibbleCount: Int     = (bcd(0) & CountMask).toInt

    // Iterate nibbles in left-to-right (oldest-first) order, invoking the
    // action for each. Used by the printer to emit a JSON-number string and
    // by conversion routines (`toBigDecimal`, `toDouble`, `toLong`).
    inline def each(inline action: Int => Unit): Unit =
      val total = bcd.nibbleCount
      val fullLongs = total/NibblesPerLong
      val partial = total - fullLongs*NibblesPerLong
      var i = 0
      while i < fullLongs do
        val w = bcd(1 + i)
        var j = NibblesPerLong - 1
        while j >= 0 do
          action(((w >>> (j*4)) & 0xFL).toInt)
          j -= 1
        i += 1
      if partial > 0 then
        val w = bcd(1 + fullLongs)
        var j = partial - 1
        while j >= 0 do
          action(((w >>> (j*4)) & 0xFL).toInt)
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
      sb.toString.nn

    def toBigDecimal: BigDecimal = BigDecimal(bcd.text)
    def toDouble:     Double     = java.lang.Double.parseDouble(bcd.text)

    // Returns the value as a `Long` if it represents an exact integer that
    // fits in `Long`'s range; `Unset` otherwise. Lightweight: a non-fitting
    // value is detected by `Long.parseLong`'s `NumberFormatException`.
    def toLong: Optional[Long] =
      val s = bcd.text
      try java.lang.Long.parseLong(s) catch case _: NumberFormatException => Unset
