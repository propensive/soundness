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
┃    Soundness, version 0.64.0.                                                                    ┃
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
package zephyrine

import java.lang as jl

import anticipation.*
import vacuous.*

// UTF-8 over a byte buffer, for parsers that scan bytes and materialize `Text` only for the
// slices they keep. Decoding is strict, as `Ductile.charset`'s UTF-8 kernel is: overlong
// forms, surrogates and values above U+10FFFF are malformed, never substituted, so a parser
// can reject bad input rather than silently carry U+FFFD into a document. An all-ASCII slice
// costs one array copy: the JDK's Latin-1 `String` constructor stores it byte for byte
// without validation.
object Utf8:
  private val Latin1: java.nio.charset.Charset = java.nio.charset.StandardCharsets.ISO_8859_1.nn

  // `point`'s two failure results, both negative so a single sign test separates them from
  // any code point.
  inline val Malformed  = -1
  inline val Incomplete = -2

  // The number of bytes in the sequence a lead byte (an unsigned value) begins: 1 for ASCII,
  // 2 to 4 for a multi-byte lead, or 0 for a byte that cannot lead a sequence — a continuation
  // byte, the overlong leads C0 and C1, or F5 to FF.
  inline def width(lead: Int): Int =
    if lead < 0x80 then 1
    else if lead < 0xc2 then 0
    else if lead < 0xe0 then 2
    else if lead < 0xf0 then 3
    else if lead < 0xf5 then 4
    else 0

  // Whether the byte is a continuation byte, `10xxxxxx`. Counting the bytes that are *not*
  // continuations counts the code points in a range.
  inline def continuation(byte: Byte): Boolean = (byte & 0xc0) == 0x80

  // The code point of the sequence beginning at `offset` and lying wholly before `end`, or
  // `Malformed` for an invalid sequence, or `Incomplete` for a valid prefix that `end` cuts
  // short — a caller with more input to come refills and retries. The second byte's range
  // depends on the lead: E0 and F0 reject overlongs, ED the surrogate range, F4 anything
  // above U+10FFFF.
  def point(bytes: scala.Array[Byte], offset: Int, end: Int): Int =
    val b0 = bytes(offset) & 0xff

    if b0 < 0x80 then b0 else
      val n = width(b0)
      val remaining = end - offset

      if n == 0 then Malformed
      else if remaining < 2 then Incomplete
      else
        val b1 = bytes(offset + 1) & 0xff

        val second =
          if b0 == 0xe0 then b1 >= 0xa0 && b1 <= 0xbf
          else if b0 == 0xed then b1 >= 0x80 && b1 <= 0x9f
          else if b0 == 0xf0 then b1 >= 0x90 && b1 <= 0xbf
          else if b0 == 0xf4 then b1 >= 0x80 && b1 <= 0x8f
          else (b1 & 0xc0) == 0x80

        if !second then Malformed
        else if n == 2 then ((b0 & 0x1f) << 6) | (b1 & 0x3f)
        else if remaining < 3 then Incomplete
        else
          val b2 = bytes(offset + 2) & 0xff

          if (b2 & 0xc0) != 0x80 then Malformed
          else if n == 3 then ((b0 & 0xf) << 12) | ((b1 & 0x3f) << 6) | (b2 & 0x3f)
          else if remaining < 4 then Incomplete
          else
            val b3 = bytes(offset + 3) & 0xff

            if (b3 & 0xc0) != 0x80 then Malformed
            else ((b0 & 0x7) << 18) | ((b1 & 0x3f) << 12) | ((b2 & 0x3f) << 6) | (b3 & 0x3f)

  // The offset of the first non-ASCII byte in `[offset, end)`, or `end` if there is none,
  // scanning eight bytes per step.
  def asciiEnd(bytes: scala.Array[Byte], offset: Int, end: Int): Int =
    var i = offset
    var scanning = true

    while scanning && i + 8 <= end do
      if Words.nonAscii(Words.load(bytes, i)) == 0L then i += 8 else scanning = false

    while i < end && bytes(i) >= 0 do i += 1

    i

  // The text of the bytes `[offset, offset + length)`, or `Unset` if they are not valid UTF-8.
  def decode(bytes: scala.Array[Byte], offset: Int, length: Int): Optional[Text] =
    val end = offset + length
    val prefix = asciiEnd(bytes, offset, end)

    if prefix == end then jl.String(bytes, offset, length, Latin1).tt
    else
      // A sequence of n bytes yields at most n/2 chars, so the byte length bounds the char
      // length; the ASCII prefix is widened straight in.
      val chars = new scala.Array[Char](length)
      var i = offset
      var n = 0

      while i < prefix do
        chars(n) = bytes(i).toChar
        i += 1
        n += 1

      val count = transcode(bytes, prefix, end, chars, n)
      if count < 0 then Unset else jl.String(chars, 0, count).tt

  // Appends the text of the bytes `[offset, offset + length)` to `target`, or appends nothing
  // and returns `false` if they are not valid UTF-8.
  def append(bytes: scala.Array[Byte], offset: Int, length: Int, target: jl.StringBuilder)
  :   Boolean =

    val end = offset + length
    val prefix = asciiEnd(bytes, offset, end)

    if prefix == end then
      target.append(jl.String(bytes, offset, length, Latin1))
      true
    else
      val chars = new scala.Array[Char](length)
      var i = offset
      var n = 0

      while i < prefix do
        chars(n) = bytes(i).toChar
        i += 1
        n += 1

      val count = transcode(bytes, prefix, end, chars, n)

      if count < 0 then false else
        target.append(chars, 0, count)
        true

  // Decodes `[offset, end)` into `chars` from index `n`, returning the index after the last
  // char written, or -1 if the bytes are not valid UTF-8 (an incomplete tail is malformed
  // here: the range is the whole slice). Astral code points become surrogate pairs.
  private def transcode
    ( bytes: scala.Array[Byte], offset: Int, end: Int, chars: scala.Array[Char]^, n0: Int )
  :   Int =

    var i = offset
    var n = n0
    var valid = true

    while valid && i < end do
      val b = bytes(i)

      if b >= 0 then
        chars(n) = b.toChar
        i += 1
        n += 1
      else
        val point = Utf8.point(bytes, i, end)

        if point < 0 then valid = false
        else
          if point < 0x10000 then
            chars(n) = point.toChar
            n += 1
          else
            val offset = point - 0x10000
            chars(n) = (0xd800 | (offset >> 10)).toChar
            chars(n + 1) = (0xdc00 | (offset & 0x3ff)).toChar
            n += 2

          i += width(b & 0xff)

    if valid then n else -1
