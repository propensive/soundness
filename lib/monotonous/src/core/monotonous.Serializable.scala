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
package monotonous

import java.nio.charset.StandardCharsets

import anticipation.*
import beneficence.*
import hypotenuse.*
import prepositional.*

object Serializable:
  def base[base <: Serialization](bits: Int)(using alphabet: Alphabet[base]): Serializable in base =
    new:
      // ASCII-byte lookup table for the `2^bits` data characters, plus the pad
      // byte, precomputed once. Encoding writes bytes (not chars) into an ASCII
      // `Array[Byte]` so the result `Text` is built directly from Latin-1 bytes,
      // avoiding both per-character boxing and the char-array compaction scan.
      // Sealed: `tabulate` closes over the alphabet, but the resulting table is an
      // immutable `IArray` of bytes that holds no reference to it.
      private val lookup: IArray[Byte] =
        IArray.tabulate(1 << bits)(alphabet(_).toByte)

      private val padding: Boolean = alphabet.padding
      private val padByte: Byte = if padding then alphabet(1 << bits).toByte else 0

      def encode(bytes: Data): Text =
        val src = bytes.asInstanceOf[Array[Byte]]

        val out = bits match
          case 4 => hex(src)
          case 5 => base32(src)
          case 6 => base64(src)
          case _ => generic(src)

        // Every alphabet character is ASCII, so decoding the output as Latin-1
        // yields identical text while letting the JVM adopt the byte array as the
        // compact-string backing directly, with no validating charset scan.
        Text(String(out, StandardCharsets.ISO_8859_1))

      // Hex: each byte is a self-contained group of two characters, so there is
      // no bit carry and never any padding. Both output bytes for a given input
      // byte are precomputed and packed into one `Short` (low byte first, to
      // match the store order), so the hot loop is a single table load plus two
      // byte stores per input byte — the JDK `HexDigits.digitPair` trick.
      private lazy val hexPairs: IArray[Short] =
        caps.unsafe.unsafeAssumePure:
          IArray.tabulate(256): b =>
            val hi = lookup(b >>> 4) & 0xff
            val lo = lookup(b & 0xf) & 0xff
            (hi | (lo << 8)).toShort

      private def hex(src: Array[Byte]): Array[Byte] =
        val pairs = hexPairs
        val n = src.length
        val out = new Array[Byte](n*2)
        var i = 0
        var j = 0

        while i < n do
          val pair = pairs(src(i) & 0xff).toInt
          out(j) = pair.toByte
          out(j + 1) = (pair >>> 8).toByte
          i += 1
          j += 2

        out

      // Base64: three input bytes become four characters; a trailing group of
      // one or two bytes is completed with padding when the alphabet demands it.
      private def base64(src: Array[Byte]): Array[Byte] =
        val n = src.length
        val full = n/3
        val rem = n - full*3

        val length =
          if padding then (full + (if rem > 0 then 1 else 0))*4
          else full*4 + (if rem == 1 then 2 else if rem == 2 then 3 else 0)

        val out = new Array[Byte](length)
        var i = 0
        var j = 0
        var g = 0

        while g < full do
          val b0 = src(i) & 0xff
          val b1 = src(i + 1) & 0xff
          val b2 = src(i + 2) & 0xff
          out(j) = lookup(b0 >>> 2)
          out(j + 1) = lookup(((b0 & 0x3) << 4) | (b1 >>> 4))
          out(j + 2) = lookup(((b1 & 0xf) << 2) | (b2 >>> 6))
          out(j + 3) = lookup(b2 & 0x3f)
          i += 3
          j += 4
          g += 1

        if rem == 1 then
          val b0 = src(i) & 0xff
          out(j) = lookup(b0 >>> 2)
          out(j + 1) = lookup((b0 & 0x3) << 4)
          j += 2
        else if rem == 2 then
          val b0 = src(i) & 0xff
          val b1 = src(i + 1) & 0xff
          out(j) = lookup(b0 >>> 2)
          out(j + 1) = lookup(((b0 & 0x3) << 4) | (b1 >>> 4))
          out(j + 2) = lookup((b1 & 0xf) << 2)
          j += 3

        while j < length do
          out(j) = padByte
          j += 1

        out

      // Base32: five input bytes become eight characters; trailing groups of
      // 1/2/3/4 bytes emit 2/4/5/7 characters, padded to a multiple of eight.
      private def base32(src: Array[Byte]): Array[Byte] =
        val n = src.length
        val full = n/5
        val rem = n - full*5

        val tail = rem match
          case 0 => 0
          case 1 => 2
          case 2 => 4
          case 3 => 5
          case _ => 7

        val length = if padding then (full + (if rem > 0 then 1 else 0))*8 else full*8 + tail
        val out = new Array[Byte](length)
        var i = 0
        var j = 0
        var g = 0

        while g < full do
          val b0 = src(i) & 0xff
          val b1 = src(i + 1) & 0xff
          val b2 = src(i + 2) & 0xff
          val b3 = src(i + 3) & 0xff
          val b4 = src(i + 4) & 0xff
          out(j) = lookup(b0 >>> 3)
          out(j + 1) = lookup(((b0 & 0x7) << 2) | (b1 >>> 6))
          out(j + 2) = lookup((b1 >>> 1) & 0x1f)
          out(j + 3) = lookup(((b1 & 0x1) << 4) | (b2 >>> 4))
          out(j + 4) = lookup(((b2 & 0xf) << 1) | (b3 >>> 7))
          out(j + 5) = lookup((b3 >>> 2) & 0x1f)
          out(j + 6) = lookup(((b3 & 0x3) << 3) | (b4 >>> 5))
          out(j + 7) = lookup(b4 & 0x1f)
          i += 5
          j += 8
          g += 1

        // The <=4-byte remainder runs once, so a small bit-accumulator emits its
        // `tail` characters rather than another unrolled case analysis.
        if rem > 0 then
          var acc = 0
          var k = 0

          while k < rem do
            acc = (acc << 8) | (src(i + k) & 0xff)
            k += 1

          val loaded = rem*8
          var t = 0

          while t < tail do
            val shift = loaded - 5*(t + 1)
            val value = if shift >= 0 then acc >>> shift else acc << -shift
            out(j + t) = lookup(value & 0x1f)
            t += 1

          j += tail

        while j < length do
          out(j) = padByte
          j += 1

        out

      // Binary/quaternary/octal: a general bit-accumulator, for the bases whose
      // group size makes an unrolled kernel unprofitable.
      private def generic(src: Array[Byte]): Array[Byte] =
        val mask = (1 << bits) - 1
        val divisor = bits/bits.gcd(8)
        val multiple = 8/bits.gcd(8)

        val length =
          if padding then multiple*((src.length + divisor - 1)/divisor)
          else (src.length*8 + bits - 1)/bits

        val out = new Array[Byte](length)
        var current = 0
        var loaded = 0
        var index = 0
        var next = 0

        while next < src.length do
          current = (current << 8) | (src(next) & 0xff)
          next += 1
          loaded += 8

          while loaded >= bits do
            out(index) = lookup((current >>> (loaded - bits)) & mask)
            index += 1
            loaded -= bits

        if loaded > 0 && index < length then
          out(index) = lookup((current << (bits - loaded)) & mask)
          index += 1

        while index < length do
          out(index) = padByte
          index += 1

        out

  given binary: Alphabet[Binary] => Serializable in Binary = base(1)
  given quaternary: Alphabet[Quaternary] => Serializable in Quaternary = base(2)
  given octal: Alphabet[Octal] => Serializable in Octal = base(3)
  given hex: Alphabet[Hex] => Serializable in Hex = base(4)
  given base32: Alphabet[Base32] => Serializable in Base32 = base(5)
  given base64: Alphabet[Base64] => Serializable in Base64 = base(6)

// `caps.Pure` directly (not `Typeclass.Pure`) because `Serializable` has no `Self`: it is
// selected by its `Form` member alone. Instances hold only immutable tables derived from a
// pure `Alphabet`, so purity is compiler-verified.
trait Serializable extends Findable, caps.Pure:
  type Form <: Serialization

  def encode(bytes: Data): Text
