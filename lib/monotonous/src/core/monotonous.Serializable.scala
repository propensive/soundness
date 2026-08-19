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
package monotonous


import scala.caps

import java.nio.charset.StandardCharsets

import anticipation.*
import denominative.*
import rudiments.*
import beneficence.*
import hypotenuse.*
import prepositional.*
import vacuous.*

object Serializable:
  def base[base <: Serialization](bits: Int)(using alphabet: Alphabet[base]): Serializable in base =
    new:
      // ASCII-byte lookup table for the `2^bits` data characters, plus the pad
      // byte, precomputed once. Encoding writes bytes (not chars) into an ASCII
      // `Array[Byte]` so the result `Text` is built directly from Latin-1 bytes,
      // avoiding both per-character boxing and the char-array compaction scan.
      // Sealed: `tabulate` closes over the alphabet, but the resulting table is an
      // immutable frozen array of bytes that holds no reference to it.
      private val lookup: Array[Byte]^{} =
        Array.tabulate(1 << bits)(alphabet(_).toByte)

      private val padding: Boolean = alphabet.padding
      private val padByte: Byte = if padding then alphabet(1 << bits).toByte else 0

      def encode(bytes: Data): Text =

        val out: Array[Byte]^{} = bits match
          case 4 => hex(bytes)
          case 5 => base32(bytes)
          case 6 => base64(bytes)
          case _ => generic(bytes)

        // Every alphabet character is ASCII, so decoding the output as Latin-1
        // yields identical text while letting the JVM adopt the byte array as the
        // compact-string backing directly, with no validating charset scan.
        Text(String(Array.unsafeJvm(out), StandardCharsets.ISO_8859_1))

      // Hex: each byte is a self-contained group of two characters, so there is
      // no bit carry and never any padding. Both output bytes for a given input
      // byte are precomputed and packed into one `Short` (low byte first, to
      // match the store order), so the hot loop is a single table load plus two
      // byte stores per input byte — the JDK `HexDigits.digitPair` trick.
      private lazy val hexPairs: Array[Short]^{} =
        Array.tabulate(256): b =>
          val hi = lookup.readUnchecked(b >>> 4) & 0xff
          val lo = lookup.readUnchecked(b & 0xf) & 0xff
          (hi | (lo << 8)).toShort

      private def hex(src: Data): Array[Byte]^{} =
        // The one raw gate of this kernel: the pair table is indexed by *value* (`& 0xff`
        // confines it to the table's 256 entries), which branding cannot express.
        val pairTable = Array.unsafeJvm(hexPairs)

        Array.scribe[Byte](src.length*2): scribe =>
          _ =>
            src.iterate: index =>
              val pair = pairTable(src.at(index) & 0xff).toInt
              scribe.append(pair.toByte)
              scribe.append((pair >>> 8).toByte)

      // Base64: three input bytes become four characters; a trailing group of
      // one or two bytes is completed with padding when the alphabet demands it.
      private def base64(src: Data): Array[Byte]^{} =
        val n = src.length
        val full = n/3
        val rem = n - full*3

        val length =
          if padding then (full + (if rem > 0 then 1 else 0))*4
          else full*4 + (if rem == 1 then 2 else if rem == 2 then 3 else 0)

        // Two raw gates in this kernel. The alphabet table is indexed by *value* (each
        // index is a masked 6-bit group), which branding cannot express; and the loop
        // reads and writes both arrays at computed indices, because `length` above is
        // the output size *exactly*, so every store is in range by construction. The
        // safe `Scribe.append` costs a buffer re-derivation, a bounds test and a cursor
        // field write-back for each of the four output bytes per input triple, and
        // `triples` reaches its elements through `Applicable.access`; together they were
        // the whole of this kernel's gap to `java.util.Base64`'s scalar path.
        val table = Array.unsafeJvm(lookup)
        val in = Array.unsafeJvm(src)
        val out = new scala.Array[Byte](length)
        // `n - 2` rather than the equivalent `full*3`: both bound the loop to whole
        // triples, but this form lets the JIT prove `i + 2` is in range from the loop
        // condition alone, folding away two of the three load bounds-checks. The three
        // bytes are then assembled into one integer and sliced into four 6-bit groups,
        // as `java.util.Base64` does, which keeps the dependency chain shorter than
        // masking and recombining each pair.
        val stop = n - 2
        var i: Int = 0
        var o: Int = 0

        while i < stop do
          val bits = ((in(i) & 0xff) << 16) | ((in(i + 1) & 0xff) << 8) | (in(i + 2) & 0xff)
          out(o) = table((bits >>> 18) & 0x3f)
          out(o + 1) = table((bits >>> 12) & 0x3f)
          out(o + 2) = table((bits >>> 6) & 0x3f)
          out(o + 3) = table(bits & 0x3f)
          i += 3
          o += 4

        if rem == 1 then
          val b0 = in(i) & 0xff
          out(o) = table(b0 >>> 2)
          out(o + 1) = table((b0 & 0x3) << 4)
          o += 2
        else if rem == 2 then
          val b0 = in(i) & 0xff
          val b1 = in(i + 1) & 0xff
          out(o) = table(b0 >>> 2)
          out(o + 1) = table(((b0 & 0x3) << 4) | (b1 >>> 4))
          out(o + 2) = table((b1 & 0xf) << 2)
          o += 3

        while o < length do
          out(o) = padByte
          o += 1

        out.asInstanceOf[Array[Byte]^{}]

      // Base32: five input bytes become eight characters; trailing groups of
      // 1/2/3/4 bytes emit 2/4/5/7 characters, padded to a multiple of eight.
      private def base32(src: Data): Array[Byte]^{} =
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

        // The one raw gate of this kernel: the alphabet table is indexed by *value* (each
        // index is a masked 5-bit group), which branding cannot express.
        val table = Array.unsafeJvm(lookup)

        Array.scribe[Byte](length): scribe =>
          _ =>
            val remainder = src.quints: (byte0, byte1, byte2, byte3, byte4) =>
              val b0 = byte0 & 0xff
              val b1 = byte1 & 0xff
              val b2 = byte2 & 0xff
              val b3 = byte3 & 0xff
              val b4 = byte4 & 0xff
              scribe.append(table(b0 >>> 3))
              scribe.append(table(((b0 & 0x7) << 2) | (b1 >>> 6)))
              scribe.append(table((b1 >>> 1) & 0x1f))
              scribe.append(table(((b1 & 0x1) << 4) | (b2 >>> 4)))
              scribe.append(table(((b2 & 0xf) << 1) | (b3 >>> 7)))
              scribe.append(table((b3 >>> 2) & 0x1f))
              scribe.append(table(((b3 & 0x3) << 3) | (b4 >>> 5)))
              scribe.append(table(b4 & 0x1f))

            // The <=4-byte remainder runs once, so a small bit-accumulator emits its
            // `tail` characters rather than another unrolled case analysis.
            if rem > 0 then
              var acc = 0
              src.iterate(remainder) { index => acc = (acc << 8) | (src.at(index) & 0xff) }

              val loaded = rem*8
              var t = 0

              while t < tail do
                val shift = loaded - 5*(t + 1)
                val value = if shift >= 0 then acc >>> shift else acc << -shift
                scribe.append(table(value & 0x1f))
                t += 1

            while scribe.mark < length do scribe.append(padByte)

      // Binary/quaternary/octal: a general bit-accumulator, for the bases whose
      // group size makes an unrolled kernel unprofitable.
      private def generic(src: Data): Array[Byte]^{} =
        val mask = (1 << bits) - 1
        val divisor = bits/bits.gcd(8)
        val multiple = 8/bits.gcd(8)

        val length =
          if padding then multiple*((src.length + divisor - 1)/divisor)
          else (src.length*8 + bits - 1)/bits

        // The one raw gate of this kernel: the alphabet table is indexed by *value* (each
        // index is a masked group of `bits`), which branding cannot express.
        val table = Array.unsafeJvm(lookup)

        Array.scribe[Byte](length): scribe =>
          _ =>
            var current = 0
            var loaded = 0

            src.iterate: index =>
              current = (current << 8) | (src.at(index) & 0xff)
              loaded += 8

              while loaded >= bits do
                scribe.append(table((current >>> (loaded - bits)) & mask))
                loaded -= bits

            if loaded > 0 && scribe.mark < length
            then scribe.append(table((current << (bits - loaded)) & mask))

            while scribe.mark < length do scribe.append(padByte)

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
