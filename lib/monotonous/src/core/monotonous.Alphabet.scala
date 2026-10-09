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

import anticipation.*
import denominative.*
import prepositional.*
import rudiments.*
import vacuous.*
import contingency.*
import gossamer.*
import hypotenuse.*
import symbolism.`+`
import zephyrine.*

// An `Alphabet` is the stage descriptor for streaming serialization in both
// directions: `stream.via(alphabets.hex.upperCase)` serializes a byte
// stream to text, or deserializes a text stream to bytes, chosen by the
// stream's medium. The demand conversion is the exact arithmetic of the
// base: a downstream credit of 1024 hex chars translates to an upstream
// credit of 512 bytes.
object Alphabet:
  // Bits per serialized character: alphabets have 2^bits characters, plus
  // one for padding.
  private def bits(alphabet: Alphabet[?]): Int =
    31 - Integer.numberOfLeadingZeros(alphabet.chars.length)

  given serialization: [encoding <: Serialization]
  =>  Ductile.Instance[Alphabet[encoding], Data, Text, Credit, Credit] =

    new Ductile:
      type Self = Alphabet[encoding]
      type Operand = Data
      type Result = Text
      type Transport = Credit
      type Upstream = Credit

      def duct(consume stage: Alphabet[encoding])(using Buffering)
      :   (Duct[Data, Text] { type Transport = Credit; type Upstream = Credit })^ =

        // An alphabet is pure, so the duct may read the consumed descriptor freely.
        val alphabet: Alphabet[encoding] = stage

        new Duct[Data, Text]:
          type Transport = Credit
          type Upstream = Credit

          private val base: Int = bits(alphabet)
          private val mask: Int = (1 << base) - 1

          // Serialized characters per group, for terminal padding.
          private val multiple: Int = 8/base.gcd(8)

          // Character lookup table for the `2^base` data symbols, so the hot
          // loop indexes an array rather than re-reading the alphabet string.
          private val table: Array[Char]^{} = Array.tabulate(1 << base)(alphabet(_))

          private var accumulator: Int = 0
          private var accumulated: Int = 0
          private var written: Long = 0
          private var flushing: Boolean = false

          def regulation: Credit is Regulation = summon[Credit is Regulation]

          def translate(demand: Credit): Credit =
            Credit((demand.count.min(Long.MaxValue/8)*base/8).max(1))

          update def step(source: Region[Data])(range: Interval in source.type)
            ( target: Slate[Text] )(space: Interval in target.type)
          :   Duct.Progress =

            val sourceInterval: Interval = range
            val sourceOffset = sourceInterval.start.n0
            val sourceLength = sourceInterval.size
            val targetInterval: Interval = space
            val targetOffset = targetInterval.start.n0
            val targetSpace = targetInterval.size
            val bytes = unsafely(source.unsafeRaw.asInstanceOf[scala.Array[Byte]])

            // The stage's own buffer, asserted exclusive at the cast rim.
            val chars: scala.Array[Char]^ =
              unsafely(target.unsafeRaw.asInstanceOf[scala.Array[Char]]).asInstanceOf[scala.Array[Char]^]
            var consumed: Int = 0
            var produced: Int = 0
            var continue: Boolean = true

            while continue do
              // Fast path: while byte-aligned (no carry), emit whole base64
              // groups — three input bytes to four characters — directly from
              // the table, skipping the per-character accumulator bookkeeping.
              if base == 6 then
                while accumulated == 0 && consumed + 3 <= sourceLength && produced + 4 <= targetSpace
                do
                  val b0 = bytes(sourceOffset + consumed) & 0xff
                  val b1 = bytes(sourceOffset + consumed + 1) & 0xff
                  val b2 = bytes(sourceOffset + consumed + 2) & 0xff
                  chars(targetOffset + produced) = table.readable(b0 >>> 2)
                  chars(targetOffset + produced + 1) = table.readable(((b0 & 0x3) << 4) | (b1 >>> 4))
                  chars(targetOffset + produced + 2) = table.readable(((b1 & 0xf) << 2) | (b2 >>> 6))
                  chars(targetOffset + produced + 3) = table.readable(b2 & 0x3f)
                  consumed += 3
                  produced += 4
                  written += 4

              if accumulated >= base then
                if produced < targetSpace then
                  chars(targetOffset + produced) = table.readable((accumulator >>> (accumulated - base)) & mask)
                  produced += 1
                  accumulated -= base
                  written += 1
                else
                  continue = false
              else if consumed < sourceLength then
                accumulator = (accumulator << 8) | (bytes(sourceOffset + consumed) & 0xff)
                accumulated += 8
                consumed += 1
              else
                continue = false

            Duct.Progress(consumed, produced)

          override update def flush(target: Slate[Text])(space: Interval in target.type): Int =
            val targetInterval: Interval = space
            val targetOffset = targetInterval.start.n0
            val targetSpace = targetInterval.size

            val chars: scala.Array[Char]^ =
              unsafely(target.unsafeRaw.asInstanceOf[scala.Array[Char]]).asInstanceOf[scala.Array[Char]^]
            var produced: Int = 0

            if !flushing then
              flushing = true

              if accumulated > 0 then
                chars(targetOffset) =
                  alphabet((accumulator << (base - accumulated)) & ((1 << base) - 1))

                produced = 1
                accumulated = 0
                written += 1

            if stage.padding then
              while written%multiple != 0 && produced < targetSpace do
                chars(targetOffset + produced) = alphabet(1 << base)
                produced += 1
                written += 1

            produced

  given deserialization: [encoding <: Serialization] => (tactic: Tactic[Serialization.Error])
  =>  ((Ductile.Instance[Alphabet[encoding], Text, Data, Credit, Credit])^{tactic}) =

    // The ducts raise through the given's tactic, so the instance honestly captures it.
    new Ductile:
      type Self = Alphabet[encoding]
      type Operand = Text
      type Result = Data
      type Transport = Credit
      type Upstream = Credit

      def duct(consume stage: Alphabet[encoding])(using Buffering)
      :   (Duct[Text, Data] { type Transport = Credit; type Upstream = Credit })^ =

        // An alphabet is pure, so the duct may read the consumed descriptor freely.
        val alphabet: Alphabet[encoding] = stage

        new Duct[Text, Data]:
          type Transport = Credit
          type Upstream = Credit

          private val base: Int = bits(alphabet)
          private val pad: Char = if stage.padding then alphabet(1 << base) else '\u0000'

          // Dense decode table and the largest valid data value, for the fast
          // path: a character outside `0..dataMax` (invalid, or a pad) bails to
          // the general path, which reports the error or realigns padding.
          private val inversions: Array[Int]^{} = alphabet.inversions
          private val invLength: Int = inversions.length
          private val dataMax: Int = (1 << base) - 1

          private var accumulator: Int = 0
          private var accumulated: Int = 0
          private var position: Int = 0

          def regulation: Credit is Regulation = summon[Credit is Regulation]

          def translate(demand: Credit): Credit =
            Credit((demand.count.min(Long.MaxValue/8)*8/base).max(1))

          update def step(source: Region[Text])(range: Interval in source.type)
            ( target: Slate[Data] )(space: Interval in target.type)
          :   Duct.Progress =

            val sourceInterval: Interval = range
            val sourceOffset = sourceInterval.start.n0
            val sourceLength = sourceInterval.size
            val targetInterval: Interval = space
            val targetOffset = targetInterval.start.n0
            val targetSpace = targetInterval.size
            val chars = unsafely(source.unsafeRaw.asInstanceOf[scala.Array[Char]])

            // The stage's own buffer, asserted exclusive at the cast rim.
            val bytes: scala.Array[Byte]^ =
              unsafely(target.unsafeRaw.asInstanceOf[scala.Array[Byte]]).asInstanceOf[scala.Array[Byte]^]
            var consumed: Int = 0
            var produced: Int = 0
            var continue: Boolean = true

            while continue do
              // Fast path: while byte-aligned, decode whole base64 groups — four
              // characters to three bytes — straight from the table, bailing to
              // the general path on any pad or invalid character.
              if base == 6 then
                var fast: Boolean = true

                while fast && accumulated == 0 && consumed + 4 <= sourceLength
                    && produced + 3 <= targetSpace do

                  val c0 = chars(sourceOffset + consumed).toInt
                  val c1 = chars(sourceOffset + consumed + 1).toInt
                  val c2 = chars(sourceOffset + consumed + 2).toInt
                  val c3 = chars(sourceOffset + consumed + 3).toInt
                  val v0 = if c0 < invLength then inversions.readUnchecked(c0) else -1
                  val v1 = if c1 < invLength then inversions.readUnchecked(c1) else -1
                  val v2 = if c2 < invLength then inversions.readUnchecked(c2) else -1
                  val v3 = if c3 < invLength then inversions.readUnchecked(c3) else -1

                  if v0 < 0 || v0 > dataMax || v1 < 0 || v1 > dataMax || v2 < 0 || v2 > dataMax
                      || v3 < 0 || v3 > dataMax
                  then fast = false
                  else
                    val group = (v0 << 18) | (v1 << 12) | (v2 << 6) | v3
                    bytes(targetOffset + produced) = (group >>> 16).toByte
                    bytes(targetOffset + produced + 1) = (group >>> 8).toByte
                    bytes(targetOffset + produced + 2) = group.toByte
                    consumed += 4
                    produced += 3
                    position += 4

              if accumulated >= 8 then
                if produced < targetSpace then
                  bytes(targetOffset + produced) =
                    ((accumulator >>> (accumulated - 8)) & 0xff).toByte

                  produced += 1
                  accumulated -= 8
                else
                  continue = false
              else if consumed < sourceLength then
                val char = chars(sourceOffset + consumed)

                // Trailing padding characters carry no data; the bits already
                // accumulated before them are alignment filler.
                if stage.padding && char == pad then accumulated = 0
                else
                  accumulator = (accumulator << base)
                    | stage.invert(position, char)
                        (using tactic)
                  accumulated += base

                position += 1
                consumed += 1
              else
                continue = false

            Duct.Progress(consumed, produced)

case class Alphabet[encoding <: Serialization]
  ( chars: Text, padding: Boolean, tolerance: Map[Char, Int] = Map() )
extends caps.Pure:

  def apply(index: Int): Char = chars.s.charAt(index)

  def invert(position: Int, char: Char): Int raises Serialization.Error =
    if char < inversions.length && inversions.readUnchecked(char) >= 0 then inversions.readUnchecked(char)
    else abort(Serialization.Error(position, char))

  lazy val inverse: Map[Char, Int] =
    tolerance + Map.from(chars.chars.readable.zipWithIndex)

  // Dense decode table, indexed directly by character code (-1 = invalid), so the
  // per-character hot path avoids boxed `Map` lookups.
  lazy val inversions: Array[Int]^{} =
    val max = inverse.keys.maximum.or(' ')

    Array.tabulate(max + 1): index =>
      inverse(index.toChar).or(-1)
