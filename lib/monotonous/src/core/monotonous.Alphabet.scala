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

import anticipation.*
import contingency.*
import denominative.*
import gossamer.*
import hypotenuse.*
import rudiments.*
import vacuous.*
import zephyrine.*

// An `Alphabet` is the stage descriptor for streaming serialization in both
// directions: `stream.through(alphabets.hex.upperCase)` serializes a byte
// stream to text, or deserializes a text stream to bytes, chosen by the
// stream's medium. The demand conversion is the exact arithmetic of the
// base: a downstream credit of 1024 hex chars translates to an upstream
// credit of 512 bytes.
object Alphabet:
  // Bits per serialized character: alphabets have 2^bits characters, plus
  // one for padding.
  private def bits(alphabet: Alphabet[?]): Int =
    31 - Integer.numberOfLeadingZeros(alphabet.chars.s.length)

  given serialization: [encoding <: Serialization]
  =>  Ductile.Of[Alphabet[encoding], Data, Text, Credit, Credit] =

    // Sealed: a Ductile is a stateless stage descriptor; instantiation freshens its
    // type arguments under capture checking, which the seal discards.
    caps.unsafe.unsafeAssumePure:
     new Ductile:
      type Self = Alphabet[encoding]
      type Operand = Data
      type Result = Text
      type Transport = Credit
      type Upstream = Credit

      def duct(stage: Alphabet[encoding])(using Buffering)
      :   Duct[Data, Text] { type Transport = Credit; type Upstream = Credit } =

        new Duct[Data, Text]:
          type Transport = Credit
          type Upstream = Credit

          private val base: Int = bits(stage)

          // Serialized characters per group, for terminal padding.
          private val multiple: Int = 8/base.gcd(8)

          private var accumulator: Int = 0
          private var accumulated: Int = 0
          private var written: Long = 0
          private var flushing: Boolean = false

          def regulation: Credit is Regulation = summon[Credit is Regulation]

          def translate(demand: Credit): Credit =
            Credit((demand.count.min(Long.MaxValue/8)*base/8).max(1))

          def step
            ( source: input.Storage,
              sourceOffset: Int,
              sourceLength: Int,
              target: output.Storage,
              targetOffset: Int,
              targetSpace: Int )
          :   Duct.Progress =

            val bytes = source.asInstanceOf[Array[Byte]]
            val chars = target.asInstanceOf[Array[Char]]
            var consumed: Int = 0
            var produced: Int = 0
            var continue: Boolean = true

            while continue do
              if accumulated >= base then
                if produced < targetSpace then
                  chars(targetOffset + produced) =
                    stage((accumulator >>> (accumulated - base)) & ((1 << base) - 1))

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

          override def flush(target: output.Storage, targetOffset: Int, targetSpace: Int)
          :   Int =

            val chars = target.asInstanceOf[Array[Char]]
            var produced: Int = 0

            if !flushing then
              flushing = true

              if accumulated > 0 then
                chars(targetOffset) =
                  stage((accumulator << (base - accumulated)) & ((1 << base) - 1))

                produced = 1
                accumulated = 0
                written += 1

            if stage.padding then
              while written%multiple != 0 && produced < targetSpace do
                chars(targetOffset + produced) = stage(1 << base)
                produced += 1
                written += 1

            produced

  given deserialization: [encoding <: Serialization] => Tactic[SerializationError]
  =>  Ductile.Of[Alphabet[encoding], Text, Data, Credit, Credit] =

    // Sealed: see `serialization` above.
    caps.unsafe.unsafeAssumePure:
     new Ductile:
      type Self = Alphabet[encoding]
      type Operand = Text
      type Result = Data
      type Transport = Credit
      type Upstream = Credit

      def duct(stage: Alphabet[encoding])(using Buffering)
      :   Duct[Text, Data] { type Transport = Credit; type Upstream = Credit } =

        // Sealed: the duct closes over the ambient `Buffering`, which is sizing policy
        // only; the instance performs no effects beyond its own mutable buffers.
        caps.unsafe.unsafeAssumePure:
         new Duct[Text, Data]:
          type Transport = Credit
          type Upstream = Credit

          private val base: Int = bits(stage)
          private val pad: Char = if stage.padding then stage(1 << base) else ' '

          private var accumulator: Int = 0
          private var accumulated: Int = 0
          private var position: Int = 0

          def regulation: Credit is Regulation = summon[Credit is Regulation]

          def translate(demand: Credit): Credit =
            Credit((demand.count.min(Long.MaxValue/8)*8/base).max(1))

          def step
            ( source: input.Storage,
              sourceOffset: Int,
              sourceLength: Int,
              target: output.Storage,
              targetOffset: Int,
              targetSpace: Int )
          :   Duct.Progress =

            val chars = source.asInstanceOf[Array[Char]]
            val bytes = target.asInstanceOf[Array[Byte]]
            var consumed: Int = 0
            var produced: Int = 0
            var continue: Boolean = true

            while continue do
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
                  accumulator = (accumulator << base) | stage.invert(position, char)
                  accumulated += base

                position += 1
                consumed += 1
              else
                continue = false

            Duct.Progress(consumed, produced)

case class Alphabet[encoding <: Serialization]
  ( chars: Text, padding: Boolean, tolerance: Map[Char, Int] = Map() ):

  def apply(index: Int): Char = chars.at(index.z).vouch

  def invert(position: Int, char: Char): Int raises SerializationError =
    inverse.getOrElse(char, abort(SerializationError(position, char)))

  lazy val inverse: Map[Char, Int] = tolerance ++ chars.chars.zipWithIndex.to(Map)
