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
package zephyrine

import java.nio as jn, jn.charset as jnc

import anticipation.*
import hieroglyph.*
import prepositional.*
import rudiments.*
import vacuous.*

// Makes a stage-descriptor value — a character decoder, a compression
// algorithm, a serialization alphabet — instantiable as a `Duct`, so it can
// be applied directly with `stream.via(stage)` or
// `intake.accepting(stage)`. Instances are typed
// `X is Ductile by In to Out over Transport`, with `Upstream` (the demand
// type the stage presents to its upstream) as a further member, `Credit` in
// almost all cases. A `Duct` is trivially its own descriptor, so raw ducts
// compose with the same `through`/`accepting` calls.
object Ductile:
  // The fully-determined type of a `Ductile` instance for `stage`, naming all
  // five type members positionally.
  type Of[stage, in, out, transport, upstream] =
    (stage is Ductile by in to out over transport) { type Upstream = upstream }

  given duct: [in, out, transport, upstream,
        stage <: (Duct[in, out] { type Transport = transport; type Upstream = upstream })^]
  =>  Of[stage, in, out, transport, upstream] =

    // The identity instance: the cast bridges the capture-decorated inferred member types
    // against the declared alias (bare `stage` under a stateful bound reads as `.rd`).
    (new Ductile:
      type Self = stage
      type Operand = in
      type Result = out
      type Transport = transport
      type Upstream = upstream

      def duct(consume stage0: stage^)(using Buffering)
      :   (Duct[in, out] { type Transport = transport; type Upstream = upstream })^ =

        // consumed pass-through: ownership moves from caller to result (the admission
        // gap for direct returns is bridged by the erasure cast)
        stage0.asInstanceOf[(Duct[in, out] { type Transport = transport; type Upstream = upstream })^]
    ).asInstanceOf[Ductile {
      type Self = stage^{caps.any.rd}
      type Operand = in
      type Result = out
      type Transport = transport
      type Upstream = upstream }]

  // Character decoding as a pipeline stage. Bytes are staged internally (so
  // multi-byte characters split across refills are carried, and `step`
  // always consumes what it is offered), and malformed input is substituted
  // through the decoder's `TextSanitizer`, exactly as `CharDecoder.decoded`.
  given charDecoder: Of[CharDecoder, Data, Text, Credit, Credit] =
    new Ductile:
      type Self = CharDecoder
      type Operand = Data
      type Result = Text
      type Transport = Credit
      type Upstream = Credit

      def duct(consume stage: CharDecoder^)(using buffering: Buffering)
      :   Duct[Data, Text] { type Transport = Credit; type Upstream = Credit } =

        new Duct[Data, Text]:
          type Transport = Credit
          type Upstream = Credit

          private val decoder: jnc.CharsetDecoder = stage.encoding.charset.newDecoder().nn

          // UTF-8 gets a hand-rolled kernel, as the base64 ducts do: the
          // charset decoder's scalar loop is the bottleneck against
          // implementations built on the JDK's intrinsified `String`
          // constructor, and UTF-8's structure (ASCII bytes are exactly the
          // non-negative ones) admits a much faster split.
          private val utf8: Boolean = stage.encoding.charset == jnc.StandardCharsets.UTF_8

          private val staging: jn.ByteBuffer =
            jn.ByteBuffer.allocate(buffering.capacity(Substrate.Bytes)).nn

          private var total: Int = 0
          private var ended: Boolean = false

          def regulation: Credit is Regulation = summon[Credit is Regulation]

          // At most one output char per input byte, so char-credit is a
          // sound byte-credit as it stands.
          def translate(demand: Credit): Credit = demand

          // An astral character emits a surrogate pair — two chars at once —
          // so guaranteed progress needs a two-slot output floor.
          override def quantum: Int = 2

          // Decode `in` into `out`, substituting malformed input through the
          // sanitizer; `base` is the absolute byte offset of `in.position == 0`,
          // for sanitizer error reporting. Returns the terminal `CoderResult`.
          private update def decode(in: jn.ByteBuffer, out: jn.CharBuffer, base: Int, last: Boolean)
          :   jnc.CoderResult =

            val result = decoder.decode(in, out, last).nn

            if !result.isMalformed then result else
              stage.sanitizer.sanitize(base + in.position, stage.encoding).let(out.put(_))
              in.position(in.position + result.length)
              decode(in, out, base, last)

          // Decode window bytes `[start, sourceOffset + sourceLength)` through
          // the charset decoder into `[first, targetOffset + targetSpace)`:
          // the whole window for a non-UTF-8 charset, or the remainder after
          // the point where the UTF-8 kernel encountered malformed input.
          private update def stepDecoder
            ( bytes: Array[Byte],
              sourceOffset: Int,
              sourceLength: Int,
              start: Int,
              chars: Array[Char]^,
              targetOffset: Int,
              targetSpace: Int,
              first: Int )
          :   Duct.Progress =

            val in = jn.ByteBuffer.wrap(bytes, start, sourceOffset + sourceLength - start).nn
            val out = jn.CharBuffer.wrap(chars, first, targetOffset + targetSpace - first).nn
            val result = decode(in, out, total - sourceOffset, false)
            val consumed = in.position - sourceOffset
            total += consumed

            if result.isUnderflow && consumed < sourceLength then
              // An incomplete trailing character: carry its bytes to the next
              // refill, where they lead the staging buffer.
              staging.put(bytes, sourceOffset + consumed, sourceLength - consumed)
              Duct.Progress(sourceLength, out.position - targetOffset)
            else
              // Output filled with complete bytes still to come, or input
              // drained cleanly: leave any remainder for the next window.
              Duct.Progress(consumed, out.position - targetOffset)

          // The UTF-8 kernel: ASCII runs widen through an unrolled block copy
          // (an `|`-reduction spots any high bit), multi-byte sequences decode
          // inline with strict validity (overlongs, the surrogate range and
          // anything above U+10FFFF all reject), and only exceptional input
          // leaves the loop: a valid-but-incomplete tail is carried exactly as
          // the decoder path carries it, and malformed input falls back to the
          // charset decoder for the window remainder, which owns sanitizer
          // semantics unchanged.
          private update def stepUtf8
            ( bytes: Array[Byte],
              sourceOffset: Int,
              sourceLength: Int,
              chars: Array[Char]^,
              targetOffset: Int,
              targetSpace: Int )
          :   Duct.Progress =

            val srcEnd = sourceOffset + sourceLength
            val dstEnd = targetOffset + targetSpace
            var src = sourceOffset
            var dst = targetOffset

            inline def continuation(index: Int): Boolean = (bytes(index) & 0xc0) == 0x80

            // 0 = advancing; 1 = incomplete tail (carry); 2 = malformed
            // (decoder fallback); 3 = surrogate pair with one slot left (stop
            // before the sequence).
            var mode: Int = 0

            while mode == 0 && src < srcEnd && dst < dstEnd do
              while src + 8 <= srcEnd && dst + 8 <= dstEnd &&
                (bytes(src) | bytes(src + 1) | bytes(src + 2) | bytes(src + 3) |
                  bytes(src + 4) | bytes(src + 5) | bytes(src + 6) | bytes(src + 7)) >= 0
              do
                chars(dst) = bytes(src).toChar
                chars(dst + 1) = bytes(src + 1).toChar
                chars(dst + 2) = bytes(src + 2).toChar
                chars(dst + 3) = bytes(src + 3).toChar
                chars(dst + 4) = bytes(src + 4).toChar
                chars(dst + 5) = bytes(src + 5).toChar
                chars(dst + 6) = bytes(src + 6).toChar
                chars(dst + 7) = bytes(src + 7).toChar
                src += 8
                dst += 8

              while src < srcEnd && dst < dstEnd && bytes(src) >= 0 do
                chars(dst) = bytes(src).toChar
                src += 1
                dst += 1

              if src < srcEnd && dst < dstEnd then
                val b0 = bytes(src) & 0xff
                val remaining = srcEnd - src

                if b0 >= 0xc2 && b0 <= 0xdf then
                  if remaining < 2 then mode = 1
                  else if !continuation(src + 1) then mode = 2
                  else
                    chars(dst) = (((b0 & 0x1f) << 6) | (bytes(src + 1) & 0x3f)).toChar
                    src += 2
                    dst += 1
                else if (b0 & 0xf0) == 0xe0 then
                  // The second byte's range depends on the lead: E0 rejects
                  // overlongs, ED rejects the surrogate range.
                  inline def second(index: Int): Boolean =
                    val b1 = bytes(index) & 0xff

                    if b0 == 0xe0 then b1 >= 0xa0 && b1 <= 0xbf
                    else if b0 == 0xed then b1 >= 0x80 && b1 <= 0x9f
                    else continuation(index)

                  if remaining >= 2 && !second(src + 1) then mode = 2
                  else if remaining < 3 then mode = 1
                  else if !continuation(src + 2) then mode = 2
                  else
                    chars(dst) =
                      (((b0 & 0xf) << 12) | ((bytes(src + 1) & 0x3f) << 6) |
                        (bytes(src + 2) & 0x3f)).toChar

                    src += 3
                    dst += 1
                else if b0 >= 0xf0 && b0 <= 0xf4 then
                  // The second byte's range depends on the lead: F0 rejects
                  // overlongs, F4 rejects anything above U+10FFFF.
                  inline def second(index: Int): Boolean =
                    val b1 = bytes(index) & 0xff

                    if b0 == 0xf0 then b1 >= 0x90 && b1 <= 0xbf
                    else if b0 == 0xf4 then b1 >= 0x80 && b1 <= 0x8f
                    else continuation(index)

                  if remaining >= 2 && !second(src + 1) then mode = 2
                  else if remaining >= 3 && !continuation(src + 2) then mode = 2
                  else if remaining < 4 then mode = 1
                  else if !continuation(src + 3) then mode = 2
                  else if dst + 2 > dstEnd then mode = 3
                  else
                    val point =
                      (((b0 & 0x7) << 18) | ((bytes(src + 1) & 0x3f) << 12) |
                        ((bytes(src + 2) & 0x3f) << 6) | (bytes(src + 3) & 0x3f)) - 0x10000

                    chars(dst) = (0xd800 | (point >> 10)).toChar
                    chars(dst + 1) = (0xdc00 | (point & 0x3ff)).toChar
                    src += 4
                    dst += 2
                else
                  // A stray continuation byte, an overlong lead (C0/C1) or an
                  // out-of-range lead (F5..FF).
                  mode = 2

            if mode == 2 then
              stepDecoder
                ( bytes, sourceOffset, sourceLength, src, chars, targetOffset, targetSpace,
                  dst )
            else
              total += src - sourceOffset

              if mode == 1 then
                staging.put(bytes, src, srcEnd - src)
                Duct.Progress(sourceLength, dst - targetOffset)
              else
                Duct.Progress(src - sourceOffset, dst - targetOffset)

          update def step
            ( source: input.Storage,
              sourceOffset: Int,
              sourceLength: Int,
              target: output.Storage,
              targetOffset: Int,
              targetSpace: Int )
          :   Duct.Progress =

            val bytes = source.asInstanceOf[Array[Byte]]
            // The exclusive cast is sound for the same reason as `Conduit.put`'s:
            // the target is the stage's single-owner output buffer.
            val chars = target.asInstanceOf[Array[Char]^]

            if staging.position == 0 then
              // Fast path: with no carried bytes, decode straight from the source
              // window — no intermediate copy of the bulk of the stream.
              if utf8 then
                stepUtf8(bytes, sourceOffset, sourceLength, chars, targetOffset, targetSpace)
              else
                stepDecoder
                  ( bytes, sourceOffset, sourceLength, sourceOffset,
                    chars, targetOffset, targetSpace, targetOffset )
            else
              // Carry path: prior incomplete bytes are staged, so append the new
              // window to make the split character contiguous, then decode.
              val out = jn.CharBuffer.wrap(chars, targetOffset, targetSpace).nn
              val copy = sourceLength.min(staging.remaining)
              staging.put(bytes, sourceOffset, copy)
              staging.flip()
              decode(staging, out, total, false)
              total += staging.position
              staging.compact()

              Duct.Progress(copy, out.position - targetOffset)

          override update def flush(target: output.Storage, targetOffset: Int, targetSpace: Int)
          :   Int =

            if ended then 0 else
              val chars = target.asInstanceOf[Array[Char]]
              val out = jn.CharBuffer.wrap(chars, targetOffset, targetSpace).nn
              staging.flip()
              decode(staging, out, total, true)
              total += staging.position
              staging.compact()

              if decoder.flush(out).nn.isUnderflow then ended = true

              out.position - targetOffset

  // Character encoding as a pipeline stage. Malformed input (a split
  // surrogate pair at end-of-stream) and unmappable characters are replaced,
  // mirroring the replacement semantics of `CharEncoder.encoded`.
  given charEncoder: Of[CharEncoder, Text, Data, Credit, Credit] =
    new Ductile:
      type Self = CharEncoder
      type Operand = Text
      type Result = Data
      type Transport = Credit
      type Upstream = Credit

      def duct(consume stage: CharEncoder^)(using buffering: Buffering)
      :   Duct[Text, Data] { type Transport = Credit; type Upstream = Credit } =

        new Duct[Text, Data]:
          type Transport = Credit
          type Upstream = Credit

          private val encoder: jnc.CharsetEncoder =
            stage.encoding.charset.newEncoder().nn
            . onMalformedInput(jnc.CodingErrorAction.REPLACE).nn
            . onUnmappableCharacter(jnc.CodingErrorAction.REPLACE).nn

          private val worst: Int = encoder.maxBytesPerChar.toDouble.ceil.toInt.max(1)

          private val staging: jn.CharBuffer =
            jn.CharBuffer.allocate(buffering.capacity(Substrate.Chars)).nn

          private var ended: Boolean = false

          def regulation: Credit is Regulation = summon[Credit is Regulation]

          // A char yields at least one byte and at most `worst`; requesting
          // the byte demand divided by the worst case can never overflow the
          // downstream, and the floor of one prevents starvation.
          def translate(demand: Credit): Credit = Credit((demand.count/worst).max(1))

          override def quantum: Int = worst

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
            val copy = sourceLength.min(staging.remaining)
            staging.put(chars, sourceOffset, copy)
            staging.flip()

            val out = jn.ByteBuffer.wrap(bytes, targetOffset, targetSpace).nn
            encoder.encode(staging, out, false)
            staging.compact()

            Duct.Progress(copy, out.position - targetOffset)

          override update def flush(target: output.Storage, targetOffset: Int, targetSpace: Int)
          :   Int =

            if ended then 0 else
              val bytes = target.asInstanceOf[Array[Byte]]
              val out = jn.ByteBuffer.wrap(bytes, targetOffset, targetSpace).nn
              staging.flip()
              encoder.encode(staging, out, true)
              staging.compact()

              if encoder.flush(out).nn.isUnderflow then ended = true

              out.position - targetOffset

trait Ductile extends Typeclass.Pure, Operable, Resultant:
  type Transport
  type Upstream

  def duct(consume stage: Self^)(using Buffering): (Duct[Operand, Result] {
    type Transport = Ductile.this.Transport
    type Upstream = Ductile.this.Upstream })^
