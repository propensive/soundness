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

import language.experimental.separationChecking

import java.nio as jn, jn.charset as jnc

import anticipation.*
import hieroglyph.*
import prepositional.*
import rudiments.*
import vacuous.*

// Makes a stage-descriptor value — a character decoder, a compression
// algorithm, a serialization alphabet — instantiable as a `Duct`, so it can
// be applied directly with `stream.through(stage)` or
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

          private val staging: jn.ByteBuffer =
            jn.ByteBuffer.allocate(buffering.capacity(Substrate.Bytes)).nn

          private var total: Int = 0
          private var ended: Boolean = false

          def regulation: Credit is Regulation = summon[Credit is Regulation]

          // At most one output char per input byte, so char-credit is a
          // sound byte-credit as it stands.
          def translate(demand: Credit): Credit = demand

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

          update def step
            ( source: input.Storage,
              sourceOffset: Int,
              sourceLength: Int,
              target: output.Storage,
              targetOffset: Int,
              targetSpace: Int )
          :   Duct.Progress =

            val bytes = source.asInstanceOf[Array[Byte]]
            val chars = target.asInstanceOf[Array[Char]]
            val out = jn.CharBuffer.wrap(chars, targetOffset, targetSpace).nn

            if staging.position == 0 then
              // Fast path: with no carried bytes, decode straight from the source
              // window — no intermediate copy of the bulk of the stream.
              val in = jn.ByteBuffer.wrap(bytes, sourceOffset, sourceLength).nn
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
            else
              // Carry path: prior incomplete bytes are staged, so append the new
              // window to make the split character contiguous, then decode.
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
