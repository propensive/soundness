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

import prepositional.*
import vacuous.*

// A synchronous transformation stage, attachable to either end of a pipeline:
// `stream.via(duct)` yields a differently-typed `Stream`, and
// `intake.accepting(duct)` yields a differently-typed `Intake` — the same
// `Duct` instance serves both, since `translate` converts demand whichever
// direction it is reported.
//
// `translate` is the demand-conversion hook: it maps downstream demand (in
// `out` terms) to upstream demand (in `in` terms), e.g. a bytes-to-hex duct
// maps a credit of 1024 chars to a credit of 512 bytes. Conversions must be
// conservative bounds, not exact arithmetic: any surplus a stage produces is
// retained in its buffer for the next transfer, which is what makes ducts
// with unknowable ratios (compression) possible.
//
// `step` is a pure buffer-to-buffer transformation. Given at least one input
// element and at least `quantum` elements of output space, it must consume or
// produce at least one element; input that cannot yet be transformed (a
// partial atom at the end of the offered input) must be consumed and carried
// in the duct's internal state, never left unconsumed across steps. A duct is
// single-owner mutable state: it belongs to the one thread driving its side
// of the pipeline.
object Duct:
  // Unboxed (consumed, produced) pair returned by `step`.
  object Progress:
    inline def apply(consumed: Int, produced: Int): Progress =
      (consumed.toLong << 32) | (produced.toLong & 0xffffffffL)

    extension (progress: Progress)
      inline def consumed: Int = (progress >> 32).toInt
      inline def produced: Int = progress.toInt

  opaque type Progress = Long

  // Drive a duct over a whole value, as if it were the single window of a
  // stream — but with none of the stream machinery: no endpoint, no credit
  // accounting, no refill cycle. `step` runs over the value's storage (its
  // backing directly when the medium exposes one, else one copy in) until the
  // input is consumed, output windows accumulate into the medium's builder,
  // and `flush` drains the duct's terminal state. This is the whole-value
  // counterpart of `stream.via(duct)`: one transformation, two drivers.
  def feed[in, out](value: in, consume duct: (Duct[in, out])^)(using buffering: Buffering): out =
    val length = duct.input.length(value)

    val source: duct.input.Storage^{caps.any.rd} = duct.input.backing(value).or:
      val copied = duct.input.allocate(length.max(1))
      duct.input.copyChunk(value, 0, copied, 0, length)
      copied

    val target = duct.output.blank(buffering.capacity(duct.output.substrate))
    val space = buffering.transfer(duct.output.substrate).max(duct.quantum)
    val window: duct.output.Storage^ = duct.output.allocate(space)

    // `step`/`flush` declare pure `Storage` parameters; the kernel drivers
    // cross that rim with cast-erased windows (the established recipe), and
    // this driver does the same — the storage never escapes this method.
    val source0 = source.asInstanceOf[duct.input.Storage]
    val window0 = window.asInstanceOf[duct.output.Storage]

    var consumed = 0

    while consumed < length do
      val progress = duct.step(source0, consumed, length - consumed, window0, 0, space)

      if progress.produced > 0
      then duct.output.cloneStorage(window0, 0, progress.produced)(target)

      // The step contract guarantees progress given input and `quantum` space;
      // a zero-progress step would loop forever, so treat it as terminal.
      if progress.consumed == 0 && progress.produced == 0 then consumed = length
      else consumed += progress.consumed

    var flushed = duct.flush(window0, 0, space)

    while flushed > 0 do
      duct.output.cloneStorage(window0, 0, flushed)(target)
      flushed = duct.flush(window0, 0, space)

    duct.close()
    duct.output.build(target)

// A duct is single-owner mutable state (compressors, partial atoms), so it is a stateful
// capability: `step`/`flush`/`close` mutate it and require exclusive access, while
// `translate`/`quantum`/`regulation` are pure queries.
abstract class Duct[in, out]
  ( using val input: in is Addressable, val output: out is Addressable )
extends caps.ExclusiveCapability, caps.Stateful:

  type Transport
  type Upstream

  def regulation: Transport is Regulation

  // Convert downstream demand into the demand this duct presents upstream.
  // Must not starve: whenever `demand` grants at least `quantum` elements, the
  // translated demand must grant at least one, and unbounded demand values
  // (`Long.MaxValue` credit) must be handled without arithmetic overflow —
  // compute ceiling divisions as `n - n/2`, not `(n + 1)/2`.
  def translate(demand: Transport): Upstream

  // The minimum output space `step` needs to guarantee progress, e.g. `2` for
  // a duct whose smallest output atom is two elements.
  def quantum: Int = 1

  update def step
    ( source: input.Storage,
      sourceOffset: Int,
      sourceLength: Int,
      target: output.Storage,
      targetOffset: Int,
      targetSpace: Int )
  :   Duct.Progress

  // Emit terminal state after the upstream ends: a compressor's tail, and —
  // critically — any pending output the transformation retains internally
  // beyond what `step` could deliver (a decompressor may hold far more
  // undelivered output than one step's space). A duct whose state can retain
  // output MUST override this; the first `0` it returns is taken as the end
  // of the stream. Called repeatedly until it returns `0`.
  update def flush(target: output.Storage, targetOffset: Int, targetSpace: Int): Int = 0

  update def close(): Unit = ()
