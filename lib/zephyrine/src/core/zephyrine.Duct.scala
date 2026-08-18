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

import scala.caps

import denominative.*
import prepositional.*
import vacuous.*

// A synchronous transformation stage, attachable to either end of a pipeline:
// `stream.viaDuct(duct)` yields a differently-typed `Stream`, and
// `intake.acceptingDuct(duct)` yields a differently-typed `Intake` — the same
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

  // Drive a duct over a whole value, as if it were the single region of a
  // stream — but with none of the stream machinery: no endpoint, no credit
  // accounting, no refill cycle. `step` runs over the value's storage (its
  // backing directly when the medium exposes one, else one copy in) until the
  // input is consumed, output regions accumulate into the medium's builder,
  // and `flush` drains the duct's terminal state. This is the whole-value
  // counterpart of `stream.viaDuct(duct)`: one transformation, two drivers.
  def feed[in, out](value: in, consume duct: (Duct[in, out])^)(using buffering: Buffering): out =
    val length = duct.input.length(value)

    val source: duct.input.Storage^{caps.any.rd} = duct.input.backing(value).or:
      val copied = duct.input.allocate(length.max(1))
      duct.input.copyChunk(value, 0, copied, 0, length)
      copied

    val target = duct.output.blank(buffering.capacity(duct.output.substrate))
    val space = buffering.transfer(duct.output.substrate).max(duct.quantum)
    val buffer: duct.output.Storage^ = duct.output.allocate(space)

    // `step`/`flush` declare pure `Storage` parameters; the kernel drivers
    // cross that rim with cast-erased regions (the established recipe), and
    // this driver does the same — the storage never escapes this method.
    val source0 = source.asInstanceOf[duct.input.Storage]
    val buffer0 = buffer.asInstanceOf[duct.output.Storage]

    var consumed = 0

    while consumed < length do
      val progress =
        Region.over[in, Duct.Progress](using duct.input)(source0, consumed, length): region =>
          range =>
            Slate.over[out, Duct.Progress](using duct.output)(buffer, 0, space): slate =>
              slateSpace => duct.step(region)(range)(slate)(slateSpace)

      if progress.produced > 0 then duct.output.cloneStorage(buffer0, 0, progress.produced)(target)

      // The step contract guarantees progress given input and `quantum` space;
      // a zero-progress step would loop forever, so treat it as terminal.
      if progress.consumed == 0 && progress.produced == 0 then consumed = length
      else consumed += progress.consumed

    def flushOnce(): Int =
      Slate.over[out, Int](using duct.output)(buffer, 0, space): slate =>
        slateSpace => duct.flush(slate)(slateSpace)

    var flushed = flushOnce()

    while flushed > 0 do
      duct.output.cloneStorage(buffer0, 0, flushed)(target)
      flushed = flushOnce()

    duct.close()
    duct.output.build(target)

// A duct is single-owner mutable state (compressors, partial atoms), so it is a stateful
// capability: `step`/`flush`/`close` mutate it and require exclusive access, while
// `translate`/`quantum`/`regulation` are pure queries.
// Type parameters are named `operand`/`result` (not `in`/`out`) so the prepositional infix
// `in` remains usable in member signatures. The two `Addressable` instances are exposed as
// plain (non-given) members: were they `using val`s, every summon of an `Addressable` inside
// a same-medium duct (`Duct[Data, Data]`) would be ambiguous between them.
abstract class Duct[operand, result]
  ( using input0: operand is Addressable, output0: result is Addressable )
extends caps.ExclusiveCapability, caps.Stateful:

  val input: operand is Addressable = input0
  val output: result is Addressable = output0

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

  // The output-window size a streaming driver should allocate for this duct.
  // The staging capacity is right for most ducts: their kernels are cheap per
  // call, so windows should stay cache-resident. A duct whose kernel pays a
  // large fixed cost per invocation (a JNI crossing) and expands its input
  // (decompression) wants transfer-sized windows instead: the fixed cost is
  // amortised over sixteen times the output, which is what lets an inflater
  // that does strictly less work per byte than its rivals also run faster
  // than them. (`Duct.feed`, the whole-value driver, already sizes its buffer
  // this way.)
  def sizing(buffering: Buffering): Int = buffering.capacity(output.substrate)

  // Transform the readable `range` of `source` into the writable `space` of `target`. The
  // regions carry their windows as branded intervals, so an implementation cannot index
  // outside them except through the greppable `unsafely(raw)` escape hatch — which kernels
  // wrapping offset-based JDK APIs (charsets, ciphers, compressors) legitimately take,
  // widening the intervals back to offsets at that boundary.
  update def step(source: Region[operand])(range: Interval in source.type)
    ( target: Slate[result] )(space: Interval in target.type)
  :   Duct.Progress

  // Emit terminal state after the upstream ends: a compressor's tail, and —
  // critically — any pending output the transformation retains internally
  // beyond what `step` could deliver (a decompressor may hold far more
  // undelivered output than one step's space). A duct whose state can retain
  // output MUST override this; the first `0` it returns is taken as the end
  // of the stream. Called repeatedly until it returns `0`.
  update def flush(target: Slate[result])(space: Interval in target.type): Int = 0

  update def close(): Unit = ()
