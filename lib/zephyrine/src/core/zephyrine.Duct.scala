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

// A synchronous transformation stage, attachable to either end of a pipeline:
// `stream.through(duct)` yields a differently-typed `Stream`, and
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

abstract class Duct[in, out]
  ( using val input: in is Addressable, val output: out is Addressable ):

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

  def step
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
  def flush(target: output.Storage, targetOffset: Int, targetSpace: Int): Int = 0

  def close(): Unit = ()
