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

import scala.caps

// Contextual configuration determining how streaming stages are instantiated
// with buffers. `capacity` is consulted once per stage at construction time, so
// an adaptive instance (e.g. one consulting available memory) sees each new
// stage, but never resizes a live one. `window` is the number of blocks of
// headroom a `Conduit` holds between its writer and reader threads.
object Buffering:
  given standard: Buffering:
    def capacity(substrate: Substrate): Int = substrate match
      case Substrate.Bytes => 4096
      case Substrate.Chars => 2048
      case Substrate.Boxes => 256

    // Deep enough that a producer/consumer pair in lockstep exchange several
    // transfer blocks per wakeup rather than parking after every few: hand-off
    // throughput scales almost linearly with depth up to this point, at a
    // bounded cost of `window` in-flight transfer blocks.
    //
    // The capacity-search stress tests show hand-off throughput under high
    // concurrency keeps improving well past this depth: a window covering a whole
    // burst, so the producer streams it without ever parking, more than doubled
    // sustained throughput at 64. But a deeper window multiplies the worst-case
    // in-flight bound of every copy-path conduit, so the default stays
    // conservative; burst-heavy pipelines should override `window` to their burst
    // size in hand-off blocks, and the shared `Blockpool` keeps the deeper ring's
    // allocation amortised across conduit instances.
    def window: Int = 16

// Pure: a `Buffering` is only sizing policy, so instances are untracked under capture
// checking and closures over them stay pure.
trait Buffering extends caps.Pure:
  def capacity(substrate: Substrate): Int
  def window: Int

  // The preferred size of a block crossing an asynchronous boundary (a `Conduit`
  // hand-off, or a fan-in/fan-out pump transfer). Staging buffers want to stay
  // cache-resident, but every asynchronous block costs a synchronized queue
  // transfer (with a likely park/unpark), so boundary blocks want to be much
  // larger: the default is sixteen staging blocks.
  def transfer(substrate: Substrate): Int = capacity(substrate)*16

  // Whether a `Conduit` recycles the transfer blocks its reader drains, handing
  // them back to the writer for reuse rather than minting a fresh (zero-filled)
  // block per hand-off. On by default; override to `false` to isolate the pool's
  // effect or to restore strict per-block allocation.
  def recycle: Boolean = true
