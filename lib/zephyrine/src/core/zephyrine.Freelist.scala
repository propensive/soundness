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

import java.util.concurrent.atomic as juca

// A bounded single-producer/single-consumer pool of spent blocks, the return
// path that lets a `Conduit`'s reader hand a drained block back to its writer
// for reuse instead of dropping it to the collector while the writer mints a
// fresh (zero-filled) replacement. Exactly one thread offers (the reader) and
// exactly one polls (the writer) — the same discipline the endpoints already
// enforce for the forward `Handoff`.
//
// Unlike `Handoff`, neither side ever blocks: this is a best-effort cache, not
// a rendezvous. A `poll` on an empty pool returns `null` (the writer allocates
// instead) and an `offer` to a full pool discards the block (the collector
// reclaims it). So the writer can never stall on an empty pool — which would
// deadlock against a reader parked waiting for that same writer — and the
// reader never stalls returning a block.
//
// A `SharedCapability`, like `Handoff`: the block ownership transfer is
// discharged by the ring's volatile publication order (a returned block is one
// the reader has finished reading and released), not by aliasing analysis.
final class Freelist(slots0: Int) extends caps.SharedCapability:
  private val capacity: Int = Integer.highestOneBit((slots0.max(1)*2) - 1)
  private val mask: Int = capacity - 1
  private val slots: juca.AtomicReferenceArray[AnyRef] = juca.AtomicReferenceArray(capacity)

  // `tail` is written only by the producer (reader), `head` only by the
  // consumer (writer); each publishes its index with a volatile write the other
  // reads to bound occupancy, exactly as `Handoff`.
  private val head: juca.AtomicLong = juca.AtomicLong(0)
  private val tail: juca.AtomicLong = juca.AtomicLong(0)

  // Producer side (the reader thread): cache a spent block, returning whether it was
  // accepted; a full pool declines, and the caller either drops the block to the
  // collector or demotes it to the shared `Blockpool`. The block must be one the reader
  // has fully drained and will never touch again — its contents are irrelevant, since
  // the writer overwrites before publishing.
  def offer(item: AnyRef): Boolean =
    val position = tail.get

    if position - head.get < capacity then
      slots.lazySet((position & mask).toInt, item)
      tail.set(position + 1)
      true
    else
      false

  // Consumer side (the writer thread): take a cached block to reuse, or `null`
  // if the pool is empty, in which case the writer allocates a fresh block.
  def poll(): AnyRef | Null =
    val position = head.get

    if position < tail.get then
      val index = (position & mask).toInt
      val item = slots.get(index)
      slots.lazySet(index, null)
      head.set(position + 1)
      item
    else
      null
