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

import java.util.concurrent as juc
import java.util.concurrent.atomic as juca

// A process-wide, bounded pool of spent transfer blocks, shared across conduits: the
// second-level cache behind each `Conduit`'s per-instance `Freelist`. The freelist
// recycles blocks between one writer/reader pair over a conduit's lifetime; this pool
// carries them *across* lifetimes, so a short-lived conduit (one hand-off pipeline per
// operation) mints its blocks from its predecessors' instead of allocating afresh and
// abandoning a whole pool to the collector each time.
//
// Blocks are pooled by their storage class and exact physical size, so a reused block is
// always type- and size-correct for the mint that requests it — the same single-size
// discipline as the freelist (a conduit only ever offers and polls `ceiling`-sized
// blocks). Each (class, size) class is bounded, targeting a fixed retained budget, so an
// idle process caches a bounded, small number of blocks and everything beyond the bound
// goes to the collector exactly as before.
//
// Unlike `Handoff` and `Freelist`, this pool is multi-producer/multi-consumer: any
// conduit's writer may poll and any reader may offer, concurrently. Both operations are
// non-blocking and best-effort: a `poll` miss means the caller allocates; an `offer`
// over the bound discards. Contents are never read — a reused block is overwritten in
// place before publication — so the only transfer discharged here is reachability, via
// the queue's own publication order.
object Blockpool:
  // The retained budget per (class, size): about 16 MiB, floored so tiny blocks still
  // pool usefully and capped so huge blocks cannot make even a few retained instances
  // enormous.
  private def bound(size: Int): Int = ((16 << 20)/size.max(1)).max(4).min(256)

  private final class Pool(size: Int):
    val queue: juc.ConcurrentLinkedQueue[AnyRef] = juc.ConcurrentLinkedQueue()
    val count: juca.AtomicInteger = juca.AtomicInteger(0)
    val limit: Int = bound(size)

  private final case class Key(cls: Class[?], size: Int)

  private val pools: juc.ConcurrentHashMap[Key, Pool] = juc.ConcurrentHashMap()

  private def pool(cls: Class[?], size: Int): Pool =
    val key = Key(cls, size)
    val existing = pools.get(key)

    if existing != null then existing else
      val created = new Pool(size)
      val race = pools.putIfAbsent(key, created)
      if race == null then created else race

  // Cache a spent block, or discard it if its class is at bound. `size` must be the
  // block's exact physical size; the block must be fully relinquished by its offerer.
  def offer(cls: Class[?], size: Int, block: AnyRef): Unit =
    val pool0 = pool(cls, size)

    // The count is raised before the block is enqueued (and lowered again if that
    // overshot the bound), so it is always an upper estimate of the queue's length and
    // the bound is strict.
    if pool0.count.incrementAndGet() <= pool0.limit then pool0.queue.offer(block)
    else pool0.count.decrementAndGet()

  // A pooled block of exactly this class and physical size, or `null`, in which case
  // the caller allocates afresh.
  def poll(cls: Class[?], size: Int): AnyRef | Null =
    val pool0 = pools.get(Key(cls, size))

    if pool0 == null then null else
      val block = pool0.queue.poll()
      if block != null then pool0.count.decrementAndGet()
      block
