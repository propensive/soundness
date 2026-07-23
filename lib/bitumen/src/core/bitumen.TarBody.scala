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
package bitumen

import anticipation.*
import prepositional.*
import rudiments.*
import vacuous.*
import zephyrine.*

object TarBody:
  // An in-memory body: its chunks are given up front, and nothing pulls lazily.
  def apply(chunks: Data*): TarBody =
    new TarBody(chunks.filter(_.length > 0).to(List), () => Unset)

  val empty: TarBody = TarBody()

  // A body fed lazily from a source the producer still owns (the shared cursor
  // of a streaming read, or an unread source stream): `pull` yields the next
  // chunk, or `Unset` when the body is complete. The producer's captures are
  // erased at this audited point — exactly the laundering the memoizing
  // `LazyList` chain this replaces performed implicitly through its cells —
  // and the producer must remain valid until the body is drained.
  private[bitumen] def deferred(pull: () => Optional[Data]): TarBody =
    new TarBody(Nil, caps.unsafe.unsafeAssumePure(pull))

// The replayable body of an archive entry. Chunks pull lazily from the
// producer and memoize, so the underlying region is read exactly once however
// many consumers stream it, and each `stream` replays from the first chunk.
// An in-order consumer of a streaming read holds memory bounded by the entries
// it retains: a body's memoized chunks are reclaimed with its entry.
class TarBody private (initial: List[Data], pull: () -> Optional[Data]):
  private val memo: scala.collection.mutable.ArrayBuffer[Data] =
    scala.collection.mutable.ArrayBuffer.from(initial)

  private var exhausted: Boolean = false

  // Extend the memo by one chunk, or record exhaustion.
  private def fetch(): Boolean =
    if exhausted then false else
      val next = pull()

      if next.absent then
        exhausted = true
        false
      else
        val chunk = next.vouch
        if chunk.length > 0 then memo += chunk
        chunk.length > 0 || fetch()

  // Read the remainder of the body from its producer, so the producer may move
  // past it. Memoized chunks are never re-read.
  private[bitumen] def drain(): Unit = while fetch() do ()

  def size: Long =
    drain()
    memo.foldLeft(0L)(_ + _.length)

  // The body's chunks, replayed from the start; unread chunks pull from the
  // producer as the iterator advances.
  def chunks: Iterator[Data] = new Iterator[Data]:
    private var index: Int = 0

    def hasNext: Boolean = index < memo.length || fetch()

    def next(): Data =
      val chunk = memo(index)
      index += 1
      chunk

  // A fresh stream over the body's chunks, replayed from the start.
  def stream: (Stream[Data] over Credit)^ = Stream(chunks)

  // The whole body as a single value.
  def memoize: Data =
    drain()

    if memo.length == 1 then memo(0) else
      val whole = new Array[Byte](size.toInt)
      var offset = 0

      memo.each: chunk =>
        System.arraycopy(chunk.mutable(using Unsafe), 0, whole, offset, chunk.length)
        offset += chunk.length

      whole.immutable(using Unsafe)
