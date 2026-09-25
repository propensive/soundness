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

import scala.caps

import anticipation.*
import fulminate.*
import prepositional.*
import rudiments.*
import vacuous.*
import zephyrine.*

// What the sequential archive formats share: the memoizing entry body a streaming read lends
// to its consumer, and the reader plumbing that pulls it off a shared cursor. `Tar` and `Ar`
// each parse their own headers; the lookahead, draining and chunked pulling are the same.
object Archive:
  object Body:
    // An in-memory body: its chunks are given up front, and nothing pulls lazily.
    def apply(chunks: Data*): Archive.Body =
      new Archive.Body(chunks.filter(_.length > 0).to(List), () => Unset)

    val empty: Archive.Body = Archive.Body()

    // A body fed lazily from a source the producer still owns (the shared cursor
    // of a streaming read, or an unread source stream): `pull` yields the next
    // chunk, or `Unset` when the body is complete. The producer's captures are
    // erased at this audited point — exactly the laundering the memoizing
    // `LazyList` chain this replaces performed implicitly through its cells —
    // and the producer must remain valid until the body is drained.
    private[bitumen] def deferred(pull: () => Optional[Data]): Archive.Body =
      new Archive.Body(Nil, caps.unsafe.unsafeAssumePure(pull))

  // The replayable body of an archive entry. Chunks pull lazily from the
  // producer and memoize, so the underlying region is read exactly once however
  // many consumers stream it, and each `stream` replays from the first chunk.
  // An in-order consumer of a streaming read holds memory bounded by the entries
  // it retains: a body's memoized chunks are reclaimed with its entry.
  class Body private (initial: List[Data], pull: () -> Optional[Data]):
    private val memo: scala.collection.mutable.ArrayBuffer[Data] =
      // `ArrayBuffer.from` demands an `IterableOnce`, which the opaque `List` is not.
      scala.collection.mutable.ArrayBuffer.from(initial.stdlib)

    @scala.caps.unsafe.untrackedCaptures
    private var exhausted: Boolean = false

    // Extend the memo by one chunk, or record exhaustion.
    private def fetch(): Boolean =
      if exhausted then false else
        // `lay` dispatches on presence, avoiding a runtime type test against the opaque `Data`.
        pull().lay:
          exhausted = true
          false

        . apply: chunk =>
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
      @scala.caps.unsafe.untrackedCaptures
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
        val whole = Array.allocate[Byte](size.toInt)
        var offset = 0

        memo.each: chunk =>
          whole.place(chunk, 0, offset, chunk.length)
          offset += chunk.length

        Array.freeze(whole)

  // A lookahead iterator over a sequential archive. The subclass's `parse()` either `emit`s
  // the next entry — with its lazily-pulled body, if the entry lends one — or `finish`es;
  // before each parse, whatever of the previous entry's body was not yet read is drained, so
  // the cursor stands at the next header. A stdlib class cannot extend `Stateful`, so its
  // state is untracked (the record-iterator precedent).
  private[bitumen] abstract class Lookahead[entry] extends Iterator[entry]:
    @caps.unsafe.untrackedCaptures
    private var lookahead: Optional[entry] = Unset

    @caps.unsafe.untrackedCaptures
    private var unread: Optional[Archive.Body] = Unset

    @caps.unsafe.untrackedCaptures
    private var finished: Boolean = false

    // Parse forward from the cursor to the next entry, or to the end of the archive.
    protected def parse(): Unit

    protected def emit(entry: entry, body: Optional[Archive.Body] = Unset): Unit =
      lookahead = entry
      unread = body

    protected def finish(): Unit = finished = true
    protected def pending: Boolean = lookahead.present
    protected def done: Boolean = finished

    private def advance(): Boolean =
      unread.let(_.drain())
      unread = Unset
      parse()
      lookahead.present

    def hasNext: Boolean = lookahead.present || (!finished && advance())

    // `lay` rather than a type test: the entry type is erased.
    def next(): entry =
      def exhausted: entry =
        if !finished && advance() then next() else panic(m"the archive has no more entries")

      lookahead.lay(exhausted): entry =>
        lookahead = Unset
        entry

  // Exactly `count` bytes off the cursor, or the caller's abort.
  private[bitumen] def takeExactly(cursor: Cursor[Data, {}]^, count: Int)
    ( truncated: (Int, Int) => Nothing )
  :   Data =

    cursor.take(truncated(count, cursor.available))(count).asInstanceOf[Data]

  // Pulls an entry's `size` bytes off the shared cursor in bounded chunks, consuming the
  // trailing padding after the final one. The closure is handed to `Body.deferred`, whose
  // memoization guarantees the region is read exactly once, in order. `truncated` is the
  // caller's abort, so no error type or tactic crosses this boundary: its capture rides in
  // the closure's set.
  private[bitumen] def bodyPull(cursor: Cursor[Data, {}]^, size: Long, padded: Long)
    ( truncated: (Int, Int) => Nothing )
  :   () ->{cursor, truncated} Optional[Data] =

    @caps.unsafe.untrackedCaptures
    var consumed: Long = 0L

    val chunkSize: Int = 65536

    () =>
      if consumed >= size then
        if consumed < padded then
          val remainder = (padded - consumed).toInt
          cursor.take(truncated(remainder, cursor.available))(remainder)
          consumed = padded

        Unset
      else
        val count = (size - consumed).min(chunkSize.toLong).toInt

        // The inline `take` expansion re-infers a fresh `any.rd` on the frozen chunk; the
        // cast reasserts the frozen form, which `take` already guarantees.
        val data = cursor.take(truncated(count, cursor.available))(count).asInstanceOf[Data]

        consumed += count
        data
