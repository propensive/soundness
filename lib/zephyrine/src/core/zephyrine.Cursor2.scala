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
┃    Soundness, version 0.54.0.                                                                    ┃
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

import scala.collection.mutable as scm

import denominative.*
import prepositional.*
import proscenium.*
import rudiments.*
import vacuous.*

object Cursor2:
  opaque type Mark = Long
  opaque type Offset = Long

  class Held()

  type Loader[data] = () => Optional[data]

  object Mark:
    final val Initial: Mark = 0L

    inline def apply(absolute: Long): Mark = absolute

    given ordered: Ordering[Mark] = Ordering.Long


  object Offset:
    inline def apply(line: Ordinal, column: Ordinal): Offset =
      (line.n0.toLong << 32) | (column.n0.toLong & 0xffffffffL)

    given ordered: Ordering[Offset] = Ordering.Long


  extension (mark: Mark)
    inline def absolute: Long = mark
    private[zephyrine] inline def increment: Mark = mark + 1
    private[zephyrine] inline def decrement: Mark = mark - 1


  extension (offset: Offset)
    inline def line: Ordinal = (offset >> 32 & 0xffffffff).toInt.z
    inline def column: Ordinal = offset.toInt.z


  // Default initial buffer size for streaming use; pre-filled buffers use the
  // exact size of the initial chunk.
  private final val DefaultCapacity: Int = 256

  // Build a Cursor2 from an explicit loader. The cursor starts empty; the
  // first `next()` triggers a load.
  transparent inline def apply[data](inline load: Loader[data])
    ( using addressable0: data is Addressable,
            lineation0:   Lineation by addressable0.Operand )
  :   Cursor2[data] =

    new Cursor2[data]
      ( () => load(),
        Unset,
        DefaultCapacity,
        addressable0,
        lineation0 )


  // Build a Cursor2 pre-filled with a single chunk; the loader is a no-op.
  // This is the "Direct" mode: parsers that already have all the data in
  // memory pay no per-call refill cost.
  transparent inline def apply[data](initial: data)
    ( using addressable0: data is Addressable,
            lineation0:   Lineation by addressable0.Operand )
  :   Cursor2[data] =

    new Cursor2[data]
      ( () => Unset,
        initial,
        addressable0.length(initial).max(1),
        addressable0,
        lineation0 )


  // Backwards-compatible factory that adapts an Iterator to the loader API.
  // Lets the existing test suite cross-compile against Cursor2.
  transparent inline def apply[data](iterator: Iterator[data])
    ( using addressable0: data is Addressable,
            lineation0:   Lineation by addressable0.Operand )
  :   Cursor2[data] =

    new Cursor2[data]
      ( () => if iterator.hasNext then iterator.next() else Unset,
        Unset,
        DefaultCapacity,
        addressable0,
        lineation0 )


final class Cursor2[data]
  (             load:        () => Optional[data],
                initial:     Optional[data],
                initialSize: Int,
    tracked val addressable: data is Addressable,
    tracked val lineation:   Lineation by addressable.Operand ):

  // ─── state ────────────────────────────────────────────────────────────────
  // A single contiguous buffer holds all currently-live data. `pos` is the
  // hot-path read index into `buffer` (an `Int`, so `peek` lowers to one
  // array access). `writeEnd` is the count of valid bytes/chars in the
  // buffer. `basePos` is the absolute stream position of `buffer(0)`, used
  // to translate buffer indices to/from `Mark` values that survive
  // compaction. `holdStart` is the buffer index at the start of the held
  // region (or -1 when no hold is active); compaction may not advance past
  // it. `ended` becomes true when the loader has returned `Unset`.

  private var buffer:    addressable.Storage = addressable.allocate(initialSize)
  private var pos:       Int = 0
  private var writeEnd:  Int = 0
  private var basePos:   Long = 0L
  private var holdStart: Int = -1
  private var ended:     Boolean = false

  private val marks:   scm.ArrayDeque[Cursor2.Mark] = scm.ArrayDeque()
  private val offsets: scm.ArrayDeque[Cursor2.Offset] = scm.ArrayDeque()

  private var lineNo:   Ordinal = Prim
  private var columnNo: Ordinal = Prim

  // Seed the buffer: prefer an explicit pre-fill chunk; otherwise pull from
  // the loader until we have non-empty data or hit EOF. Matches the existing
  // `Cursor.apply` behaviour, which pre-loads the first block so `datum` is
  // valid before any `next()` call.
  locally:
    initial.let: chunk =>
      val len = addressable.length(chunk)
      if len > 0 then
        if len > addressable.storageSize(buffer) then
          buffer = addressable.allocate(len)
        addressable.copyChunk(chunk, 0, buffer, 0, len)
        writeEnd = len
    if writeEnd == 0 then refill()

  // ─── slow path: refill ────────────────────────────────────────────────────
  // Kept as a regular (non-inline) method so the slow path's bytecode bloat
  // doesn't push `next()` past the JIT's inline budgets. Mirrors the rationale
  // for the original `Cursor.forward()`.
  private def refill(): Unit =
    if !ended then
      // Compact: drop any data that is no longer reachable. Outside a hold,
      // everything before `pos` is dead; inside a hold, everything before
      // `holdStart` is dead. `keep` is capped at `writeEnd` because `pos` may
      // sit one past the last loaded byte after a `next()` that consumed the
      // tail of the buffer.
      val rawKeep = if holdStart >= 0 then holdStart else pos
      val keep = rawKeep.min(writeEnd)
      if keep > 0 then
        val live = writeEnd - keep
        if live > 0 then addressable.transfer(buffer, keep, buffer, 0, live)
        basePos += keep
        pos -= keep
        writeEnd = live
        if holdStart >= 0 then holdStart = 0

      // Pull chunks until we either receive non-empty data or hit EOF.
      var loaded = false
      while !loaded && !ended do
        val chunk = load()
        if chunk.absent then ended = true
        else
          val data = chunk.vouch
          val len = addressable.length(data)
          if len > 0 then
            ensureCapacity(writeEnd + len)
            addressable.copyChunk(data, 0, buffer, writeEnd, len)
            writeEnd += len
            loaded = true

  private def ensureCapacity(needed: Int): Unit =
    val cap = addressable.storageSize(buffer)
    if needed > cap then
      var newCap = if cap == 0 then needed.max(16) else cap
      while newCap < needed do newCap *= 2
      val newBuf = addressable.allocate(newCap)
      if writeEnd > 0 then addressable.transfer(buffer, 0, newBuf, 0, writeEnd)
      buffer = newBuf

  // ─── core navigation ──────────────────────────────────────────────────────

  // `advance()` is unchecked (it just increments `pos`); it's safe to leave
  // `pos` past `writeEnd` because the next `more` / `peek` / `next()` call
  // forces a refill before the buffer is read. This mirrors the existing
  // `Cursor.forward` rationale and keeps the inner loop one instruction
  // tighter — important for raw byte-scan parsers like Merino where the
  // hot loop is `while more && {peek-test} do advance()`.
  inline def advance(): Unit =
    if lineation.active then
      val operand = addressable.storageAddress(buffer, pos)
      pos += 1
      columnNo =
        if !lineation.track(operand) then columnNo.next
        else { lineNo = lineNo.next; Prim }
    else pos += 1

  // `next()` is `advance(); more`, so it returns `true` while more data is
  // available and `false` when the stream is exhausted.
  inline def next(): Boolean =
    advance()
    more

  // Hot path. The first comparison short-circuits when there's still data
  // in the buffer; only when the buffer is drained do we pay for the slow
  // path. `moreSlow` is non-inline so the inline budget for `more` stays
  // small enough that callers (parser hot loops) get tight bytecode.
  inline def more: Boolean = pos < writeEnd || moreSlow()

  private def moreSlow(): Boolean =
    !ended && { refill(); pos < writeEnd }

  inline def finished: Boolean = !more
  inline def position: Ordinal = (basePos + pos).toInt.z
  inline def available: Int = writeEnd - pos
  inline def line: Ordinal = lineNo
  inline def column: Ordinal = columnNo

  // Stream of all unconsumed data from the current position onwards. Yields
  // the buffered tail first (one chunk materialised from `pos` to `writeEnd`),
  // then drains the loader, returning chunks as it goes. Caller-driven, so a
  // streaming consumer pays nothing until it pulls.
  def remainder: Stream[data] =
    val tailLen = writeEnd - pos
    val tail: data =
      if tailLen <= 0 then addressable.empty
      else addressable.materialize(buffer, pos, tailLen)
    pos = writeEnd
    if tailLen > 0 then tail #:: loaderStream else loaderStream

  private def loaderStream: Stream[data] =
    if ended then Stream.empty
    else load() match
      case Unset =>
        ended = true
        Stream.empty
      case chunk: data @unchecked =>
        if addressable.length(chunk) > 0 then chunk #:: loaderStream
        else loaderStream

  // ─── current element ──────────────────────────────────────────────────────

  inline def datum(using erased Unsafe): addressable.Operand =
    addressable.storageAddress(buffer, pos)

  inline def lay[result](inline otherwise: => result)(inline lambda: addressable.Operand => result)
  :   result =

    if !finished then lambda(addressable.storageAddress(buffer, pos)) else otherwise

  inline def let(inline lambda: addressable.Operand => Unit): Unit =
    if !finished then lambda(addressable.storageAddress(buffer, pos))

  inline def process[result](inline lambda: addressable.Operand => result): Unit =
    if !finished then lambda(addressable.storageAddress(buffer, pos))

  // ─── search primitives ────────────────────────────────────────────────────

  inline def seek(target: addressable.Operand): Boolean =
    var found = false
    var continue = true
    while continue do
      found = datum(using Unsafe) == target
      continue = !found && next()
    found

  inline def consume(inline otherwise: => Unit)(inline text: String): Unit =
    ${zephyrine.internal.consume2('this, 'text, 'otherwise)}

  // ─── hold / mark / cue / grab / clone ─────────────────────────────────────

  // `mark` requires `using Cursor2.Held` so callers can only mark inside a
  // hold block, where compaction cannot drop the marked region.
  inline def mark(using held: Cursor2.Held): Cursor2.Mark =
    Cursor2.Mark(basePos + pos).tap: mark =>
      if lineation.active then
        marks.append(mark)
        offsets.append(Cursor2.Offset(lineNo, columnNo))

  inline def offset(mark: Cursor2.Mark): Cursor2.Offset = offsets(marks.lastIndexOf(mark))

  inline def cue(mark: Cursor2.Mark): Unit =
    pos = (mark.absolute - basePos).toInt
    if lineation.active then
      val o = offset(mark)
      lineNo = o.line
      columnNo = o.column

  inline def hold[result](inline action: Cursor2.Held ?=> result): result =
    val wasHeld = holdStart >= 0
    if !wasHeld then holdStart = pos
    action(using new Cursor2.Held()).also:
      if !wasHeld then
        holdStart = -1
        marks.clear()
        offsets.clear()

  inline def grab(start: Cursor2.Mark, end: Cursor2.Mark): data =
    val len = (end.absolute - start.absolute).toInt
    if len <= 0 then addressable.empty
    else addressable.materialize(buffer, (start.absolute - basePos).toInt, len)

  // Zero-copy access to the live region between two marks. The lambda
  // receives the buffer, offset, and length; it must not retain the storage
  // reference, since compaction may invalidate it after the call returns.
  inline def slice[result](start: Cursor2.Mark, end: Cursor2.Mark)
                          (inline lambda: (addressable.Storage, Int, Int) => result)
  :   result =
    val off = (start.absolute - basePos).toInt
    val len = (end.absolute - start.absolute).toInt
    lambda(buffer, off, len)

  inline def clone(start: Cursor2.Mark, end: Cursor2.Mark)(target: addressable.Target): Unit =
    val len = (end.absolute - start.absolute).toInt
    if len > 0
    then addressable.cloneStorage(buffer, (start.absolute - basePos).toInt, len)(target)

  inline def take(inline otherwise: => data)(length: Int): data =
    var count = 0
    hold:
      val start = mark
      while count < length do
        next()
        count += 1

      grab(start, mark)
