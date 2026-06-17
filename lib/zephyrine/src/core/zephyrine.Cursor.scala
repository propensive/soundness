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

import scala.annotation.targetName

import anticipation.Data
import anticipation.Text
import contingency.*
import denominative.*
import fulminate.Diagnostics
import prepositional.*
import rudiments.*
import vacuous.*

object Cursor:
  opaque type Mark = Long
  opaque type Offset = Long

  // `Held` is a witness type — it has no per-instance state, and the marker
  // APIs (`mark`, `cue`) only consult its type to gate access. Share a
  // single instance across all `cursor.hold` calls instead of allocating a
  // fresh one each time; parsers using narrow per-leaf holds open one
  // `cursor.hold` per line and would otherwise pay one tiny `Held`
  // allocation per line.
  final class Held private[zephyrine] ()
  val shared: Held = new Held()

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
    private[zephyrine] inline def toLong: Long = offset

  private[zephyrine] inline def offsetFromLong(long: Long): Offset = long


  // Default initial buffer size for streaming use; pre-filled buffers use the
  // exact size of the initial chunk.
  private final val DefaultCapacity: Int = 256

  // Build a Cursor from an explicit loader. The cursor starts empty; the
  // first `next()` triggers a load.
  transparent inline def apply[data](inline load: Loader[data])
    ( using addressable0: data is Addressable,
            lineation0:   Lineation by addressable0.Operand )
  :   Cursor[data] =

    new Cursor[data]
      ( () => load(),
        Unset,
        DefaultCapacity,
        addressable0,
        lineation0 )


  // Build a Cursor pre-filled with a single chunk; the loader is a no-op.
  // This is the "Direct" mode: parsers that already have all the data in
  // memory pay no per-call refill cost.
  transparent inline def apply[data](initial: data)
    ( using addressable0: data is Addressable,
            lineation0:   Lineation by addressable0.Operand )
  :   Cursor[data] =

    new Cursor[data]
      ( () => Unset,
        initial,
        addressable0.length(initial).max(1),
        addressable0,
        lineation0 )


  // Backwards-compatible factory that adapts an Iterator to the loader API.
  // Lets the existing test suite cross-compile against Cursor.
  transparent inline def apply[data](iterator: Iterator[data])
    ( using addressable0: data is Addressable,
            lineation0:   Lineation by addressable0.Operand )
  :   Cursor[data] =

    new Cursor[data]
      ( () => if iterator.hasNext then iterator.next() else Unset,
        Unset,
        DefaultCapacity,
        addressable0,
        lineation0 )

  // Safe, allocation-free single-element peek. Returns `Datum.End` when the
  // cursor is finished; otherwise the current byte (unsigned, `0..255`) or
  // char wrapped as a `Datum`. The `Datum` opaque type (backed by `Int`)
  // is deliberately distinct from raw `Int` so that arithmetic or
  // `Byte`/`Char` confusion can't happen silently — comparison with the
  // expected literal is via the dedicated `Datum.==` overloads, which still
  // compile to a single `int == int`. Two extensions disambiguated by
  // `@targetName` (they erase to the same JVM signature) — callers just
  // write `cursor.peek` without caring whether the cursor is byte- or
  // char-based.
  extension (cursor: Cursor[Data])
    @targetName("peekByte")
    inline def peek: Datum =
      if cursor.finished then Datum.End
      else Datum.fromRaw(cursor.buffer(using Unsafe)(cursor.unsafePos(using Unsafe)) & 0xff)

  extension (cursor: Cursor[Text])
    @targetName("peekChar")
    inline def peek: Datum =
      if cursor.finished then Datum.End
      else Datum.fromRaw(cursor.buffer(using Unsafe)(cursor.unsafePos(using Unsafe)).toInt)

  // Match the cursor's current operand against `target`. On a match, advance
  // past it; on a mismatch (or EOF), raise `failure` via the ambient
  // `Tactic`. Replaces the hand-rolled `if cursor.peek != X then raise(…);
  // cursor.next()` pair that every header / framer parser was writing. The
  // target is `Char` for both variants so callers don't need `'X'.toByte`
  // on `Cursor[Data]`; for ASCII targets the `Datum`-vs-`Char` comparison
  // compiles to a single primitive `int == int`.
  extension (cursor: Cursor[Data])
    @targetName("expectByte")
    inline def expect[error <: Exception](target: Char)
      ( inline failure: Diagnostics ?=> error )
      ( using Tactic[error] )
    :   Unit =

      if cursor.peek == target then cursor.next() else raise(failure)

  extension (cursor: Cursor[Text])
    @targetName("expectChar")
    inline def expect[error <: Exception](target: Char)
      ( inline failure: Diagnostics ?=> error )
      ( using Tactic[error] )
    :   Unit =

      if cursor.peek == target then cursor.next() else raise(failure)

  // Run `action` inside a hold and always restore the cursor position to
  // where it was on entry, regardless of the result — i.e. a non-consuming
  // lookahead. Marks taken inside the action are available via the implicit
  // `Cursor.Held`; the outer cursor is `cue`d back on exit so subsequent
  // processing sees the same bytes. Replaces the explicit
  // `hold { val mk = mark; … cue(mk); result }` idiom that parsers like
  // Multipart's boundary detector and Zeppelin's ZIP signature scanner
  // were writing.
  extension [data](cursor: Cursor[data])
    inline def lookahead[result](inline action: Cursor.Held ?=> result): result =
      cursor.hold:
        val saved = cursor.mark
        val outcome: result = action
        cursor.cue(saved)
        outcome

  // Typed, allocation-free view of the cursor's current backing storage.
  // Returns the same array as `unsafeBuffer` but with its concrete element
  // type, so parsers that snapshot the buffer for a hot-loop scan don't have
  // to write `cursor.unsafeBuffer(using Unsafe).asInstanceOf[Array[Byte]]`
  // themselves — the unsafe cast lives in one place inside zephyrine.
  // `Unsafe` is still required: the returned reference is only valid until
  // the next cursor operation that may compact or grow the buffer.
  extension (cursor: Cursor[Data])
    @targetName("dataBuffer")
    inline def buffer(using erased Unsafe): Array[Byte] =
      cursor.unsafeBuffer(using Unsafe).asInstanceOf[Array[Byte]]

  extension (cursor: Cursor[Text])
    @targetName("textBuffer")
    inline def buffer(using erased Unsafe): Array[Char] =
      cursor.unsafeBuffer(using Unsafe).asInstanceOf[Array[Char]]


final class Cursor[data]
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

  // Cache `lineation.active` once at construction so the per-`advance()`
  // dispatch is a final-field load instead of an interface call. The JIT
  // can no longer prove that `lineation`'s concrete `Lineation` instance has
  // a constant `active`, since `lineation: Lineation` erases the given's
  // concrete type at the field boundary, and the parser-side hot loops
  // would otherwise pay an `invokeinterface` on every character advance.
  private val lineationActive: Boolean = lineation.active

  // Parallel arrays (not deques) of unboxed `Long`-typed `Mark` and `Offset`
  // values for the currently-held region. Using `Array[Long]` rather than
  // `ArrayDeque[Mark]`/`ArrayDeque[Offset]` avoids two `java.lang.Long` boxes
  // per `mark()` call — a meaningful saving on parser hot paths that mark
  // every token boundary.
  private var marks:     Array[Long] = new Array[Long](16)
  private var offsets:   Array[Long] = new Array[Long](16)
  private var marksSize: Int = 0

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
    if lineationActive then
      val operand = addressable.storageAddress(buffer, pos)
      pos += 1

      columnNo =
        if !lineation.track(operand) then columnNo.next
        else { lineNo = lineNo.next; Prim }
    else
      pos += 1

  // Variant of `advance` for callers that have just read the current operand
  // (e.g. via `unsafeBuffer(pos)` in a tight scan loop). Reuses the supplied
  // `operand` instead of re-loading it from the buffer for lineation tracking.
  inline def unsafeAdvanceWith(operand: addressable.Operand)(using erased Unsafe): Unit =
    pos += 1

    if lineationActive then
      columnNo =
        if !lineation.track(operand) then columnNo.next
        else { lineNo = lineNo.next; Prim }

  // Bulk-advance primitives. Allow a caller (typically a parser running a
  // register-resident scan loop) to consume `n` characters without paying the
  // per-character lineation update inside `advance`/`unsafeAdvanceWith`, then
  // reconcile lineation in one shot. The caller is responsible for tracking
  // newlines while it scans.
  inline def unsafeBumpPos(by: Int)(using erased Unsafe): Unit = pos += by

  // Increase the line counter by `by`, leaving the column counter untouched.
  inline def unsafeBumpLine(by: Int)(using erased Unsafe): Unit =
    lineNo = denominative.Ordinal.zerary(lineNo.n0 + by)

  // Increase the column counter by `by`. Must not be called across newlines.
  inline def unsafeBumpColumn(by: Int)(using erased Unsafe): Unit =
    columnNo = denominative.Ordinal.zerary(columnNo.n0 + by)

  // Set the column counter directly. Used after a bulk advance over a range
  // that contained at least one newline, to set the column to the offset
  // since the most-recent newline.
  inline def unsafeSetColumn(value: Int)(using erased Unsafe): Unit =
    columnNo = denominative.Ordinal.zerary(value)

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

  // Cached active-flag for the configured lineation. Hot-loop callers should
  // use this in preference to `lineation.active` to avoid the per-call
  // interface dispatch that the abstract `Lineation` member would imply.
  inline def lineActive: Boolean = lineationActive

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

    // Both branches must stay lazy. `loaderStream` performs a blocking `load()`
    // on its first force, so calling it eagerly would read (and potentially
    // block on) the underlying source before the caller has pulled a single
    // byte — breaking the "pays nothing until it pulls" contract above and
    // deadlocking protocols that withhold data until they get a reply (e.g. a
    // WebSocket upgrade, whose body is the post-handshake frame stream the peer
    // only sends after our `101`). `#::` keeps the non-empty branch lazy; the
    // empty branch must defer the call explicitly.
    if tailLen > 0 then tail #:: loaderStream else Stream.empty.lazyAppendedAll(loaderStream)

  private def loaderStream: Stream[data] =
    if ended then Stream.empty
    else load() match
      case Unset =>
        ended = true
        Stream.empty

      case chunk: data @unchecked =>
        if addressable.length(chunk) > 0 then chunk #:: loaderStream
        else loaderStream

  // ─── unsafe direct buffer access ──────────────────────────────────────────
  //
  // These bypass `addressable.storageAddress`, which compiles to an
  // `invokeinterface` returning `Object` (the abstract `Storage` type member
  // erases that way) plus a `BoxesRuntime.unbox*` per access. For raw
  // byte/char scan loops this is a major hot-path tax. Callers who know the
  // concrete buffer type can `asInstanceOf` it once at the call site and let
  // the JIT keep the array reference in a register across the inner loop.
  inline def unsafeBuffer(using erased Unsafe): addressable.Storage = buffer
  inline def unsafePos(using erased Unsafe): Int = pos
  inline def unsafeWriteEnd(using erased Unsafe): Int = writeEnd

  // Bulk-advance without per-byte lineation tracking. Caller is responsible
  // for line/column updates if `lineation.active`. Intended for callers that
  // maintain a parser-local copy of `pos` for register-resident hot loops
  // and only push it back to the cursor at refill or mark/slice points.
  inline def unsafeAdvanceBy(n: Int)(using erased Unsafe): Unit = pos += n

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
    ${zephyrine.internal.consume('this, 'text, 'otherwise)}

  // ─── hold / mark / cue / grab / clone ─────────────────────────────────────

  // `mark` requires `using Cursor.Held` so callers can only mark inside a
  // hold block, where compaction cannot drop the marked region.
  inline def mark(using held: Cursor.Held): Cursor.Mark =
    Cursor.Mark(basePos + pos).tap: mark =>
      if lineationActive then
        recordMark(mark.absolute, Cursor.Offset(lineNo, columnNo).toLong)

  // Append `(mark, offset)` to the parallel `Long` buffers, growing geometrically
  // when full. Off the hot path's inline budget so `mark()` itself stays small.
  private def recordMark(mark: Long, offset: Long): Unit =
    val cap = marks.length

    if marksSize >= cap then
      val newCap = cap*2
      val nm = new Array[Long](newCap)
      val no = new Array[Long](newCap)
      System.arraycopy(marks, 0, nm, 0, marksSize)
      System.arraycopy(offsets, 0, no, 0, marksSize)
      marks   = nm
      offsets = no

    marks(marksSize)   = mark
    offsets(marksSize) = offset
    marksSize += 1

  // Linear scan from the most-recently appended mark backwards. `cue()` is the
  // only caller and only fires on backtrack, so this stays cold relative to
  // `mark()` itself.
  private def offsetForMark(mark: Long): Long =
    var i = marksSize - 1
    while i >= 0 && marks(i) != mark do i -= 1
    offsets(i)

  inline def offset(mark: Cursor.Mark): Cursor.Offset =
    Cursor.offsetFromLong(offsetForMark(mark.absolute))

  inline def cue(mark: Cursor.Mark): Unit =
    pos = (mark.absolute - basePos).toInt

    if lineationActive then
      val o = offset(mark)
      lineNo = o.line
      columnNo = o.column

  inline def hold[result](inline action: Cursor.Held ?=> result): result =
    val wasHeld = holdStart >= 0
    if !wasHeld then holdStart = pos

    action(using Cursor.shared).also:
      if !wasHeld then
        holdStart = -1
        marksSize = 0

  inline def grab(start: Cursor.Mark, end: Cursor.Mark): data =
    val len = (end.absolute - start.absolute).toInt

    if len <= 0 then addressable.empty
    else addressable.materialize(buffer, (start.absolute - basePos).toInt, len)

  // Zero-copy access to the live region between two marks. The lambda
  // receives the buffer, offset, and length; it must not retain the storage
  // reference, since compaction may invalidate it after the call returns.
  inline def slice[result](start: Cursor.Mark, end: Cursor.Mark)
    ( inline lambda: (addressable.Storage, Int, Int) => result )
  :   result =

    val off = (start.absolute - basePos).toInt
    val len = (end.absolute - start.absolute).toInt
    lambda(buffer, off, len)

  inline def clone(start: Cursor.Mark, end: Cursor.Mark)(target: addressable.Target): Unit =
    val len = (end.absolute - start.absolute).toInt

    if len > 0
    then addressable.cloneStorage(buffer, (start.absolute - basePos).toInt, len)(target)

  inline def take(inline otherwise: => data)(length: Int): data =
    hold:
      val start = mark
      var count = 0
      var truncated = false

      // Make each byte present *before* consuming it (refilling as needed), and
      // never refill after the last. The old `next()` form (advance-then-`more`,
      // `length` times) instead forced a blocking read for the byte *after* the
      // region — fatal on a live stream that sends one frame at a time — and,
      // checking availability only afterwards, it relied on the previous read
      // having pre-loaded the first byte; back-to-back `take`s (a WebSocket
      // frame's mask then payload, arriving a byte per chunk) would then park at
      // end-of-buffer and skip a byte. If the stream ends early, yield `otherwise`.
      while count < length && !truncated do
        if more then
          advance()
          count += 1
        else
          truncated = true

      if truncated then otherwise else grab(start, mark)
