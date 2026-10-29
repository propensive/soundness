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

import scala.annotation.targetName
import scala.caps
import scala.compiletime
import scala.compiletime.erasedValue

import anticipation.Data
import anticipation.Text
import contingency.*
import denominative.*
import fulminate.{Diagnostics, Hazard, m, panic}
import prepositional.*
import rudiments.*
import symbolism.*
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

  // A direct refill strategy: writes up to `space` elements straight into the cursor's
  // buffer at `offset`, returning the count written, `0` when nothing was granted (the
  // cursor retries), or `-1` at end-of-stream. Bypasses the `Loader` chunk protocol so a
  // pull endpoint's region can be transferred into the cursor in a single copy, instead
  // of materializing an intermediate chunk that `refill` then copies again. `block` is
  // the growth hint: the cursor guarantees at least one element of space and grows its
  // buffer towards `writeEnd + block` before each fill.
  trait Filler[storage]:
    def block: Int
    def fill(storage: storage, offset: Int, space: Int): Int

  // A filler whose source can also lend: `borrow` exposes the source's next region — its
  // storage, start and count — for the cursor to read in place, with no copy, which the
  // cursor does whenever nothing in its own buffer is live. The region stays valid until the
  // next `borrow` or `fill`, which releases it, so a source that recycles its blocks never
  // overwrites one the cursor is still reading. Returns the count, `0` when nothing was
  // granted (the cursor retries), or `-1` at end-of-stream.
  trait Lender[storage] extends Filler[storage]:
    def borrow(): Int
    def lentStorage: storage
    def lentStart: Int

  // A byte sequence to be found in bulk — a multipart boundary, an archive signature — with
  // its Horspool skip table computed once. `find` probes the last byte of each candidate
  // window and shifts by the table, so on data unlike the delimiter it visits about one byte
  // in `length` instead of every byte, and a partial match costs a few comparisons rather
  // than a lookahead. Searched for with `Cursor[Data]`'s `distance`.
  object Delimiter:
    def apply(bytes: Data): Delimiter = new Delimiter(bytes)

  final class Delimiter private (data: Data):
    // Read-only views: the delimiter's bytes are already frozen, and the shift table is frozen
    // once built, so the instance holds nothing mutable.
    val bytes = data
    val length: Int = bytes.length

    // For each byte value, how far the window shifts when that byte is the last one probed:
    // to align it with its last occurrence inside the delimiter, or past the window if none.
    // Chars, not ints: half a kilobyte of table for a delimiter of up to 65535 bytes.
    private val shifts: Array[Char]^{} =
      val table = Array.allocate[Char](256)
      table.fill(length.min(Char.MaxValue.toInt).toChar)
      var index = 0

      while index < length - 1 do
        table(bytes.readUnchecked(index) & 0xff) = (length - 1 - index).min(Char.MaxValue.toInt).toChar
        index += 1

      Array.freeze(table)

    // The index at which the delimiter first begins in `buffer`, wholly within `from` until
    // `end`, or `-1` if it does not.
    def find(buffer: scala.Array[Byte], from: Int, end: Int): Int =
      if length == 0 then from else
        val last = length - 1
        var index = from
        var found = -1

        // Horspool: `index + last < end` keeps every probe inside the window.
        while found < 0 && index + last < end do
          val probe = buffer(index + last)

          if probe == bytes.readUnchecked(last) then
            var offset = 0
            while offset < last && buffer(index + offset) == bytes.readUnchecked(offset) do offset += 1
            if offset == last then found = index

          if found < 0 then index += shifts.readUnchecked(probe & 0xff)

        found

  // ─── parser-local snapshot ────────────────────────────────────────────────
  //
  // The fields and the sync protocol a fast parser keeps beside its cursor. The cursor
  // remains the source of truth at refill, mark, slice and error points, but for the
  // per-element hot loops (`peek`, `advance`, `more`) the parser holds its own copy of the
  // buffer reference, read position and write end: plain fields the JIT keeps in registers
  // across tight scans, where re-reading them through the cursor's accessors would not.
  // Reads of `cells` are direct array loads — a `byte[]` or a `char[]`, never the erased
  // `Object` that `addressable.storageAddress` returns (see `unsafeBuffer`).
  //
  // Invariant: between `syncTo()` and `syncFrom()`, `pos` is the authoritative read
  // position and `cursor.unsafePos` may lag. Every cursor operation that consults the
  // position (a refill through `more`'s slow path, mark, slice, cue, error reporting) is
  // preceded by `syncTo()` and, if it may compact, grow or adopt a buffer, followed by
  // `syncFrom()`.
  //
  // An inline trait: its body is copied into each parser with `operand` substituted, so a
  // parser extending `Snapshot[Data, Byte]` gets a `byte[]` field and `baload`s, and one
  // extending `Snapshot[Text, Char]` a `char[]` field and `caload`s — the specialisation
  // an ordinary trait cannot express, since its members erase to `Object`. The parser is
  // the receiver: `this` in the copied body is the parser, so the fields are the parser's
  // own, exactly as when each parser wrote them out by hand. A consumer needs no language
  // flag: the body travels in zephyrine's TASTy.
  //
  // The cursor is held as a neutral `AnyRef` with an exclusive-view accessor, so the
  // parser's own methods are not hidden by an exclusive-typed field (the `Tel.Reader.parser0`
  // pattern), and so a pooled parser can re-point it at each reset. The view's capture set is
  // empty whatever the cursor's loader captured: the neutral carrier has already erased it
  // from the checker's view, so the parser is the audited point of confinement. (A capture
  // parameter in its place — `Snapshot[Data, Byte, ?]` — crashes the separation checker.)
  inline trait Snapshot[medium, operand] extends caps.Stateful:
    // [registry-lifetime] cast-erased AnyRef cursor handle in parser
    @caps.unsafe.untrackedCaptures
    protected var cursor0: AnyRef = null.asInstanceOf[AnyRef]

    protected inline def cursor: Cursor[medium, {}]^ = cursor0.asInstanceOf[Cursor[medium, {}]^]

    // [cursor-snapshot] parser's snapshot of cursor buffer
    @caps.unsafe.untrackedCaptures
    protected var cells:  scala.Array[operand] = null.asInstanceOf[scala.Array[operand]]

    protected var pos:    Int = 0
    protected var bufEnd: Int = 0

    // The absolute stream offset of `cells(0)`, re-anchored by every `syncFrom()`, so that
    // `position` survives compaction.
    protected var base: Long = 0L

    // The offset up to which the cursor's line and column are current: the hot loop
    // advances `pos` without lineation (`syncTo`), and the `reconcile…` methods catch the
    // cursor up from here at each capture point, and before a refill discards the elements.
    protected var lineationPos: Int = 0

    // The absolute stream offset of the next element: the position every error reports.
    // Public, as `more` is: a parser's driver asks both of a finished parse.
    inline def position: Long = base + pos

    // Push the parser's `pos` back to the cursor, before any cursor operation that
    // consults it. Zero-branch on the hot path; a tracked parse reconciles lineation
    // separately.
    protected update def syncTo(): Unit =
      cursor.unsafeAdvanceBy(pos - cursor.unsafePos(using Unsafe))(using Unsafe)

    // Refresh the snapshot from the cursor, after any cursor operation that may have
    // changed the buffer reference, the read position or the write end. Not inline: a
    // field assignment expanded from an inline method trips the separation checker's
    // provenance rule, and this is a slow path.
    protected update def syncFrom(): Unit =
      cells  = cursor.unsafeBuffer(using Unsafe).asInstanceOf[scala.Array[operand]]
      pos    = cursor.unsafePos(using Unsafe)
      bufEnd = cursor.unsafeWriteEnd(using Unsafe)
      base   = cursor.position.n0.toLong - pos
      lineationPos = pos
      resynced()

    // Called after every `syncFrom()`: a parser that derives anything from the buffer's scale
    // (a window's end relative to `cells(0)`, say) re-derives it here; one that does not
    // implements it as `()`. Abstract for the reason `reconcile` is.
    protected update def resynced(): Unit

    // Called before every refill, after `syncTo()`: a parser that tracks positions
    // reconciles the cursor's lineation here, while the consumed elements can still be
    // walked; one that does not implements it as `()`. Abstract, deliberately: the copied
    // body's calls to a member the receiver *overrides* bind to the trait's own copy of it,
    // not to the override (an overridden member is inlined under a mangled name, and the
    // trait's calls follow it there), so a hook with a default body would never reach the
    // parser's version. An abstract member has no copy to bind to.
    protected update def reconcile(): Unit

    // Whether another element is available, refilling from the source if the buffer is
    // exhausted. The slow path is out of line so the JIT keeps `pos < bufEnd` as one
    // register comparison in hot loops.
    inline update def more: Boolean = pos < bufEnd || moreSlow()

    // A snapshot may stand over a whole input with no cursor at all (`cells` is the input's
    // own array, `cursor0` null): then the input ends at `bufEnd`. A parser in that mode
    // must guard its own cursor operations the same way.
    protected update def moreSlow(): Boolean = if cursor0 == null then false else
      syncTo()
      reconcile()
      val hasMore = cursor.more
      // Re-anchored even at the end of the input: a compaction has changed the buffer's
      // scale, which a later `position` read must reflect.
      syncFrom()
      hasMore

    protected inline def peek: operand = cells(pos)
    protected inline update def advance(): Unit = pos += 1

    // Catch the cursor's line and column up to `pos`, one column per element.
    protected inline update def reconcileLineation(): Unit =
      inline erasedValue[operand] match
        case _: Byte => reconcileBytes(false)
        case _: Char => reconcileChars(false)

    // As `reconcileLineation`, counting UTF-8 code points: a continuation byte adds no
    // column.
    protected inline update def reconcileUtf8(): Unit = reconcileBytes(true)

    // As `reconcileLineation`, counting UTF-16 code points: a low surrogate adds no column.
    protected inline update def reconcileUtf16(): Unit = reconcileChars(true)

    private update def reconcileBytes(utf8: Boolean): Unit =
      val end = pos

      if lineationPos < end then
        val bytes = cells.asInstanceOf[scala.Array[Byte]]
        var newlines = 0
        var lastNewlineAt = -1
        var skipped = 0

        (lineationPos.z till end.z).each: index =>
          val byte = bytes(index.n0)

          if byte == '\n' then
            newlines += 1
            lastNewlineAt = index.n0
            skipped = 0
          else if utf8 && Utf8.continuation(byte) then skipped += 1

        bumpLineation(end, newlines, lastNewlineAt, skipped)

    private update def reconcileChars(utf16: Boolean): Unit =
      val end = pos

      if lineationPos < end then
        val chars = cells.asInstanceOf[scala.Array[Char]]
        var newlines = 0
        var lastNewlineAt = -1
        var skipped = 0

        (lineationPos.z till end.z).each: index =>
          val char = chars(index.n0)

          if char == '\n' then
            newlines += 1
            lastNewlineAt = index.n0
            skipped = 0
          else if utf16 && Character.isLowSurrogate(char) then skipped += 1

        bumpLineation(end, newlines, lastNewlineAt, skipped)

    // Lineation caught up to `end`: `newlines` lines, and the elements since the last
    // newline (or since `lineationPos`) as columns, less the `skipped` ones that add none.
    private update def bumpLineation(end: Int, newlines: Int, lastNewlineAt: Int, skipped: Int)
    :   Unit =

      if newlines > 0 then
        cursor.unsafeBumpLine(newlines)(using Unsafe)
        cursor.unsafeSetColumn(end - lastNewlineAt - 1 - skipped)(using Unsafe)
      else cursor.unsafeBumpColumn(end - lineationPos - skipped)(using Unsafe)

      lineationPos = end

  object Mark:
    final val Initial: Mark = 0L

    inline def apply(absolute: Long): Mark = absolute

    given comparable: Mark is Comparable = Comparable.long

  object Offset:
    inline def apply(line: Ordinal, column: Ordinal): Offset =
      (line.n0.toLong << 32) | (column.n0.toLong & 0xffffffffL)

    given comparable: Offset is Comparable = Comparable.long

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
  transparent inline def apply[data, cap^](inline load: () ->{cap} Optional[data])
    ( using addressable0: data is Addressable,
            lineation0:   Lineation by addressable0.Operand )
  :   Cursor[data, cap]^ =

    // The ascribed binding is load-bearing — see the iterator factory below. The seal
    // is the audited unsafety that keeps the class Unscoped; the loader's capabilities
    // remain visible in the `cap` type argument.
    val cursor: Cursor[data, cap]^ =
      new Cursor[data, cap]
        // [closure-capture] loader closure capture would collapse cursor to read-only
        ( caps.unsafe.unsafeAssumePure{ () => load() },
          Unset,
          DefaultCapacity,
          addressable0,
          lineation0,
          Unset )

    cursor


  // Build a Cursor pre-filled with a single chunk; the loader is a no-op.
  // This is the "Direct" mode: parsers that already have all the data in
  // memory pay no per-call refill cost.
  def apply[data](initial: data)
    ( using addressable0: data is Addressable,
            lineation0:   Lineation by addressable0.Operand )
  :   Cursor[data, {}]^ =

    // Zero-copy when the medium exposes its backing storage (an immutable
    // value's own array): the cursor borrows it as a `preset` and never
    // writes it — see the `preset` note on the class. Otherwise the block
    // is copied into a fresh buffer as before.
    val backing: Optional[AnyRef] =
      addressable0.backing(initial).asInstanceOf[Optional[AnyRef]]

    // The ascribed binding is load-bearing — see the iterator factory below.
    val cursor: Cursor[data, {}]^ =
      backing.lay
        ( new Cursor[data, {}]
            ( () => Unset,
              initial,
              addressable0.length(initial).max(1),
              addressable0,
              lineation0,
              Unset ) ): storage =>

        new Cursor[data, {}]
          ( () => Unset,
            Unset,
            addressable0.length(initial),
            addressable0,
            lineation0,
            Unset,
            preset = storage )

    cursor


  // Build a Cursor over a pull endpoint. When nothing of its own is live, the cursor reads
  // the stream's next region in place — no copy at all, with a credit of the ambient
  // `Buffering`'s window — and releases it at the next fill. Otherwise (a hold spans the
  // fill) each fill refills the stream with a credit bounded by the `Buffering` block size
  // and transfers the delivered region straight into the cursor's buffer — a single copy,
  // with no intermediate chunk allocation. The credit bounds how much any upstream stage
  // produces per fill, so memory stays bounded through a parse of an arbitrarily large
  // input. A copied region is read and skipped within the fill, before the stream can
  // refill again — the borrow discipline `Stream.lend` documents, applied at the one place
  // a cursor touches it.
  def apply[data](consume stream: (Stream[data] over Credit)^)
    ( using addressable0: data is Addressable,
            lineation0:   Lineation by addressable0.Operand,
            buffering:    Buffering )
  // `Stream over Credit` is not (yet) a declared capability, so the closure over it is
  // untracked and the cursor's capture set is empty; if the reactive kernel's types become
  // capabilities, this factory should be re-typed like the iterator one above.
  :   Cursor[data, {}]^ =

    // The ascribed binding is load-bearing — see the iterator factory below.
    val cursor: Cursor[data, {}]^ = new Cursor[data, {}]
      ( () => Unset,
        Unset,
        DefaultCapacity,
        addressable0,
        lineation0,
        // Sealed like the loader: the filler captures the adopted stream (consumed by
        // this factory — sole ownership), and Cursor's Unscoped classification cannot
        // hold a non-Unscoped capture.
        locally:
          val filler = new Lender[addressable0.Storage]:
            val block: Int = buffering.capacity(addressable0.substrate)
            private val window: Int = buffering.window(addressable0.substrate)

            // The region last lent, released at the next borrow or fill. Untracked: a filler
            // is reached only through its cursor, which alone calls it, from `refill`.
            // [abstract-storage] abstract Storage var in anonymous Lender
            @caps.unsafe.untrackedCaptures
            private var lentStorage0: addressable0.Storage =
              addressable0.allocate(0).asInstanceOf[addressable0.Storage]

            // [field-purity] plain Int var in anonymous Lender
            @caps.unsafe.untrackedCaptures
            private var lentStart0: Int = 0

            // [field-purity]
            @caps.unsafe.untrackedCaptures
            private var lent: Int = 0

            def lentStorage: addressable0.Storage = lentStorage0
            def lentStart: Int = lentStart0

            private def release(): Unit =
              if lent > 0 then
                stream.skip(lent)
                lent = 0

            def borrow(): Int =
              release()

              stream.refill(Credit(window)).lay(-1): count =>
                lentStorage0 = stream.unsafeStorage(using Unsafe).asInstanceOf[addressable0.Storage]
                lentStart0 = stream.start
                lent = count
                count

            def fill(storage: addressable0.Storage, offset: Int, space: Int): Int =
              release()

              stream.refill(Credit(space.min(block))).lay(-1): count =>
                val copied = count.min(space)
                val source = stream.unsafeStorage(using Unsafe).asInstanceOf[addressable0.Storage]
                addressable0.transfer(source, stream.start, storage, offset, copied)
                stream.skip(copied)
                copied

          // Sealed like the loader: the filler captures the adopted stream (consumed by
          // this factory — sole ownership), and Cursor's Unscoped classification cannot
          // hold a non-Unscoped capture.
          // [construction-fresh] fresh filler over consumed stream laundered at factory
          caps.unsafe.unsafeAssumePure(filler) )

    cursor


  // Adapts a lazy `Chain` of chunks to the loader API: the native sibling of the
  // Iterator factory below. Each load forces at most one cell; the mutable cursor
  // over the chain is confined to the sealed loader.
  def apply[data](chain: Chain[data])
    ( using addressable0: data is Addressable,
            lineation0:   Lineation by addressable0.Operand )
  :   Cursor[data, {}]^ =

    var remaining: Chain[data] = chain

    val load: () => Optional[data] = () => remaining match
      case chunk #:: tail =>
        remaining = tail
        chunk

      case _ =>
        Unset

    val cursor: Cursor[data, {}]^ =
      new Cursor[data, {}]
        // [closure-capture] loader closure over mutable chain cursor sealed
        ( caps.unsafe.unsafeAssumePure(load),
          Unset,
          DefaultCapacity,
          addressable0,
          lineation0,
          Unset )

    cursor

  // Backwards-compatible factory that adapts an Iterator to the loader API.
  // Lets the existing test suite cross-compile against Cursor.
  // The loader is SEALED (`unsafeAssumePure`, the CC-rollout fallback idiom): a loader
  // that visibly captures the iterator drags the call site's fresh iterator temp into the
  // instance's capture set, and the transparent expansion's exclusivity then collapses to
  // `^{}` at CC-only call sites — every update call through the cursor is rejected as
  // read-only. Re-track the iterator once that interaction is understood (needs a minimal
  // repro; the toy version works).
  def apply[data](iterator: Iterator[data]^)
    ( using addressable0: data is Addressable,
            lineation0:   Lineation by addressable0.Operand )
  :   Cursor[data, {}]^ =

    // The `cursor` binding's ascription is load-bearing: without it, the transparent
    // expansion's type pins the tracked-val refinement to expansion-local `$proxy`
    // singletons, and avoiding those collapses the fresh capture to `^{}` at CC-only
    // call sites — every update call through the cursor is then rejected as read-only.
    // The ascription costs the refinement (`Operand`/`Target` go abstract on cursors
    // from this factory); per-medium extensions (`peek`, `expect`, `buffer`) are
    // unaffected, and direct `clone`/`datum` users cast at the call site.
    val cursor: Cursor[data, {}]^ =
      new Cursor[data, {}]
        // [closure-capture] iterator-capturing loader collapses cursor to read-only
        ( caps.unsafe.unsafeAssumePure
            ( () => iterator.seek(_ => true)(Unset)(identity(_)) ),
          Unset,
          DefaultCapacity,
          addressable0,
          lineation0,
          Unset )

    cursor

  // Safe, allocation-free single-element peek. Returns `Datum.End` when the
  // cursor is finished; otherwise the current byte (unsigned, `0..255`) or
  // char wrapped as a `Datum`. May block (via `finished` → refill) to fetch
  // the operand being inspected — acceptable, since that operand must exist
  // for any match — but never reads past it. The `Datum` opaque type (backed by `Int`)
  // is deliberately distinct from raw `Int` so that arithmetic or
  // `Byte`/`Char` confusion can't happen silently — comparison with the
  // expected literal is via the dedicated `Datum.==` overloads, which still
  // compile to a single `int == int`. One definition serves both media,
  // selected at compile time: a `Data` cursor by its medium, and a `Text`
  // cursor — whose opaque type argument an inline match cannot see — by the
  // `Operand` of the medium's `Addressable`, summoned at the call site where
  // it is the concrete `text` instance. (The medium comes first because a
  // call site inside a `Stream` body sees the stream's own `addressable`
  // member, whose unrefined type would not reduce.)
  extension [data, cap^](cursor: Cursor[data, cap])(using addressable: data is Addressable)
    inline def peek: Datum =
      if cursor.finished then Datum.End
      else inline erasedValue[data] match
        case _: Data =>
          val buffer = cursor.unsafeBuffer(using Unsafe).asInstanceOf[scala.Array[Byte]]
          Datum.fromRaw(buffer(cursor.unsafePos(using Unsafe)) & 0xff)

        case _ => inline erasedValue[addressable.Operand] match
          case _: Char =>
            val buffer = cursor.unsafeBuffer(using Unsafe).asInstanceOf[scala.Array[Char]]
            Datum.fromRaw(buffer(cursor.unsafePos(using Unsafe)).toInt)

          case _ => compiletime.error("peek is defined only for byte and char cursors")

    // Match the cursor's current operand against `target`. On a match, advance
    // past it; on a mismatch (or EOF), raise `failure` via the ambient
    // `Tactic`. Replaces the hand-rolled `if cursor.peek != X then raise(…);
    // cursor.next()` pair that every header / framer parser was writing. The
    // target is `Char` for both media so callers don't need `'X'.toByte`
    // on `Cursor[Data]`; for ASCII targets the `Datum`-vs-`Char` comparison
    // compiles to a single primitive `int == int`.
    //
    // The match consumes with `advance()`, not `next()`: `peek` has already
    // confirmed the operand is present, and `next()`'s trailing `more` would
    // force a blocking refill for the byte *after* the match — fatal when
    // `expect` consumes the final delimiter of a message on a keep-alive
    // stream whose peer sends nothing more until we reply (issue #1301).
    inline def expect[error <: Hazard](target: Char)
      ( inline failure: Diagnostics ?=> error )
      ( using Tactic[error] )
    :   Unit =

      if cursor.peek == target then cursor.advance() else raise(failure)

  // Run `action` inside a hold and always restore the cursor position to
  // where it was on entry, regardless of the result — i.e. a non-consuming
  // lookahead. Marks taken inside the action are available via the implicit
  // `Cursor.Held`; the outer cursor is `cue`d back on exit so subsequent
  // processing sees the same bytes. Replaces the explicit
  // `hold { val mk = mark; … cue(mk); result }` idiom that parsers like
  // Multipart's boundary detector and Zeppelin's ZIP signature scanner
  // were writing.
  extension [data, cap^](cursor: Cursor[data, cap])
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
  extension [cap^](cursor: Cursor[Data, cap])
    @targetName("dataBuffer")
    inline def unsafeDataBuffer(using erased unsafe: Unsafe): scala.Array[Byte] =
      cursor.unsafeBuffer(using Unsafe).asInstanceOf[scala.Array[Byte]]

    // How far ahead of the cursor `delimiter` first begins, wholly within the buffered
    // region, or `-1` if it does not. The buffer is searched in bulk; the cursor is not
    // moved and nothing is refilled, so unlike `seek` this cannot block on a live stream
    // (see the hazard note). A scan that finds nothing should consume all but the last
    // `delimiter.length - 1` buffered bytes — a possible prefix of a delimiter straddling
    // the refill — and search again once more has arrived.
    inline def distance(delimiter: Cursor.Delimiter): Int =
      val position = cursor.unsafePos(using Unsafe)

      val index =
        delimiter.find
          ( cursor.unsafeDataBuffer(using Unsafe), position, cursor.unsafeWriteEnd(using Unsafe) )

      if index < 0 then -1 else index - position

  extension [cap^](cursor: Cursor[Text, cap])
    @targetName("textBuffer")
    inline def unsafeTextBuffer(using erased unsafe: Unsafe): scala.Array[Char] =
      cursor.unsafeBuffer(using Unsafe).asInstanceOf[scala.Array[Char]]

// BLOCKING-LOOKAHEAD HAZARD (issue #1301): every operation that asks whether data exists —
// `more`, `finished`, `peek`, and `next()` (whose trailing `more` runs after consuming) —
// refills from the source once the buffer drains, and a refill BLOCKS until the source
// yields data or EOF. On finite or EOF-terminated input that is always prompt and harmless.
// But a parser that stops at a message boundary on a LIVE stream with no EOF between
// messages (HTTP/1.1 keep-alive, any half-duplex request/response protocol) must never
// look past the message's final operand: the peer sends nothing more until we reply, so
// the refill deadlocks. Such parsers must consume the final delimiter with `peek` (or
// `expect`) then `advance()` — which never refills — and bound reads with `available`,
// `take`, or an explicit length. `expect` and `take` are internally safe; the exemplar
// call site is telekinesis's `Http.Request.parseHead`.
//
// `cap^` is the capture set of the `load` thunk: `{}` for an in-memory cursor (the loader is a
// no-op), or the capabilities a streaming loader draws from (e.g. a socket). Tracking it as an
// explicit capture parameter — rather than letting capture checking infer a blanket self-capture —
// keeps a pure cursor's type free of a `load`-bearing refinement, so its path-dependent members
// (`addressable.Storage`/`Operand`) still resolve through any reference.
// A cursor is a stateful capability: a bare `Cursor` reference is read-only (position and
// lineation queries only), while `Cursor[...]^` grants the exclusive access that the
// mutating navigation methods (`update`-classified) require. Separation checking — enabled
// in this unit — verifies the classification; consumers need only capture checking.
//
// The class extends `Mutable` (Unscoped-classified), which is what lets consumers hold a
// cursor in an exclusive field (e.g. jacinta's pooled parser) — scoped capabilities may
// not flow into longer-lived locations. Unscoped requires that the class captures nothing
// non-Unscoped, so the `load` field is PURE and the loader factory seals capturing
// loaders at construction (the audited point). Confinement is still enforced at the API:
// the loader's capabilities appear in the `cap` type argument, so a cursor over a scoped
// loader cannot leave the loader's scope (verified by probe: escaping `withFile` with a
// file-reading cursor is rejected via the type argument).
final class Cursor[data, cap^]
  (             load:        () -> Optional[data],
                initial:     Optional[data],
                initialSize: Int,
    tracked val addressable: data is Addressable,
    tracked val lineation:   Lineation by addressable.Operand,
                filler:      Optional[Cursor.Filler[addressable.Storage]],
                preset:      Optional[AnyRef] = Unset )
extends caps.Mutable:

  // ─── state ────────────────────────────────────────────────────────────────
  // A single contiguous buffer holds all currently-live data. `pos` is the
  // hot-path read index into `buffer` (an `Int`, so `peek` lowers to one
  // array access). `writeEnd` is the count of valid bytes/chars in the
  // buffer. `basePos` is the absolute stream position of `buffer(0)`, used
  // to translate buffer indices to/from `Mark` values that survive
  // compaction. `holdStart` is the buffer index at the start of the held
  // region (or -1 when no hold is active); compaction may not advance past
  // it. `ended` becomes true when the loader has returned `Unset`.

  // A `preset` cursor BORROWS its storage (a neutral carrier for
  // `addressable.Storage`, typically an immutable value's backing array) and
  // is `static`: it never writes the buffer — no seed copy, and `refill`
  // neither compacts nor pulls — so the borrow is sound despite the
  // buffer's writable type. The reads-only discipline is this class's; the
  // factory cast below is the audited point.
  private val static: Boolean = preset.present

  // `owned` is the buffer this cursor allocates and writes; `buffer` is the storage it is
  // currently reading: `owned`, or — after a fill made while nothing of its own was live — a
  // region its source lent (a `Data` chunk's own array, a `Lender`'s region), read in place
  // and never written, which `borrowed` records. Untracked, with cast-erased assignments:
  // both are reached only through this (exclusive) cursor.
  // [abstract-storage] abstract Storage owned buffer var
  @caps.unsafe.untrackedCaptures
  private var owned:     addressable.Storage =
    preset.lay(addressable.allocate(initialSize).asInstanceOf[addressable.Storage]): storage =>
      storage.asInstanceOf[addressable.Storage]

  // [abstract-storage] abstract Storage current buffer var
  @caps.unsafe.untrackedCaptures
  private var buffer:    addressable.Storage = owned

  private var borrowed:  Boolean = false

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

  // Cached for the same reason: `seek` selects its scan by the medium once per call, not by
  // an interface dispatch per element.
  private val substrate: Substrate = addressable.substrate

  // Parallel arrays (not deques) of unboxed `Long`-typed `Mark` and `Offset`
  // values for the currently-held region. Using `Array[Long]` rather than
  // `ArrayDeque[Mark]`/`ArrayDeque[Offset]` avoids two `java.lang.Long` boxes
  // per `mark()` call — a meaningful saving on parser hot paths that mark
  // every token boundary.
  private var marks:     scala.Array[Long]^ = new scala.Array[Long](16)
  private var offsets:   scala.Array[Long]^ = new scala.Array[Long](16)
  private var marksSize: Int = 0

  private var lineNo:   Ordinal = Prim
  private var columnNo: Ordinal = Prim

  // Seed the buffer: prefer an explicit pre-fill chunk; otherwise pull from
  // the loader until we have non-empty data or hit EOF. Matches the existing
  // `Cursor.apply` behaviour, which pre-loads the first block so `datum` is
  // valid before any `next()` call.
  locally:
    if static then writeEnd = initialSize
    else
      initial.let: chunk =>
        val len = addressable.length(chunk)

        if len > 0 then
          if len > addressable.storageSize(owned) then
            owned = addressable.allocate(len).asInstanceOf[addressable.Storage]
            buffer = owned

          addressable.copyChunk(chunk, 0, buffer, 0, len)
          writeEnd = len

    if writeEnd == 0 then refill()

  // Re-point a static cursor at another borrowed region, resetting every position: the
  // one-cursor-per-connection idiom, where a server parses each request head straight out
  // of its accumulator without allocating a cursor (two mark arrays and a dozen fields) or
  // copying the head out. Only a static cursor may be re-pointed: it never writes its
  // buffer, which is what makes borrowing the region sound. The region must stay unwritten
  // until the parse that reads it has finished. The storage arrives as the same neutral
  // carrier `preset` uses (a caller holding a `Cursor[data, {}]^` cannot name
  // `addressable.Storage`); the cast is the same audited one as the constructor's.
  update def repoint(storage: AnyRef, size: Int): Unit =
    if !static then panic(m"only a static (preset) cursor may be re-pointed")
    buffer = storage.asInstanceOf[addressable.Storage]
    pos = 0
    writeEnd = size
    basePos = 0L
    holdStart = -1
    ended = false
    marksSize = 0
    lineNo = Prim
    columnNo = Prim

  // ─── slow path: refill ────────────────────────────────────────────────────
  // Kept as a regular (non-inline) method so the slow path's bytecode bloat
  // doesn't push `next()` past the JIT's inline budgets. Mirrors the rationale
  // for the original `Cursor.forward()`.
  private update def refill(): Unit =
    // A static cursor holds everything it will ever hold, in borrowed
    // storage: nothing to pull, and compaction (which writes the buffer)
    // must not run.
    if static then ended = true
    else if !ended then
      // Compact: drop any data that is no longer reachable. Outside a hold,
      // everything before `pos` is dead; inside a hold, everything before
      // `holdStart` is dead. `keep` is capped at `writeEnd` because `pos` may
      // sit one past the last loaded byte after a `next()` that consumed the
      // tail of the buffer. What is live moves to the start of the owned
      // buffer — out of a borrowed region too, whose loan this ends.
      val rawKeep = if holdStart >= 0 then holdStart else pos
      val keep = rawKeep.min(writeEnd)
      val live = writeEnd - keep

      if keep > 0 || borrowed then
        if borrowed && live > addressable.storageSize(owned) then
          owned = addressable.allocate(live).asInstanceOf[addressable.Storage]

        if live > 0 then addressable.transfer(buffer, keep, owned, 0, live)
        buffer = owned
        borrowed = false
        basePos += keep
        pos -= keep
        writeEnd = live
        if holdStart >= 0 then holdStart = 0

      // Pull until we either receive non-empty data or hit EOF: directly into the
      // buffer when a `Filler` was provided (single copy), otherwise through the
      // chunk-materializing `Loader` protocol — or, when nothing is live, in place:
      // the buffer is re-pointed at a region the source lends, with no copy at all.
      filler.lay(pullChunks())(pullDirect(_))

  // Re-point the buffer at a lent region. Nothing is buffered (`writeEnd == 0`), so
  // `basePos` is the absolute position of the region's first element, which lies at
  // `start` in `storage`.
  private update def adopt(storage: addressable.Storage, start: Int, count: Int): Unit =
    buffer = storage
    borrowed = true
    basePos -= start
    pos = start
    writeEnd = start + count
    if holdStart >= 0 then holdStart = start

  // The direct path: a region lent by a `Lender` is adopted in place; otherwise grow
  // towards a full block of space, then let the filler transfer straight into the buffer
  // at `writeEnd` — the single-copy path.
  private update def pullDirect(filler: Cursor.Filler[addressable.Storage]): Unit =
    var loaded = false

    filler match
      case lender: Cursor.Lender[addressable.Storage @unchecked] if writeEnd == 0 =>
        while !loaded && !ended do
          val count = lender.borrow()

          if count < 0 then ended = true
          else if count > 0 then
            adopt(lender.lentStorage, lender.lentStart, count)
            loaded = true

      case _ =>
        while !loaded && !ended do
          ensureCapacity(writeEnd + filler.block)
          val space = addressable.storageSize(buffer) - writeEnd
          val count = filler.fill(buffer, writeEnd, space)

          if count < 0 then ended = true
          else if count > 0 then
            writeEnd += count
            loaded = true

  private update def pullChunks(): Unit =
    var loaded = false

    while !loaded && !ended do
      load().lay({ ended = true }): data =>
        val len = addressable.length(data)

        if len > 0 then
          // A chunk exposing its own immutable backing is read in place when nothing is
          // buffered; otherwise it is copied in.
          val backing: Optional[addressable.Storage] =
            if writeEnd == 0
            then addressable.backing(data).asInstanceOf[Optional[addressable.Storage]]
            else Unset

          backing.lay:
            ensureCapacity(writeEnd + len)
            addressable.copyChunk(data, 0, buffer, writeEnd, len)
            writeEnd += len

          . apply: storage => adopt(storage, 0, len)

          loaded = true

  // Grows the owned buffer, which is the one being read: `refill` has already moved any
  // live data out of a borrowed region before it pulls.
  private update def ensureCapacity(needed: Int): Unit =
    val cap = addressable.storageSize(buffer)

    if needed > cap then
      var newCap = if cap == 0 then needed.max(16) else cap
      while newCap < needed do newCap *= 2
      val newBuf = addressable.allocate(newCap)
      if writeEnd > 0 then addressable.transfer(buffer, 0, newBuf, 0, writeEnd)
      owned = newBuf.asInstanceOf[addressable.Storage]
      buffer = owned

  // ─── core navigation ──────────────────────────────────────────────────────

  // `advance()` is unchecked (it just increments `pos`); it's safe to leave
  // `pos` past `writeEnd` because the next `more` / `peek` / `next()` call
  // forces a refill before the buffer is read. This mirrors the existing
  // `Cursor.forward` rationale and keeps the inner loop one instruction
  // tighter — important for raw byte-scan parsers like Merino where the
  // hot loop is `while more && {peek-test} do advance()`.
  //
  // Deferring the refill also makes `advance()` the boundary-safety
  // primitive: it is the only way to consume an operand *without* touching
  // the source. A parser stopping at a message boundary on a live stream
  // (see the hazard note on the class) must consume its final delimiter
  // with `peek`-then-`advance()` — as `expect` and `take` do — never with
  // `next()`.
  inline update def advance(): Unit =
    if lineationActive then
      val operand = addressable.storageAddress(buffer, pos)
      pos += 1

      columnNo =
        if !lineation.track(operand) then columnNo.next else { lineNo = lineNo.next; Prim }
    else
      pos += 1

  // Variant of `advance` for callers that have just read the current operand
  // (e.g. via `unsafeBuffer(pos)` in a tight scan loop). Reuses the supplied
  // `operand` instead of re-loading it from the buffer for lineation tracking.
  inline update def unsafeAdvanceWith(operand: addressable.Operand)(using erased unsafe: Unsafe)
  :   Unit =

    pos += 1

    if lineationActive then
      columnNo =
        if !lineation.track(operand) then columnNo.next else { lineNo = lineNo.next; Prim }

  // Bulk-advance primitives. Allow a caller (typically a parser running a
  // register-resident scan loop) to consume `n` characters without paying the
  // per-character lineation update inside `advance`/`unsafeAdvanceWith`, then
  // reconcile lineation in one shot. The caller is responsible for tracking
  // newlines while it scans.
  inline update def unsafeBumpPos(by: Int)(using erased unsafe: Unsafe): Unit = pos += by

  // Increase the line counter by `by`, leaving the column counter untouched.
  inline update def unsafeBumpLine(by: Int)(using erased unsafe: Unsafe): Unit =
    lineNo = denominative.Ordinal.zerary(lineNo.n0 + by)

  // Increase the column counter by `by`. Must not be called across newlines.
  inline update def unsafeBumpColumn(by: Int)(using erased unsafe: Unsafe): Unit =
    columnNo = denominative.Ordinal.zerary(columnNo.n0 + by)

  // Set the column counter directly. Used after a bulk advance over a range
  // that contained at least one newline, to set the column to the offset
  // since the most-recent newline.
  inline update def unsafeSetColumn(value: Int)(using erased unsafe: Unsafe): Unit =
    columnNo = denominative.Ordinal.zerary(value)

  // `next()` is `advance(); more`, so it returns `true` while more data is
  // available and `false` when the stream is exhausted. The trailing `more`
  // means it blocks on a refill once the buffer drains — so never use it to
  // consume the last operand of a message on a live stream (see the hazard
  // note on the class); consume with `peek`-then-`advance()` instead.
  inline update def next(): Boolean =
    advance()
    more

  // Hot path. The first comparison short-circuits when there's still data
  // in the buffer; only when the buffer is drained do we pay for the slow
  // path. `moreSlow` is non-inline so the inline budget for `more` stays
  // small enough that callers (parser hot loops) get tight bytecode.
  inline update def more: Boolean = pos < writeEnd || moreSlow()

  private update def moreSlow(): Boolean =
    !ended && { refill(); pos < writeEnd }

  inline update def finished: Boolean = !more
  inline def position: Ordinal = (basePos + pos).toInt.z
  inline def available: Int = writeEnd - pos
  inline def line: Ordinal = lineNo
  inline def column: Ordinal = columnNo

  // Cached active-flag for the configured lineation. Hot-loop callers should
  // use this in preference to `lineation.active` to avoid the per-call
  // interface dispatch that the abstract `Lineation` member would imply.
  inline def lineActive: Boolean = lineationActive

  // Chain of all unconsumed data from the current position onwards. Yields
  // the buffered tail first (one chunk materialised from `pos` to `writeEnd`),
  // then drains the loader, returning chunks as it goes. Caller-driven, so a
  // streaming consumer pays nothing until it pulls.
  update def remainder: Chain[data] =
    val tailLen = writeEnd - pos

    val tail: data =
      if tailLen <= 0 then addressable.empty else addressable.materialize(buffer, pos, tailLen)

    pos = writeEnd

    // Both branches must stay lazy. `loaderStream` performs a blocking `load()`
    // on its first force, so calling it eagerly would read (and potentially
    // block on) the underlying source before the caller has pulled a single
    // byte — breaking the "pays nothing until it pulls" contract above and
    // deadlocking protocols that withhold data until they get a reply (e.g. a
    // WebSocket upgrade, whose body is the post-handshake frame stream the peer
    // only sends after our `101`). `#::` keeps the non-empty branch lazy; the
    // empty branch must defer the call explicitly.
    if tailLen > 0 then tail #:: loaderStream else Chain.defer(loaderStream)

  private update def loaderStream: Chain[data] =
    if ended then Chain.empty else load() match
      case chunk: data @unchecked =>
        if addressable.length(chunk) > 0 then chunk #:: loaderStream else loaderStream

      case _ =>
        ended = true
        Chain.empty

  // ─── unsafe direct buffer access ──────────────────────────────────────────
  //
  // These bypass `addressable.storageAddress`, which compiles to an
  // `invokeinterface` returning `Object` (the abstract `Storage` type member
  // erases that way) plus a `BoxesRuntime.unbox*` per access. For raw
  // byte/char scan loops this is a major hot-path tax. Callers who know the
  // concrete buffer type can `asInstanceOf` it once at the call site and let
  // the JIT keep the array reference in a register across the inner loop.
  inline def unsafeBuffer(using erased unsafe: Unsafe): addressable.Storage = buffer
  inline def unsafePos(using erased unsafe: Unsafe): Int = pos
  inline def unsafeWriteEnd(using erased unsafe: Unsafe): Int = writeEnd

  // Bounds-safe view of the cursor's readable region, elements `pos until writeEnd`: the
  // region and its branded interval are lent to `lambda`, so every index a consumer can form
  // is in range (see `zephyrine.Region.scala`) — the preferred boundary for scan loops that
  // don't cache the buffer across cursor operations. Valid only until the next operation that
  // may refill, compact or grow the buffer — the same single-owner discipline as
  // `unsafeBuffer`, with the bounds arithmetic taken away. Parsers that snapshot the buffer
  // into fields for register-resident hot loops (the `unsafeBuffer` protocol) remain trusted
  // kernels behind `Unsafe`.
  inline def lend[result]
    ( inline lambda: (region: Region[data]) => (Interval in region.type) => result )
  :   result =

    Region.over[data, result](using addressable)(buffer, pos, writeEnd)(lambda)

  // Bulk-advance without per-byte lineation tracking. Caller is responsible
  // for line/column updates if `lineation.active`. Intended for callers that
  // maintain a parser-local copy of `pos` for register-resident hot loops
  // and only push it back to the cursor at refill or mark/slice points.
  inline update def unsafeAdvanceBy(n: Int)(using erased unsafe: Unsafe): Unit = pos += n

  // ─── current element ──────────────────────────────────────────────────────

  inline def unsafeDatum(using erased unsafe: Unsafe): addressable.Operand =
    addressable.storageAddress(buffer, pos)

  inline update def lay[result]
    ( inline otherwise: => result )
    ( inline lambda: addressable.Operand => result )
  :   result =

    if !finished then lambda(addressable.storageAddress(buffer, pos)) else otherwise

  inline update def let(inline lambda: addressable.Operand => Unit): Unit =
    if !finished then lambda(addressable.storageAddress(buffer, pos))

  inline update def process[result](inline lambda: addressable.Operand => result): Unit =
    if !finished then lambda(addressable.storageAddress(buffer, pos))

  // ─── search primitives ────────────────────────────────────────────────────

  // Entered via `more`, not unconditionally: the previous operation may have
  // consumed the buffer's last operand with a deferred-refill `advance()`
  // (directly, or via `expect`/`take`), leaving `pos` at `writeEnd` with the
  // current operand not yet loaded. Reading `datum` before a refill would
  // then see stale storage. Stopping on a match performs no further refill,
  // so `seek` never reads past the operand it finds.
  // The target arrives through the erased `Operand` (every caller's `cursor.addressable.Operand`
  // is abstract, so callers write `Lf.toByte.asInstanceOf[cursor.addressable.Operand]` — a
  // boxed `Byte`, whatever the medium), so it is unboxed once here and each element is
  // compared at the primitive type from the concrete array; through
  // `addressable.storageAddress` the comparison would box every element (see `unsafeBuffer`).
  // A target of the other primitive type compares as the boxed equality it replaces did: by
  // numeric value, so a byte matches the ASCII char of the same value and nothing else.
  // Boxed media compare as references.
  update def seek(target: addressable.Operand): Boolean = substrate match
    case Substrate.Bytes => (target: Any) match
      case byte: Byte => seekByte(byte)
      case char: Char => char <= 127 && seekByte(char.toByte)
      case other      => seekBoxed(target)

    case Substrate.Chars => (target: Any) match
      case char: Char => seekChar(char)
      case byte: Byte => byte >= 0 && seekChar(byte.toChar)
      case other      => seekBoxed(target)

    case Substrate.Boxes => seekBoxed(target)

  private update def seekByte(target: Byte): Boolean =
    var found = false
    var continue = more

    while continue do
      found = buffer.asInstanceOf[scala.Array[Byte]](pos) == target
      continue = !found && next()

    found

  private update def seekChar(target: Char): Boolean =
    var found = false
    var continue = more

    while continue do
      found = buffer.asInstanceOf[scala.Array[Char]](pos) == target
      continue = !found && next()

    found

  private update def seekBoxed(target: addressable.Operand): Boolean =
    var found = false
    var continue = more

    while continue do
      found = unsafeDatum(using Unsafe) == target
      continue = !found && next()

    found

  inline update def consume(inline otherwise: => Unit)(inline text: String): Unit =
    ${zephyrine.internal.consume('this, 'text, 'otherwise)}

  // ─── hold / mark / cue / grab / clone ─────────────────────────────────────

  // `mark` requires `using Cursor.Held` so callers can only mark inside a
  // hold block, where compaction cannot drop the marked region.
  inline update def mark(using held: Cursor.Held): Cursor.Mark =
    Cursor.Mark(basePos + pos).tap: mark =>
      if lineationActive then recordMark(mark.absolute, Cursor.Offset(lineNo, columnNo).toLong)

  // Append `(mark, offset)` to the parallel `Long` buffers, growing geometrically
  // when full. Off the hot path's inline budget so `mark()` itself stays small.
  private update def recordMark(mark: Long, offset: Long): Unit =
    val cap = marks.length

    if marksSize >= cap then
      val newCap = cap*2
      val nm = new scala.Array[Long](newCap)
      val no = new scala.Array[Long](newCap)
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

  inline update def cue(mark: Cursor.Mark): Unit =
    pos = (mark.absolute - basePos).toInt

    if lineationActive then
      val o = offset(mark)
      lineNo = o.line
      columnNo = o.column

  inline update def hold[result](inline action: Cursor.Held ?=> result): result =
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

    if len > 0 then addressable.cloneStorage(buffer, (start.absolute - basePos).toInt, len)(target)

  inline update def take(inline otherwise: => data)(length: Int): data =
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
