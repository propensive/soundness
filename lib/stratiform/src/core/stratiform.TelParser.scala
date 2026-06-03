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
package stratiform

import scala.language.unsafeNulls

import java.lang as jl
import java.nio.charset.StandardCharsets

import anticipation.*
import contingency.*
import denominative.*
import fulminate.*
import gossamer.*
import rudiments.*
import vacuous.*
import zephyrine.*

import TelError.Reason

// Streaming TEL parser. Consumes a `Cursor[Data]` on demand — no upfront
// String materialisation, no pre-split line array. Implements both the
// schemaless and §19.5 schema-aware E107 recovery paths.
//
// Hot-path optimisations:
//   - parser-local `bytes`/`pos`/`bufEnd` snapshot of the cursor's buffer
//     keeps the inner-loop reads in registers; sync to / from the cursor
//     only at refill, mark, or error points;
//   - a single mutable `LineHead` look-ahead record threaded through
//     recursive descent — no per-line allocation;
//   - 64-slot two-Long byte-fingerprint keyword cache (allocation-free on
//     hit), one `cursor.grab` materialisation on miss;
//   - source / literal atom payloads built into a shared `StringBuilder`.

object TelParser:

  // Per-thread cached parser. The parser's mutable state (scratch buffers,
  // keyword cache, StringBuilder, ArrayBuffer of ancestors, etc.) is
  // expensive to allocate fresh on every call; reusing the same parser
  // across calls on a thread amortises those allocations to ~one per
  // thread-lifetime. `reset()` re-initialises all per-parse state at the
  // start of each call. The atom arena is REPLACED (not reused) so any
  // `Tel.Atom.Inline` instances returned from prior parses keep their
  // backing bytes valid; the previous arena array stays alive via those
  // Inlines and gets GC'd once all references to the prior Document are
  // released.
  private val cached: ThreadLocal[TelParser] =
    ThreadLocal.withInitial(() => new TelParser())

  def parse(cursor: Cursor[Data]): Tel.Document raises TelError =
    val p = cached.get.nn
    p.reset(cursor, Unset)
    p.parse()

  def parse(cursor: Cursor[Data], schema: Tels): Tel.Document raises TelError =
    val p = cached.get.nn
    p.reset(cursor, schema: Optional[Tels])
    p.parse()

  // Convenience overloads for callers that already have the whole input as
  // a single byte chunk. The cursor is constructed with `untrackedData`
  // lineation: TelParser tracks the 1-indexed source line via its own
  // `lineNo` field (bumped at every LF-consumption point) and never reads
  // `cursor.line` / `cursor.column`. Driving cursor's lineation would
  // record (line, column) on every `cursor.mark` call — for a parser that
  // marks O(atoms + keywords + remarks + payloads) times per parse this is
  // significant wasted work, since the recorded offsets are never consulted.
  def parse(input: Data): Tel.Document raises TelError =
    import zephyrine.Lineation.untrackedData
    val p = cached.get.nn
    p.reset(Cursor[Data](input), Unset)
    p.parse()

  def parse(input: Data, schema: Tels): Tel.Document raises TelError =
    import zephyrine.Lineation.untrackedData
    val p = cached.get.nn
    p.reset(Cursor[Data](input), schema: Optional[Tels])
    p.parse()

  private final val SP: Byte = 0x20
  private final val LF: Byte = 0x0A
  private final val CR: Byte = 0x0D
  private final val BOM0: Byte = 0xEF.toByte
  private final val BOM1: Byte = 0xBB.toByte
  private final val BOM2: Byte = 0xBF.toByte

  // ── SWAR (SIMD-within-a-register) byte scan helpers ──────────────────────
  // The inner byte-scan loops in the parser read a `Long` of 8 bytes at a
  // time from the buffer and use the classic "haszero" trick to detect a
  // target byte across all 8 lanes in two arithmetic ops. On long content
  // runs (literal payloads, source-atom lines, inline atoms) this replaces
  // an 8-iteration byte loop with one Long load plus a couple of bitwise
  // operations.

  // `classOf[Array[Long]]` resolves to `java.lang.Object` under some Scala 3
  // compilation paths (notably macro-expansion contexts) — and
  // `byteArrayViewVarHandle` rejects that. Get `long[].class` directly from
  // a runtime instance instead.
  private val longView: java.lang.invoke.VarHandle =
    java.lang.invoke.MethodHandles.byteArrayViewVarHandle
     ( (new Array[Long](0)).getClass.nn, java.nio.ByteOrder.LITTLE_ENDIAN )

  private final val OnesMask:     Long = 0x0101010101010101L
  private final val HighBitsMask: Long = 0x8080808080808080L

  // The byte `b` replicated across all 8 lanes of a Long.
  private inline def replicate(b: Byte): Long = (b & 0xFFL) * OnesMask

  // Pre-computed replications for the constant scan targets.
  private final val SpRepl: Long = (SP & 0xFFL) * OnesMask
  private final val LfRepl: Long = (LF & 0xFFL) * OnesMask
  private final val CrRepl: Long = (CR & 0xFFL) * OnesMask

  // Per-lane "is this byte zero?" mask: each 0x80 marks a zero byte in `v`.
  private inline def haszero(v: Long): Long =
    (v - OnesMask) & ~v & HighBitsMask

  // Per-lane "does this byte equal the target?" mask.
  private inline def matchByte(v: Long, replicated: Long): Long =
    haszero(v ^ replicated)

  // Carries the look-ahead state for the next unconsumed line. Parsed once
  // by `fillHead`, then consulted by recursive-descent functions to decide
  // whether to continue at the current indent, descend, ascend, or stop.
  // Reused (mutated in place) across every line — single allocation per
  // parser instance.
  //
  // `startLine` is the 1-indexed source line number of this line. We store
  // the line number directly (not a byte position) because the streaming
  // parser uses narrow per-leaf holds: once a hold closes the cursor may
  // compact the buffer, so a byte offset recorded earlier can no longer
  // be used to compute a line number later. Line numbers are bumped
  // incrementally at every LF-consumption point (consumeLineEnding plus
  // the two raw LF advances in parseLiteralAtom).
  private final class LineHead:
    var leadingSpaces: Int = 0
    var indentLevels:  Int = 0      // (leadingSpaces - margin) / 2 or -1
    var blank:         Boolean = false
    var eof:           Boolean = false
    var startLine:     Int = 1      // 1-indexed source line of this line


private final class TelParser():
  import TelParser.*

  // ── Local snapshot ────────────────────────────────────────────────────────
  // Mirrors YamlParser's pattern: a parser-local snapshot of the cursor's
  // buffer keeps `bytes`/`pos`/`bufEnd` as plain fields so the JIT can hold
  // them in registers across hot byte loops. Sync to the cursor before any
  // mark/slice/refill operation; resync after.
  //
  // `cursor` and `schema` are `var`s rather than constructor args because
  // the parser is cached per-thread and reset across calls. `reset()`
  // re-binds both before each `parse()` invocation.

  private var cursor: Cursor[Data] = null.asInstanceOf[Cursor[Data]]
  private var schema: Optional[Tels] = Unset
  private var bytes:  Array[Byte] = null.asInstanceOf[Array[Byte]]
  private var pos:    Int = 0
  private var bufEnd: Int = 0

  // Incrementally tracked 1-indexed source line number of the current
  // cursor position. Bumped at every LF-consumption point — `consumeLineEnding`
  // and the two raw `advance()` calls over LF in `parseLiteralAtom`. Stays
  // valid across hold boundaries (and therefore across buffer compaction)
  // because it doesn't depend on resident buffer bytes. The cursor's own
  // `lineation` is left disabled on the hot path; we don't drive
  // `cursor.unsafeBumpLine` because column reconstruction is done from the
  // current buffer (always inside a hold containing the line's bytes) by
  // `columnForCurrentBytePos`, which doesn't depend on the cursor's
  // `columnNo`.
  private var lineNo: Int = 1

  // ── Parser state ──────────────────────────────────────────────────────────

  private var margin: Int = 0
  private var sigil:  Byte = '#'.toByte
  private var crlfMode: Boolean = false
  private var lineEndingsDetected: Boolean = false
  private var lineEndings: Tel.LineEndings = Tel.LineEndings.Lf

  // Look-ahead record describing the next unconsumed line.
  private val head: LineHead = new LineHead

  // Tracks state for the §9 CommentNotPreceded check.
  // `prevLineWasBoundary` is true iff the immediately-previous consumed line
  // was a blank, a comment, the prologue (directive / pragma), or we are
  // still at start-of-file — i.e. any line that may legitimately precede a
  // comment. `prevContentLeadingSpaces` records the leadingSpaces of the
  // most-recent content line (compound or tabulation), used to compare
  // against the indent at which a new comment appears.
  private var prevLineWasBoundary: Boolean = true
  private var prevContentLeadingSpaces: Int = -1

  // E102 detection: a "tel" / "tel …" line at column 0 that isn't the first
  // non-blank line is a misplaced pragma. We set this flag whenever a
  // non-blank line (directive, pragma, comment, compound, tabulation) has
  // been consumed.
  private var hasConsumedNonBlankLine: Boolean = false

  // TelParser surfaces an empty sentinel line when the input ends with LF
  // (so a file "code\n" parses as two lines: "code" and ""). The streaming
  // parser hits EOF after the LF, so the next consumeTrailingBlanksFor at
  // the innermost block needs to claim one extra blank. Set when the LF we
  // just consumed is the final byte of the document; consumed exactly once
  // by the first consumeTrailingBlanksFor that reaches EOF.
  private var documentEndsWithLf: Boolean = false

  // Ancestor stack of Struct types known for each open compound, used by
  // §19.5's schema-aware E107 recovery. The element at index `i` is the
  // schema struct corresponding to the compound at depth `i+1` (so the
  // document root at depth 0 is implicit and refers to `schema.document`).
  // `Unset` entries mark compounds whose schema position couldn't be
  // resolved (e.g. an unknown keyword) — both shallower and deeper
  // recovery treat that ancestor as schema-blind.
  private val ancestors =
    scala.collection.mutable.ArrayBuffer.empty[Optional[Tels.Struct]]

  // Reusable string builder for source-atom / literal-atom payloads.
  private val sb: jl.StringBuilder = new jl.StringBuilder(256)

  // Atom-bytes arena: one growing byte buffer per parser instance into which
  // every inline atom's UTF-8 bytes are written. Each `Tel.Atom.Inline`
  // stores `(arenaArray, offset, length)` referring to a slice of the
  // arena. When the arena's capacity is exhausted, the parser allocates a
  // fresh, larger backing array; previously committed atoms keep the old
  // arena array alive via their slice references. This replaces the prior
  // "one freshly-allocated exact-size byte[] per atom" scheme: a workload
  // with N atoms now performs O(log capacity) arena allocations instead of
  // N per-atom allocations.
  //
  // `inFlightStart` is the arena offset where the currently-open atom
  // started; -1 when no atom is open. On a mid-atom arena growth we carry
  // the in-flight bytes (`atomArena(inFlightStart..arenaPos)`) into the
  // new array at offset 0 so the atom remains contiguous; the old array
  // (which still holds completed atoms) stays alive via their Inline
  // references.
  private var atomArena:     Array[Byte] = new Array[Byte](256)
  private var arenaPos:      Int = 0
  private var inFlightStart: Int = -1

  private inline def beginInFlightAtom(): Unit = inFlightStart = arenaPos

  private inline def endInFlightAtom(): Int =
    val len = arenaPos - inFlightStart
    inFlightStart = -1
    len

  private inline def arenaInFlightOffset: Int = inFlightStart

  private inline def ensureArenaSpace(n: Int): Unit =
    if arenaPos + n > atomArena.length then growArena(n)

  // Non-inline: keep the slow path out of the inline budget. Allocates a
  // fresh arena array sized to at least `arenaPos + n`, copies the
  // in-flight atom's bytes (if any) into it starting at offset 0, and
  // resets `arenaPos` accordingly. Already-committed atoms continue to
  // reference the previous array.
  private def growArena(n: Int): Unit =
    val inFlightLen = if inFlightStart >= 0 then arenaPos - inFlightStart else 0
    val needed = inFlightLen + n
    var newCap = (atomArena.length * 2).max(256)
    while newCap < needed do newCap *= 2
    val newArena = new Array[Byte](newCap)
    if inFlightLen > 0 then
      System.arraycopy(atomArena, inFlightStart, newArena, 0, inFlightLen)
    atomArena = newArena
    if inFlightStart >= 0 then inFlightStart = 0
    arenaPos = inFlightLen

  private inline def appendToArena(b: Byte): Unit =
    ensureArenaSpace(1)
    atomArena(arenaPos) = b
    arenaPos += 1

  private inline def appendToArenaRange(src: Array[Byte], off: Int, len: Int): Unit =
    ensureArenaSpace(len)
    System.arraycopy(src, off, atomArena, arenaPos, len)
    arenaPos += len

  // ── Honeycomb-style scratch buffers ───────────────────────────────────────
  // A single parser-lifetime array per child-collection type holds every
  // pending child across the whole recursive descent. Each
  // `parseBlock` / `parseCompoundLine` invocation snapshots the current
  // index at scope-entry; on scope-exit it computes `count = current - start`,
  // snapshots that range into a freshly allocated exact-size `Array` (wrapped
  // as `IArray` via `.immutable(using Unsafe)`), and rewinds the index.
  // Empty scopes return `IArray.empty` without any allocation.
  //
  // This replaces one `mutable.ArrayBuffer` (which itself allocates a backing
  // array, grows geometrically, and copies on growth) + one `IArray.from`
  // call per scope with a single exact-size `Array.copyOfRange` allocation
  // per non-empty scope. For typical workloads (deep nesting, many siblings)
  // it eliminates several thousand allocations per parse.

  private var scratchAtoms:     Array[Tel.Atom]     = new Array[Tel.Atom](16)
  private var atomScratchIx:    Int = 0

  private var scratchComments:  Array[Tel.Comment]  = new Array[Tel.Comment](8)
  private var commentScratchIx: Int = 0

  private var scratchCompounds: Array[Tel.Compound] = new Array[Tel.Compound](8)
  private var compoundScratchIx: Int = 0

  private var scratchBlocks:    Array[Tel.Block]    = new Array[Tel.Block](16)
  private var blockScratchIx:   Int = 0

  private inline def reserveAtom(): Unit =
    if atomScratchIx >= scratchAtoms.length then
      val grown = new Array[Tel.Atom](scratchAtoms.length*2)
      System.arraycopy(scratchAtoms, 0, grown, 0, atomScratchIx)
      scratchAtoms = grown

  private inline def reserveComment(): Unit =
    if commentScratchIx >= scratchComments.length then
      val grown = new Array[Tel.Comment](scratchComments.length*2)
      System.arraycopy(scratchComments, 0, grown, 0, commentScratchIx)
      scratchComments = grown

  private inline def reserveCompound(): Unit =
    if compoundScratchIx >= scratchCompounds.length then
      val grown = new Array[Tel.Compound](scratchCompounds.length*2)
      System.arraycopy(scratchCompounds, 0, grown, 0, compoundScratchIx)
      scratchCompounds = grown

  private inline def reserveBlock(): Unit =
    if blockScratchIx >= scratchBlocks.length then
      val grown = new Array[Tel.Block](scratchBlocks.length*2)
      System.arraycopy(scratchBlocks, 0, grown, 0, blockScratchIx)
      scratchBlocks = grown

  private inline def pushAtom(atom: Tel.Atom): Unit =
    reserveAtom()
    scratchAtoms(atomScratchIx) = atom
    atomScratchIx += 1

  private inline def pushComment(c: Tel.Comment): Unit =
    reserveComment()
    scratchComments(commentScratchIx) = c
    commentScratchIx += 1

  private inline def pushCompound(c: Tel.Compound): Unit =
    reserveCompound()
    scratchCompounds(compoundScratchIx) = c
    compoundScratchIx += 1

  private inline def pushBlock(b: Tel.Block): Unit =
    reserveBlock()
    scratchBlocks(blockScratchIx) = b
    blockScratchIx += 1

  // Extract `count` items ending at the current index, rewind, and return
  // them as an IArray. Empty scopes return the shared empty IArray with
  // zero allocation.
  private inline def takeAtoms(count: Int): IArray[Tel.Atom] =
    if count == 0 then IArray.empty[Tel.Atom]
    else
      val result = new Array[Tel.Atom](count)
      System.arraycopy(scratchAtoms, atomScratchIx - count, result, 0, count)
      atomScratchIx -= count
      result.asInstanceOf[IArray[Tel.Atom]]

  private inline def takeComments(count: Int): IArray[Tel.Comment] =
    if count == 0 then IArray.empty[Tel.Comment]
    else
      val result = new Array[Tel.Comment](count)
      System.arraycopy(scratchComments, commentScratchIx - count, result, 0, count)
      commentScratchIx -= count
      result.asInstanceOf[IArray[Tel.Comment]]

  private inline def takeCompounds(count: Int): IArray[Tel.Compound] =
    if count == 0 then IArray.empty[Tel.Compound]
    else
      val result = new Array[Tel.Compound](count)
      System.arraycopy(scratchCompounds, compoundScratchIx - count, result, 0, count)
      compoundScratchIx -= count
      result.asInstanceOf[IArray[Tel.Compound]]

  private inline def takeBlocks(count: Int): IArray[Tel.Block] =
    if count == 0 then IArray.empty[Tel.Block]
    else
      val result = new Array[Tel.Block](count)
      System.arraycopy(scratchBlocks, blockScratchIx - count, result, 0, count)
      blockScratchIx -= count
      result.asInstanceOf[IArray[Tel.Block]]

  // ── parseCompoundLine result channels ──────────────────────────────────────
  // parseCompoundLine deposits its keyword + remark into these single-slot
  // fields rather than returning a Tel.Compound. The atoms it reads remain
  // on the scratchAtoms stack; the caller (parseBlock) optionally pushes
  // an extra Source/Literal atom, then takes the entire atom run at once.
  // This eliminates the prior `parsed.copy(atoms = finalAtoms, children = ...)`
  // double-allocation: Tel.Compound is built exactly once with its final
  // atoms and children set.
  private var compoundLineKeyword: Text = t""
  private var compoundLineRemark:  Optional[Text] = Unset

  // ── Keyword interning cache ───────────────────────────────────────────────
  // 64-slot two-Long fingerprint cache. The byte-level variant: the first
  // four bytes pack into `low` and the next four into `high`. ASCII keywords
  // fingerprint injectively; multibyte UTF-8 keywords fingerprint by raw
  // bytes (still injective for ≤8 bytes).
  private val keyCache:     Array[String] = new Array[String](64)
  private val keyCacheLow:  Array[Long]   = new Array[Long](64)
  private val keyCacheHigh: Array[Long]   = new Array[Long](64)

  // ── Substrate ─────────────────────────────────────────────────────────────

  private inline def syncTo(): Unit =
    cursor.unsafeAdvanceBy(pos - cursor.unsafePos(using Unsafe))(using Unsafe)

  private inline def syncFrom(): Unit =
    bytes  = cursor.buffer(using Unsafe)
    pos    = cursor.unsafePos(using Unsafe)
    bufEnd = cursor.unsafeWriteEnd(using Unsafe)

  private inline def more: Boolean = pos < bufEnd || moreSlow()

  private def moreSlow(): Boolean =
    syncTo()
    if cursor.more then { syncFrom(); true }
    else { syncFrom(); false }

  private inline def peek: Byte = bytes(pos)

  private inline def advance(): Unit = pos += 1

  // Ensure that at least `n` bytes are available from the current position
  // (or that we are at EOF). Pulls chunks from the cursor as needed. After
  // this returns, either `pos + n <= bufEnd` or we have hit EOF and no more
  // bytes will arrive. Used by the multi-byte look-ahead points (CR/LF
  // pairs, sigil + soft-space + content checks, pragma detection).
  //
  // The trick: mark, advance up to `n` steps to force successive refills,
  // then cue back. Inside the outer `hold` block, the mark prevents the
  // buffer from compacting past our current position, so the bytes we
  // need stay resident.
  private def ensureLookahead(n: Int): Unit =
    if pos + n <= bufEnd then ()
    else
      syncTo()
      val mk = cursor.mark(using Cursor.shared)
      var steps = 0
      while steps < n && cursor.more do
        cursor.advance()
        steps += 1
      cursor.cue(mk)
      syncFrom()

  // Peek the byte one past the current position, refilling if necessary.
  // Returns -1 if there is no byte one past the current position.
  private def peekNext(): Int =
    ensureLookahead(2)
    if pos + 1 < bufEnd then bytes(pos + 1) & 0xff else -1

  // ── SWAR in-buffer scans ─────────────────────────────────────────────────
  // Each `scanUntilN` method advances the parser-local `pos` until either a
  // byte matching one of the targets is found, or `pos == bufEnd`. It does
  // NOT refill the cursor — callers wrap the call in a refill loop when
  // they need to scan across chunk boundaries.

  // Advance pos until bytes(pos) == target1, or pos == bufEnd.
  private def scanUntil1(target1: Byte, repl1: Long): Unit =
    while pos + 8 <= bufEnd do
      val v = TelParser.longView.get(bytes, pos).asInstanceOf[Long]
      val mask = TelParser.matchByte(v, repl1)
      if mask != 0L then
        pos += (java.lang.Long.numberOfTrailingZeros(mask) >>> 3)
        return
      pos += 8
    while pos < bufEnd && bytes(pos) != target1 do pos += 1

  // Advance pos until bytes(pos) ∈ {target1, target2}, or pos == bufEnd.
  private def scanUntil2(target1: Byte, target2: Byte, repl1: Long, repl2: Long): Unit =
    while pos + 8 <= bufEnd do
      val v = TelParser.longView.get(bytes, pos).asInstanceOf[Long]
      val combined = TelParser.matchByte(v, repl1) | TelParser.matchByte(v, repl2)
      if combined != 0L then
        pos += (java.lang.Long.numberOfTrailingZeros(combined) >>> 3)
        return
      pos += 8
    while pos < bufEnd && bytes(pos) != target1 && bytes(pos) != target2 do pos += 1

  // Advance pos until bytes(pos) ∈ {a, b, c}, or pos == bufEnd.
  private def scanUntil3
                ( a: Byte, b: Byte, c: Byte,
                  rA: Long, rB: Long, rC: Long )
  : Unit =
    while pos + 8 <= bufEnd do
      val v = TelParser.longView.get(bytes, pos).asInstanceOf[Long]
      val combined =
        TelParser.matchByte(v, rA)
        | TelParser.matchByte(v, rB)
        | TelParser.matchByte(v, rC)
      if combined != 0L then
        pos += (java.lang.Long.numberOfTrailingZeros(combined) >>> 3)
        return
      pos += 8
    while pos < bufEnd && bytes(pos) != a && bytes(pos) != b && bytes(pos) != c do pos += 1

  // Advance pos until bytes(pos) ∈ {a, b, c, d}, or pos == bufEnd.
  private def scanUntil4
                ( a: Byte, b: Byte, c: Byte, d: Byte,
                  rA: Long, rB: Long, rC: Long, rD: Long )
  : Unit =
    while pos + 8 <= bufEnd do
      val v = TelParser.longView.get(bytes, pos).asInstanceOf[Long]
      val combined =
        TelParser.matchByte(v, rA)
        | TelParser.matchByte(v, rB)
        | TelParser.matchByte(v, rC)
        | TelParser.matchByte(v, rD)
      if combined != 0L then
        pos += (java.lang.Long.numberOfTrailingZeros(combined) >>> 3)
        return
      pos += 8
    while pos < bufEnd && bytes(pos) != a && bytes(pos) != b
          && bytes(pos) != c && bytes(pos) != d
    do pos += 1

  // ── Errors ────────────────────────────────────────────────────────────────

  // Compute the column corresponding to the current cursor position
  // (1-indexed; 1 = first byte of line). Walks the *current* buffer backwards
  // looking for the most-recent LF. Because errors are always raised while a
  // hold is active around the line being parsed, the line's starting LF (or
  // start-of-stream) is always inside the resident region of the buffer.
  private def columnForCurrentBytePos(): Int =
    var i = pos - 1
    var col = 1
    while i >= 0 && bytes(i) != LF do
      col += 1
      i -= 1
    col

  private def errorHere(reason: Reason): Nothing raises TelError =
    val column = columnForCurrentBytePos()
    abort(TelError(reason, TelError.Position(lineNo, column)))

  // Direct line-number variant: callers pass the 1-indexed source line of
  // the offending location and its column.
  private def errorAt(reason: Reason, line: Int, column: Int)
  :     Nothing raises TelError =
    abort(TelError(reason, TelError.Position(line, column)))

  // ── Mark / hold plumbing ──────────────────────────────────────────────────
  //
  // The streaming parser uses narrow per-leaf holds: every method that takes
  // a `cursor.mark` (or that calls down to one — `peekNext`, `ensureLookahead`,
  // `consumeLineEnding`, `sliceText`, etc.) wraps its mark-using scope in
  // `inHold`. Outside any hold, the cursor is free to compact away consumed
  // bytes when a refill needs space; that's exactly the streaming property
  // we want. State carried between holds is restricted to plain primitives
  // — `head.*`, `lineNo`, `prevLineWasBoundary`, accumulated `sb` content —
  // never byte offsets into a particular buffer.
  //
  // `inHold` nests safely: an inner `inHold` does not change `holdStart`,
  // it just executes inside the outer hold's protection. `Cursor.Held` is
  // a witness type and `Cursor.shared` is a singleton we always pass to
  // `cursor.mark` — there's no run-time tracking of "are we in a hold"
  // inside the parser because no run-time decision depends on it; the
  // compile-time `using Cursor.Held` requirement at every mark call site
  // is the static guarantee that the code path is reachable only from
  // inside a hold.
  private inline def inHold[T](inline body: => T): T =
    syncTo()
    cursor.hold:
      try
        syncFrom()
        body
      finally syncTo()

  private inline def beginMark(): Cursor.Mark =
    syncTo()
    cursor.mark(using Cursor.shared)

  // Materialise the byte range between `start` and the current position as
  // a UTF-8 `String`. The only allocation point in the keyword / atom path.
  // Non-inline: the cursor.slice lambda's path-dependent `addressable.Storage`
  // resolves once here against `this.cursor`.
  private def sliceText(start: Cursor.Mark): String =
    syncTo()
    val endMk = cursor.mark(using Cursor.shared)
    cursor.slice(start, endMk): (storage, off, len) =>
      val arr = storage.asInstanceOf[Array[Byte]]
      if len <= 0 then "" else new String(arr, off, len, StandardCharsets.UTF_8)

  // ── Reset (per-thread reuse) ──────────────────────────────────────────────
  //
  // Re-initialise the parser for a new parse. Called by the cached factory
  // in `object TelParser` before each `parse()` invocation. Every mutable
  // field that survives across parses is reset here; reusable arrays
  // (scratch buffers, ancestors, sb) are cleared in place so we don't pay
  // their allocation again. The atom arena is REPLACED with a fresh array
  // because previously-parsed Inlines reference the old one — we cannot
  // overwrite their bytes. The keyword-fingerprint cache is preserved
  // across resets so frequent keywords stay interned.
  private[stratiform] def reset(c: Cursor[Data], s: Optional[Tels]): Unit =
    cursor = c
    schema = s
    bytes  = null.asInstanceOf[Array[Byte]]
    pos    = 0
    bufEnd = 0
    lineNo = 1
    margin = 0
    sigil  = '#'.toByte
    crlfMode = false
    lineEndingsDetected = false
    lineEndings = Tel.LineEndings.Lf
    head.leadingSpaces = 0
    head.indentLevels  = 0
    head.blank = false
    head.eof   = false
    head.startLine = 1
    prevLineWasBoundary = true
    prevContentLeadingSpaces = -1
    hasConsumedNonBlankLine = false
    documentEndsWithLf = false
    ancestors.clear()
    sb.setLength(0)
    // Fresh arena: previous parse's Inlines hold the old array via their
    // (arena, off, len) backing reference, so it stays alive as long as
    // those Inlines are referenced.
    atomArena     = new Array[Byte](256)
    arenaPos      = 0
    inFlightStart = -1
    // Null out the scratch arrays so they don't pin references from the
    // previous parse. Cheap (one pass over what are typically small arrays);
    // worth it to let GC collect the previous Document's transient nodes
    // promptly while the parser sits in the ThreadLocal between calls.
    java.util.Arrays.fill(scratchAtoms.asInstanceOf[Array[AnyRef]], null)
    atomScratchIx = 0
    java.util.Arrays.fill(scratchComments.asInstanceOf[Array[AnyRef]], null)
    commentScratchIx = 0
    java.util.Arrays.fill(scratchCompounds.asInstanceOf[Array[AnyRef]], null)
    compoundScratchIx = 0
    java.util.Arrays.fill(scratchBlocks.asInstanceOf[Array[AnyRef]], null)
    blockScratchIx = 0
    compoundLineKeyword = t""
    compoundLineRemark  = Unset

  // ── Top-level parse ───────────────────────────────────────────────────────

  def parse(): Tel.Document raises TelError =
    syncFrom()
    checkBom()
    val directive = parseInterpreterDirective()
    val pragma = parsePragma()
    fillHead()  // park head at the next line
    if directive.absent && pragma.absent then determineMargin()
    else
      // A directive or pragma may be followed by blank lines before the
      // first content line; consume them so parseChildren can dispatch
      // on a real content line.
      margin = 0
      while head.blank && !head.eof do fillHead()

    val children = parseChildren(parentIndent = -1)
    Tel.Document(directive, pragma, lineEndings, children)

  // ── BOM ──────────────────────────────────────────────────────────────────

  private def checkBom(): Unit raises TelError =
    if more && peek == BOM0 then errorHere(Reason.BomPresent)

  // ── Line endings ─────────────────────────────────────────────────────────

  // Detect line-endings mode from the first LF seen. CR before LF → CRLF
  // mode. After detection, every subsequent byte is checked against the
  // mode: in LF mode a CR is E121; in CRLF mode a lone LF (not preceded by
  // CR) is E121. The detection is `lazy` — driven by `consumeLfFromHere`,
  // which is the only place we consume an LF.
  private inline def detectLineEndingMode(crBefore: Boolean): Unit =
    if !lineEndingsDetected then
      lineEndingsDetected = true
      crlfMode = crBefore
      lineEndings = if crBefore then Tel.LineEndings.Crlf else Tel.LineEndings.Lf

  // ── Prologue ─────────────────────────────────────────────────────────────

  // Reads "#!..." line if present. The directive payload excludes the
  // "#!" prefix and the terminating LF.
  private def parseInterpreterDirective(): Optional[Text] raises TelError = inHold:
    // We can peek the first two bytes without consuming.
    if !more then Unset
    else if peek != '#'.toByte then Unset
    else
      val second = peekNext()
      if second != '!'.toInt then Unset
      else
        // Consume "#!"
        advance()
        advance()
        val mk = beginMark()
        // Read until LF or CR
        while more && peek != LF && peek != CR do advance()
        val payload = sliceText(mk)
        consumeLineEnding()
        prevLineWasBoundary = true
        hasConsumedNonBlankLine = true
        Text(payload)

  // Reads a pragma line ("tel ..." or "tel") if present as the first
  // non-blank line. Marks before consuming any blanks; if the first
  // non-blank line is NOT a pragma, cues back so the cursor remains at
  // the original position and the caller can run determineMargin from
  // scratch. The mark must survive the blank-line scan, so the entire
  // body runs inside one hold.
  private def parsePragma(): Optional[Tel.Pragma] raises TelError = inHold:
    // `pragmaStartAbs` is the absolute byte position of the start of the
    // first non-blank line, used to check the 4096-byte pragma cap (§3.5)
    // against absolute stream position rather than the (possibly compacted)
    // buffer-relative `pos`. Computed via cursor.position after syncTo so it
    // remains correct once we narrow holds elsewhere in the parser.
    val mk = beginMark()
    val savedBoundary = prevLineWasBoundary
    val savedLineNo = lineNo

    // Skip blank lines (consuming them; we may cue back).
    var foundPragma: Optional[Tel.Pragma] = Unset
    var done = false
    while !done do
      val spaceCount = countLeadingSpaces()
      if !more then done = true
      else if peek == LF || peek == CR then
        consumeLineEnding()
      else
        // First non-blank line. Is it a pragma?
        if spaceCount == 0 && startsWithPragma() then
          val pragmaLine = lineNo
          syncTo()
          val pragmaStartAbs = cursor.position.n0
          if pragmaStartAbs >= 4096 then
            errorAt(Reason.PragmaTooLong, pragmaLine, 1)
          val pragmaMk = beginMark()
          while more && peek != LF && peek != CR do advance()
          val payload = sliceText(pragmaMk)
          syncTo()
          val pragmaEndAbs = cursor.position.n0
          if pragmaEndAbs > 4096 then
            errorAt(Reason.PragmaTooLong, pragmaLine, 1)
          consumeLineEnding()
          prevLineWasBoundary = true
          hasConsumedNonBlankLine = true
          foundPragma = Optional(parsePragmaContent(payload, pragmaLine))
        done = true

    if foundPragma.present then foundPragma
    else
      // Not a pragma — cue back to before any blanks were consumed.
      syncTo()
      cursor.cue(mk)
      syncFrom()
      prevLineWasBoundary = savedBoundary
      lineNo = savedLineNo
      Unset

  // Count leading spaces at the current position (does not consume the LF /
  // content byte). Stops at LF, CR, EOF, or any non-space byte. Does NOT
  // advance lineation across LF — caller must handle that.
  private inline def countLeadingSpaces(): Int =
    var count = 0
    while more && peek == SP do
      advance()
      count += 1
    count

  // Consume one line-ending sequence (LF or CR LF). Validates that bare CR
  // (in LF mode) and bare LF (in CRLF mode) raise E121. Sets
  // `documentEndsWithLf` when the LF we just consumed is the final byte of
  // the document — used by consumeTrailingBlanksFor to count the virtual
  // empty trailing line that TelParser surfaces when its lines array ends
  // with a sentinel empty entry.
  private def consumeLineEnding(): Unit raises TelError =
    if !more then ()
    else if peek == LF then
      if !lineEndingsDetected then detectLineEndingMode(crBefore = false)
      else if crlfMode then errorHere(Reason.BadLineEnding)
      advance()
      lineNo += 1
      if !more then documentEndsWithLf = true
    else if peek == CR then
      val next = peekNext()
      if next != LF.toInt then errorHere(Reason.BadLineEnding)
      else
        if !lineEndingsDetected then detectLineEndingMode(crBefore = true)
        else if !crlfMode then errorHere(Reason.BadLineEnding)
        advance()  // CR
        advance()  // LF
        lineNo += 1
        if !more then documentEndsWithLf = true
    else
      // not at a line ending — caller error
      ()

  // True iff the cursor is at the start of a pragma line (already past any
  // leading spaces, on the first content byte). A pragma is "tel" followed
  // by EOL or " ". Requires four bytes of look-ahead, or three bytes at EOF.
  private def startsWithPragma(): Boolean =
    ensureLookahead(4)
    if pos + 2 < bufEnd
       && bytes(pos) == 't'.toByte && bytes(pos + 1) == 'e'.toByte
       && bytes(pos + 2) == 'l'.toByte
    then
      if pos + 3 < bufEnd
      then bytes(pos + 3) == SP || bytes(pos + 3) == LF || bytes(pos + 3) == CR
      else true  // exactly "tel" at EOF
    else false

  private def parsePragmaContent(content: String, line: Int)
  : Tel.Pragma raises TelError =
    val parts = splitPragmaPhrases(content)
    if parts.head != "tel" then errorAt(Reason.PragmaNotFirst, line, 1)
    val version =
      if parts.length >= 2 then parseVersion(parts(1), line)
      else (1, 0)

    if parts.length > 4 then errorAt(Reason.ExtraPragmaContent, line, 1)

    val schemaText: Optional[Text] =
      if parts.length >= 3 then
        val s = parts(2)
        // §8.1: the schema identifier is either an HTTP/HTTPS URL (with a
        // `://`) or a bare BASE-256-encoded schema signature. The BASE-256
        // alphabet (§4) is exactly the Unicode letters and ASCII digits, so
        // a bare signature is a non-empty run of letters/digits with no
        // whitespace or punctuation. Palimpsest length/decodability are
        // checked later, at signature resolution — not at pragma parse time.
        val isUrl = s.indexOf("://") >= 0
        val isBase256 = s.nonEmpty && s.forall: c =>
          Character.isLetter(c) || (c >= '0' && c <= '9')
        if !isUrl && !isBase256
        then errorAt(Reason.BadSchemaIdentifier, line, 1)
        Text(s): Optional[Text]
      else Unset

    val pragmaSigil: Optional[Char] =
      if parts.length >= 4 && parts(3).length == 1 then
        val c = parts(3).charAt(0)
        if c.isLetterOrDigit then errorAt(Reason.BadSigil, line, 1)
        sigil = c.toByte
        c: Optional[Char]
      else Unset

    Tel.Pragma(version, schemaText, pragmaSigil)

  private def parseVersion(s: String, line: Int)
  : (Int, Int) raises TelError =
    val dot = s.indexOf('.')
    if dot <= 0 || dot == s.length - 1 then errorAt(Reason.BadVersion, line, 1)
    try
      val major = s.substring(0, dot).toInt
      val minor = s.substring(dot + 1).toInt
      if major < 0 || minor < 0 then errorAt(Reason.BadVersion, line, 1)
      (major, minor)
    catch case _: NumberFormatException => errorAt(Reason.BadVersion, line, 1)

  private def splitPragmaPhrases(content: String): List[String] =
    val parts = scala.collection.mutable.ListBuffer.empty[String]
    val builder = new StringBuilder()
    var i = 0
    var hardSpaceMode = false
    while i < content.length do
      val ch = content.charAt(i)
      if ch == ' ' then
        var j = i
        while j < content.length && content.charAt(j) == ' ' do j += 1
        val runLength = j - i
        if !hardSpaceMode && runLength == 1 then
          if builder.nonEmpty then { parts += builder.toString; builder.clear() }
          i += 1
        else
          if !hardSpaceMode then hardSpaceMode = true
          if builder.nonEmpty then { parts += builder.toString; builder.clear() }
          i = j
      else
        builder.append(ch)
        i += 1

    if builder.nonEmpty then parts += builder.toString
    parts.to(List)

  // ── Margin determination ─────────────────────────────────────────────────

  // Sets `margin` to the leadingSpaces of the first non-blank content line.
  // The caller's outer fillHead has already parked head at the first line;
  // we walk through blanks if needed.
  private def determineMargin(): Unit raises TelError =
    while head.blank && !head.eof do fillHead()
    if !head.eof then
      margin = head.leadingSpaces
      head.indentLevels = 0  // by definition of margin

  // ── §19.5 schema-aware E107 recovery ─────────────────────────────────────

  // Peek the keyword of the line that head is currently parked at (i.e. read
  // it without advancing past the line). Uses mark + cue to restore the
  // cursor's byte position, lineation, and the parser's local snapshot.
  private def peekKeyword(): Text raises TelError =
    val outerMark = beginMark()
    val kw = readKeyword()
    syncTo()
    cursor.cue(outerMark)
    syncFrom()
    kw

  // Resolve `t` to a Struct, optionally following one Reference indirection.
  // Returns Unset for Scalars, Flag, Select etc.
  private def resolveTypeToStruct(t: Tels.Type, s: Tels): Optional[Tels.Struct] =
    t match
      case struct: Tels.Struct => struct
      case Tels.Reference(name) =>
        s.records.find(_.name == name) match
          case Some(rec) => Tels.Struct(rec.members, rec.validators)
          case None      => Unset
      case _ => Unset

  // Resolve `keyword` against `parent`'s direct Field / SelectRef members.
  // Returns the Struct type if the keyword names a child whose resolved
  // type is a Struct, or Unset otherwise.
  private def resolveKeywordStruct(parent: Tels.Struct, keyword: Text, s: Tels)
  :     Optional[Tels.Struct] raises TelError =
    var found: Optional[Tels.Type] = Unset
    var i = 0
    while i < parent.members.length && found.absent do
      parent.members(i) match
        case f: Tels.Field =>
          if f.keyword == keyword then found = f.fieldType
        case sr: Tels.SelectRef =>
          s.selects.find(_.name == sr.reference).foreach: selectDef =>
            selectDef.variants.find(_.keyword == keyword).foreach: variant =>
              found = variant.variantType
        case _: Tels.Exclude => ()
      i += 1
    found.let(t => resolveTypeToStruct(t, s)).or(Unset)

  // Does `parent` admit `keyword` in its direct Field / SelectRef set?
  private def keywordAdmissible(parent: Tels.Struct, keyword: Text, s: Tels): Boolean =
    var i = 0
    var hit = false
    while !hit && i < parent.members.length do
      parent.members(i) match
        case f: Tels.Field =>
          if f.keyword == keyword then hit = true
        case sr: Tels.SelectRef =>
          s.selects.find(_.name == sr.reference).foreach: selectDef =>
            if selectDef.variants.exists(_.keyword == keyword) then hit = true
        case _: Tels.Exclude => ()
      i += 1
    hit

  // Push a child compound's resolved struct onto the ancestor stack. No-op
  // (records Unset for depth bookkeeping) without a schema.
  private def pushAncestor(keyword: Text): Unit raises TelError =
    schema.let: s =>
      val parent: Optional[Tels.Struct] =
        if ancestors.isEmpty then s.document else ancestors(ancestors.length - 1)
      val resolved: Optional[Tels.Struct] =
        parent.let(p => resolveKeywordStruct(p, keyword, s))
      ancestors += resolved
    .or:
      ancestors += Unset

  private def popAncestor(): Unit =
    if ancestors.nonEmpty then ancestors.remove(ancestors.length - 1)

  // §19.5 odd-indent recovery. Without a schema, raise E107. With a schema,
  // compute admissibility at both candidate depths via the current ancestor
  // stack and pick deeper if and only if shallower is invalid AND deeper is
  // valid; tie-break favours shallower.
  private def recoverOddIndent(spaces: Int, line: Int): Int raises TelError =
    val rel = spaces - margin
    schema.let: s =>
      val shallower = rel / 2
      val deeper    = shallower + 1
      val keyword   = peekKeyword()
      val shallowerParent: Optional[Tels.Struct] =
        if shallower == 0 then s.document
        else if shallower - 1 < ancestors.length then ancestors(shallower - 1)
        else Unset
      val deeperParent: Optional[Tels.Struct] =
        if shallower < ancestors.length then ancestors(shallower) else Unset
      val shallowerValid =
        shallowerParent.let(p => keywordAdmissible(p, keyword, s)).or(false)
      val deeperValid =
        deeperParent.let(p => keywordAdmissible(p, keyword, s)).or(false)
      if !shallowerValid && deeperValid then deeper else shallower
    .or:
      errorAt(Reason.OddIndentation, line, 1)

  // ── Line-head fill ───────────────────────────────────────────────────────

  // Consume the next line's leading spaces and parks at the first content
  // byte (or LF/EOF for a blank line). Updates `head` in place. Always
  // advances past EOF cleanly: head.eof = true, head.blank = true.
  //
  // The 1-indexed line number is read from the incrementally-maintained
  // `lineNo` counter (bumped at every LF-consumption point), so the success
  // path pays no per-line lineation cost. Runs inside its own hold so the
  // intermediate `consumeLineEnding` (which calls `peekNext` → `ensureLookahead`,
  // a mark-using lookahead) and `recoverOddIndent` (which calls `peekKeyword`,
  // also mark-using) execute inside an active `cursor.hold` scope.
  private def fillHead(): Unit raises TelError = inHold:
    head.startLine = lineNo

    val spaces = countLeadingSpaces()
    head.leadingSpaces = spaces

    if !more then
      head.eof = true
      head.blank = true
      head.indentLevels = -1
    else if peek == LF || peek == CR then
      head.blank = true
      head.eof = false
      head.indentLevels = -1
      consumeLineEnding()
      // The line we just consumed was blank — it acts as a boundary for
      // E109.
      prevLineWasBoundary = true
    else
      head.blank = false
      head.eof = false
      val rel = spaces - margin
      head.indentLevels =
        if rel < 0 then
          errorAt(Reason.LessThanMargin, head.startLine, 1)
        else if rel % 2 == 0 then rel / 2
        else recoverOddIndent(spaces, head.startLine)

  // ── parseChildren ────────────────────────────────────────────────────────

  private def parseChildren(parentIndent: Int): IArray[Tel.Block] raises TelError =
    val expected = parentIndent + 1

    if head.eof || head.indentLevels < expected then IArray.empty[Tel.Block]
    else
      val start = blockScratchIx
      while !head.eof && head.indentLevels == expected do
        parseBlock(expected)  // pushes one block onto scratchBlocks

      // After consuming children at `expected`, a remaining non-blank line
      // more indented than `expected` is an error.
      if !head.eof && head.indentLevels > expected then
        val line = head.startLine
        val lastIx = blockScratchIx - 1
        if lastIx >= start then
          val last = scratchBlocks(lastIx)
          if last.tabulation.present && last.compounds.isEmpty then
            errorAt(Reason.RowWrongIndent, line, 1)
          else if last.tabulation.present then
            errorAt(Reason.ChildOfNonCompound, line, 1)
          else if last.compounds.isEmpty then
            errorAt(Reason.ChildOfNonCompound, line, 1)
          else
            errorAt(Reason.OverIndentation, line, 1)
        else errorAt(Reason.OverIndentation, line, 1)

      takeBlocks(blockScratchIx - start)

  // ── parseBlock ───────────────────────────────────────────────────────────

  // Parses one block at the given indent: leading comments, optional
  // tabulation header, a run of compounds (each possibly with source /
  // literal / children attached), then trailing blank lines (those that
  // semantically belong to this block). Pushes the resulting Tel.Block
  // onto the parser's `scratchBlocks` stack; the caller (parseChildren)
  // takes a contiguous range when its loop completes.
  private def parseBlock(indent: Int): Unit raises TelError =
    val commentStart  = commentScratchIx
    val compoundStart = compoundScratchIx

    // Leading comments at this indent.
    while !head.eof && !head.blank && head.indentLevels == indent && isCommentBody() do
      // §9 E109 check — fires only if the immediately preceding line was a
      // content line (compound / tabulation) at indent ≥ this comment's.
      if !prevLineWasBoundary && prevContentLeadingSpaces >= 0
         && prevContentLeadingSpaces >= margin + indent * 2
      then errorAt(Reason.CommentNotPreceded, head.startLine, 1)

      val text = parseCommentLine()
      pushComment(Tel.Comment(text))
      prevLineWasBoundary = true
      hasConsumedNonBlankLine = true
      fillHead()

    val hasComments = commentScratchIx > commentStart

    // Interior blanks: if we have comments and the next line is blank, look
    // ahead to see whether the blanks separate the comments from a compound
    // group at the same indent. If so, consume them as interior whitespace.
    if hasComments && !head.eof && head.blank then
      skipInteriorBlanksIfFollowedByContentAtIndent(indent)

    // Optional tabulation header.
    val tabulation: Optional[Tel.Tabulation] =
      if !head.eof && !head.blank && head.indentLevels == indent && isTabulationBody()
      then
        val ls = head.leadingSpaces
        val tab = parseTabulationLine()
        prevContentLeadingSpaces = ls
        prevLineWasBoundary = false
        hasConsumedNonBlankLine = true
        fillHead()
        Optional(tab)
      else Unset

    // Compound loop.
    var keepLoop = true
    while keepLoop && !head.eof && !head.blank && head.indentLevels == indent do
      if isCommentBody() || isTabulationBody() then keepLoop = false
      else
        val compoundLeadingSpaces = head.leadingSpaces
        val compoundLine = head.startLine
        // §16.2: validate tabulated rows BEFORE parseCompoundLine consumes
        // the row's bytes. validateTabulatedRowInline uses mark + cue so the
        // cursor remains parked at the row start for parseCompoundLine.
        if tabulation.present then
          validateTabulatedRowInline(compoundLeadingSpaces, tabulation.vouch, compoundLine)

        // parseCompoundLine pushes the compound's inline atoms onto
        // scratchAtoms and deposits keyword + remark into the parser's
        // compoundLine* fields. The atoms stay on the stack so we can
        // append an optional source/literal extra atom alongside them
        // and take the entire run at once into the final Tel.Compound,
        // skipping the previous `parsed.copy(...)` double-allocation.
        val atomsStart = atomScratchIx
        parseCompoundLine(compoundLine)
        val compoundKeyword = compoundLineKeyword
        val compoundRemark  = compoundLineRemark
        prevContentLeadingSpaces = compoundLeadingSpaces
        prevLineWasBoundary = false
        // §19.5 recovery needs this compound's keyword on the ancestor
        // stack BEFORE fillHead reads the next line — odd-indent recovery
        // there consults the ancestors. No-schema mode skips push/pop
        // entirely so the hot path is unaffected.
        val pushed = schema.present
        if pushed then pushAncestor(compoundKeyword)
        try
          fillHead()

          val extraAtom: Optional[Tel.Atom] =
            if tabulation.absent then parseSourceOrLiteralAtomIfPresent(compoundLeadingSpaces)
            else Unset

          if extraAtom.present then pushAtom(extraAtom.vouch)

          val children =
            if extraAtom.absent && tabulation.absent then parseChildren(indent)
            else IArray.empty[Tel.Block]

          val atoms = takeAtoms(atomScratchIx - atomsStart)
          pushCompound(Tel.Compound(compoundKeyword, atoms, compoundRemark, children))
        finally if pushed then popAncestor()

    val trailingBlankLines = consumeTrailingBlanksFor(indent)

    val comments  = takeComments(commentScratchIx - commentStart)
    val compounds = takeCompounds(compoundScratchIx - compoundStart)
    pushBlock(Tel.Block(comments, tabulation, compounds, trailingBlankLines))

  // After the comment-group, consume blank lines if the next non-blank line
  // is a content (not comment) line at the same `indent`. Otherwise leave
  // the blanks for the enclosing block. Uses mark+cue. The hold spans the
  // entire probe so the mark survives every nested fillHead.
  private def skipInteriorBlanksIfFollowedByContentAtIndent(indent: Int)
  : Unit raises TelError = inHold:
    val mk = beginMark()
    val savedHeadSnapshot = (head.leadingSpaces, head.indentLevels, head.blank,
                             head.eof, head.startLine)
    val savedBoundary = prevLineWasBoundary
    val savedLineNo = lineNo
    while !head.eof && head.blank do fillHead()
    val keep =
      !head.eof && head.indentLevels == indent && !isCommentBody()
    if !keep then
      // Rewind.
      syncTo()
      cursor.cue(mk)
      syncFrom()
      head.leadingSpaces = savedHeadSnapshot._1
      head.indentLevels  = savedHeadSnapshot._2
      head.blank         = savedHeadSnapshot._3
      head.eof           = savedHeadSnapshot._4
      head.startLine     = savedHeadSnapshot._5
      prevLineWasBoundary = savedBoundary
      lineNo = savedLineNo
      // Re-park: skip leading spaces of the line we just rewound to.
      var i = 0
      while i < savedHeadSnapshot._1 && more && peek == SP do
        advance()
        i += 1

  // ── Trailing blanks ──────────────────────────────────────────────────────

  // Consume blank lines that "belong" to the current block at `indent`. A
  // blank line belongs if the next non-blank line (if any) matches `indent`
  // or EOF.
  private def consumeTrailingBlanksFor(indent: Int): Int raises TelError =
    if head.eof then
      // The first parseBlock to reach EOF after a document that ended with
      // LF claims the virtual sentinel trailing line that TelParser emits.
      // Cleared so outer parseBlock invocations don't double-count.
      if documentEndsWithLf then
        documentEndsWithLf = false
        1
      else 0
    else if !head.blank then 0
    else inHold:
      val mk = beginMark()
      val firstBlankSnapshot = (head.leadingSpaces, head.indentLevels, head.blank,
                                head.eof, head.startLine)
      val savedBoundary = prevLineWasBoundary
      val savedLineNo = lineNo
      var count = 0
      while !head.eof && head.blank do
        count += 1
        fillHead()

      val keep =
        head.eof || head.indentLevels == indent

      if keep then count
      else
        // Rewind to the first blank.
        syncTo()
        cursor.cue(mk)
        syncFrom()
        head.leadingSpaces = firstBlankSnapshot._1
        head.indentLevels  = firstBlankSnapshot._2
        head.blank         = firstBlankSnapshot._3
        head.eof           = firstBlankSnapshot._4
        head.startLine     = firstBlankSnapshot._5
        prevLineWasBoundary = savedBoundary
        lineNo = savedLineNo
        var i = 0
        while i < firstBlankSnapshot._1 && more && peek == SP do
          advance()
          i += 1
        0

  // ── Predicates over the parked head ──────────────────────────────────────

  // True iff the parked head's first content byte is the sigil and the
  // line is a comment (sigil-only or sigil + soft space + text), as opposed
  // to a tabulation line or `#foo`-style keyword.
  private def isCommentBody(): Boolean raises TelError = inHold:
    if !more || peek != sigil then false
    else
      // Peek the rest of the line to decide between comment and tabulation.
      // A comment line has at most one space after the sigil; tabulation
      // lines have a hard space (2+) somewhere with another sigil.
      // Strategy: look one byte after the sigil.
      val nextByte = peekNext()
      if nextByte < 0 then true  // sigil at EOF — bare comment
      else if nextByte == LF.toInt || nextByte == CR.toInt then true
      else if nextByte != SP.toInt then false  // `#foo` — not a comment
      else
        // sigil + space + ... — could be comment or tabulation. Look ahead
        // to detect tabulation marker pattern. For now use mark+cue.
        !lineHasTabulationMarker()

  // Returns true iff the parked head's line, when read forward from current
  // position, contains a 2-space-or-more run followed by sigil somewhere
  // before the next LF. Uses mark+cue.
  private def lineHasTabulationMarker(): Boolean raises TelError = inHold:
    val mk = beginMark()
    var found = false
    var done = false
    while !done do
      if !more then done = true
      else
        val b = peek
        if b == LF || b == CR then done = true
        else if b == SP then
          // Count run.
          var run = 0
          while more && peek == SP do { advance(); run += 1 }
          if run >= 2 && more && peek == sigil then
            found = true
            done = true
        else advance()

    // Rewind.
    syncTo()
    cursor.cue(mk)
    syncFrom()
    found

  private def isTabulationBody(): Boolean raises TelError = inHold:
    if !more || peek != sigil then false
    else lineHasTabulationMarker()

  // ── Comment parsing ──────────────────────────────────────────────────────

  // Cursor is at the sigil. Returns the comment text, advancing past LF.
  private def parseCommentLine(): Text raises TelError = inHold:
    // Consume the sigil.
    advance()
    if !more || peek == LF || peek == CR then
      consumeLineEnding()
      t""
    else if peek == SP then
      // Skip the introducer space.
      advance()
      val mk = beginMark()
      while more && peek != LF && peek != CR do advance()
      val payload = sliceText(mk)
      consumeLineEnding()
      Text(payload)
    else
      // `#foo` — but we already classified this as a comment, so this
      // shouldn't happen. Treat as bare-content.
      val mk = beginMark()
      while more && peek != LF && peek != CR do advance()
      val payload = sliceText(mk)
      consumeLineEnding()
      Text(payload)

  // ── Tabulation line parsing ──────────────────────────────────────────────

  // Cursor is at the sigil. Reads marker offsets + headings, advances past
  // LF.
  private def parseTabulationLine(): Tel.Tabulation raises TelError = inHold:
    val lineStartCol = head.leadingSpaces  // first marker offset (column 0 = sigil)
    val markers = scala.collection.mutable.ArrayBuffer.empty[Int]
    val headings = scala.collection.mutable.ArrayBuffer.empty[Text]

    markers += lineStartCol  // first marker at first non-space position

    // Consume the first sigil.
    advance()

    // Repeating: heading text until next marker (hard-space + sigil) or
    // end of line.
    var lineCol = lineStartCol + 1  // column index just past the sigil
    var done = false
    while !done do
      // After a marker, an optional soft space introduces the heading.
      // §16 / E120: non-space immediately after marker is malformed; >1
      // leading space is malformed (unless empty heading).
      if !more || peek == LF || peek == CR then
        headings += t""
        done = true
      else if peek != SP then
        errorAt(Reason.BadTabulationHeading, head.startLine, lineCol + 1)
      else
        // peek == SP
        // Check if the next byte is also a space — if so, heading is empty
        // or malformed.
        val nextB = peekNext()
        if nextB == SP.toInt then
          // Two spaces after marker: empty heading? Look further.
          // Consume the two spaces; check if next is a sigil (start of next
          // column) or content (malformed).
          advance(); lineCol += 1  // first space
          advance(); lineCol += 1  // second space
          if more && peek == sigil then
            // Empty heading; new marker.
            markers += lineCol
            advance(); lineCol += 1
            headings += t""
            // continue outer loop for next column
          else if !more || peek == LF || peek == CR then
            // Empty heading at line end.
            headings += t""
            done = true
          else
            // E120: more spaces or non-sigil content after empty.
            errorAt(Reason.BadTabulationHeading, head.startLine, lineCol + 1)
        else
          // One soft space — heading text follows.
          advance(); lineCol += 1
          val mk = beginMark()
          var headingEnd = -1
          var stop = false
          while !stop do
            if !more || peek == LF || peek == CR then
              headingEnd = lineCol
              stop = true
            else if peek == sigil then
              errorAt(Reason.BadTabulationHeading, head.startLine, lineCol + 1)
            else if peek == SP then
              // Hard space check: two spaces in a row?
              val nb = peekNext()
              if nb == SP.toInt then
                headingEnd = lineCol
                stop = true
              else
                advance(); lineCol += 1
            else
              advance(); lineCol += 1

          headings += Text(sliceText(mk))
          // Now we're either at LF/CR/EOF or at a hard-space run before a
          // marker.
          if !more || peek == LF || peek == CR then done = true
          else
            // Consume hard spaces.
            while more && peek == SP do { advance(); lineCol += 1 }
            if more && peek == sigil then
              markers += lineCol
              advance(); lineCol += 1
              // Loop continues.
            else if !more || peek == LF || peek == CR then done = true
            else
              errorAt(Reason.BadTabulationHeading, head.startLine, lineCol + 1)

    consumeLineEnding()
    Tel.Tabulation(IArray.from(markers), IArray.from(headings))

  // §16.2 column-rule validation. The cursor must be parked at the row's
  // first content byte (past leading spaces). Walks the bytes up to LF/CR
  // checking that every hard-space (2+) run ends exactly at one of the
  // tabulation header's marker positions (E117) and that no non-final
  // column value exceeds its declared width (E119). Uses mark + cue so the
  // cursor is restored to the same position for the subsequent
  // parseCompoundLine call.
  private def validateTabulatedRowInline
    ( rowLeadingSpaces: Int, tabulation: Tel.Tabulation, lineNumber: Int )
  : Unit raises TelError = inHold:
    val markers = tabulation.markerOffsets
    val mk = beginMark()
    var col = rowLeadingSpaces
    var columnIdx = 0
    var phraseStart = rowLeadingSpaces
    var stopped = false

    while !stopped && more && peek != LF && peek != CR do
      val b = peek
      if b == SP then
        val runStart = col
        while more && peek == SP do { advance(); col += 1 }
        val runLen = col - runStart
        if runLen >= 2 then
          val sigilNext = more && peek == sigil
          val isRemark =
            if !sigilNext then false
            else
              ensureLookahead(3)
              val afterSigil = if pos + 1 < bufEnd then bytes(pos + 1) & 0xff else -1
              afterSigil == SP.toInt
              && (pos + 2 >= bufEnd || bytes(pos + 2) != SP)
          if isRemark then
            // Remark terminates column validation.
            while more && peek != LF && peek != CR do { advance(); col += 1 }
            stopped = true
          else
            if columnIdx >= 1 && columnIdx < markers.length - 1 then
              val phraseWidth = runStart - phraseStart
              val colMax = markers(columnIdx + 1) - markers(columnIdx) - 2
              if phraseWidth > colMax then
                errorAt(Reason.ColumnValueTooWide, lineNumber, phraseStart + 1)
            var foundIdx = -1
            var k = 1
            while k < markers.length && foundIdx < 0 do
              if markers(k) == col then foundIdx = k
              k += 1
            if foundIdx < 0 then
              errorAt(Reason.HardSpaceWrongPosition, lineNumber, col + 1)
            columnIdx = foundIdx
            phraseStart = col
      else
        advance()
        col += 1

    // Cue back so parseCompoundLine reads the row from the start.
    syncTo()
    cursor.cue(mk)
    syncFrom()

  // ── Source / literal atom dispatch ───────────────────────────────────────

  private def parseSourceOrLiteralAtomIfPresent(compoundLeadingSpaces: Int)
  : Optional[Tel.Atom] raises TelError =
    if head.eof || head.blank then Unset
    else
      val sourceIndent = compoundLeadingSpaces + 4
      val literalIndent = compoundLeadingSpaces + 6
      val first =
        if head.leadingSpaces == literalIndent then Optional(parseLiteralAtom(literalIndent))
        else if head.leadingSpaces == sourceIndent then Optional(parseSourceAtom(sourceIndent))
        else Unset

      // §10.4 / §11.1: at most one source / literal atom per compound.
      if first.present && !head.eof && !head.blank then
        if head.leadingSpaces == literalIndent
        then errorAt(Reason.DuplicateLiteral, head.startLine, 1)
        else if head.leadingSpaces == sourceIndent
        then errorAt(Reason.DuplicateSource, head.startLine, 1)

      first

  // ── Source atom ──────────────────────────────────────────────────────────

  // Cursor is at the first content byte of a source line (past the
  // sourceIndent leading spaces). head reflects that. Reads lines while
  // they have leadingSpaces >= sourceIndent (or are blanks followed by
  // more source / EOF). Each captured line contributes its content (with
  // first sourceIndent spaces stripped) plus a trailing "\n" to the
  // payload.
  private def parseSourceAtom(sourceIndent: Int): Tel.Atom.Source raises TelError =
    sb.setLength(0)

    var done = false
    while !done do
      if head.eof then
        // The virtual trailing blank at EOF (when the document ended with
        // LF) is claimed by the source atom rather than the enclosing
        // block — matches TelParser, where the source-atom loop's
        // probe-past-EOF emits one extra empty line.
        if documentEndsWithLf then
          sb.append('\n')
          documentEndsWithLf = false
        done = true
      else if head.blank then
        // Probe across blanks to see if more source follows. The mark must
        // survive several nested fillHead calls so the probe runs inside its
        // own hold; the cursor is free to compact again once the probe ends.
        done = inHold:
          val mk = beginMark()
          val firstBlankSnapshot = (head.leadingSpaces, head.indentLevels, head.blank,
                                    head.eof, head.startLine)
          val savedBoundary = prevLineWasBoundary
          val savedLineNo = lineNo
          var blanks = 0
          while !head.eof && head.blank do
            blanks += 1
            fillHead()
          val keep = head.eof || head.leadingSpaces >= sourceIndent
          if keep then
            // Emit `blanks` empty lines.
            var i = 0
            while i < blanks do { sb.append('\n'); i += 1 }
            false
          else
            // Rewind.
            syncTo()
            cursor.cue(mk)
            syncFrom()
            head.leadingSpaces = firstBlankSnapshot._1
            head.indentLevels  = firstBlankSnapshot._2
            head.blank         = firstBlankSnapshot._3
            head.eof           = firstBlankSnapshot._4
            head.startLine     = firstBlankSnapshot._5
            prevLineWasBoundary = savedBoundary
            lineNo = savedLineNo
            var i = 0
            while i < firstBlankSnapshot._1 && more && peek == SP do
              advance(); i += 1
            true
      else if head.leadingSpaces >= sourceIndent then
        // Read content: skip the first sourceIndent spaces — but wait, the
        // fillHead already consumed all leading spaces. We need to back up
        // and consume only sourceIndent of them, leaving (leadingSpaces -
        // sourceIndent) for the content. Alternative: pad the appended
        // string with the extra leading spaces, since they ARE part of the
        // content (per §14).
        //
        // §14: source-atom payload is each line's content with the first
        // `sourceIndent` characters stripped. So if leadingSpaces > sourceIndent,
        // the excess is part of the content as leading spaces.
        // Since fillHead consumed ALL the leadingSpaces, we lost the
        // (leadingSpaces - sourceIndent) excess. Recompose it.
        val excess = head.leadingSpaces - sourceIndent
        var i = 0
        while i < excess do { sb.append(' '); i += 1 }

        // Now read the line content (until LF/CR), tracking trailing spaces
        // so we can strip them. One hold per payload line so the cursor can
        // compact between lines.
        inHold:
          val mk = beginMark()
          while
            scanUntil2(LF, CR, TelParser.LfRepl, TelParser.CrRepl)
            pos == bufEnd && more
          do ()
          val payload = sliceText(mk)
          // Strip trailing spaces.
          var endIdx = payload.length
          while endIdx > 0 && payload.charAt(endIdx - 1) == ' ' do endIdx -= 1
          sb.append(payload, 0, endIdx)
          sb.append('\n')

          consumeLineEnding()
        fillHead()
      else done = true

    Tel.Atom.Source(Text(sb.toString))

  // ── Literal atom ─────────────────────────────────────────────────────────

  // Cursor is at the first content byte of the opening line (the delimiter
  // text). The delimiter is the rest of the opening line. The payload is
  // everything between the next LF and the first line whose content equals
  // the delimiter (at column 0 / flush left).
  private def parseLiteralAtom(literalIndent: Int): Tel.Atom.Literal raises TelError =
    val openingLine = head.startLine
    // Read the delimiter — opening line, terminated by LF or CR per the
    // document's line-ending mode. One hold for the opening line.
    val delimiter: String = inHold:
      val delimMk = beginMark()
      while
        scanUntil2(LF, CR, TelParser.LfRepl, TelParser.CrRepl)
        pos == bufEnd && more
      do ()
      val d = sliceText(delimMk)
      // The opening line is now consumed up to but not including the LF.
      consumeLineEnding()
      d

    sb.setLength(0)
    var done = false
    while !done do
      if !more then errorAt(Reason.UnclosedLiteral, openingLine, 1)
      // Read a line. Inside the literal payload, line endings are not
      // subject to the document's LF/CRLF mode (per §15 the payload is
      // verbatim, with CRLF normalised to LF). One hold per payload line so
      // the cursor can compact between lines.
      done = inHold:
        val lineMk = beginMark()
        while
          scanUntil1(LF, TelParser.LfRepl)
          pos == bufEnd && more
        do ()
        val raw = sliceText(lineMk)
        // The CR-stripped form is used only to recognise the closing
        // delimiter line; the payload itself preserves every byte between
        // structural LFs, including CR (§15).
        val line = if raw.length > 0 && raw.charAt(raw.length - 1) == '\r'
                   then raw.substring(0, raw.length - 1) else raw
        if line == delimiter then
          // Closing delimiter — the LF *before* the closing delimiter is the
          // delimiter's leading separator, not part of the payload. Strip the
          // trailing '\n' we appended for the previous payload line (if any),
          // then consume the optional EOL after the delimiter.
          if sb.length > 0 && sb.charAt(sb.length - 1) == '\n' then
            sb.setLength(sb.length - 1)
          if more then
            advance()  // consume LF (may be absent at EOF)
            lineNo += 1
            if !more then documentEndsWithLf = true
          true
        else
          // Payload line — must have an LF terminator; EOF here is E115.
          if !more then errorAt(Reason.UnclosedLiteral, openingLine, 1)
          advance()  // consume LF
          lineNo += 1
          if !more then documentEndsWithLf = true
          sb.append(raw)
          sb.append('\n')
          false

    fillHead()
    // §15: the payload is the verbatim bytes between structural LFs — CR is
    // preserved (only the LF before the closing delimiter is dropped, above).
    Tel.Atom.Literal(Text(delimiter), Text(sb.toString))

  // ── Compound line parsing ────────────────────────────────────────────────

  // Cursor is at the first content byte (past leading spaces). Reads the
  // compound line's keyword, atoms, and remark, depositing the keyword and
  // remark into `compoundLineKeyword` / `compoundLineRemark`. Atoms are
  // pushed onto the `scratchAtoms` stack; the caller (parseBlock) is
  // responsible for taking them — typically together with an optional
  // source/literal extra atom — and constructing the final Tel.Compound.
  private def parseCompoundLine(lineNumber: Int): Unit raises TelError = inHold:
    val isAtColumnZero = head.leadingSpaces == 0
    val mayBeMisplacedPragma = isAtColumnZero && hasConsumedNonBlankLine

    // First phrase = keyword. Read until space or LF/CR.
    val keyword = readKeyword()

    // E102: a `tel` or `tel …` line at column 0 after the first non-blank
    // line is a misplaced pragma. The valid pragma was already consumed
    // earlier by parsePragma; anything matching here is a violation.
    if mayBeMisplacedPragma && keyword == t"tel" then
      errorAt(Reason.PragmaNotFirst, lineNumber, 1)
    hasConsumedNonBlankLine = true

    var remark: Optional[Text] = Unset
    // Read atom bytes directly into the parser's atom-bytes arena. With
    // narrow holds, parseCompoundLine's hold has holdStart > 0, so refills
    // inside the line can compact and shift the cursor's `bytes` —
    // we therefore copy bytes out into our own buffer (the arena) as we
    // read them. Each Tel.Atom.Inline references its slice of the arena
    // (arenaArray, offset, length); no per-atom byte[] is allocated.
    var precedingSpaces = 0
    var hardSpaceMode = false
    var atomOpen = false

    inline def commit(): Unit =
      if atomOpen then
        val off = arenaInFlightOffset
        val len = endInFlightAtom()
        pushAtom(Tel.Atom.Inline.fromArena(atomArena, off, len, precedingSpaces))
        atomOpen = false

    var stopped = false
    while !stopped && remark.absent do
      if !more || peek == LF || peek == CR then stopped = true
      else
        val ch = peek
        if ch == SP then
          var run = 0
          while more && peek == SP do { advance(); run += 1 }
          if hardSpaceMode then
            if run >= 2 then
              commit()
              precedingSpaces = run
            else
              // Single space inside a hard-space-mode atom: the SP byte is
              // part of the atom's content. atomOpen is necessarily already
              // true here (hard-space-mode is only entered after a content
              // commit, and hard-space-mode + run==1 only fires while
              // reading content).
              appendToArena(SP)
              atomOpen = true
          else
            if run == 1 then
              commit()
              precedingSpaces = 1
            else
              commit()
              precedingSpaces = run
              hardSpaceMode = true
        else if ch == sigil && !atomOpen then
          // Could be remark introducer: sigil + soft space + non-space.
          ensureLookahead(3)
          val afterSigil = if pos + 1 < bufEnd then bytes(pos + 1) & 0xff else -1
          val softSpaceAfter =
            afterSigil == SP.toInt
            && (pos + 2 >= bufEnd || bytes(pos + 2) != SP)
          if softSpaceAfter then
            // Consume sigil + space, then read remark text until LF/CR.
            advance()  // sigil
            advance()  // space
            val mk = beginMark()
            while more && peek != LF && peek != CR do advance()
            remark = Text(sliceText(mk))
          else
            beginInFlightAtom()
            appendToArena(ch)
            advance()
            atomOpen = true
        else
          // Read a run of non-space, non-sigil, non-LF, non-CR bytes,
          // copying them into the parser's atom arena. After a refill
          // compacts the cursor buffer, the source content stays valid
          // because we read it out into our own arena before the next
          // refill can fire.
          val runStart = pos
          while pos < bufEnd
                && bytes(pos) != SP
                && bytes(pos) != LF
                && bytes(pos) != CR
                && (atomOpen || bytes(pos) != sigil)
          do pos += 1
          val runLen = pos - runStart
          if runLen > 0 then
            if !atomOpen then beginInFlightAtom()
            appendToArenaRange(bytes, runStart, runLen)
            atomOpen = true
          else
            // Defensive: only reachable if the outer guards were ever
            // relaxed. Treat the byte at `pos` as one atom byte.
            if !atomOpen then beginInFlightAtom()
            appendToArena(ch)
            advance()
            atomOpen = true

    commit()

    // E108: a non-blank compound line must not end with a space character.
    // Inside the outer `hold`, the buffer byte just before the current pos
    // is still resident — peek it directly. (`pos > 0` because we have
    // consumed at least the keyword.)
    if remark.absent && more && (peek == LF || peek == CR)
       && pos > 0 && bytes(pos - 1) == SP
    then errorAt(Reason.TrailingSpaces, lineNumber, head.leadingSpaces + 1)

    consumeLineEnding()
    compoundLineKeyword = keyword
    compoundLineRemark  = remark

  // Read a keyword from the current position. The keyword runs until SP, LF,
  // CR, or EOF. Returns the interned `Text`. Uses the 64-slot fingerprint
  // cache for ≤8-byte keywords (allocation-free on hit). The keyword's
  // start position is remembered via a `cursor.mark` (rather than a raw
  // buffer offset) because the per-leaf hold under which `readKeyword`
  // runs has `holdStart > 0`, so a refill inside the loop can compact the
  // buffer and shift live content toward index 0 — the absolute-position
  // mark survives that, a buffer-offset would not.
  private def readKeyword(): Text raises TelError =
    val startMark = beginMark()
    var low:  Long = 0L
    var high: Long = 0L
    var len = 0
    while
      while pos < bufEnd && bytes(pos) != SP && bytes(pos) != LF && bytes(pos) != CR do
        val b = bytes(pos)
        if len < 4 then
          low |= (b & 0xff).toLong << (len * 8)
        else if len < 8 then
          high |= (b & 0xff).toLong << ((len - 4) * 8)
        len += 1
        pos += 1
      pos == bufEnd && more
    do ()

    if len == 0 then t""
    else if len > 8 then
      Text(sliceText(startMark))
    else
      val hash = ((low ^ (low >>> 32)) ^ (high ^ (high >>> 17))).toInt
      var slot = hash & 0x3F
      var probes = 0
      var result: String = null
      while result == null && probes < 4 do
        val existing = keyCache(slot)
        if existing == null then
          val s = sliceText(startMark)
          keyCache(slot) = s
          keyCacheLow(slot) = low
          keyCacheHigh(slot) = high
          result = s
        else if keyCacheLow(slot) == low && keyCacheHigh(slot) == high then
          result = existing
        else
          slot = (slot + 1) & 0x3F
          probes += 1

      if result != null then Text(result)
      else Text(sliceText(startMark))
