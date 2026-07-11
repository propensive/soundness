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
package jacinta

import language.dynamics
import language.experimental.pureFunctions
import language.experimental.separationChecking

import scala.collection.mutable as scm
import scala.compiletime.*

import anticipation.*
import contingency.*
import denominative.*
import fulminate.*
import gossamer.*
import hieroglyph.*
import prepositional.*
import proscenium.*
import rudiments.*
import vacuous.*
import zephyrine.*

private[jacinta] object Parser:
  private[jacinta] type Raw =
    Long | Int | Double | Bcd | String | IArray[Any] |
      Array[Long] | Array[Int] |
      Boolean | Json.JsonNull.type | Unset

  private inline val NumZero       = 0
  private inline val NumInt        = 1
  private inline val NumAfterDot   = 2
  private inline val NumFrac       = 3
  private inline val NumAfterE     = 4
  private inline val NumAfterESign = 5
  private inline val NumExp        = 6

  // `parseArray` number-container modes (see comment on `parseArray`).
  private[jacinta] inline val ModeUndecided = 0
  private[jacinta] inline val ModeBoxed     = 1
  private[jacinta] inline val ModeBcdLong   = 2
  private[jacinta] inline val ModeBcdInt    = 3

  // ASCII bytes of the JSON keyword literals, packed little-endian: byte at
  // offset i goes into bits (i*8)..(i*8+7). Four-byte literals (`true`,
  // `null`) fit an `Int`; `false` spans 5 bytes and uses a `Long` whose
  // top 24 bits are unused (the parser reads exactly 5 bytes, so those
  // bits aren't loaded). Allows the keyword check to collapse from a
  // four-or-five-step `expect` chain into a single integer compare.
  //
  //   't' 'r' 'u' 'e'  → 0x65_75_72_74  (read LSB-first)
  //   'n' 'u' 'l' 'l'  → 0x6C_6C_75_6E
  //   'f' 'a' 'l' 's' 'e' (low 40 bits) → 0x0000_0065_736C_6166
  private inline val TrueWord  = 0x6575_7274
  private inline val NullWord  = 0x6C6C_756E
  private inline val FalseWord = 0x0000_0065_736C_6166L

  private val TenPow: Array[Double] = Array.tabulate(23): i =>
    var p = 1.0
    var n = i
    while n > 0 do { p *= 10.0; n -= 1 }
    p

  // Byte-class table for `parseString`'s fast-scan loop. Entry `i` is
  // 1 when byte `i` keeps the scan going (printable ASCII other than
  // `"` and `\`), 0 when it ends it (`"`, `\`, any control byte, or
  // any byte ≥ 128 — UTF-8 continuation or lead). Single load + compare
  // per byte beats the three-compare chain
  // `b >= 32 && b != Quote && b != Backslash`. The 128–255 range stops
  // for the same reason the original signed-Byte `b >= 32` test did:
  // those bytes appear as negative when read as signed and need the
  // multi-byte UTF-8 decoder in `tail`, not the fast slice path.
  // Immutable (frozen) so class methods can index it without a global-mutable uses clause.
  private val StringScanContinue: IArray[Byte] =
    val arr = new Array[Byte](256)
    var i = 0

    while i < 256 do
      arr(i) =
        if i >= 32 && i < 128 && i != 0x22 && i != 0x5C
        then 1.toByte
        else 0.toByte

      i += 1

    arr.immutable(using Unsafe)

  private val pool: ThreadLocal[Parser] =
    new ThreadLocal[Parser]:
      override def initialValue(): Parser = new Parser

  def parse(source: Data, mode: NumberMode = NumberMode.Full): Raw raises ParseError =
    val parser = pool.get.nn.asInstanceOf[Parser^]
    parser.tracking = false
    parser.resetData(source)
    parser.holes = false
    parser.numberMode = mode
    parser.parse()

  def parse(source: Data, holes: Boolean, mode: NumberMode): Raw raises ParseError =
    val parser = pool.get.nn.asInstanceOf[Parser^]
    parser.tracking = false
    parser.resetData(source)
    parser.holes = holes
    parser.numberMode = mode
    parser.parse()

  def parse(input: Iterator[Data], mode: NumberMode): Raw raises ParseError =
    val parser = pool.get.nn.asInstanceOf[Parser^]
    parser.tracking = false
    parser.resetIterator(input)
    parser.holes = false
    parser.numberMode = mode
    parser.parse()

  def parse(input: Iterator[Data], holes: Boolean, mode: NumberMode): Raw raises ParseError =
    val parser = pool.get.nn.asInstanceOf[Parser^]
    parser.tracking = false
    parser.resetIterator(input)
    parser.holes = holes
    parser.numberMode = mode
    parser.parse()

  def parseTracked(source: Data, mode: NumberMode = NumberMode.Full)
  :   (Raw, IArray[Int]) raises ParseError =

    val parser = pool.get.nn.asInstanceOf[Parser^]
    parser.tracking = true
    parser.resetData(source)
    parser.holes = false
    parser.numberMode = mode
    val raw = parser.parse()
    (raw, parser.rootIndex.nn)

  def parseTracked(input: Iterator[Data], mode: NumberMode)
  :   (Raw, IArray[Int]) raises ParseError =

    val parser = pool.get.nn.asInstanceOf[Parser^]
    parser.tracking = true
    parser.resetIterator(input)
    parser.holes = false
    parser.numberMode = mode
    val raw = parser.parse()
    (raw, parser.rootIndex.nn)

  def parse(input: Stream[Data] over Credit, mode: NumberMode): Raw raises ParseError =
    val parser = pool.get.nn.asInstanceOf[Parser^]
    parser.tracking = false
    parser.resetStream(input)
    parser.holes = false
    parser.numberMode = mode
    parser.parse()

  def parseTracked(input: Stream[Data] over Credit, mode: NumberMode)
  :   (Raw, IArray[Int]) raises ParseError =

    val parser = pool.get.nn.asInstanceOf[Parser^]
    parser.tracking = true
    parser.resetStream(input)
    parser.holes = false
    parser.numberMode = mode
    val raw = parser.parse()
    (raw, parser.rootIndex.nn)

// The parser is a stateful capability: one exclusive owner per instance (guaranteed by
// the per-thread pool), with every state-mutating method classified `update`. This unit
// is separation-checked; consumers (capture-checked only) interact through the exclusive
// reference the pool hands out.
private[jacinta] final class Parser extends caps.ExclusiveCapability, caps.Stateful:
  import scala.annotation.switch
  import scala.collection.mutable.ArrayBuffer
  import Json.Ast.AsciiByte.*
  import Json.Ast.{Issue, Position}

  import Parser.*

  // The cursor remains the source of truth at refill, mark, slice and error
  // points, but for the per-byte hot loops (`peek`, `advance`, `more`) the
  // parser maintains its own snapshot of the current buffer reference and
  // read position. Keeping `pos` and `bytes` as parser fields (rather than
  // accessing them through the cursor on every byte) gives the JIT the
  // freedom to keep both in registers across long inner loops, recovering
  // most of the per-byte cost of the Direct/Streaming split removed during
  // the substrate unification.
  //
  // Invariant: between `syncTo()` and `syncFrom()` calls, `pos` is the
  // authoritative read position; `cursor.unsafePos` is allowed to lag.
  // Whenever a cursor operation that depends on `pos` is performed (refill
  // via `more`'s slow path, mark, slice, error reporting, BOM probing) the
  // parser pushes `pos` to the cursor first, then refreshes its snapshot
  // from the cursor afterwards — refill may compact the buffer, reallocate
  // it, or reset `pos`.
  private var cursor:    Cursor[Data, {}]^    = null.asInstanceOf[Cursor[Data, {}]^]
  private var heldToken: Cursor.Held | Null = null

  // Parser-local snapshot (see comment above).
  private var bytes:  Array[Byte] = null.asInstanceOf[Array[Byte]]
  private var pos:    Int = 0
  private var bufEnd: Int = 0

  protected[jacinta] var holes: Boolean = false

  // When true, the cursor is built with a line-feed-tracking `Lineation` so
  // `cursor.line` / `cursor.column` reflect real source coordinates, and the
  // parser emits a parallel `PositionIndex`.
  protected[jacinta] var tracking: Boolean = false

  // Storage shape `parseNumber` uses for each parsed JSON number. See
  // `NumberMode` for semantics. Reset before each `parse()` call.
  protected[jacinta] var numberMode: NumberMode = NumberMode.Full

  protected var arraySize:           Int = 16
  protected var chars:               Array[Char]^ = new Array(arraySize)
  protected var stringCursor:        Int = 0
  protected var arrayBufferId:       Int = -1
  protected val arrayBuffers:        ArrayBuffer[ArrayBuffer[Any]]  = ArrayBuffer.empty
  protected var bcdLongBufferId:     Int = -1
  protected val bcdLongBuffers:      ArrayBuffer[ArrayBuffer[Long]] = ArrayBuffer.empty
  protected var bcdIntBufferId:      Int = -1
  protected val bcdIntBuffers:       ArrayBuffer[ArrayBuffer[Int]]  = ArrayBuffer.empty
  protected var indexBufferId:       Int = -1
  protected val indexBuffers:        ArrayBuffer[ArrayBuffer[Int]]  = ArrayBuffer.empty

  // Finalised root-level position index produced by the previous `parse()`
  // call when `tracking` was on. Reset to `null` at the start of every parse.
  protected[jacinta] var rootIndex: IArray[Int] | Null = null

  // Local-buffer offset up to which `cursor.lineNo` / `cursor.columnNo`
  // have been brought up to date. The hot-loop `syncTo()` bypasses the
  // cursor's lineation tracking via `unsafeAdvanceBy`, so the parser
  // keeps track of how far ahead `cursor.pos` has been pushed and
  // catches lineation up here only at tracking-mode capture points and
  // before any refill discards consumed bytes.
  private var lineationPos: Int = 0

  // Small open-addressed cache of recently-seen object keys. Index is
  // `hash & (KeyCacheSize - 1)`; on a hash collision the existing entry is
  // overwritten. Persisted across parses (per-thread, since the parser
  // pool is `ThreadLocal`) so repeating JSON shapes hit the cache from
  // the first key onward. Only consulted on the fast ASCII path with no
  // escapes — escaped keys still go through `parseString`'s slow tail.
  // Small open-addressed object-key cache. Keys of up to 16 ASCII bytes are
  // packed losslessly into a pair of Longs (low = bytes 0–7, high = bytes
  // 8–15, both LSB-first, with trailing positions zero) — and since JSON
  // object keys can't contain `\0` (the parser fast-path rejects bytes
  // < 32), every distinct ASCII key produces a distinct (low, high) pair.
  // Lookup is then a pair of Long equality checks — *no* byte-by-byte
  // comparison, and *no* hash-collision false positives. Keys longer than
  // 16 bytes (rare for typical record-shape JSON) bypass the cache and
  // allocate normally.
  private inline val KeyCacheSize = 256
  private inline val KeyCacheMaxBytes = 16
  private val keyCache:    Array[String | Null]^ = new Array(KeyCacheSize)
  private val keyCacheLow:  Array[Long]^        = new Array(KeyCacheSize)
  private val keyCacheHigh: Array[Long]^        = new Array(KeyCacheSize)

  update def resetData(input: Data): Unit =
    if tracking then
      import zephyrine.lineation.linefeedByte
      val fresh = Cursor[Data](input)
      cursor = fresh
    else
      import Lineation.untrackedData
      val fresh = Cursor[Data](input)
      cursor = fresh
    syncFrom()
    stringCursor = 0
    arrayBufferId = -1
    bcdLongBufferId = -1
    bcdIntBufferId = -1
    indexBufferId = -1
    rootIndex = null
    heldToken = null

  update def resetIterator(input: Iterator[Data]): Unit =
    if tracking then
      import zephyrine.lineation.linefeedByte
      val fresh = Cursor[Data](input)
      cursor = fresh
    else
      import Lineation.untrackedData
      val fresh = Cursor[Data](input)
      cursor = fresh
    syncFrom()
    stringCursor = 0
    arrayBufferId = -1
    bcdLongBufferId = -1
    bcdIntBufferId = -1
    indexBufferId = -1
    rootIndex = null
    heldToken = null

  update def resetStream(input: Stream[Data] over Credit): Unit =
    if tracking then
      import zephyrine.lineation.linefeedByte
      val fresh = Cursor[Data](input)
      cursor = fresh
    else
      import Lineation.untrackedData
      val fresh = Cursor[Data](input)
      cursor = fresh
    syncFrom()
    stringCursor = 0
    arrayBufferId = -1
    bcdLongBufferId = -1
    bcdIntBufferId = -1
    indexBufferId = -1
    rootIndex = null
    heldToken = null




  // ──────────────────────────────────────────────────────────────────────────
  // Substrate.

  // Push the parser's local `pos` back to the cursor. Required before any
  // cursor operation that consults `pos` (mark, slice, refill, position).
  // This must stay zero-branch on the hot path; tracking-mode callers
  // separately invoke `reconcileLineation()` when they need accurate
  // `cursor.line` / `cursor.column`.
  private inline update def syncTo(): Unit =
    cursor.unsafeAdvanceBy(pos - cursor.unsafePos(using Unsafe))(using Unsafe)

  // Walk the buffer bytes between the last lineation-reconciled position
  // and `cursor.unsafePos`, bumping `cursor.lineNo` / `cursor.columnNo`
  // accordingly. Called at tracking-mode capture points (`parseValue`,
  // `parseObject`) and before any refill in `moreSlow()` so that
  // consumed bytes are accounted for before they're discarded.
  private update def reconcileLineation(): Unit =
    val end = cursor.unsafePos(using Unsafe)

    if lineationPos < end then
      var i = lineationPos
      var newlines = 0
      var lastNewlineAt = -1

      while i < end do
        if bytes(i) == Newline then
          newlines += 1
          lastNewlineAt = i

        i += 1

      if newlines > 0 then
        cursor.unsafeBumpLine(newlines)(using Unsafe)
        cursor.unsafeSetColumn(end - lastNewlineAt - 1)(using Unsafe)
      else
        cursor.unsafeBumpColumn(end - lineationPos)(using Unsafe)

      lineationPos = end

  // Refresh the parser's snapshot from the cursor. Required after any
  // cursor operation that may have changed the buffer reference, the read
  // position, or the write end (refill, cue).
  //
  // `lineationPos` is re-anchored to the cursor's current position because
  // refill compacts the buffer (bytes 0..old-pos are discarded; subsequent
  // walks would read different data).
  private inline update def syncFrom(): Unit =
    bytes  = cursor.buffer(using Unsafe)
    pos    = cursor.unsafePos(using Unsafe)
    bufEnd = cursor.unsafeWriteEnd(using Unsafe)
    lineationPos = pos

  protected inline update def more: Boolean = pos < bufEnd || moreSlow()

  // Out-of-line slow path so the inline budget for `more` stays small enough
  // for the JIT to keep `pos < bufEnd` as a single register comparison in
  // hot loops.
  //
  // In tracking mode the parser later reads `cursor.position` to compute
  // descriptor lengths; if `cursor.refill()` compacts the buffer here we
  // must re-anchor the parser's local `pos` against the new buffer scale
  // even when there's no more data, otherwise subsequent `syncTo()` calls
  // would advance the cursor by a now-stale delta. Lineation also needs
  // to be reconciled before compaction discards the consumed bytes.
  private update def moreSlow(): Boolean =
    syncTo()
    if tracking then reconcileLineation()

    if cursor.more then { syncFrom(); true }
    else
      if tracking then syncFrom()
      false

  protected inline update def peek: Byte = bytes(pos)

  protected inline update def advance(): Unit = pos += 1

  protected update def errorAt(issue: Issue, start: Optional[Cursor.Mark] = Unset)
    ( using Tactic[ParseError] )
  :   Nothing =

    syncTo()
    val end = cursor.position.n0
    val offset: Optional[Int] = start.let(_.absolute.toInt)
    val length: Optional[Int] = start.let: mark => end - mark.absolute.toInt
    abort(ParseError(Json.Ast, Position(0, end, offset = offset, length = length), issue))

  // A `Region` is just a `Cursor.Mark` (an absolute `Long` position). With
  // the single-buffer model there's no need to remember the starting block
  // for boundary detection.
  type Region = Cursor.Mark

  protected inline update def begin(): Cursor.Mark =
    syncTo()
    cursor.mark(using heldToken.nn)

  protected inline update def slice(start: Cursor.Mark): String =
    syncTo()
    val end = cursor.mark(using heldToken.nn)

    cursor.slice(start, end): (storage, off, len) =>
      val arr = storage.asInstanceOf[Array[Byte]]
      new String(arr, off, len, java.nio.charset.StandardCharsets.US_ASCII)

  protected inline update def appendRegionToBuffer(start: Cursor.Mark): Unit =
    syncTo()
    val end = cursor.mark(using heldToken.nn)

    cursor.slice(start, end): (storage, off, len) =>
      if len > 0 then
        val arr = storage.asInstanceOf[Array[Byte]]
        ensureStringSpace(len)
        var i = 0

        while i < len do
          chars(stringCursor + i) = (arr(off + i) & 0xFF).toChar
          i += 1

        stringCursor += len

  // BOM probing runs once per parse, and almost no JSON inputs actually
  // start with one. The fast path peeks the first byte directly from the
  // parser-local snapshot — if it isn't `0xEF` (the BOM lead byte), no
  // valid BOM is possible and we return immediately, skipping the
  // `cursor.hold` token allocation, the mark/cue, and the
  // `syncTo`/`syncFrom` round-trip. The slow path retains full BOM
  // semantics (including rolling back if only the first byte matches).
  protected update def bom(): Unit =
    if !more || peek != -17.toByte then return
    syncTo()

    cursor.hold:
      val mk = cursor.mark

      val bom =
        cursor.more && cursor.datum(using Unsafe) == -17.toByte &&
          { cursor.next(); cursor.more && cursor.datum(using Unsafe) == -69.toByte } &&
          { cursor.next(); cursor.more && cursor.datum(using Unsafe) == -65.toByte }

      if bom then cursor.next() else cursor.cue(mk)

    syncFrom()

  protected inline update def holding[result](inline action: => result): result =
    syncTo()

    cursor.hold:
      heldToken = summon[Cursor.Held]
      try action finally heldToken = null

  // ──────────────────────────────────────────────────────────────────────────
  // String buffer plumbing (unchanged).

  protected inline update def resetString(): Unit = stringCursor = 0

  protected inline update def ensureStringSpace(n: Int): Unit =
    while stringCursor + n > arraySize do arraySize *= 2

    if chars.length < arraySize then
      val newArr = new Array[Char](arraySize)
      System.arraycopy(chars, 0, newArr, 0, stringCursor)
      chars = newArr

  protected inline update def appendChar(char: Char): Unit =
    if stringCursor == arraySize then
      arraySize *= 2
      val newArray = new Array[Char](arraySize)
      System.arraycopy(chars, 0, newArray, 0, stringCursor)
      chars = newArray

    chars(stringCursor) = char
    stringCursor += 1

  protected inline update def getString(): String = String(chars, 0, stringCursor)

  protected inline update def getArrayBuffer(): ArrayBuffer[Any] =
    arrayBufferId += 1

    if arrayBuffers.length <= arrayBufferId then
      val newBuffer = ArrayBuffer.empty[Any]
      arrayBuffers += newBuffer
      newBuffer
    else
      val buffer = arrayBuffers(arrayBufferId)
      buffer.clear()
      buffer

  protected inline update def relinquishArrayBuffer(): Unit = arrayBufferId -= 1

  protected inline update def getBcdLongBuffer(): ArrayBuffer[Long] =
    bcdLongBufferId += 1

    if bcdLongBuffers.length <= bcdLongBufferId then
      val newBuffer = ArrayBuffer.empty[Long]
      bcdLongBuffers += newBuffer
      newBuffer
    else
      val buffer = bcdLongBuffers(bcdLongBufferId)
      buffer.clear()
      buffer

  protected inline update def relinquishBcdLongBuffer(): Unit = bcdLongBufferId -= 1

  protected inline update def getBcdIntBuffer(): ArrayBuffer[Int] =
    bcdIntBufferId += 1

    if bcdIntBuffers.length <= bcdIntBufferId then
      val newBuffer = ArrayBuffer.empty[Int]
      bcdIntBuffers += newBuffer
      newBuffer
    else
      val buffer = bcdIntBuffers(bcdIntBufferId)
      buffer.clear()
      buffer

  protected inline update def relinquishBcdIntBuffer(): Unit = bcdIntBufferId -= 1

  protected inline update def getIndexBuffer(): ArrayBuffer[Int] =
    indexBufferId += 1

    if indexBuffers.length <= indexBufferId then
      val newBuffer = ArrayBuffer.empty[Int]
      indexBuffers += newBuffer
      newBuffer
    else
      val buffer = indexBuffers(indexBufferId)
      buffer.clear()
      buffer

  protected inline update def relinquishIndexBuffer(): Unit = indexBufferId -= 1

  // Assemble a composite (array or object) descriptor in `indexOut`. `scratch`
  // holds the concatenated entry descriptors back-to-back (an entry is a
  // child for arrays, or `[keyLine, keyColumn, keyLength, value descriptor]`
  // for objects). `ends(i)` is the position in `scratch` immediately after
  // the i-th entry, so the i-th entry's size is `ends(i) - ends(i-1)`.
  protected update def emitCompositeDescriptor
    ( indexOut:    ArrayBuffer[Int],
      scratch:     ArrayBuffer[Int],
      ends:        ArrayBuffer[Int],
      startLine:   Int,
      startColumn: Int,
      startMark:   Long )
  :   Unit =

    syncTo()
    val n = ends.length
    val sourceLength = (cursor.position.n0 - startMark).toInt
    val sizeSlot = indexOut.length

    indexOut += 0
    indexOut += startLine
    indexOut += startColumn
    indexOut += sourceLength
    indexOut += n

    val headerSize = 5 + n
    var i = 0

    while i < n do
      val childStart = if i == 0 then 0 else ends(i - 1)
      indexOut += headerSize + childStart
      i += 1

    indexOut ++= scratch
    indexOut(sizeSlot) = indexOut.length - sizeSlot

  // ──────────────────────────────────────────────────────────────────────────
  // Parser body (unchanged from the previous abstract base).

  protected inline update def must()(using Tactic[ParseError]): Byte =
    if more then peek else errorAt(Issue.PrematureEnd)

  protected inline update def next()(using Tactic[ParseError]): Byte =
    advance()
    if more then peek else errorAt(Issue.PrematureEnd)

  private update def skip(): Unit =
    while
      more && {
        val ch = peek
        ch == Space || ch == Tab || ch == Newline || ch == Return
      }
    do advance()

  private update def fromHex(ch: Byte)(using Tactic[ParseError]): Int =
    if ch <= Num9 && ch >= Num0 then ch - Num0
    else if ch <= UpperF && ch >= UpperA then ch - UpperA + 10
    else if ch <= LowerF && ch >= LowerA then ch - LowerA + 10
    else errorAt(Issue.ExpectedHexDigit(ch.toChar))

  private update def parseUnicode()(using Tactic[ParseError]): Char =
    var acc = fromHex(next()) << 12
    acc |= fromHex(next()) << 8
    acc |= fromHex(next()) << 4
    acc |= fromHex(next())
    acc.toChar

  private update def parseString()(using Tactic[ParseError]): String = holding:
    val region = begin()

    // Fast scan for plain printable ASCII that needs no escape handling.
    // The 256-entry `StringScanContinue` table collapses the three
    // comparisons (`>= 32`, `!= "`, `!= \`) into a single load + compare.
    while more && StringScanContinue(peek & 0xFF) != 0 do advance()

    if !more then errorAt(Issue.PrematureEnd, region)

    if peek == Quote then slice(region).also(advance())
    else tail(region)

  // Like `parseString`, but routes the no-escape result through `keyCache`.
  // The key's bytes are packed losslessly into a `(packedLow, packedHigh)`
  // Long pair *after* the scan, inside the `cursor.slice` callback where
  // the byte array is directly addressable — this keeps the per-byte
  // inner loop identical to `parseString` (no hash/packing overhead per
  // byte) and confines the cache work to a single per-key step.
  //
  // Every distinct ≤16-byte ASCII key produces a distinct (low, high)
  // pair (JSON keys can't contain `\0`, so trailing-zero padding can't
  // alias another key), so the lookup needs only two Long equality
  // checks — no byte-by-byte comparison and no hash-collision false
  // positives. Keys longer than 16 bytes bypass the cache.
  private update def parseObjectKey()(using Tactic[ParseError]): String = holding:
    val region = begin()

    while more && StringScanContinue(peek & 0xFF) != 0 do advance()

    if !more then errorAt(Issue.PrematureEnd, region)

    if peek != Quote then tail(region)
    else
      syncTo()
      val end = cursor.mark(using heldToken.nn)

      val out = cursor.slice(region, end): (storage, off, len) =>
        val arr = storage.asInstanceOf[Array[Byte]]

        if len > KeyCacheMaxBytes then
          new String(arr, off, len, java.nio.charset.StandardCharsets.US_ASCII)
        else
          val packedLow  = packBytes(arr, off, math.min(len, 8))
          val packedHigh = if len > 8 then packBytes(arr, off + 8, len - 8) else 0L

          val idx        = ((packedLow.toInt ^ (packedLow >>> 32).toInt) ^
            (packedHigh.toInt ^ (packedHigh >>> 32).toInt)) & (KeyCacheSize - 1)

          val cached     = keyCache(idx)

          if cached != null && keyCacheLow(idx) == packedLow &&
            keyCacheHigh(idx) == packedHigh
          then cached
          else
            val fresh = new String(arr, off, len, java.nio.charset.StandardCharsets.US_ASCII)
            keyCache(idx)     = fresh
            keyCacheLow(idx)  = packedLow
            keyCacheHigh(idx) = packedHigh
            fresh

      advance()
      out

  // Pack up to 8 bytes from `arr[off..off+n)` into a Long, LSB-first
  // (byte at offset `i` goes to bit position `i*8`). Bytes beyond `n` are
  // zero — combined with the fact that JSON keys can't contain `\0`,
  // this means two distinct byte sequences of length ≤ 8 always pack to
  // distinct Longs.
  private inline update def packBytes(arr: Array[Byte], off: Int, n: Int): Long =
    var out: Long = 0L
    var i = 0

    while i < n do
      out = out | ((arr(off + i) & 0xFFL) << (i << 3))
      i += 1

    out

  private update def tail(start: Region): String raises ParseError =
    resetString()
    appendRegionToBuffer(start)

    var continue = true

    while continue do
      if !more then errorAt(Issue.PrematureEnd, start)
      val ch = peek

      ch match
        case Quote =>
          continue = false

        case Tab | Newline | Return =>
          errorAt(Issue.InvalidWhitespace, start)

        case Backslash =>
          advance()
          if !more then errorAt(Issue.PrematureEnd, start)

          (peek: @switch) match
            case Quote     => appendChar('"')
            case Slash     => appendChar('/')
            case Backslash => appendChar('\\')
            case LowerB    => appendChar('\b')
            case LowerF    => appendChar('\f')
            case LowerN    => appendChar('\n')
            case LowerR    => appendChar('\r')
            case LowerT    => appendChar('\t')
            case LowerU    => appendChar(parseUnicode())
            case bad       => errorAt(Issue.IncorrectEscape(bad.toChar), start)

        case _ =>
          if ch == 0 && holes then appendChar(' ')
          else ((ch >> 5): @switch) match
            case 0                 => errorAt(Issue.NotEscaped(ch.toChar), start)
            case 1 | 2 | 3 | 4 | 5 => appendChar(ch.toChar)

            case _ =>
              if (ch & 0xE0) == 0xC0 then
                var char: Int = (ch & 0x1F) << 6
                char |= next() & 0x3F
                appendChar(char.toChar)
              else if (ch & 0xF0) == 0xE0 then
                var char: Int = (ch & 0x0F) << 12
                char |= (next() & 0x3F) << 6
                char |= next() & 0x3F
                appendChar(char.toChar)
              else if (ch & 0xF8) == 0xF0 then
                var char: Int = (ch & 0x07) << 18
                char |= (next() & 0x3F) << 12
                char |= (next() & 0x3F) << 6
                char |= next() & 0x3F
                appendChar(char.toChar)

      if continue then advance()

    advance()
    getString()

  protected inline def expect(byte: Byte, issue: Issue): Unit raises ParseError =
    if next() != byte then errorAt(issue)

  private def parseFalse(): false raises ParseError =
    // Fast path: 5 bytes available in the local buffer. Pack them into a
    // Long and compare against `FalseWord` in one step. Falls back to the
    // byte-by-byte `expect` chain only at a buffer boundary.
    if pos + 5 <= bufEnd then
      val word: Long =
        (bytes(pos)     & 0xFFL)         |
          ((bytes(pos + 1) & 0xFFL) <<  8) |
          ((bytes(pos + 2) & 0xFFL) << 16) |
          ((bytes(pos + 3) & 0xFFL) << 24) |
          ((bytes(pos + 4) & 0xFFL) << 32)

      if word != FalseWord then errorAt(Issue.ExpectedFalse)
      pos += 5
      false
    else
      expect(LowerA, Issue.ExpectedFalse)
      expect(LowerL, Issue.ExpectedFalse)
      expect(LowerS, Issue.ExpectedFalse)
      expect(LowerE, Issue.ExpectedFalse)
      advance()
      false

  private def parseTrue(): true raises ParseError =
    if pos + 4 <= bufEnd then
      val word: Int =
        (bytes(pos)     & 0xFF)         |
          ((bytes(pos + 1) & 0xFF) <<  8) |
          ((bytes(pos + 2) & 0xFF) << 16) |
          ((bytes(pos + 3) & 0xFF) << 24)

      if word != TrueWord then errorAt(Issue.ExpectedTrue)
      pos += 4
      true
    else
      expect(LowerR, Issue.ExpectedTrue)
      expect(LowerU, Issue.ExpectedTrue)
      expect(LowerE, Issue.ExpectedTrue)
      advance()
      true

  private def parseNull(): Json.JsonNull.type raises ParseError =
    if pos + 4 <= bufEnd then
      val word: Int =
        (bytes(pos)     & 0xFF)         |
          ((bytes(pos + 1) & 0xFF) <<  8) |
          ((bytes(pos + 2) & 0xFF) << 16) |
          ((bytes(pos + 3) & 0xFF) << 24)

      if word != NullWord then errorAt(Issue.ExpectedNull)
      pos += 4
      Json.JsonNull
    else
      expect(LowerU, Issue.ExpectedNull)
      expect(LowerL, Issue.ExpectedNull)
      expect(LowerL, Issue.ExpectedNull)
      advance()
      Json.JsonNull

  private def parseNumber(first: Int, negative: Boolean, bcdOnly: Boolean = false)
  :   Double | Long | Bcd raises ParseError =

    var content: Long = first.toLong
    var nibbles: Int = 1
    var bcdValid: Boolean = true
    var floating: Boolean = false
    var continue: Boolean = true
    var state: Int = if first == 0 then NumZero else NumInt
    var bcdBuilder: Bcd.Builder | Null = null

    // When the in-Long fast path overflows (the 16th nibble is about to be
    // appended), behavior depends on `numberMode`:
    //   - Full   : seed a `Bcd.Builder` with the existing 15 nibbles plus the
    //              overflowing one and continue accumulation there. Preserves
    //              full precision; one heap allocation per overflowing number.
    //   - Bcd    : drop the overflowing nibble and continue scanning the
    //              number's remaining digits without recording them. The
    //              accumulator stays the truncated first 15 nibbles. No
    //              heap allocation.
    //   - Double : same drop-on-overflow behavior as `Bcd`; the construction
    //              site then derives a `Double` from the truncated nibbles.
    inline def fallback(extraNibble: Int): Unit =
      if bcdOnly || numberMode == NumberMode.Full then
        val b = new Bcd.Builder
        b.seedFromLong(content, nibbles)
        b.add(extraNibble)
        bcdBuilder = b
        bcdValid = false

    inline def appendNibble(n: Int): Unit =
      if bcdValid then
        if nibbles >= 15 then fallback(n)
        else
          content = (content << 4) | n.toLong
          nibbles += 1
      else
        bcdBuilder.nn.add(n)

    inline def rewriteEAsNeg(): Unit =
      if bcdValid then content = (content & ~0xFL) | 0xCL
      else bcdBuilder.nn.overwriteLast(0xC)

    while continue && more do
      val ch = peek

      (state: @switch) match
        case NumZero =>
          ch match
            case Period =>
              // Drop the leading "0" — it's redundant for "0.xxx" and the
              // BCD printer reinserts it on emit. Saves one nibble per such
              // input, which can shift a value from `Array[Long]` to the
              // tighter `Array[Int]` shape when it lands right at the
              // 7-nibble boundary.
              nibbles = 0
              appendNibble(0xA); floating = true; state = NumAfterDot; advance()

            case UpperE | LowerE =>
              appendNibble(0xB); floating = true; state = NumAfterE; advance()

            case _ =>
              continue = false

        case NumInt =>
          ch match
            case Num0 | Num1 | Num2 | Num3 | Num4 | Num5 | Num6 | Num7 | Num8 | Num9 =>
              appendNibble(ch & 0xF); advance()

            case Period =>
              appendNibble(0xA); floating = true; state = NumAfterDot; advance()

            case UpperE | LowerE =>
              appendNibble(0xB); floating = true; state = NumAfterE; advance()

            case _ =>
              continue = false

        case NumAfterDot =>
          ch match
            case Num0 | Num1 | Num2 | Num3 | Num4 | Num5 | Num6 | Num7 | Num8 | Num9 =>
              appendNibble(ch & 0xF); state = NumFrac; advance()

            case _ =>
              errorAt(Issue.ExpectedDigit(ch.toChar))

        case NumFrac =>
          ch match
            case Num0 | Num1 | Num2 | Num3 | Num4 | Num5 | Num6 | Num7 | Num8 | Num9 =>
              appendNibble(ch & 0xF); advance()

            case UpperE | LowerE =>
              appendNibble(0xB); state = NumAfterE; advance()

            case _ =>
              continue = false

        case NumAfterE =>
          ch match
            case Plus =>
              advance(); state = NumAfterESign

            case Minus =>
              rewriteEAsNeg(); advance(); state = NumAfterESign

            case Num0 | Num1 | Num2 | Num3 | Num4 | Num5 | Num6 | Num7 | Num8 | Num9 =>
              appendNibble(ch & 0xF); state = NumExp; advance()

            case _ =>
              errorAt(Issue.ExpectedDigit(ch.toChar))

        case NumAfterESign =>
          ch match
            case Num0 | Num1 | Num2 | Num3 | Num4 | Num5 | Num6 | Num7 | Num8 | Num9 =>
              appendNibble(ch & 0xF); state = NumExp; advance()

            case _ =>
              errorAt(Issue.ExpectedDigit(ch.toChar))

        case NumExp =>
          ch match
            case Num0 | Num1 | Num2 | Num3 | Num4 | Num5 | Num6 | Num7 | Num8 | Num9 =>
              appendNibble(ch & 0xF); advance()

            case _ =>
              continue = false

        case _ => ()

    (state: @switch) match
      case NumAfterDot | NumAfterE | NumAfterESign =>
        errorAt(Issue.PrematureEnd)

      case _ => ()

    if bcdOnly then
      // Array-element fast path: skip the decode loop and the `Double`
      // materialisation. Return a single-Long BCD value if it fits the
      // 14-nibble cap, allocate a 2-word `Bcd` directly for the 15-nibble
      // boundary case, and use the Builder only when the in-Long path
      // already overflowed.
      if bcdValid then
        if nibbles <= Bcd.MaxBcdLongNibbles then
          Bcd.packBcdLong(content, nibbles, negative)
        else
          Bcd.fromContent15(content, negative)
      else
        bcdBuilder.nn.finish(negative): Bcd
    else if bcdValid then
      if numberMode == NumberMode.Bcd then
        // BCD mode: skip the decode loop and hand back the raw in-Long BCD
        // accumulator. Sign and numeric value aren't recoverable from this
        // Long — consumers under this mode treat it as opaque.
        content: Long
      else
        var mantissa: Long = 0L
        var decimalDigits: Int = 0
        var explicitExp: Int = 0
        var expSign: Int = 1
        var inFraction: Boolean = false
        var inExponent: Boolean = false

        var i = nibbles - 1

        while i >= 0 do
          val n = ((content >>> (i * 4)) & 0xFL).toInt

          if n <= 9 then
            if inExponent then explicitExp = explicitExp*10 + n
            else
              mantissa = mantissa*10 + n
              if inFraction then decimalDigits += 1
          else if n == 0xA then
            inFraction = true
          else if n == 0xB then
            inExponent = true
          else if n == 0xC then
            inExponent = true
            expSign = -1

          i -= 1

        if !floating then
          val signed: Long = if negative then -mantissa else mantissa
          if numberMode == NumberMode.Double then signed.toDouble else signed
        else
          val totalExp = expSign * explicitExp - decimalDigits

          val mag =
            if mantissa == 0L then 0.0
            else if mantissa < (1L << 53) && totalExp >= 0 && totalExp <= 22 then
              mantissa.toDouble * TenPow(totalExp)
            else if mantissa < (1L << 53) && totalExp < 0 && totalExp >= -22 then
              mantissa.toDouble / TenPow(-totalExp)
            else
              java.math.BigDecimal.valueOf(mantissa).nn.scaleByPowerOfTen(totalExp).nn.doubleValue

          if negative then -mag else mag
    else
      // High-precision path: hand back the `Bcd` directly. Reached only in
      // `NumberMode.Full`; `Bcd` and `Double` modes leave `bcdValid` true
      // and truncate on overflow rather than allocating a `Bcd.Builder`.
      bcdBuilder.nn.finish(negative): Bcd

  // `bcdOnly` propagates through the `Minus` recursion so that `-3.14`
  // parsed from an array context still short-circuits the Double path.
  // The flag has no effect on non-number values, so we don't need to
  // propagate it to `parseObject` / `parseArray` / `parseString`.
  private def parseValue(minus: Boolean = false, bcdOnly: Boolean = false)
    ( using Tactic[ParseError] )
  :   Raw =

    if !more then errorAt(Issue.PrematureEnd)
    val ch = peek

    if (ch & 0xF8) == Num0 || (ch & 0xFE) == 0x38 then
      advance()
      parseNumber(ch & 0x0F, minus, bcdOnly)
    else if minus then
      errorAt(Issue.ExpectedDigit(ch.toChar))
    else if holes && ch == 0 then
      advance()
      Unset
    else
      (ch: @switch) match
        case Quote       => advance() yet parseString()
        case Minus       => advance() yet parseValue(true, bcdOnly)
        case OpenBracket => advance() yet parseArray()
        case LowerF      => parseFalse()
        case LowerN      => parseNull()
        case LowerT      => parseTrue()
        case OpenBrace   => advance() yet parseObject()
        case other       => errorAt(Issue.ExpectedSomeValue(other.toChar))

  // Tracked-mode `parseValue`. Captures source position before dispatch,
  // delegates composite cases to the `*Tracked` variants (so they emit
  // their own descriptors into `indexOut`), and writes a 4-int primitive
  // descriptor afterwards for everything else. The `Minus` recursion
  // routes to the untracked variant since the outer call already owns
  // the descriptor for the full `-N` span.
  private def parseValueTracked
    ( indexOut: ArrayBuffer[Int],
      minus:    Boolean = false,
      bcdOnly:  Boolean = false )
    ( using Tactic[ParseError] )
  :   Raw =

    if !more then errorAt(Issue.PrematureEnd)

    syncTo()
    reconcileLineation()
    val startLine   = cursor.line.n1
    val startColumn = cursor.column.n1
    val startMark   = cursor.position.n0.toLong

    val ch = peek

    val result: Raw =
      if (ch & 0xF8) == Num0 || (ch & 0xFE) == 0x38 then
        advance()
        parseNumber(ch & 0x0F, minus, bcdOnly)
      else if minus then
        errorAt(Issue.ExpectedDigit(ch.toChar))
      else if holes && ch == 0 then
        advance()
        Unset
      else
        (ch: @switch) match
          case Quote       => advance() yet parseString()
          case Minus       => advance() yet parseValue(true, bcdOnly)

          case OpenBracket =>
            advance()
            parseArrayTracked(indexOut, startLine, startColumn, startMark)

          case LowerF      => parseFalse()
          case LowerN      => parseNull()
          case LowerT      => parseTrue()

          case OpenBrace =>
            advance()
            parseObjectTracked(indexOut, startLine, startColumn, startMark)

          case other       => errorAt(Issue.ExpectedSomeValue(other.toChar))

    if ch != OpenBracket && ch != OpenBrace then
      syncTo()
      val length = (cursor.position.n0 - startMark).toInt
      indexOut += 4
      indexOut += startLine
      indexOut += startColumn
      indexOut += length

    result

  private def parseArray()(using Tactic[ParseError]): Raw =
    // The array starts in "undecided" mode. Each element is parsed with
    // `bcdOnly = true`, so a number comes back as either a single-Long
    // BCD packing (count + sign + nibbles encoded in the Long itself) or
    // a heap-allocated `Bcd` if it overflows the 14-nibble cap. The mode
    // is decided from the first element and tightens / widens as we go:
    //
    //   `Array[Int]`  if every element fits 7 nibbles  (4 bytes / element)
    //   `Array[Long]` if every element fits 14 nibbles (8 bytes / element)
    //   `IArray[Any]` otherwise                        (boxed)
    //
    // Subsequent elements either fit the current container (cheap append)
    // or trigger a one-step migration to the next-wider form, with the
    // existing elements re-packed in place. We never widen past the
    // boxed form; once boxed we stay boxed.
    var mode: Int = ModeUndecided
    var intItems:  ArrayBuffer[Int]  | Null = null
    var longItems: ArrayBuffer[Long] | Null = null
    var anyItems:  ArrayBuffer[Any]  | Null = null
    var first    = true
    var continue = true

    inline def migrateIntToLong(): Unit =
      val src = intItems.nn
      val dst = getBcdLongBuffer()
      val n = src.length
      var i = 0

      while i < n do
        dst += Bcd.repackBcdIntAsLong(src(i))
        i += 1

      relinquishBcdIntBuffer()
      intItems = null
      longItems = dst
      mode = ModeBcdLong

    inline def migrateIntToBoxed(): Unit =
      val src = intItems.nn
      val dst = getArrayBuffer()
      val n = src.length
      var i = 0

      // Each Int *is* a small-BCD `JsonNumber` variant — append directly.
      while i < n do
        dst += src(i)
        i += 1

      relinquishBcdIntBuffer()
      intItems = null
      anyItems = dst
      mode = ModeBoxed

    inline def migrateLongToBoxed(): Unit =
      val src = longItems.nn
      val dst = getArrayBuffer()
      val n = src.length
      var i = 0

      // Decode each BCD-Long to its scalar JsonNumber form. Long if the
      // value is an exact integer that fits Long, else Double.
      while i < n do
        val v = src(i)
        val text = Bcd.bcdLongText(v)

        val ast: Any =
          try java.lang.Long.parseLong(text)
          catch case _: NumberFormatException => java.lang.Double.parseDouble(text)

        dst += ast
        i += 1

      relinquishBcdLongBuffer()
      longItems = null
      anyItems = dst
      mode = ModeBoxed

    while continue do
      skip()

      must() match
        case CloseBracket =>
          if !first then errorAt(Issue.ExpectedSomeValue(']'))
          continue = false

        case _ =>
          val value = parseValue(bcdOnly = true)
          skip()

          val terminator: Byte = must()

          terminator match
            case Comma | CloseBracket => ()
            case char                 => errorAt(Issue.ExpectedSomeValue(char.toChar))

          // Classify the parsed value:
          //   bcdLong:    a Long return — parseNumber's bcdOnly fast path
          //               packed sign+count+nibbles into a single Long.
          //   nonNumber:  anything else (Bcd, String, Boolean, Null,
          //               IArray[Any], etc.) — goes to boxed mode.
          value match
            case bcdLong: Long =>
              val nibbles = ((bcdLong >>> 56) & 0x7FL).toInt

              if first then
                first = false

                if nibbles <= Bcd.MaxBcdIntNibbles then
                  mode = ModeBcdInt
                  val buf = getBcdIntBuffer()
                  intItems = buf
                  buf += Bcd.packBcdIntFromLong(bcdLong)
                else
                  mode = ModeBcdLong
                  val buf = getBcdLongBuffer()
                  longItems = buf
                  buf += bcdLong
              else
                (mode: @switch) match
                  case ModeBcdInt =>
                    if nibbles <= Bcd.MaxBcdIntNibbles then
                      intItems.nn += Bcd.packBcdIntFromLong(bcdLong)
                    else
                      migrateIntToLong()
                      longItems.nn += bcdLong

                  case ModeBcdLong =>
                    longItems.nn += bcdLong

                  case _ =>
                    // Boxed mode — surface the value as a JsonNumber.
                    // For ≤7 nibbles, the small-BCD `Int` is the right
                    // shape; otherwise the BCD-Long itself is fine.
                    if nibbles <= Bcd.MaxBcdIntNibbles then
                      anyItems.nn += Bcd.packBcdIntFromLong(bcdLong)
                    else
                      anyItems.nn += bcdLong

            case _ =>
              if first then
                first = false
                mode = ModeBoxed
                val buf = getArrayBuffer()
                anyItems = buf
                buf += value
              else
                (mode: @switch) match
                  case ModeBcdInt =>
                    migrateIntToBoxed()
                    anyItems.nn += value

                  case ModeBcdLong =>
                    migrateLongToBoxed()
                    anyItems.nn += value

                  case _ =>
                    anyItems.nn += value

          if terminator == CloseBracket then continue = false

      advance()

    if first then
      // Empty array — no buffer was ever allocated. The empty case has
      // even (zero) length so the sentinel pad is required to keep arrays
      // distinguishable from objects.
      val out = new Array[Any](1)
      out(0) = Json.Ast.arrayPad
      out.asInstanceOf[IArray[Any]]
    else
      (mode: @switch) match
        case ModeBcdInt =>
          val src = intItems.nn
          val out = new Array[Int](src.length)
          src.copyToArray(out)
          relinquishBcdIntBuffer()
          out

        case ModeBcdLong =>
          val src = longItems.nn
          val out = new Array[Long](src.length)
          src.copyToArray(out)
          relinquishBcdLongBuffer()
          out

        case _ =>
          // Mixed/boxed array — stored as `IArray[Any]` and parity-padded
          // when the element count is even, so arrays always have odd
          // length and can be distinguished from objects (always even).
          val src = anyItems.nn
          val n = src.length

          val out =
            if (n & 1) == 1 then
              val arr = new Array[Any](n)
              src.copyToArray(arr)
              arr
            else
              val arr = new Array[Any](n + 1)
              src.copyToArray(arr)
              arr(n) = Json.Ast.arrayPad
              arr

          relinquishArrayBuffer()
          out.asInstanceOf[IArray[Any]]

  // Tracked-mode `parseArray`. Mirrors `parseArray` exactly but uses
  // `parseValueTracked` for children and emits a composite descriptor
  // into `indexOut` at close.
  private def parseArrayTracked
    ( indexOut:    ArrayBuffer[Int],
      startLine:   Int,
      startColumn: Int,
      startMark:   Long )
    ( using Tactic[ParseError] )
  :   Raw =

    var mode: Int = ModeUndecided
    var intItems:  ArrayBuffer[Int]  | Null = null
    var longItems: ArrayBuffer[Long] | Null = null
    var anyItems:  ArrayBuffer[Any]  | Null = null
    var first    = true
    var continue = true

    val indexScratch = getIndexBuffer()
    val indexEnds    = getIndexBuffer()

    inline def migrateIntToLong(): Unit =
      val src = intItems.nn
      val dst = getBcdLongBuffer()
      val n = src.length
      var i = 0

      while i < n do
        dst += Bcd.repackBcdIntAsLong(src(i))
        i += 1

      relinquishBcdIntBuffer()
      intItems = null
      longItems = dst
      mode = ModeBcdLong

    inline def migrateIntToBoxed(): Unit =
      val src = intItems.nn
      val dst = getArrayBuffer()
      val n = src.length
      var i = 0

      while i < n do
        dst += src(i)
        i += 1

      relinquishBcdIntBuffer()
      intItems = null
      anyItems = dst
      mode = ModeBoxed

    inline def migrateLongToBoxed(): Unit =
      val src = longItems.nn
      val dst = getArrayBuffer()
      val n = src.length
      var i = 0

      while i < n do
        val v = src(i)
        val text = Bcd.bcdLongText(v)

        val ast: Any =
          try java.lang.Long.parseLong(text)
          catch case _: NumberFormatException => java.lang.Double.parseDouble(text)

        dst += ast
        i += 1

      relinquishBcdLongBuffer()
      longItems = null
      anyItems = dst
      mode = ModeBoxed

    while continue do
      skip()

      must() match
        case CloseBracket =>
          if !first then errorAt(Issue.ExpectedSomeValue(']'))
          continue = false

        case _ =>
          val value = parseValueTracked(indexScratch, bcdOnly = true)
          indexEnds += indexScratch.length
          skip()

          val terminator: Byte = must()

          terminator match
            case Comma | CloseBracket => ()
            case char                 => errorAt(Issue.ExpectedSomeValue(char.toChar))

          value match
            case bcdLong: Long =>
              val nibbles = ((bcdLong >>> 56) & 0x7FL).toInt

              if first then
                first = false

                if nibbles <= Bcd.MaxBcdIntNibbles then
                  mode = ModeBcdInt
                  val buf = getBcdIntBuffer()
                  intItems = buf
                  buf += Bcd.packBcdIntFromLong(bcdLong)
                else
                  mode = ModeBcdLong
                  val buf = getBcdLongBuffer()
                  longItems = buf
                  buf += bcdLong
              else
                (mode: @switch) match
                  case ModeBcdInt =>
                    if nibbles <= Bcd.MaxBcdIntNibbles then
                      intItems.nn += Bcd.packBcdIntFromLong(bcdLong)
                    else
                      migrateIntToLong()
                      longItems.nn += bcdLong

                  case ModeBcdLong =>
                    longItems.nn += bcdLong

                  case _ =>
                    if nibbles <= Bcd.MaxBcdIntNibbles then
                      anyItems.nn += Bcd.packBcdIntFromLong(bcdLong)
                    else
                      anyItems.nn += bcdLong

            case _ =>
              if first then
                first = false
                mode = ModeBoxed
                val buf = getArrayBuffer()
                anyItems = buf
                buf += value
              else
                (mode: @switch) match
                  case ModeBcdInt =>
                    migrateIntToBoxed()
                    anyItems.nn += value

                  case ModeBcdLong =>
                    migrateLongToBoxed()
                    anyItems.nn += value

                  case _ =>
                    anyItems.nn += value

          if terminator == CloseBracket then continue = false

      advance()

    val astValue: Raw =
      if first then
        val out = new Array[Any](1)
        out(0) = Json.Ast.arrayPad
        out.asInstanceOf[IArray[Any]]
      else
        (mode: @switch) match
          case ModeBcdInt =>
            val src = intItems.nn
            val out = new Array[Int](src.length)
            src.copyToArray(out)
            relinquishBcdIntBuffer()
            out

          case ModeBcdLong =>
            val src = longItems.nn
            val out = new Array[Long](src.length)
            src.copyToArray(out)
            relinquishBcdLongBuffer()
            out

          case _ =>
            val src = anyItems.nn
            val n = src.length

            val out =
              if (n & 1) == 1 then
                val arr = new Array[Any](n)
                src.copyToArray(arr)
                arr
              else
                val arr = new Array[Any](n + 1)
                src.copyToArray(arr)
                arr(n) = Json.Ast.arrayPad
                arr

            relinquishArrayBuffer()
            out.asInstanceOf[IArray[Any]]

    emitCompositeDescriptor
      ( indexOut, indexScratch, indexEnds, startLine, startColumn, startMark )

    relinquishIndexBuffer()
    relinquishIndexBuffer()

    astValue

  // Parse an object directly into the flat alternating-key/value layout. The
  // buffer always grows in pairs, so its length stays even, which is the
  // object/array parity invariant.
  private def parseObject()(using Tactic[ParseError]): IArray[Any] =
    val items: ArrayBuffer[Any] = getArrayBuffer()
    var continue = true

    while continue do
      skip()

      must() match
        case Quote =>
          advance()
          val string = parseObjectKey()
          skip()

          must() match
            case Colon =>
              advance()
              skip()
              val value = parseValue()
              skip()

              must() match
                case Comma =>
                  advance()
                  items += string
                  items += value
                  skip()

                case CloseBrace =>
                  advance()
                  items += string
                  items += value
                  continue = false

                case ch  => errorAt(Issue.UnexpectedChar(ch.toChar))

            case ch => errorAt(Issue.ExpectedColon(ch.toChar))

        case 0 if holes =>
          advance()
          skip()

          must() match
            case Colon =>
              advance()
              skip()
              val value = parseValue()
              skip()

              must() match
                case Comma =>
                  advance()
                  items += " "
                  items += value
                  skip()

                case CloseBrace =>
                  advance()
                  items += " "
                  items += value
                  continue = false

                case ch => errorAt(Issue.UnexpectedChar(ch.toChar))

            case Comma =>
              advance()
              items += " "
              items += Unset
              skip()

            case CloseBrace =>
              advance()
              items += " "
              items += Unset
              continue = false

            case ch => errorAt(Issue.UnexpectedChar(ch.toChar))

        case CloseBrace =>
          if !items.nil then errorAt(Issue.ExpectedSomeValue('}'))
          advance()
          continue = false

        case ch =>
          errorAt(Issue.ExpectedString(ch.toChar))

    val out = new Array[Any](items.length)
    items.copyToArray(out)
    relinquishArrayBuffer()
    out.asInstanceOf[IArray[Any]]

  // Tracked-mode `parseObject`. Mirrors `parseObject` exactly but captures
  // key positions, runs values through `parseValueTracked`, and emits a
  // composite descriptor into `indexOut` at close.
  private def parseObjectTracked
    ( indexOut:    ArrayBuffer[Int],
      startLine:   Int,
      startColumn: Int,
      startMark:   Long )
    ( using Tactic[ParseError] )
  :   IArray[Any] =

    val items: ArrayBuffer[Any] = getArrayBuffer()
    var continue = true

    val indexScratch = getIndexBuffer()
    val indexEnds    = getIndexBuffer()

    var keyLine:   Int  = 0
    var keyColumn: Int  = 0
    var keyMark:   Long = 0L

    while continue do
      skip()
      syncTo()
      reconcileLineation()
      keyLine   = cursor.line.n1
      keyColumn = cursor.column.n1
      keyMark   = cursor.position.n0.toLong

      must() match
        case Quote =>
          advance()
          val string = parseObjectKey()
          syncTo()
          val keyLength = (cursor.position.n0 - keyMark).toInt
          skip()

          must() match
            case Colon =>
              advance()
              skip()
              indexScratch += keyLine
              indexScratch += keyColumn
              indexScratch += keyLength
              val value = parseValueTracked(indexScratch)
              indexEnds += indexScratch.length
              skip()

              must() match
                case Comma =>
                  advance()
                  items += string
                  items += value
                  skip()

                case CloseBrace =>
                  advance()
                  items += string
                  items += value
                  continue = false

                case ch  => errorAt(Issue.UnexpectedChar(ch.toChar))

            case ch => errorAt(Issue.ExpectedColon(ch.toChar))

        case 0 if holes =>
          advance()
          val keyLength = 1
          skip()

          must() match
            case Colon =>
              advance()
              skip()
              indexScratch += keyLine
              indexScratch += keyColumn
              indexScratch += keyLength
              val value = parseValueTracked(indexScratch)
              indexEnds += indexScratch.length
              skip()

              must() match
                case Comma =>
                  advance()
                  items += " "
                  items += value
                  skip()

                case CloseBrace =>
                  advance()
                  items += " "
                  items += value
                  continue = false

                case ch => errorAt(Issue.UnexpectedChar(ch.toChar))

            case Comma =>
              syncTo()
              reconcileLineation()
              val unsetLine   = cursor.line.n1
              val unsetColumn = cursor.column.n1
              indexScratch += keyLine
              indexScratch += keyColumn
              indexScratch += keyLength
              indexScratch += 4
              indexScratch += unsetLine
              indexScratch += unsetColumn
              indexScratch += 0
              indexEnds += indexScratch.length
              advance()
              items += " "
              items += Unset
              skip()

            case CloseBrace =>
              syncTo()
              reconcileLineation()
              val unsetLine   = cursor.line.n1
              val unsetColumn = cursor.column.n1
              indexScratch += keyLine
              indexScratch += keyColumn
              indexScratch += keyLength
              indexScratch += 4
              indexScratch += unsetLine
              indexScratch += unsetColumn
              indexScratch += 0
              indexEnds += indexScratch.length
              advance()
              items += " "
              items += Unset
              continue = false

            case ch => errorAt(Issue.UnexpectedChar(ch.toChar))

        case CloseBrace =>
          if !items.nil then errorAt(Issue.ExpectedSomeValue('}'))
          advance()
          continue = false

        case ch =>
          errorAt(Issue.ExpectedString(ch.toChar))

    val out = new Array[Any](items.length)
    items.copyToArray(out)
    relinquishArrayBuffer()

    emitCompositeDescriptor
      ( indexOut, indexScratch, indexEnds, startLine, startColumn, startMark )

    relinquishIndexBuffer()
    relinquishIndexBuffer()

    out.asInstanceOf[IArray[Any]]

  def parse()(using Tactic[ParseError]): Raw =
    bom()
    skip()
    if !more then abort(ParseError(Json.Ast, Position(0, 0), Issue.EmptyInput))

    val result =
      if tracking then
        val rootBuf = getIndexBuffer()
        val r = parseValueTracked(rootBuf)
        rootIndex = IArray.from(rootBuf)
        relinquishIndexBuffer()
        r
      else
        parseValue()

    while more do
      peek match
        case Tab | Return | Newline | Space => advance()
        case char                           => errorAt(Issue.SpuriousContent(char.toChar))

    result


