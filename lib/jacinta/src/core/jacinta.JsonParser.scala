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
package jacinta

import scala.annotation.*
import scala.collection.mutable.ArrayBuffer

import anticipation.*
import contingency.*
import denominative.*
import rudiments.*
import vacuous.*
import zephyrine.*

import Json.Ast.AsciiByte.*
import Json.Ast.{Issue, Position}

private[jacinta] object JsonParser:
  private[jacinta] type Raw =
    Long | Double | Bcd | String | IArray[Any] | Array[Double] | Boolean | Null | Unset.type

  private inline val NumZero       = 0
  private inline val NumInt        = 1
  private inline val NumAfterDot   = 2
  private inline val NumFrac       = 3
  private inline val NumAfterE     = 4
  private inline val NumAfterESign = 5
  private inline val NumExp        = 6

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

  private val pool: ThreadLocal[JsonParser] =
    ThreadLocal.withInitial{ () => new JsonParser }.nn

  def parse(source: Data, mode: NumberMode = NumberMode.Full): Raw raises ParseError =
    val parser = pool.get.nn
    parser.resetData(source)
    parser.holes = false
    parser.numberMode = mode
    parser.parse()

  def parse(source: Data, holes: Boolean, mode: NumberMode): Raw raises ParseError =
    val parser = pool.get.nn
    parser.resetData(source)
    parser.holes = holes
    parser.numberMode = mode
    parser.parse()

  def parse(input: Iterator[Data], mode: NumberMode): Raw raises ParseError =
    val parser = pool.get.nn
    parser.resetIterator(input)
    parser.holes = false
    parser.numberMode = mode
    parser.parse()

  def parse(input: Iterator[Data], holes: Boolean, mode: NumberMode): Raw raises ParseError =
    val parser = pool.get.nn
    parser.resetIterator(input)
    parser.holes = holes
    parser.numberMode = mode
    parser.parse()

private[jacinta] final class JsonParser:
  import JsonParser.*
  import Lineation.untrackedData

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
  private var cursor:    Cursor[Data]      = null.asInstanceOf[Cursor[Data]]
  private var heldToken: Cursor.Held | Null = null

  // Parser-local snapshot (see comment above).
  private var bytes:  Array[Byte] = null.asInstanceOf[Array[Byte]]
  private var pos:    Int = 0
  private var bufEnd: Int = 0

  protected[jacinta] var holes: Boolean = false

  // Storage shape `parseNumber` uses for each parsed JSON number. See
  // `NumberMode` for semantics. Reset before each `parse()` call.
  protected[jacinta] var numberMode: NumberMode = NumberMode.Full

  protected var arraySize:           Int = 16
  protected var chars:               Array[Char] = new Array(arraySize)
  protected var stringCursor:        Int = 0
  protected var arrayBufferId:       Int = -1
  protected val arrayBuffers:        ArrayBuffer[ArrayBuffer[Any]] = ArrayBuffer.empty
  protected var numberBufferId:      Int = -1
  protected val numberBuffers:       ArrayBuffer[ArrayBuffer[Double]] = ArrayBuffer.empty

  // Side-channel between `parseNumber` and `parseArray`: when the parsed
  // number stays in the in-Long fast path (≤ 15 nibbles, no Bcd fallback),
  // `numberAsDouble` carries the parsed value and `numberFitsDouble` is
  // true. Cleared at the top of `parseValue`. Numbers that overflow into
  // `Bcd` clear the flag, forcing the enclosing array to migrate from the
  // unboxed `Array[Double]` form to the boxed `JsonArray`.
  protected var numberAsDouble:   Double  = 0.0
  protected var numberFitsDouble: Boolean = false

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
  private val keyCache:    Array[String | Null] = new Array(KeyCacheSize)
  private val keyCacheLow:  Array[Long]         = new Array(KeyCacheSize)
  private val keyCacheHigh: Array[Long]         = new Array(KeyCacheSize)

  def resetData(input: Data): Unit =
    cursor = Cursor[Data](input)
    syncFrom()
    stringCursor = 0
    arrayBufferId = -1
    numberBufferId = -1
    heldToken = null

  def resetIterator(input: Iterator[Data]): Unit =
    cursor = Cursor[Data](input)
    syncFrom()
    stringCursor = 0
    arrayBufferId = -1
    numberBufferId = -1
    heldToken = null

  // ──────────────────────────────────────────────────────────────────────────
  // Substrate.

  // Push the parser's local `pos` back to the cursor. Required before any
  // cursor operation that consults `pos` (mark, slice, refill, position).
  private inline def syncTo(): Unit =
    cursor.unsafeAdvanceBy(pos - cursor.unsafePos(using Unsafe))(using Unsafe)

  // Refresh the parser's snapshot from the cursor. Required after any
  // cursor operation that may have changed the buffer reference, the read
  // position, or the write end (refill, cue).
  private inline def syncFrom(): Unit =
    bytes  = cursor.unsafeBuffer(using Unsafe).asInstanceOf[Array[Byte]]
    pos    = cursor.unsafePos(using Unsafe)
    bufEnd = cursor.unsafeWriteEnd(using Unsafe)

  protected inline def more: Boolean = pos < bufEnd || moreSlow()

  // Out-of-line slow path so the inline budget for `more` stays small enough
  // for the JIT to keep `pos < bufEnd` as a single register comparison in
  // hot loops.
  private def moreSlow(): Boolean =
    syncTo()
    if cursor.more then { syncFrom(); true } else false

  protected inline def peek: Byte = bytes(pos)

  protected inline def advance(): Unit = pos += 1

  protected def errorAt(issue: Issue, start: Optional[Cursor.Mark] = Unset)
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

  protected inline def begin(): Cursor.Mark =
    syncTo()
    cursor.mark(using heldToken.nn)

  protected inline def slice(start: Cursor.Mark): String =
    syncTo()
    val end = cursor.mark(using heldToken.nn)

    cursor.slice(start, end): (storage, off, len) =>
      val arr = storage.asInstanceOf[Array[Byte]]
      new String(arr, off, len, java.nio.charset.StandardCharsets.US_ASCII)

  protected inline def appendRegionToBuffer(start: Cursor.Mark): Unit =
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
  protected def bom(): Unit =
    if !more || peek != -17.toByte then return
    syncTo()

    cursor.hold:
      val mk = cursor.mark

      val bom =
        cursor.more && cursor.datum(using Unsafe) == -17.toByte
        && { cursor.next(); cursor.more && cursor.datum(using Unsafe) == -69.toByte }
        && { cursor.next(); cursor.more && cursor.datum(using Unsafe) == -65.toByte }

      if bom then cursor.next() else cursor.cue(mk)

    syncFrom()

  protected inline def holding[result](inline action: => result): result =
    syncTo()

    cursor.hold:
      heldToken = summon[Cursor.Held]
      try action finally heldToken = null

  // ──────────────────────────────────────────────────────────────────────────
  // String buffer plumbing (unchanged).

  protected inline def resetString(): Unit = stringCursor = 0

  protected inline def ensureStringSpace(n: Int): Unit =
    while stringCursor + n > arraySize do arraySize *= 2

    if chars.length < arraySize then
      val newArr = new Array[Char](arraySize)
      System.arraycopy(chars, 0, newArr, 0, stringCursor)
      chars = newArr

  protected inline def appendChar(char: Char): Unit =
    if stringCursor == arraySize then
      arraySize *= 2
      val newArray = new Array[Char](arraySize)
      System.arraycopy(chars, 0, newArray, 0, stringCursor)
      chars = newArray

    chars(stringCursor) = char
    stringCursor += 1

  protected inline def getString(): String = String(chars, 0, stringCursor)

  protected inline def getArrayBuffer(): ArrayBuffer[Any] =
    arrayBufferId += 1

    if arrayBuffers.length <= arrayBufferId then
      val newBuffer = ArrayBuffer.empty[Any]
      arrayBuffers += newBuffer
      newBuffer
    else
      val buffer = arrayBuffers(arrayBufferId)
      buffer.clear()
      buffer

  protected inline def relinquishArrayBuffer(): Unit = arrayBufferId -= 1

  protected inline def getNumberBuffer(): ArrayBuffer[Double] =
    numberBufferId += 1

    if numberBuffers.length <= numberBufferId then
      val newBuffer = ArrayBuffer.empty[Double]
      numberBuffers += newBuffer
      newBuffer
    else
      val buffer = numberBuffers(numberBufferId)
      buffer.clear()
      buffer

  protected inline def relinquishNumberBuffer(): Unit = numberBufferId -= 1

  // ──────────────────────────────────────────────────────────────────────────
  // Parser body (unchanged from the previous abstract base).

  protected inline def must()(using Tactic[ParseError]): Byte =
    if more then peek else errorAt(Issue.PrematureEnd)

  protected inline def next()(using Tactic[ParseError]): Byte =
    advance()
    if more then peek else errorAt(Issue.PrematureEnd)

  private def skip(): Unit =
    while
      more && {
        val ch = peek
        ch == Space || ch == Tab || ch == Newline || ch == Return
      }
    do advance()

  private def fromHex(ch: Byte)(using Tactic[ParseError]): Int =
    if ch <= Num9 && ch >= Num0 then ch - Num0
    else if ch <= UpperF && ch >= UpperA then ch - UpperA + 10
    else if ch <= LowerF && ch >= LowerA then ch - LowerA + 10
    else errorAt(Issue.ExpectedHexDigit(ch.toChar))

  private def parseUnicode()(using Tactic[ParseError]): Char =
    var acc = fromHex(next()) << 12
    acc |= fromHex(next()) << 8
    acc |= fromHex(next()) << 4
    acc |= fromHex(next())
    acc.toChar

  private def parseString()(using Tactic[ParseError]): String = holding:
    val region = begin()

    // Fast scan for plain printable ASCII that needs no escape handling. A
    // signed Byte is >= 32 only when it's printable ASCII (32..127); negative
    // bytes (0x80..0xFF) come out as -128..-1 < 32, so this single comparison
    // rejects both control characters and UTF-8 lead bytes.
    while
      more && {
        val b = peek
        b >= 32 && b != Quote && b != Backslash
      }
    do advance()

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
  private def parseObjectKey()(using Tactic[ParseError]): String = holding:
    val region = begin()

    while
      more && {
        val b = peek
        b >= 32 && b != Quote && b != Backslash
      }
    do advance()

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
          val packedLow  = packBytes(arr, off,     math.min(len, 8))
          val packedHigh = if len > 8 then packBytes(arr, off + 8, len - 8) else 0L
          val idx        = ((packedLow.toInt ^ (packedLow >>> 32).toInt) ^
                            (packedHigh.toInt ^ (packedHigh >>> 32).toInt)) & (KeyCacheSize - 1)
          val cached     = keyCache(idx)

          if cached != null && keyCacheLow(idx) == packedLow
             && keyCacheHigh(idx) == packedHigh
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
  private inline def packBytes(arr: Array[Byte], off: Int, n: Int): Long =
    var out: Long = 0L
    var i = 0
    while i < n do
      out = out | ((arr(off + i) & 0xFFL) << (i << 3))
      i += 1
    out

  private def tail(start: Region): String raises ParseError =
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

  private def parseNull(): Null raises ParseError =
    if pos + 4 <= bufEnd then
      val word: Int =
        (bytes(pos)     & 0xFF)         |
        ((bytes(pos + 1) & 0xFF) <<  8) |
        ((bytes(pos + 2) & 0xFF) << 16) |
        ((bytes(pos + 3) & 0xFF) << 24)

      if word != NullWord then errorAt(Issue.ExpectedNull)
      pos += 4
      null
    else
      expect(LowerU, Issue.ExpectedNull)
      expect(LowerL, Issue.ExpectedNull)
      expect(LowerL, Issue.ExpectedNull)
      advance()
      null

  private def parseNumber(first: Int, negative: Boolean)
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
      if numberMode == NumberMode.Full then
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

    if bcdValid then
      if numberMode == NumberMode.Bcd then
        // BCD mode: skip the decode loop and hand back the raw in-Long BCD
        // accumulator. Sign and numeric value aren't recoverable from this
        // Long — consumers under this mode treat it as opaque.
        numberAsDouble = 0.0
        numberFitsDouble = false
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
          // All bcdValid mantissas fit in 15 nibbles (≤ 10^15 - 1 < 2^50),
          // so casting to Double is exact.
          numberAsDouble = signed.toDouble
          numberFitsDouble = true
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

          val signed: Double = if negative then -mag else mag
          numberAsDouble = signed
          numberFitsDouble = true
          signed
    else
      // High-precision path: hand back the `Bcd` directly. Reached only in
      // `NumberMode.Full`; `Bcd` and `Double` modes leave `bcdValid` true
      // and truncate on overflow rather than allocating a `Bcd.Builder`.
      bcdBuilder.nn.finish(negative): Bcd

  private def parseValue(minus: Boolean = false)(using Tactic[ParseError]): Raw =
    // Clear the parseNumber side channel so a non-number value doesn't leave
    // stale state visible to the caller.
    numberFitsDouble = false
    if !more then errorAt(Issue.PrematureEnd)
    val ch = peek

    if (ch & 0xF8) == Num0 || (ch & 0xFE) == 0x38 then
      advance()
      parseNumber(ch & 0x0F, minus)
    else if minus then
      errorAt(Issue.ExpectedDigit(ch.toChar))
    else if holes && ch == 0 then
      advance()
      Unset
    else
      (ch: @switch) match
        case Quote       => advance() yet parseString()
        case Minus       => advance() yet parseValue(true)
        case OpenBracket => advance() yet parseArray()
        case LowerF      => parseFalse()
        case LowerN      => parseNull()
        case LowerT      => parseTrue()
        case OpenBrace   => advance() yet parseObject()
        case other       => errorAt(Issue.ExpectedSomeValue(other.toChar))

  private def parseArray()(using Tactic[ParseError]): Raw =
    // The array starts in "undecided" mode: on the first element we pick
    // either the unboxed Double buffer (if the value fits a Double, i.e.
    // came from the in-Long fast path) or the boxed Any buffer. Subsequent
    // non-fitting elements while in numbers mode trigger a one-time
    // migration into the boxed buffer.
    var numbersMode = false
    var numItems: ArrayBuffer[Double] | Null = null
    var anyItems: ArrayBuffer[Any]    | Null = null
    var first    = true
    var continue = true

    while continue do
      skip()

      must() match
        case CloseBracket =>
          if !first then errorAt(Issue.ExpectedSomeValue(']'))
          continue = false

        case _ =>
          val value      = parseValue()
          val asDouble   = numberAsDouble
          val fitsDouble = numberFitsDouble
          skip()

          val terminator: Byte = must()

          terminator match
            case Comma | CloseBracket => ()
            case char                 => errorAt(Issue.ExpectedSomeValue(char.toChar))

          if first then
            first = false

            if fitsDouble then
              numbersMode = true
              val buf = getNumberBuffer()
              numItems = buf
              buf += asDouble
            else
              val buf = getArrayBuffer()
              anyItems = buf
              buf += value
          else if numbersMode && fitsDouble then
            numItems.nn += asDouble
          else if numbersMode then
            // Migrate from the unboxed Double buffer to a boxed Any buffer
            // and continue in mixed mode for the rest of the array.
            val src = numItems.nn
            val dst = getArrayBuffer()
            val n = src.length
            var i = 0

            while i < n do
              dst += unpackToAst(src(i))
              i += 1

            relinquishNumberBuffer()
            numItems = null
            numbersMode = false
            anyItems = dst
            dst += value
          else
            anyItems.nn += value

          if terminator == CloseBracket then continue = false

      advance()

    // The parseNumber side channel may have been left set by a number
    // parsed inside the array; the array itself doesn't fit a Double, so
    // clear before returning.
    numberFitsDouble = false

    if first then
      // Empty array — no buffer was ever allocated. The empty case has
      // even (zero) length so the sentinel pad is required to keep arrays
      // distinguishable from objects.
      val out = new Array[Any](1)
      out(0) = Json.Ast.arrayPad
      out.asInstanceOf[IArray[Any]]
    else if numbersMode then
      val src = numItems.nn
      val out = new Array[Double](src.length)
      src.copyToArray(out)
      relinquishNumberBuffer()
      out
    else
      // Mixed/boxed array — stored as `IArray[Any]` and parity-padded
      // when the element count is even, so arrays always have odd length
      // and can be distinguished from objects (always even).
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

  // Decode a single Double back into the boxed AST node form. Used during
  // migration when a number-only array sees a non-fitting element. We
  // recover `Long` for whole-valued numbers in the Long range so that
  // `t"[1, 2, "x"]"` and `t"1"` both yield `Long(1)` for the first
  // element, preserving the type the parser would have assigned outside
  // the array context.
  private def unpackToAst(double: Double): Long | Double =
    if double.isWhole && double >= Long.MinValue.toDouble && double <= Long.MaxValue.toDouble
    then double.toLong
    else double

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
    // The parseNumber side channel may have been left set by a value
    // inside the object; the object itself doesn't fit a Double.
    numberFitsDouble = false
    out.asInstanceOf[IArray[Any]]

  def parse()(using Tactic[ParseError]): Raw =
    bom()
    skip()
    if !more then abort(ParseError(Json.Ast, Position(0, 0), Issue.EmptyInput))
    val result = parseValue()

    while more do
      peek match
        case Tab | Return | Newline | Space => advance()
        case char                           => errorAt(Issue.SpuriousContent(char.toChar))

    result
