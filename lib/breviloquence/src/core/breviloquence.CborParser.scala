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
package breviloquence

import scala.caps
import scala.collection.mutable as scm
import scala.language.experimental.pureFunctions

import anticipation.*
import contingency.*
import denominative.*
import prepositional.*
import rudiments.*
import turbulence.*
import vacuous.*
import zephyrine.*
import Cbor.Error.Reason

object CborParser:
  // Reasserts the capability of a parser that travelled as a neutral carrier (a
  // `Cbor.Reader`'s `rawParser`), for the generated parsers spliced into user modules.
  // Inline for the reason `Cbor.Reader.of` is: only the cast expression itself is exclusive
  // in a module that is capture-checked but not separation-checked. The carrier is only
  // obtainable through the reader's package-private accessor, so this reveals nothing.
  inline def of(carrier: AnyRef): CborParser^ = carrier.asInstanceOf[CborParser^]

  // The break stop code (0xFF) terminates an indefinite-length item.
  private inline val Break = 0xFF

  // Boxed-Long cache covering CBOR's uint16 range. The JDK's `Long.valueOf`
  // only caches -128..127; corpus payloads dominated by small unsigned
  // integers (timestamps, ids, counts) routinely fall outside that window
  // and pay a fresh `java.lang.Long` allocation per value. A flat array
  // lookup is two-to-three times cheaper than allocation in steady state.
  private inline val LongCacheSize = 65536

  private val longCache: Array[AnyRef]^{} =
    Array.scribe[AnyRef](LongCacheSize): scribe => extent =>
      extent.each: index => scribe(index) = java.lang.Long.valueOf(index.n0.toLong).nn

  private inline def boxLong(value: Long): AnyRef =
    // The guard proves `0 <= value < LongCacheSize`, the cache's extent.
    if value >= 0L && value < LongCacheSize then longCache.readUnchecked(value.toInt)
    else java.lang.Long.valueOf(value).nn

  // Joins the chunks of an indefinite-length string: one `System.arraycopy`
  // per chunk through `Scribe`'s bulk `append`.
  private def join(chunks: scm.ArrayBuffer[Array[Byte]^{}]): Array[Byte]^{} =
    Array.collect[Byte](): buffer => chunks.foreach: chunk => buffer.append(chunk, 0, chunk.length)

  // One parser per parse: the parser owns no scratch arrays (the box cache is
  // shared here), so an instance is four fields and a cursor, and the
  // per-thread pool jacinta's parser needs for its buffers buys nothing.
  private[breviloquence] def apply(input: Data): CborParser^ =
    val parser = new CborParser
    parser.resetData(input)
    parser

  private[breviloquence] def apply(input: Chain[Data]): CborParser^ =
    val parser = new CborParser
    parser.resetChain(input)
    parser

  private[breviloquence] def apply(consume input: (Stream[Data] over Credit)^): CborParser^ =
    val parser = new CborParser
    // consume-to-consume forwarding is not admitted; the hop re-asserts the transfer
    val moved: AnyRef = input.asInstanceOf[AnyRef]
    parser.resetStream(moved.asInstanceOf[(Stream[Data] over Credit)^])
    parser

  // Parses exactly one data item, rejecting trailing bytes. The trailing
  // check is the one place a complete item's read touches the source after
  // the item's last byte — the `read[Cbor.Ast]` contract consumes the whole
  // stream; the direct rim never reads past an item it has consumed.
  private[breviloquence] def parse(source: Data): Cbor.Ast raises Cbor.Error =
    val parser = apply(source)
    val result = parser.value()
    if parser.more then abort(Cbor.Error(Reason.Trailing(parser.position)))
    result

  private[breviloquence] def parse(source: Chain[Data]): Cbor.Ast raises Cbor.Error =
    val parser = apply(source)
    val result = parser.value()
    if parser.more then abort(Cbor.Error(Reason.Trailing(parser.position)))
    result

  private[breviloquence] def parse(consume source: (Stream[Data] over Credit)^): Cbor.Ast raises Cbor.Error =
    val moved: AnyRef = source.asInstanceOf[AnyRef]
    val parser = apply(moved.asInstanceOf[(Stream[Data] over Credit)^])
    val result = parser.value()
    if parser.more then abort(Cbor.Error(Reason.Trailing(parser.position)))
    result

  // The `Cbor.Ast` aggregator: parses straight off a stream's windows under
  // `accept`, so chunked input is never assembled into one array first.
  // Resolution-scoped (the tactic), and defined here rather than in `Cbor.Ast`'s
  // companion so the seal lands in this file's census row.
  private[breviloquence] def aggregable(using tactic: Tactic[Cbor.Error]): (Cbor.Ast is Aggregable by Data)^{tactic} =
    // [field-purity] given Aggregable codec over tactic, codec-thunk seal
    caps.unsafe.unsafeAssumePure:
      new Aggregable:
        type Self = Cbor.Ast
        type Operand = Data

        def aggregate(source: Chain[Data]): Cbor.Ast = parse(source)

        // The parameter is not `consume`: `Aggregable.accept`'s signature is pinned
        // non-consuming by overrides in modules outside separation checking, so the stream
        // crosses to the consuming parser as a neutral reference — each accept call delivers
        // a stream that is used exactly once, by construction.
        override def accept(stream: (Stream[Data] over Credit)^): Cbor.Ast =
          val moved: AnyRef = stream.asInstanceOf[AnyRef]
          parse(moved.asInstanceOf[(Stream[Data] over Credit)^])

// The CBOR data-item parser: one data item at a time off a `Cursor`, so a
// document arriving in chunks is read as it arrives and the buffer holds only
// the item being read (the whole of a byte or text string, since the result
// is one array, but never a container's elements once they are consumed).
//
// Public as a type — generated parsers, spliced into user modules, bind it
// once per record and read through its direct rim — but only breviloquence's
// read paths can construct one. A stateful capability (jacinta's parser
// pattern): every state-mutating method is `update`, and the cursor is
// touched only from plain (non-inline) methods, each binding it to a
// block-scoped local; the per-byte hot paths read the parser's own snapshot
// of the cursor's buffer (`bytes`, `pos`, `bufEnd`), which the JIT can keep
// in registers across a whole item.
//
// Invariant: between `syncTo()` and `syncFrom()`, `pos` is the authoritative
// read position and the cursor's own position may lag. Every cursor operation
// that depends on the position (refill, mark, cue) is bracketed by the two:
// refill may compact, reallocate or adopt a new buffer, after which `bytes`
// and `pos` are re-read and `base` re-anchored, so `position` — the absolute
// stream offset reported in every `Cbor.Error` — survives compaction.
final class CborParser private[breviloquence] () extends caps.ExclusiveCapability, caps.Stateful:
  import CborParser.{Break, boxLong}

  // The cursor's storage, as zephyrine types it (`Addressable.bytes.Storage`): the one
  // place the raw JVM array appears, so reads compile to BALOAD.
  private var bytes:  scala.Array[Byte] = null.asInstanceOf[scala.Array[Byte]]
  private var pos:    Int = 0
  private var bufEnd: Int = 0

  // The absolute stream offset of `bytes(0)`, re-anchored by every `syncFrom()`.
  private var base: Long = 0L

  // The high word of the last key packed by `directKeyWord()` (its low word is
  // the return value).
  var directKeyHigh: Long = 0L

  private[breviloquence] update def resetData(input: Data): Unit =
    import Lineation.untrackedData
    val fresh = Cursor[Data](input)
    cursor = fresh
    syncFrom()

  private[breviloquence] update def resetChain(input: Chain[Data]): Unit =
    import Lineation.untrackedData
    val fresh = Cursor[Data](input)
    cursor = fresh
    syncFrom()

  private[breviloquence] update def resetStream(consume input: (Stream[Data] over Credit)^)
  :   Unit =

    import Lineation.untrackedData
    val fresh = Cursor[Data](input)
    cursor = fresh
    syncFrom()

  // ── Substrate ──────────────────────────────────────────────────────────

  // The absolute stream offset of the next byte: the position every error
  // reports, unchanged by buffer compaction.
  inline def position: Long = base + pos

  // Push the parser's `pos` back to the cursor, before any cursor operation
  // that consults it (refill, mark, cue).
  private update def syncTo(): Unit =
    val parserPos = pos
    val current = cursor
    current.unsafeAdvanceBy(parserPos - current.unsafePos(using Unsafe))(using Unsafe)

  // Refresh the snapshot from the cursor, after any cursor operation that may
  // have changed the buffer, the read position or the write end. Each cursor
  // read is block-scoped so the parser-field writes that follow stay legal.
  private update def syncFrom(): Unit =
    val snapshot =
      locally:
        val current = cursor
        current.unsafeDataBuffer(using Unsafe)

    val readPos =
      locally:
        val current = cursor
        current.unsafePos(using Unsafe)

    val writeEnd =
      locally:
        val current = cursor
        current.unsafeWriteEnd(using Unsafe)

    val absolute =
      locally:
        val current = cursor
        current.position.n0.toLong

    bytes  = snapshot
    pos    = readPos
    bufEnd = writeEnd
    base   = absolute - readPos

  // Whether another byte is available, refilling from the source if the
  // buffer is exhausted. The slow path is out of line so the JIT keeps
  // `pos < bufEnd` as one register comparison in hot loops.
  update def more: Boolean = pos < bufEnd || moreSlow()

  private update def moreSlow(): Boolean =
    syncTo()

    val hasMore =
      locally:
        val current = cursor
        current.more

    syncFrom()
    hasMore

  // Buffers `count` contiguous bytes from the read position, pulling from
  // the source as needed: the buffer is held while it fills, so compaction
  // keeps everything from the read position and a multi-byte item split
  // across chunks is read exactly as one that arrived whole. Returns `false`
  // if the source ends first, with the read position unchanged.
  private update def fill(count: Int): Boolean =
    syncTo()

    val complete =
      locally:
        val current = cursor

        current.hold:
          val mark = current.mark
          var remaining = count

          // Ends on the cursor's state: the source is exhausted or the count is met.
          while remaining > 0 && current.more do
            val step = current.available.min(remaining)
            current.unsafeAdvanceBy(step)(using Unsafe)
            remaining -= step

          current.cue(mark)
          remaining == 0

    syncFrom()
    complete

  // These inline helpers take an explicit `Tactic` clause rather than `raises` sugar: the
  // context-function result the sugar expands to synthesizes a closure per inline expansion,
  // and from the 2026-07-17 upstream nightlies (#26547) a second expansion in the same
  // method fails cc root-visibility against the first expansion's memoized root capability.
  private inline update def expect(count: Int)(using Tactic[Cbor.Error]): Unit =
    if bufEnd - pos < count then ensureSlow(count)

  private update def ensureSlow(count: Int)(using Tactic[Cbor.Error]): Unit =
    val start = position
    if !fill(count) then abort(Cbor.Error(Reason.Truncated(start)))

  // Advances past `count` bytes without buffering them — the skip of a
  // definite-length string's content, which may be larger than any buffer.
  private inline update def skip(count: Int)(using Tactic[Cbor.Error]): Unit =
    if bufEnd - pos >= count then pos += count else skipSlow(count)

  private update def skipSlow(count: Int)(using Tactic[Cbor.Error]): Unit =
    val start = position
    syncTo()

    val remaining =
      locally:
        val current = cursor
        var left = count

        // Ends on the cursor's state: the source is exhausted or the count is met.
        while left > 0 && current.more do
          val step = current.available.min(left)
          current.unsafeAdvanceBy(step)(using Unsafe)
          left -= step

        left

    syncFrom()
    if remaining > 0 then abort(Cbor.Error(Reason.Truncated(start)))

  // A rewindable region: the action receives the hold token contextually, and
  // the buffer keeps everything from the read position until it returns, so a
  // `begin()` mark inside it can be `rewind`ed to.
  private update def holding[result](action: Cursor.Held ?->{caps.any, this} result): result =
    syncTo()
    // The action captures the parser, whose cursor is the held receiver — legitimate
    // single-threaded reentrancy (parser methods inside a hold use the cursor through
    // the parser), which the hidden-set check cannot see. Sealed before the cursor
    // binding hides the parser: the audited rim.
    val act: Cursor.Held -> result =
      // [by-name-receiver] hold action captures parser owning the held cursor
      caps.unsafe.unsafeAssumePure: (held: Cursor.Held) => action(using held)

    val current = cursor
    current.hold(act(summon[Cursor.Held]))

  private update def begin()(using Cursor.Held): Cursor.Mark =
    syncTo()
    val current = cursor
    current.mark

  private update def rewind(mark: Cursor.Mark): Unit =
    syncTo()

    // Block-scoped like `begin`; the parser snapshot is refreshed afterwards.
    locally:
      val current = cursor
      current.cue(mark)

    syncFrom()

  // ── Primitive reads, over the buffered bytes ───────────────────────────

  private inline update def readByte(): Int =
    (bytes(pos)&0xFF).also(pos += 1)

  private update def readUInt8()(using Tactic[Cbor.Error]): Int =
    expect(1)
    readByte()

  private update def readUInt16()(using Tactic[Cbor.Error]): Int =
    expect(2)
    val start = pos
    pos = start + 2
    ((bytes(start) & 0xFF) << 8) | (bytes(start + 1) & 0xFF)

  private update def readUInt32()(using Tactic[Cbor.Error]): Long =
    expect(4)
    val start = pos
    pos = start + 4
    ((bytes(start) & 0xFFL) << 24) |
      ((bytes(start + 1) & 0xFFL) << 16) |
      ((bytes(start + 2) & 0xFFL) << 8) |
      (bytes(start + 3) & 0xFFL)

  private update def readUInt64()(using Tactic[Cbor.Error]): Long =
    expect(8)
    val start = pos
    pos = start + 8
    ((bytes(start) & 0xFFL) << 56) |
      ((bytes(start + 1) & 0xFFL) << 48) |
      ((bytes(start + 2) & 0xFFL) << 40) |
      ((bytes(start + 3) & 0xFFL) << 32) |
      ((bytes(start + 4) & 0xFFL) << 24) |
      ((bytes(start + 5) & 0xFFL) << 16) |
      ((bytes(start + 6) & 0xFFL) << 8) |
      (bytes(start + 7) & 0xFFL)

  // Decodes the additional-info length field, returning the unsigned value as
  // a `Long`. A negative result means indefinite length.
  //
  // The `info < 24` fast path covers the in-head case (RFC 8949 §3.1) which
  // dominates real-world workloads (small integers, short strings, small
  // arrays/maps). The remaining cases dispatch through a `match` so the JVM
  // can compile them to a tableswitch. Plain (non-inline), as are the fixed-width
  // readers: an inline `update` method expanded inside another binds `this` to a
  // read-only proxy, so the inline helpers (`expect`, `skip`, `head`) call only plain ones.
  private update def readLength(info: Int, headOffset: Long)(using Tactic[Cbor.Error])
  :   Long =

    if info < 24 then info.toLong
    else info match
      case 24 => readUInt8().toLong
      case 25 => readUInt16().toLong
      case 26 => readUInt32()

      case 27 =>
        val v = readUInt64()
        // Bit 63 set means the value > Long.MaxValue; CBOR allows this for
        // major types 0/1 but breviloquence rejects it.
        if v < 0 then abort(Cbor.Error(Reason.Overflow(headOffset)))
        v

      case 31 => -1L
      case _  => abort(Cbor.Error(Reason.Reserved(headOffset, info)))

  // Copies `length` already-buffered bytes out as a frozen array.
  private update def readBytes(length: Int): Array[Byte]^{} =
    val result = Array.allocate[Byte](length)
    System.arraycopy(bytes, pos, result.raw, 0, length)
    pos += length
    Array.freeze(result)

  // Checks a definite length fits an array and buffers that many bytes.
  private update def boundedLength(length: Long, headOffset: Long)
    ( using Tactic[Cbor.Error] )
  :   Int =

    if length < 0 || length > Int.MaxValue then abort(Cbor.Error(Reason.Overflow(headOffset)))
    val count = length.toInt
    expect(count)
    count

  // As `boundedLength`, but skips the bytes rather than buffering them.
  private update def skippedLength(length: Long, headOffset: Long)
    ( using Tactic[Cbor.Error] )
  :   Unit =

    if length < 0 || length > Int.MaxValue then abort(Cbor.Error(Reason.Overflow(headOffset)))
    skip(length.toInt)

  // Reads the definite-length chunks of an indefinite-length string (each
  // prefixed with the given major type) until a Break stop code, one chunk
  // buffered at a time. The chunks are copied out as they are read and joined
  // afterwards (`CborParser.join`): the join's builder lambda must not close
  // over this parser, whose cursor it would then hold read-only.
  private update def readChunks(major: Int)(using Tactic[Cbor.Error])
  :   scm.ArrayBuffer[Array[Byte]^{}] =

    val chunks = scm.ArrayBuffer.empty[Array[Byte]^{}]
    var done = false

    while !done do
      expect(1)
      val head = bytes(pos) & 0xFF

      if head == Break then
        pos += 1
        done = true
      else
        val info = head & 0x1F
        if (head >>> 5) != major then abort(Cbor.Error(Reason.Reserved(position, head)))
        val chunkOffset = position
        pos += 1
        val length = boundedLength(readLength(info, chunkOffset), chunkOffset)
        chunks += readBytes(length)

    chunks

  private update def readIndefiniteByteString()(using Tactic[Cbor.Error]): Array[Byte]^{} =
    CborParser.join(readChunks(2))

  private update def readIndefiniteTextString()(using Tactic[Cbor.Error]): String =
    val raw = Array.unsafeJvm(CborParser.join(readChunks(3)))
    decodeUtf8(raw, 0, raw.length, 0L)

  private inline def decodeUtf8
    ( source: scala.Array[Byte], start: Int, length: Int, errorOffset: Long )
    ( using Tactic[Cbor.Error] )
  :   String =

    try new String(source, start, length, java.nio.charset.StandardCharsets.UTF_8)
    catch case _: Throwable => abort(Cbor.Error(Reason.InvalidUtf8(errorOffset)))

  // IEEE 754 half precision (16-bit) → Double, per RFC 8949 §3.3.
  // Assembles the 64-bit pattern directly rather than going through
  // `math.pow` and a multiplication: half-floats have only 65 536 possible
  // values and the conversion is a fixed sequence of bit moves.
  private def halfToDouble(half: Int): Double =
    val sign = (half.toLong & 0x8000L) << 48 // sign bit → bit 63
    val exp = (half >>> 10) & 0x1F
    val mant = half & 0x3FF

    val bits: Long =
      if exp == 0 then
        if mant == 0 then sign
        else
          // Subnormal half: re-normalise by shifting until bit 10 is set,
          // adjusting the (double) exponent accordingly.
          var m = mant
          var e = -14 + 1023
          while (m & 0x400) == 0 do { m <<= 1; e -= 1 }
          sign | (e.toLong << 52) | ((m.toLong & 0x3FF) << 42)
      else if exp == 31 then
        // Infinity (mant == 0) or NaN. Sign is preserved for both.
        sign | (2047L << 52) | (mant.toLong << 42)
      else
        sign | ((exp + 1023 - 15).toLong << 52) | (mant.toLong << 42)

    java.lang.Double.longBitsToDouble(bits)

  // Fails with `Truncated` at the current position unless a byte is available.
  private inline update def head()(using Tactic[Cbor.Error]): Int =
    if pos >= bufEnd && !more then abort(Cbor.Error(Reason.Truncated(position)))
    bytes(pos) & 0xFF

  // ── The AST path ───────────────────────────────────────────────────────

  update def value()(using Tactic[Cbor.Error]): Cbor.Ast =
    val head = this.head()
    val headOffset = position
    pos += 1

    // Fast paths for in-head small integers — by far the most common CBOR
    // head bytes in real workloads. Returning early skips the major/info
    // split, the `readLength` dispatch and the `headOffset` capture. Boxing
    // routes through the shared `boxLong` cache so the resulting
    // `java.lang.Long` is reused on the next parse.
    //   head 0x00–0x17 : major 0, info 0–23  → value is head itself
    //   head 0x20–0x37 : major 1, info 0–23  → value is -1 - (head & 0x1F)
    if head < 0x18 then return Cbor.Ast.fromRef(boxLong(head.toLong))

    if head >= 0x20 && head < 0x38 then return Cbor.Ast.fromRef(boxLong(-1L - (head & 0x1F).toLong))

    // Fast path for short text strings (major 3, info 0–23, head 0x60–0x77).
    // These dominate map keys and short literals; a length-prefixed UTF-8
    // payload skips the major-switch and `readLength` chain.
    if head >= 0x60 && head < 0x78 then
      val length = head & 0x1F
      expect(length)
      val str = new String(bytes, pos, length, java.nio.charset.StandardCharsets.UTF_8)
      pos += length
      return Cbor.Ast(str)

    // Fast path for short byte strings (major 2, info 0–23, head 0x40–0x57).
    if head >= 0x40 && head < 0x58 then
      val length = head & 0x1F
      expect(length)
      return Cbor.Ast(readBytes(length))

    val major = head >>> 5
    val info = head & 0x1F

    (major: @scala.annotation.switch) match
      case 0 =>
        val length = readLength(info, headOffset)
        if length < 0 then abort(Cbor.Error(Reason.Reserved(headOffset, head)))
        Cbor.Ast.fromRef(boxLong(length))

      case 1 =>
        val length = readLength(info, headOffset)
        if length < 0 then abort(Cbor.Error(Reason.Reserved(headOffset, head)))
        if length == Long.MinValue then abort(Cbor.Error(Reason.Overflow(headOffset)))
        Cbor.Ast.fromRef(boxLong(-1L - length))

      case 2 =>
        if info == 31 then Cbor.Ast(readIndefiniteByteString())
        else
          val length = boundedLength(readLength(info, headOffset), headOffset)
          Cbor.Ast(readBytes(length))

      case 3 =>
        if info == 31 then Cbor.Ast(readIndefiniteTextString())
        else
          val length = boundedLength(readLength(info, headOffset), headOffset)
          val str = decodeUtf8(bytes, pos, length, headOffset)
          pos += length
          Cbor.Ast(str)

      case 4 =>
        if info == 31 then
          // Build directly into an `Array[Any]`; flip to parity-padded shape
          // once the Break is seen rather than copying through `Array.from`
          // and then re-allocating in `Ast.array`.
          val items = scm.ArrayBuffer.empty[Any]
          var done = false

          while !done do
            expect(1)

            if (bytes(pos) & 0xFF) == Break then
              pos += 1
              done = true
            else
              items += value()

          val count = items.length
          val padded = (count&1) == 0
          val out = Array.allocate[Any](if padded then count + 1 else count)
          var index = 0

          while index < count do
            out(index) = items(index)
            index += 1

          if padded then out(count) = Cbor.Ast.Sentinel
          Cbor.Ast(Array.freeze(out))
        else
          val length = readLength(info, headOffset)

          if length < 0 || length > Int.MaxValue then abort(Cbor.Error(Reason.Overflow(headOffset)))
          val count = length.toInt
          // Allocate directly in the parity-padded shape used by `Cbor.Ast.array`
          // (odd length, with sentinel pad if logical count is even). One allocation
          // instead of two; no separate Array.from copy.
          val padded = (count&1) == 0
          val items = Array.allocate[Any](if padded then count + 1 else count)
          var index = 0

          while index < count do
            items(index) = value()
            index += 1

          if padded then items(count) = Cbor.Ast.Sentinel
          Cbor.Ast(Array.freeze(items))

      case 5 =>
        if info == 31 then
          // Build directly into one interleaved `Array[Any]`: one buffer and
          // one copy loop.
          val items = scm.ArrayBuffer.empty[Any]
          var done = false

          while !done do
            expect(1)

            if (bytes(pos) & 0xFF) == Break then
              pos += 1
              done = true
            else
              items += value()
              items += value()

          val out = Array.allocate[Any](items.length)
          var index = 0
          while index < items.length do { out(index) = items(index); index += 1 }
          Cbor.Ast(Array.freeze(out))

        else
          val length = readLength(info, headOffset)

          if length < 0 || length > Int.MaxValue then abort(Cbor.Error(Reason.Overflow(headOffset)))

          val count = length.toInt
          val items = Array.allocate[Any](count*2)
          var index = 0

          while index < count do
            items(index*2) = value()
            items(index*2 + 1) = value()
            index += 1

          Cbor.Ast(Array.freeze(items))

      case 6 =>
        val tag = readLength(info, headOffset)
        if tag < 0 then abort(Cbor.Error(Reason.Reserved(headOffset, head)))
        val inner = value()
        Cbor.Ast(Cbor.Tag(tag, inner))

      case 7 =>
        info match
          case 20 => Cbor.Ast(false)
          case 21 => Cbor.Ast(true)
          case 22 => Cbor.Ast(Cbor.CborNull)
          case 23 => Cbor.Ast(vacuous.Unset)
          case 25 => Cbor.Ast(halfToDouble(readUInt16()))
          case 26 => Cbor.Ast(java.lang.Float.intBitsToFloat(readUInt32().toInt).toDouble)
          case 27 => Cbor.Ast(java.lang.Double.longBitsToDouble(readUInt64()))
          case 24 =>
            // The error message reads this parser only to render its diagnostic detail.
            val value = readUInt8()
            abort(Cbor.Error(Reason.BadSimpleValue(headOffset, value)))

          case 31 => abort(Cbor.Error(Reason.UnexpectedBreak(headOffset)))
          case _  => abort(Cbor.Error(Reason.BadSimpleValue(headOffset, info)))

      case _ => abort(Cbor.Error(Reason.Reserved(headOffset, head)))

  // ── The direct rim ───────────────────────────────────────────────────
  // Byte-level reads for direct parsing (`Cbor.Parsable`): each consumes
  // one complete item, with fast paths for the dominant head shapes and a
  // fallback through the general `value()` path on anything exotic (tags,
  // mistyped items, absence), so values and failures agree with the AST
  // accessors exactly.

  update def directLong()(using Tactic[Cbor.Error]): Long =
    val head = this.head()

    if head < 0x18 then
      pos += 1
      head.toLong
    else if head >= 0x20 && head < 0x38 then
      pos += 1
      -1L - (head & 0x1F).toLong
    else
      val major = head >>> 5
      val headOffset = position

      if major == 0 then
        pos += 1
        val length = readLength(head & 0x1F, headOffset)
        if length < 0 then abort(Cbor.Error(Reason.Reserved(headOffset, head)))
        length
      else if major == 1 then
        pos += 1
        val length = readLength(head & 0x1F, headOffset)
        if length < 0 then abort(Cbor.Error(Reason.Reserved(headOffset, head)))
        if length == Long.MinValue then abort(Cbor.Error(Reason.Overflow(headOffset)))
        -1L - length
      else
        value().long

  update def directDouble()(using Tactic[Cbor.Error]): Double =
    val head = this.head()

    if head == 0xFB then
      pos += 1
      java.lang.Double.longBitsToDouble(readUInt64())
    else if head == 0xFA then
      pos += 1
      java.lang.Float.intBitsToFloat(readUInt32().toInt).toDouble
    else if head == 0xF9 then
      pos += 1
      halfToDouble(readUInt16())
    else
      value().double

  update def directBoolean()(using Tactic[Cbor.Error]): Boolean =
    val head = this.head()

    if head == 0xF5 then
      pos += 1
      true
    else if head == 0xF4 then
      pos += 1
      false
    else
      value().boolean

  update def directString()(using Tactic[Cbor.Error]): String =
    val head = this.head()

    if head >= 0x60 && head < 0x78 then
      val length = head & 0x1F
      pos += 1
      expect(length)
      val str = new String(bytes, pos, length, java.nio.charset.StandardCharsets.UTF_8)
      pos += length
      str
    else if (head >>> 5) == 3 then
      val headOffset = position
      pos += 1

      if (head & 0x1F) == 31 then readIndefiniteTextString() else
        val length = boundedLength(readLength(head & 0x1F, headOffset), headOffset)
        val str = decodeUtf8(bytes, pos, length, headOffset)
        pos += length
        str
    else
      value().string

  update def directBytes()(using Tactic[Cbor.Error]): Array[Byte]^{} =
    val head = this.head()

    if head >= 0x40 && head < 0x58 then
      val length = head & 0x1F
      pos += 1
      expect(length)
      readBytes(length)
    else if (head >>> 5) == 2 then
      val headOffset = position
      pos += 1

      if (head & 0x1F) == 31 then readIndefiniteByteString() else
        val length = boundedLength(readLength(head & 0x1F, headOffset), headOffset)
        readBytes(length)
    else
      value().byteString

  // The undefined-item peek for optional wrappers: a wire `undefined`
  // (0xF7) reads as an absent value, exactly as the AST path's `optional`.
  update def directIsUndefined: Boolean =
    (pos < bufEnd || more) && (bytes(pos) & 0xFF) == 0xF7

  update def directUndefined(): Unit = pos += 1

  // Opens a map, returning its entry count, or -1 for indefinite length.
  // Any other item is consumed whole and reads as an empty map (every
  // field absent), exactly as the AST record decoder's
  // `if root.isMap then root.entries else 0`.
  update def directOpenMap()(using Tactic[Cbor.Error]): Int =
    val head = this.head()

    if (head >>> 5) == 5 then
      val headOffset = position
      pos += 1
      val info = head & 0x1F

      if info == 31 then -1 else
        val length = readLength(info, headOffset)
        if length < 0 || length > Int.MaxValue then abort(Cbor.Error(Reason.Overflow(headOffset)))
        length.toInt
    else
      directSkipValue()
      0

  // Opens an array, returning its element count, or -1 for indefinite
  // length. Any other item classifies through the AST accessor, so the
  // failure agrees with the AST collection decoder's `.array`.
  update def directOpenArray()(using Tactic[Cbor.Error]): Int =
    val head = this.head()

    if (head >>> 5) == 4 then
      val headOffset = position
      pos += 1
      val info = head & 0x1F

      if info == 31 then -1 else
        val length = readLength(info, headOffset)
        if length < 0 || length > Int.MaxValue then abort(Cbor.Error(Reason.Overflow(headOffset)))
        length.toInt
    else
      value().array
      0

  // Consumes a Break stop code if one is next — the end step of an
  // indefinite-length map or array.
  update def directBreak()(using Tactic[Cbor.Error]): Boolean =
    if this.head() == Break then
      pos += 1
      true
    else
      false

  // The next map key in packed form, for parsers that compare keys against
  // literal constants (generated parsers compile field names to
  // immediates): the packed low word of a definite-length, 1-16 byte,
  // 7-bit-clean text key (its high word left in `directKeyHigh`), or
  // `Cbor.Reader.KeyOpaque` without consuming anything — the caller then
  // takes the `directKeyName` step, which consumes the key generally. A key
  // split across a chunk boundary is buffered whole first, so it packs as
  // one that arrived whole; only a truncated key is opaque here, and the
  // general step then reports the truncation.
  update def directKeyWord(): Long =
    if pos >= bufEnd && !more then return Cbor.Reader.KeyOpaque
    val head = bytes(pos) & 0xFF

    if head > 0x60 && head <= 0x70 then
      val length = head & 0x1F
      if bufEnd - pos < 1 + length && !fill(1 + length) then return Cbor.Reader.KeyOpaque
      var low = 0L
      var high = 0L
      var ascii = 0
      var index = 0

      while index < length do
        val byte = bytes(pos + 1 + index).toLong & 0xFF
        ascii |= byte.toInt

        if index < 8 then low |= byte << (index*8) else high |= byte << ((index - 8)*8)

        index += 1

      if (ascii & 0x80) != 0 then Cbor.Reader.KeyOpaque else
        pos += 1 + length
        directKeyHigh = high
        low
    else
      Cbor.Reader.KeyOpaque

  // The general key step: consumes the key and returns a text key's
  // content, or `null` for a non-text key — whose entry the AST record
  // decoder ignores, so the caller skips its value and continues.
  update def directKeyName()(using Tactic[Cbor.Error]): String | Null =
    if (this.head() >>> 5) == 3 then directString()
    else
      directSkipValue()
      null

  // Skips one complete item, building nothing — for unknown keys and
  // non-map-shaped records. Rejects exactly the head shapes `value()`
  // rejects, so a skipped malformed item fails as the AST path (which
  // parses every entry) would. A string's content is skipped, not
  // buffered, so skipping is bounded-memory whatever the item's size.
  update def directSkipValue()(using Tactic[Cbor.Error]): Unit =
    val head = this.head()
    val headOffset = position
    pos += 1
    val major = head >>> 5
    val info = head & 0x1F

    (major: @scala.annotation.switch) match
      case 0 | 1 =>
        if readLength(info, headOffset) < 0
        then abort(Cbor.Error(Reason.Reserved(headOffset, head)))

      case 2 | 3 =>
        if info == 31 then
          var done = false

          while !done do
            expect(1)
            val chunkHead = bytes(pos) & 0xFF

            if chunkHead == Break then
              pos += 1
              done = true
            else
              if (chunkHead >>> 5) != major
              then abort(Cbor.Error(Reason.Reserved(position, chunkHead)))

              val chunkOffset = position
              pos += 1
              skippedLength(readLength(chunkHead & 0x1F, chunkOffset), chunkOffset)
        else
          skippedLength(readLength(info, headOffset), headOffset)

      case 4 =>
        if info == 31 then
          while !directBreak() do directSkipValue()
        else
          val length = readLength(info, headOffset)

          if length < 0 || length > Int.MaxValue then abort(Cbor.Error(Reason.Overflow(headOffset)))

          repeat(length.toInt):
            directSkipValue()

      case 5 =>
        if info == 31 then
          while !directBreak() do
            directSkipValue()
            directSkipValue()
        else
          val length = readLength(info, headOffset)

          if length < 0 || length > Int.MaxValue then abort(Cbor.Error(Reason.Overflow(headOffset)))

          repeat(length.toInt):
            directSkipValue()
            directSkipValue()

      case 6 =>
        if readLength(info, headOffset) < 0
        then abort(Cbor.Error(Reason.Reserved(headOffset, head)))

        directSkipValue()

      case 7 =>
        info match
          case 20 | 21 | 22 | 23 => ()
          case 25                => skip(2)
          case 26                => skip(4)
          case 27                => skip(8)

          case 24 =>
            // As above.
            val value = readUInt8()
            abort(Cbor.Error(Reason.BadSimpleValue(headOffset, value)))

          case 31 => abort(Cbor.Error(Reason.UnexpectedBreak(headOffset)))
          case _  => abort(Cbor.Error(Reason.BadSimpleValue(headOffset, info)))

      case _ => abort(Cbor.Error(Reason.Reserved(headOffset, head)))

  // Scans the upcoming map for the given text key and returns its text
  // value, leaving the parser where it started — the dispatch primitive
  // for a sum's discriminant entry, which may appear anywhere in the map.
  // `null` when the item is not a map, has no such key, or the key's
  // value is not text — the caller raises `Absent`, mirroring the AST
  // path's `discriminate(cbor).lest(...)`. The map is held in the buffer
  // for the scan's duration — the one place the buffer grows past a single
  // item — and released before the variant's own parse begins.
  update def directDiscriminant(key: String)(using Tactic[Cbor.Error])
  :   String | Null =

    holding:
      val start = begin()
      val result = scanDiscriminant(key)
      rewind(start)
      result

  private update def scanDiscriminant(key: String)(using Tactic[Cbor.Error]): String | Null =
    val head = if pos < bufEnd || more then bytes(pos) & 0xFF else 0
    if (head >>> 5) != 5 then return null
    val headOffset = position
    pos += 1
    val info = head & 0x1F

    var remaining =
      if info == 31 then -1 else
        val length = readLength(info, headOffset)

        if length < 0 || length > Int.MaxValue then abort(Cbor.Error(Reason.Overflow(headOffset)))

        length.toInt

    while remaining != 0 do
      if remaining < 0 && directBreak() then return null
      val name = directKeyName()

      if name != null && name == key then
        val valueHead = if pos < bufEnd || more then bytes(pos) & 0xFF else 0
        return if (valueHead >>> 5) == 3 then directString() else null
      else
        directSkipValue()

      remaining -= 1

    null

  // No INLINE member may read this field: the synthesized accessor's exclusive result
  // type acts as a template-level hider that bars other member definitions (the
  // member-order rule). Methods that touch the cursor are therefore plain (non-inline).
  private var cursor: Cursor[Data, {}]^ = null.asInstanceOf[Cursor[Data, {}]^]
