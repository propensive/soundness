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
package locomotion

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
import Protobuf.Error.Reason

@unexported
object ProtobufParser:
  // Reasserts the capability of a parser that travelled as a neutral carrier (a
  // `ProtobufReader`'s `rawParser`), for the generated parsers spliced into user modules.
  // Inline for the reason `ProtobufReader.of` is: only the cast expression itself is
  // exclusive in a module that is capture-checked but not separation-checked. The carrier
  // is only obtainable through the reader's package-private accessor.
  inline def of(carrier: AnyRef): ProtobufParser^ = carrier.asInstanceOf[ProtobufParser^]

  // The whole input as the parser's window: a message is delimited only by the end of its
  // input, which a stream reveals only by ending.
  private inline val Unbounded = Long.MaxValue

  // ── Whole-payload reads, for the ADT path ───────────────────────────────
  // A `Protobuf.Wire` holds one field's recorded payload in memory, so the
  // AST accessors read it in place: a scalar through the allocation-free
  // functions below, a message through one `Payload` walk per call. (The
  // streaming parser has the same varint loop over its buffer; the two are
  // kept apart so that neither allocates for the other's needs.)

  def varintOf(data: Data)(using Tactic[Protobuf.Error]): Long =
    val view = data.readable
    val limit = data.length
    var result = 0L
    var shift = 0
    var index = 0
    var continue = true

    // Ends on the data: a varint's last byte has its top bit clear; every read is below
    // `limit`, the payload's length.
    while continue do
      if index >= limit then abort(Protobuf.Error(Reason.Truncated(index)))
      if shift >= 70 then abort(Protobuf.Error(Reason.MalformedVarint(0)))
      val byte = view(index) & 0xff
      index += 1
      // The 10th byte (shift == 63) may only contribute bit 63; any higher bit set
      // means the value does not fit in 64 bits.
      if shift == 63 && (byte & 0x7f) > 1 then abort(Protobuf.Error(Reason.Overflow(0)))
      if shift < 64 then result |= (byte.toLong & 0x7f) << shift
      shift += 7
      if (byte & 0x80) == 0 then continue = false

    result

  def fixed32Of(data: Data)(using Tactic[Protobuf.Error]): Int =
    if data.length < 4 then abort(Protobuf.Error(Reason.Truncated(0)))
    val view = data.readable
    var result = 0
    var index = 0

    // Ends on a constant: four bytes, all below the checked length.
    while index < 4 do
      result |= (view(index) & 0xff) << (index*8)
      index += 1

    result

  def fixed64Of(data: Data)(using Tactic[Protobuf.Error]): Long =
    if data.length < 8 then abort(Protobuf.Error(Reason.Truncated(0)))
    val view = data.readable
    var result = 0L
    var index = 0

    // Ends on a constant: eight bytes, all below the checked length.
    while index < 8 do
      result |= (view(index).toLong & 0xff) << (index*8)
      index += 1

    result

  // Parses a whole message payload into a number-keyed map of (one or more) raw wire
  // values, preserving repeats and unknown fields — the structure the message decoder
  // looks fields up in by number. Each field's payload is copied out once, into its
  // `Wire`.
  def fields(data: Data)(using Tactic[Protobuf.Error]): Map[Int, List[Protobuf]] =
    Payload(data).fields()

  // Splits a packed `repeated` payload (a single length-delimited field holding
  // concatenated scalar values) into one wire value per element. `wireType` is the
  // element encoding, supplied by the element's `Packable` instance.
  def packed(data: Data, wireType: WireType)(using Tactic[Protobuf.Error]): List[Protobuf] =
    Payload(data).packed(wireType)

  // A single-pass walk over one in-memory payload: the message-level reads of the ADT
  // path, where every varint is read once for both its value and its extent.
  private final class Payload(data: Data) extends caps.Mutable:
    private val view = data.readable
    private val limit = data.length
    private var pos: Int = 0

    private update def varint()(using Tactic[Protobuf.Error]): Long =
      val start = pos
      var result = 0L
      var shift = 0
      var continue = true

      // Ends on the data: a varint's last byte has its top bit clear; every read is below
      // `limit`, the payload's length.
      while continue do
        if pos >= limit then abort(Protobuf.Error(Reason.Truncated(pos)))
        if shift >= 70 then abort(Protobuf.Error(Reason.MalformedVarint(start)))
        val byte = view(pos) & 0xff
        pos += 1
        if shift == 63 && (byte & 0x7f) > 1 then abort(Protobuf.Error(Reason.Overflow(start)))
        if shift < 64 then result |= (byte.toLong & 0x7f) << shift
        shift += 7
        if (byte & 0x80) == 0 then continue = false

      result

    private update def slice(length: Int)(using Tactic[Protobuf.Error]): Data =
      if length < 0 || pos + length > limit then abort(Protobuf.Error(Reason.Truncated(pos)))
      val result = data.segment(pos.z till (pos + length).z)
      pos += length
      result

    private update def wire(wireType: WireType)(using Tactic[Protobuf.Error]): Protobuf = wireType match
      case WireType.Varint =>
        val start = pos
        varint()
        Protobuf.Wire(WireType.Varint, data.segment(start.z till pos.z))

      case WireType.I64 => Protobuf.Wire(WireType.I64, slice(8))
      case WireType.I32 => Protobuf.Wire(WireType.I32, slice(4))
      case WireType.Len => Protobuf.Wire(WireType.Len, slice(varint().toInt))

    update def fields()(using Tactic[Protobuf.Error]): Map[Int, List[Protobuf]] =
      val accumulator = scm.LinkedHashMap.empty[Int, scm.ListBuffer[Protobuf]]

      // Ends on the data: each step consumes one whole field.
      while pos < limit do
        val tagStart = pos
        val tag = varint().toInt
        val number = tag >>> 3
        val code = tag & 0x7

        val wireType =
          WireType.fromId(code).lest(Protobuf.Error(Reason.UnexpectedWireType(code, tagStart)))

        accumulator.getOrElseUpdate(number, scm.ListBuffer()).addOne(wire(wireType))

      (accumulator.view.mapValues(_.to(List))).to(Map)

    update def packed(wireType: WireType)(using Tactic[Protobuf.Error]): List[Protobuf] =
      val builder = scala.collection.immutable.List.newBuilder[Protobuf]

      // Ends on the data: each step consumes one whole element.
      while pos < limit do builder += wire(wireType)

      builder.result().to(List)

  // ── The streaming parser's entry points ─────────────────────────────────

  // Over a whole message in memory, read in place: the window is the array.
  def apply(input: Data): ProtobufParser^ =
    val parser = new ProtobufParser
    parser.resetData(input)
    parser

  // Over chunks pulled as the parser needs them: the window ends where the input does.
  def apply(input: Chain[Data]): ProtobufParser^ =
    val parser = new ProtobufParser
    parser.resetChain(input)
    parser

  def apply(consume input: (Stream[Data] over Credit)^): ProtobufParser^ =
    val parser = new ProtobufParser
    // consume-to-consume forwarding is not admitted; the hop re-asserts the transfer
    val moved: AnyRef = input.asInstanceOf[AnyRef]
    parser.resetStream(moved.asInstanceOf[(Stream[Data] over Credit)^])
    parser

// Reads Protocol Buffers wire bytes straight off a `Cursor`, one field at a
// time, so a message arriving in chunks is read as it arrives and the buffer
// holds only the field being read — except across a oneof's scan, which holds
// its message (see `directBufferWindow`).
//
// Public as a type — generated parsers, spliced into user modules, bind it
// once per record and read through its direct rim — but only locomotion's
// read paths can construct one. A stateful capability (jacinta's parser
// pattern): every state-mutating method is `update`, and the cursor is
// touched only from plain (non-inline) methods, each binding it to a
// block-scoped local; the per-byte hot paths read the parser's own snapshot
// of the cursor's buffer (`bytes`, `pos`, `bufEnd`), which the JIT can keep
// in registers across a whole message.
//
// Reads are bounded by a *window* — the extent of the value being parsed, as
// an absolute stream offset (`boundary`), so nested messages parse in place
// instead of over a copied payload. A `parse` call receives the reader with
// its window set to the value's payload and must consume to the window's
// end; `directEnterField`/`directLeaveField` bracket one field's wire value
// per its tag's wire code, exactly as `fields` slices it. Window positions
// cross the rim as `Int`s: a message is at most 2 GiB (its length prefixes
// are `int32`) and one read parses one message from offset 0 of its input.
//
// Invariant: between `syncTo()` and `syncFrom()`, `pos` is the authoritative
// read position and the cursor's own position may lag. Every cursor operation
// that depends on the position (refill, mark, cue) is bracketed by the two:
// refill may compact, reallocate or adopt a new buffer, after which `bytes`
// and `pos` are re-read and `base` re-anchored, so `position` — the absolute
// stream offset reported in every `Protobuf.Error` — survives compaction.
@unexported
final class ProtobufParser private () extends caps.ExclusiveCapability, caps.Stateful:
  import ProtobufParser.Unbounded

  // The cursor's storage, as zephyrine types it (`Addressable.bytes.Storage`): the one
  // place the raw JVM array appears, so reads compile to BALOAD.
  private var bytes:  scala.Array[Byte] = null.asInstanceOf[scala.Array[Byte]]
  private var pos:    Int = 0
  private var bufEnd: Int = 0

  // The absolute stream offset of `bytes(0)`, re-anchored by every `syncFrom()`.
  private var base: Long = 0L

  // The window's end as an absolute offset (`Unbounded`: the end of the input), and the
  // same relative to `bytes(0)`, clamped — the hot-path bound, re-derived with `base`.
  private var boundary: Long = Unbounded
  private var limit:    Int = Int.MaxValue

  private[locomotion] update def resetData(input: Data): Unit =
    import Lineation.untrackedData
    val fresh = Cursor[Data](input)
    cursor = fresh
    boundary = input.length.toLong
    syncFrom()

  private[locomotion] update def resetChain(input: Chain[Data]): Unit =
    import Lineation.untrackedData
    val fresh = Cursor[Data](input)
    cursor = fresh
    boundary = Unbounded
    syncFrom()

  private[locomotion] update def resetStream(consume input: (Stream[Data] over Credit)^): Unit =
    import Lineation.untrackedData
    val fresh = Cursor[Data](input)
    cursor = fresh
    boundary = Unbounded
    syncFrom()

  // ── Substrate ──────────────────────────────────────────────────────────

  // The absolute stream offset of the next byte: the position every error
  // reports, unchanged by buffer compaction.
  private inline def position: Long = base + pos

  private inline def offset: Int = position.toInt

  private inline def relimit(): Unit =
    val relative = boundary - base
    limit = if relative > Int.MaxValue then Int.MaxValue else relative.toInt

  private update def syncTo(): Unit =
    val parserPos = pos
    val current = cursor
    current.unsafeAdvanceBy(parserPos - current.unsafePos(using Unsafe))(using Unsafe)

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
    relimit()

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
  // the source as needed under a hold, so a multi-byte value split across
  // chunks is read exactly as one that arrived whole. `false` if the input
  // ends first, with the read position unchanged.
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

  // Buffers everything up to the end of the input — the content of an
  // unbounded window read whole (a top-level string or bytes message).
  private update def fillToEnd(): Unit =
    syncTo()

    locally:
      val current = cursor

      current.hold:
        val mark = current.mark

        // Ends on the cursor's state: the source is exhausted.
        while current.more do current.unsafeAdvanceBy(current.available)(using Unsafe)

        current.cue(mark)

    syncFrom()

  private inline update def expect(count: Int)(using Tactic[Protobuf.Error]): Unit =
    if bufEnd - pos < count then ensureSlow(count)

  private update def ensureSlow(count: Int)(using Tactic[Protobuf.Error]): Unit =
    val start = offset
    if !fill(count) then abort(Protobuf.Error(Reason.Truncated(start)))

  // Advances to the absolute offset `target` without buffering the bytes passed over.
  private update def skipTo(target: Long)(using Tactic[Protobuf.Error]): Unit =
    val count = target - position

    if count <= 0 then () else if count <= bufEnd - pos then pos += count.toInt else
      val start = offset
      syncTo()

      val remaining =
        locally:
          val current = cursor
          var left = count

          // Ends on the cursor's state: the source is exhausted or the count is met.
          while left > 0 && current.more do
            val step = current.available.toLong.min(left)
            current.unsafeAdvanceBy(step.toInt)(using Unsafe)
            left -= step

          left

      syncFrom()
      if remaining > 0 then abort(Protobuf.Error(Reason.Truncated(start)))

  // The length of the window's remaining content, buffered whole: the window's extent, or
  // for an unbounded window, whatever the input has left.
  private update def windowLength()(using Tactic[Protobuf.Error]): Int =
    if boundary == Unbounded then
      fillToEnd()
      bufEnd - pos
    else
      val length = (boundary - position).toInt
      expect(length)
      length

  // Copies `length` already-buffered bytes out as a frozen array.
  private update def copyOut(length: Int): Data =
    val result = Array.allocate[Byte](length)
    System.arraycopy(bytes, pos, result.raw, 0, length)
    pos += length
    Array.freeze(result)

  private update def stringOut(length: Int): String =
    val result = java.lang.String(bytes, pos, length, java.nio.charset.StandardCharsets.UTF_8)
    pos += length
    result

  // ── The direct rim ─────────────────────────────────────────────────────

  update def directAtLimit: Boolean =
    if boundary == Unbounded then pos >= bufEnd && !more else pos >= limit

  update def directMark: Int = offset

  update def directBoundary: Int =
    if boundary > Int.MaxValue then Int.MaxValue else boundary.toInt

  // The next field tag: `(number << 3) | code`. Only called below the
  // window's limit.
  update def directTag()(using Tactic[Protobuf.Error]): Int =
    directVarint().toInt

  // The window-bounded varint read: the one varint loop of the streaming parser.
  update def directVarint()(using Tactic[Protobuf.Error]): Long =
    val start = offset
    var result = 0L
    var shift = 0
    var continue = true

    // Ends on the data: a varint's last byte has its top bit clear.
    while continue do
      if pos >= limit || (pos >= bufEnd && !more)
      then abort(Protobuf.Error(Reason.Truncated(offset)))

      if shift >= 70 then abort(Protobuf.Error(Reason.MalformedVarint(start)))
      val byte = bytes(pos) & 0xff
      pos += 1

      // The 10th byte (shift == 63) may only contribute bit 63; any higher bit set
      // means the value does not fit in 64 bits.
      if shift == 63 && (byte & 0x7f) > 1 then abort(Protobuf.Error(Reason.Overflow(start)))
      if shift < 64 then result |= (byte.toLong & 0x7f) << shift
      shift += 7
      if (byte & 0x80) == 0 then continue = false

    result

  update def directFixed32()(using Tactic[Protobuf.Error]): Int =
    if position + 4 > boundary then abort(Protobuf.Error(Reason.Truncated(offset)))
    expect(4)
    var result = 0
    var index = 0

    // Ends on a constant: four buffered bytes.
    while index < 4 do
      result |= (bytes(pos + index) & 0xff) << (index*8)
      index += 1

    pos += 4
    result

  update def directFixed64()(using Tactic[Protobuf.Error]): Long =
    if position + 8 > boundary then abort(Protobuf.Error(Reason.Truncated(offset)))
    expect(8)
    var result = 0L
    var index = 0

    // Ends on a constant: eight buffered bytes.
    while index < 8 do
      result |= (bytes(pos + index).toLong & 0xff) << (index*8)
      index += 1

    pos += 8
    result

  // The length of the varint at the read position, buffering it, without consuming it.
  private update def varintLength()(using Tactic[Protobuf.Error]): Int =
    var count = 0
    var done = false

    // Ends on the data: a varint's last byte has its top bit clear.
    while !done do
      if position + count >= boundary || (bufEnd - pos <= count && !fill(count + 1))
      then abort(Protobuf.Error(Reason.Truncated(offset + count)))

      if count >= 10 then abort(Protobuf.Error(Reason.MalformedVarint(offset)))
      done = (bytes(pos + count) & 0x80) == 0
      count += 1

    count

  // Narrows the window to one field's wire value — the same extent
  // `fields` slices for the field's payload — returning the enclosing
  // boundary for `directLeaveField`. For a length-delimited field the length
  // prefix is consumed; for a varint field the window covers the varint's
  // own bytes, mirroring the `segment(start till pos)` payload. An
  // unbounded enclosing window cannot prove a field's bytes exist on entry;
  // its reads report the truncation instead, at the same offset.
  update def directEnterField(code: Int)(using Tactic[Protobuf.Error])
  :   Int =

    val saved = directBoundary

    code match
      case 0 =>
        boundary = position + varintLength()

      case 1 =>
        if position + 8 > boundary then abort(Protobuf.Error(Reason.Truncated(offset)))
        boundary = position + 8

      case 2 =>
        val length = directVarint().toInt

        if length < 0 || position + length > boundary
        then abort(Protobuf.Error(Reason.Truncated(offset)))

        boundary = position + length

      case 5 =>
        if position + 4 > boundary then abort(Protobuf.Error(Reason.Truncated(offset)))
        boundary = position + 4

      case other =>
        abort(Protobuf.Error(Reason.UnexpectedWireType(other, offset)))

    relimit()
    saved

  // Restores the enclosing window, consuming whatever of the field's value
  // remains — a parse may legitimately read less than the payload, as the
  // AST accessors ignore a payload's trailing bytes. The remainder is
  // skipped, not buffered.
  update def directLeaveField(saved: Int)(using Tactic[Protobuf.Error])
  :   Unit =

    skipTo(boundary)
    boundary = if saved == Int.MaxValue then Unbounded else saved.toLong
    relimit()

  update def directSkipField(code: Int)(using Tactic[Protobuf.Error]): Unit =
    directLeaveField(directEnterField(code))

  // ── Scalar field reads, dispatching on the tag's wire code. The fast
  // path reads the natural encoding in place; a mismatched code reads the
  // field's payload window and interprets it exactly as the AST accessor
  // interprets the recorded payload. ──

  update def directLong(code: Int)(using Tactic[Protobuf.Error]): Long =
    if code == 0 then directVarint() else
      val saved = directEnterField(code)
      val result = directVarint()
      directLeaveField(saved)
      result

  update def directDouble(code: Int)(using Tactic[Protobuf.Error]): Double =
    if code == 1 then java.lang.Double.longBitsToDouble(directFixed64()) else
      val saved = directEnterField(code)
      val result = java.lang.Double.longBitsToDouble(directFixed64())
      directLeaveField(saved)
      result

  update def directFloat(code: Int)(using Tactic[Protobuf.Error]): Float =
    if code == 5 then java.lang.Float.intBitsToFloat(directFixed32()) else
      val saved = directEnterField(code)
      val result = java.lang.Float.intBitsToFloat(directFixed32())
      directLeaveField(saved)
      result

  update def directString(code: Int)(using Tactic[Protobuf.Error]): String =
    val saved = directEnterField(code)
    val result = stringOut(windowLength())
    directLeaveField(saved)
    result

  update def directData(code: Int)(using Tactic[Protobuf.Error]): Data =
    val saved = directEnterField(code)
    val result = copyOut(windowLength())
    directLeaveField(saved)
    result

  // One field's wire value, materialized — the runtime seam for field types
  // without an `Inlinable`, gathered per occurrence and decoded through the
  // field's `Decodable in Protobuf` exactly as the AST path.
  update def directWire(code: Int)(using Tactic[Protobuf.Error]): Protobuf =
    val wireType =
      WireType.fromId(code).lest(Protobuf.Error(Reason.UnexpectedWireType(code, offset)))

    val saved = directEnterField(code)
    val result = Protobuf.Wire(wireType, copyOut(windowLength()))
    directLeaveField(saved)
    result

  // The whole window's content as text or bytes — the window-level reads
  // behind the reader's leaf accessors, mirroring how the AST accessors
  // interpret a field's recorded payload.
  update def directStringWindow()(using Tactic[Protobuf.Error]): String =
    stringOut(windowLength())

  update def directDataWindow()(using Tactic[Protobuf.Error]): Data =
    copyOut(windowLength())

  // The remaining window as a length-delimited message — the whole-value
  // seam for `Parsable.fromDecodable`.
  update def directMessage()(using Tactic[Protobuf.Error]): Protobuf =
    Protobuf.Wire(WireType.Len, copyOut(windowLength()))

  // ── The sum window: a oneof's variant is chosen by a scan of the whole
  // message (the lowest variant number present, its last occurrence), then
  // parsed from its recorded extent. The message is buffered whole first
  // (`directBufferWindow`), so neither the scan nor the re-entry can refill —
  // and so compact — the buffer, and the scanned extent is simply returned
  // to within it; the packed save restores both position and limit. ──

  // Buffers the window's remaining content whole: the message about to be scanned. An
  // unbounded window becomes bounded by the input's end, which the fill has found.
  update def directBufferWindow()(using Tactic[Protobuf.Error]): Unit =
    if boundary == Unbounded then
      fillToEnd()
      boundary = position + (bufEnd - pos)
      relimit()
    else
      expect((boundary - position).toInt)

  // Moves to an absolute offset within the buffered window.
  private inline def seek(target: Long): Unit = pos = (target - base).toInt

  update def directWindow(start: Int, end: Int): Long =
    val saved = (offset.toLong << 32) | (directBoundary.toLong & 0xFFFFFFFFL)
    seek(start.toLong)
    boundary = end.toLong
    relimit()
    saved

  // An empty window at the read position — an absent nested value parses from it as
  // the empty message — restored by `directRestore` like any other.
  update def directEmptyWindow(): Long =
    val saved = (offset.toLong << 32) | (directBoundary.toLong & 0xFFFFFFFFL)
    boundary = position
    relimit()
    saved

  update def directRestore(saved: Long): Unit =
    seek(saved >>> 32)
    val end = (saved & 0xFFFFFFFFL).toInt
    boundary = if end == Int.MaxValue then Unbounded else end.toLong
    relimit()

  // No INLINE member may read this field: the synthesized accessor's exclusive result
  // type acts as a template-level hider that bars other member definitions (the
  // member-order rule). Methods that touch the cursor are therefore plain (non-inline).
  private var cursor: Cursor[Data, {}]^ = null.asInstanceOf[Cursor[Data, {}]^]
