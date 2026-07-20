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
package locomotion

import scala.collection.mutable as scm

import anticipation.*
import contingency.*

import ProtobufError.Reason

// Reads Protocol Buffers wire bytes. `fields` parses a whole message payload into a
// number-keyed map of (one or more) raw wire values, preserving repeats and unknown
// fields — the structure the message decoder looks fields up in by number.
@unexported
class ProtobufParser(data: Data):
  private var pos: Int = 0

  def atEnd: Boolean = pos >= data.length

  def varint(): Long raises ProtobufError =
    val start = pos
    var result = 0L
    var shift = 0
    var continue = true

    while continue do
      if pos >= data.length then abort(ProtobufError(Reason.Truncated(pos)))
      if shift >= 70 then abort(ProtobufError(Reason.MalformedVarint(start)))
      val byte = data(pos) & 0xff
      pos += 1
      // The 10th byte (shift == 63) may only contribute bit 63; any higher bit set
      // means the value does not fit in 64 bits.
      if shift == 63 && (byte & 0x7f) > 1 then abort(ProtobufError(Reason.Overflow(start)))
      if shift < 64 then result |= (byte.toLong & 0x7f) << shift
      shift += 7
      if (byte & 0x80) == 0 then continue = false

    result

  def fixed32(): Int raises ProtobufError =
    if pos + 4 > data.length then abort(ProtobufError(Reason.Truncated(pos)))
    var result = 0
    var i = 0

    while i < 4 do
      result |= (data(pos + i) & 0xff) << (i*8)
      i += 1

    pos += 4
    result

  def fixed64(): Long raises ProtobufError =
    if pos + 8 > data.length then abort(ProtobufError(Reason.Truncated(pos)))
    var result = 0L
    var i = 0

    while i < 8 do
      result |= (data(pos + i).toLong & 0xff) << (i*8)
      i += 1

    pos += 8
    result

  def slice(length: Int): Data raises ProtobufError =
    if length < 0 || pos + length > data.length then abort(ProtobufError(Reason.Truncated(pos)))
    val result = data.slice(pos, pos + length)
    pos += length
    result

  def fields(): Map[Int, List[Protobuf]] raises ProtobufError =
    val accumulator = scm.LinkedHashMap.empty[Int, scm.ListBuffer[Protobuf]]

    while !atEnd do
      val tagStart = pos
      val tag = varint().toInt
      val number = tag >>> 3
      val code = tag & 0x7

      val wireType =
        WireType.fromId(code).lest(ProtobufError(Reason.UnexpectedWireType(code, tagStart)))

      val value = wireType match
        case WireType.Varint =>
          val start = pos
          varint()
          Protobuf.Wire(WireType.Varint, data.slice(start, pos))

        case WireType.I64 => Protobuf.Wire(WireType.I64, slice(8))
        case WireType.I32 => Protobuf.Wire(WireType.I32, slice(4))
        case WireType.Len => Protobuf.Wire(WireType.Len, slice(varint().toInt))

      accumulator.getOrElseUpdate(number, scm.ListBuffer()).addOne(value)

    Map.from(accumulator.view.mapValues(_.to(List)))

  // ── The direct rim ─────────────────────────────────────────────────────
  // Byte-level reads for direct parsing (`Protobuf.Parsable`), bounded by a
  // *window* — a limit within the input marking the extent of the value
  // being parsed, so nested messages parse in place instead of slicing a
  // fresh parser over a copied payload. A `parse` call receives the reader
  // with its window set to the value's payload and must consume to the
  // window's end; `directEnterField`/`directLeaveField` bracket one field's
  // wire value per its tag's wire code, exactly as `fields()` slices it.

  private var boundary: Int = data.length

  def directAtLimit: Boolean = pos >= boundary
  def directMark: Int = pos
  def directBoundary: Int = boundary

  // The next field tag: `(number << 3) | code`. Only called below the
  // window's limit.
  def directTag()(using Tactic[ProtobufError]): Int = directVarint().toInt

  // A limit-bounded `varint()`: identical semantics against the window
  // rather than the whole input.
  def directVarint()(using Tactic[ProtobufError]): Long =
    val start = pos
    var result = 0L
    var shift = 0
    var continue = true

    while continue do
      if pos >= boundary then abort(ProtobufError(Reason.Truncated(pos)))
      if shift >= 70 then abort(ProtobufError(Reason.MalformedVarint(start)))
      val byte = data(pos) & 0xff
      pos += 1

      if shift == 63 && (byte & 0x7f) > 1 then abort(ProtobufError(Reason.Overflow(start)))
      if shift < 64 then result |= (byte.toLong & 0x7f) << shift
      shift += 7
      if (byte & 0x80) == 0 then continue = false

    result

  def directFixed32()(using Tactic[ProtobufError]): Int =
    if pos + 4 > boundary then abort(ProtobufError(Reason.Truncated(pos)))
    var result = 0
    var i = 0

    while i < 4 do
      result |= (data(pos + i) & 0xff) << (i*8)
      i += 1

    pos += 4
    result

  def directFixed64()(using Tactic[ProtobufError]): Long =
    if pos + 8 > boundary then abort(ProtobufError(Reason.Truncated(pos)))
    var result = 0L
    var i = 0

    while i < 8 do
      result |= (data(pos + i).toLong & 0xff) << (i*8)
      i += 1

    pos += 8
    result

  // Narrows the window to one field's wire value — the same extent
  // `fields()` slices for the field's payload — returning the enclosing
  // limit for `directLeaveField`. For a length-delimited field the length
  // prefix is consumed; for a varint field the window covers the varint's
  // own bytes, mirroring the `data.slice(start, pos)` payload.
  def directEnterField(code: Int)(using Tactic[ProtobufError]): Int =
    val saved = boundary

    code match
      case 0 =>
        val start = pos
        directVarint()
        boundary = pos
        pos = start

      case 1 =>
        if pos + 8 > boundary then abort(ProtobufError(Reason.Truncated(pos)))
        boundary = pos + 8

      case 2 =>
        val length = directVarint().toInt

        if length < 0 || pos + length > boundary
        then abort(ProtobufError(Reason.Truncated(pos)))

        boundary = pos + length

      case 5 =>
        if pos + 4 > boundary then abort(ProtobufError(Reason.Truncated(pos)))
        boundary = pos + 4

      case other =>
        abort(ProtobufError(Reason.UnexpectedWireType(other, pos)))

    saved

  // Restores the enclosing window, consuming whatever of the field's value
  // remains — a parse may legitimately read less than the payload, as the
  // AST accessors ignore a payload's trailing bytes.
  def directLeaveField(saved: Int): Unit =
    pos = boundary
    boundary = saved

  def directSkipField(code: Int)(using Tactic[ProtobufError]): Unit =
    directLeaveField(directEnterField(code))

  // ── Scalar field reads, dispatching on the tag's wire code. The fast
  // path reads the natural encoding in place; a mismatched code reads the
  // field's payload window and interprets it exactly as the AST accessor
  // interprets the recorded payload. ──

  def directLong(code: Int)(using Tactic[ProtobufError]): Long =
    if code == 0 then directVarint() else
      val saved = directEnterField(code)
      val result = directVarint()
      directLeaveField(saved)
      result

  def directDouble(code: Int)(using Tactic[ProtobufError]): Double =
    if code == 1 then java.lang.Double.longBitsToDouble(directFixed64()) else
      val saved = directEnterField(code)
      val result = java.lang.Double.longBitsToDouble(directFixed64())
      directLeaveField(saved)
      result

  def directFloat(code: Int)(using Tactic[ProtobufError]): Float =
    if code == 5 then java.lang.Float.intBitsToFloat(directFixed32()) else
      val saved = directEnterField(code)
      val result = java.lang.Float.intBitsToFloat(directFixed32())
      directLeaveField(saved)
      result

  def directString(code: Int)(using Tactic[ProtobufError]): String =
    val saved = directEnterField(code)

    val result =
      java.lang.String
        ( data.asInstanceOf[Array[Byte]], pos, boundary - pos,
          java.nio.charset.StandardCharsets.UTF_8 )

    directLeaveField(saved)
    result

  def directData(code: Int)(using Tactic[ProtobufError]): Data =
    val saved = directEnterField(code)
    val result = data.slice(pos, boundary)
    directLeaveField(saved)
    result

  // One field's wire value, materialized — the runtime seam for field types
  // without an `Inlinable`, gathered per occurrence and decoded through the
  // field's `Decodable in Protobuf` exactly as the AST path.
  def directWire(code: Int)(using Tactic[ProtobufError]): Protobuf =
    val wireType =
      WireType.fromId(code).lest(ProtobufError(Reason.UnexpectedWireType(code, pos)))

    val saved = directEnterField(code)
    val result = Protobuf.Wire(wireType, data.slice(pos, boundary))
    directLeaveField(saved)
    result

  // The whole window's content as text or bytes — the window-level reads
  // behind the reader's leaf accessors, mirroring how the AST accessors
  // interpret a field's recorded payload.
  def directStringWindow(): String =
    val result =
      java.lang.String
        ( data.asInstanceOf[Array[Byte]], pos, boundary - pos,
          java.nio.charset.StandardCharsets.UTF_8 )

    pos = boundary
    result

  def directDataWindow(): Data =
    val result = data.slice(pos, boundary)
    pos = boundary
    result

  // The remaining window as a length-delimited message — the whole-value
  // seam for `Parsable.fromDecodable`.
  def directMessage(): Protobuf =
    val result = Protobuf.Wire(WireType.Len, data.slice(pos, boundary))
    pos = boundary
    result

  // ── The sum window: a oneof's variant is chosen by a scan of the whole
  // message (the lowest variant number present, its last occurrence), then
  // parsed from its recorded extent. `directRewindTo` re-enters a scanned
  // extent; the packed save restores both position and limit. ──

  def directWindow(start: Int, end: Int): Long =
    val saved = (pos.toLong << 32) | (boundary.toLong & 0xFFFFFFFFL)
    pos = start
    boundary = end
    saved

  def directRestore(saved: Long): Unit =
    pos = (saved >>> 32).toInt
    boundary = (saved & 0xFFFFFFFFL).toInt

  // Splits a packed `repeated` payload (a single length-delimited field holding
  // concatenated scalar values) into one wire value per element. `wireType` is the
  // element encoding, supplied by the element's `Packable` instance.
  def packed(wireType: WireType): List[Protobuf] raises ProtobufError =
    val builder = scala.collection.immutable.List.newBuilder[Protobuf]

    while !atEnd do
      val value = wireType match
        case WireType.Varint =>
          val start = pos
          varint()
          Protobuf.Wire(WireType.Varint, data.slice(start, pos))

        case WireType.I64 => Protobuf.Wire(WireType.I64, slice(8))
        case WireType.I32 => Protobuf.Wire(WireType.I32, slice(4))
        case WireType.Len => Protobuf.Wire(WireType.Len, slice(varint().toInt))

      builder += value

    List.of(builder.result())
