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
package breviloquence

import scala.collection.mutable.ArrayBuffer

import contingency.*

import CborError.Reason

private[breviloquence] object CborParser:

  // The break stop code (0xFF) terminates an indefinite-length item.
  private inline val Break = 0xFF

  def parse(source: IArray[Byte]): CborAst raises CborError =
    val parser = new CborParser(source)
    val result = parser.readValue()
    if parser.offset < parser.data.length
    then abort(CborError(Reason.Trailing(parser.offset.toLong)))
    result

private[breviloquence] final class CborParser(input: IArray[Byte]):
  import CborParser.Break

  // Cache the underlying primitive array so reads compile to BALOAD rather
  // than going through `IArray$.apply`. `data.length` is constant-folded by
  // the JIT and cheaper than going through a separate `length` accessor.
  private[breviloquence] val data: Array[Byte] = input.asInstanceOf[Array[Byte]]

  // `offset` is exposed only to the package-private parse() entry point so it
  // can detect trailing bytes after a successful parse. All hot-path reads
  // mutate it directly through the JVM PUTFIELD/GETFIELD.
  var offset: Int = 0

  private inline def need(count: Int): Unit raises CborError =
    if data.length - offset < count then abort(CborError(Reason.Truncated(offset.toLong)))

  private inline def readByte(): Int =
    val b = data(offset) & 0xFF
    offset += 1
    b

  private inline def readUInt8(): Int raises CborError =
    need(1)
    readByte()

  private inline def readUInt16(): Int raises CborError =
    need(2)
    val pos = offset
    offset = pos + 2
    ((data(pos) & 0xFF) << 8) | (data(pos + 1) & 0xFF)

  private inline def readUInt32(): Long raises CborError =
    need(4)
    val pos = offset
    offset = pos + 4
    ((data(pos) & 0xFFL) << 24)
    | ((data(pos + 1) & 0xFFL) << 16)
    | ((data(pos + 2) & 0xFFL) << 8)
    | (data(pos + 3) & 0xFFL)

  private inline def readUInt64(): Long raises CborError =
    need(8)
    val pos = offset
    offset = pos + 8
    ((data(pos) & 0xFFL) << 56)
    | ((data(pos + 1) & 0xFFL) << 48)
    | ((data(pos + 2) & 0xFFL) << 40)
    | ((data(pos + 3) & 0xFFL) << 32)
    | ((data(pos + 4) & 0xFFL) << 24)
    | ((data(pos + 5) & 0xFFL) << 16)
    | ((data(pos + 6) & 0xFFL) << 8)
    | (data(pos + 7) & 0xFFL)

  // Decodes the additional-info length field, returning the unsigned value as
  // a `Long`. A negative result means indefinite length.
  //
  // The `info < 24` fast path covers the in-head case (RFC 8949 §3.1) which
  // dominates real-world workloads (small integers, short strings, small
  // arrays/maps). The remaining cases dispatch through a `match` so the JVM
  // can compile them to a tableswitch.
  private inline def readLength(info: Int, headOffset: Long): Long raises CborError =
    if info < 24 then info.toLong
    else info match
      case 24 => readUInt8().toLong
      case 25 => readUInt16().toLong
      case 26 => readUInt32()
      case 27 =>
        val v = readUInt64()
        // Bit 63 set means the value > Long.MaxValue; CBOR allows this for
        // major types 0/1 but breviloquence rejects it.
        if v < 0 then abort(CborError(Reason.Overflow(headOffset)))
        v
      case 31 => -1L
      case _  => abort(CborError(Reason.Reserved(headOffset, info)))

  private inline def readBytes(len: Int): IArray[Byte] =
    val result = new Array[Byte](len)
    System.arraycopy(data, offset, result, 0, len)
    offset += len
    result.asInstanceOf[IArray[Byte]]

  private inline def boundedLength(len: Long, headOffset: Long): Int raises CborError =
    if len < 0 || len > Int.MaxValue then abort(CborError(Reason.Overflow(headOffset)))
    val n = len.toInt
    need(n)
    n

  // Reads an indefinite-length byte string by concatenating its definite-
  // length chunks (each prefixed with major type 2) until a Break stop code.
  private def readIndefiniteByteString(): IArray[Byte] raises CborError =
    val buffer = ArrayBuffer.empty[Byte]
    var done = false
    while !done do
      need(1)
      val head = data(offset) & 0xFF
      if head == Break then
        offset += 1
        done = true
      else
        val major = head >>> 5
        val info = head & 0x1F
        if major != 2 then abort(CborError(Reason.Reserved(offset.toLong, head)))
        val chunkOffset = offset.toLong
        offset += 1
        val len = boundedLength(readLength(info, chunkOffset), chunkOffset)
        var i = 0
        while i < len do { buffer += data(offset + i); i += 1 }
        offset += len

    val out = new Array[Byte](buffer.length)
    var i = 0
    while i < buffer.length do { out(i) = buffer(i); i += 1 }
    out.asInstanceOf[IArray[Byte]]

  private def readIndefiniteTextString(): String raises CborError =
    val buffer = ArrayBuffer.empty[Byte]
    var done = false
    while !done do
      need(1)
      val head = data(offset) & 0xFF
      if head == Break then
        offset += 1
        done = true
      else
        val major = head >>> 5
        val info = head & 0x1F
        if major != 3 then abort(CborError(Reason.Reserved(offset.toLong, head)))
        val chunkOffset = offset.toLong
        offset += 1
        val len = boundedLength(readLength(info, chunkOffset), chunkOffset)
        var i = 0
        while i < len do { buffer += data(offset + i); i += 1 }
        offset += len

    val out = new Array[Byte](buffer.length)
    var i = 0
    while i < buffer.length do { out(i) = buffer(i); i += 1 }
    decodeUtf8(out, 0, out.length, 0L)

  private inline def decodeUtf8
    ( bytes: Array[Byte], start: Int, len: Int, errorOffset: Long )
  :   String raises CborError =

    try new String(bytes, start, len, java.nio.charset.StandardCharsets.UTF_8)
    catch case _: Throwable => abort(CborError(Reason.InvalidUtf8(errorOffset)))

  // IEEE 754 half precision (16-bit) → Double, per RFC 8949 §3.3.
  private def halfToDouble(half: Int): Double =
    val sign = (half >>> 15) & 0x1
    val exp = (half >>> 10) & 0x1F
    val mant = half & 0x3FF
    val value =
      if exp == 0 then
        if mant == 0 then 0.0
        else math.pow(2, -14)*(mant.toDouble/1024.0)
      else if exp == 31 then
        if mant == 0 then Double.PositiveInfinity else Double.NaN
      else
        math.pow(2, exp - 15)*(1 + mant.toDouble/1024.0)
    if sign == 1 then -value else value

  def readValue(): CborAst raises CborError =
    val pos = offset
    if pos >= data.length then abort(CborError(Reason.Truncated(pos.toLong)))
    val head = data(pos) & 0xFF
    offset = pos + 1

    // Fast paths for in-head small integers — by far the most common CBOR
    // head bytes in real workloads. Returning early skips the major/info
    // split, the `readLength` dispatch and the `headOffset` capture.
    //   head 0x00–0x17 : major 0, info 0–23  → value is head itself
    //   head 0x20–0x37 : major 1, info 0–23  → value is -1 - (head & 0x1F)
    if head < 0x18 then return CborAst(head.toLong)
    if head >= 0x20 && head < 0x38 then return CborAst(-1L - (head & 0x1F).toLong)

    // Fast path for short text strings (major 3, info 0–23, head 0x60–0x77).
    // These dominate map keys and short literals; a length-prefixed UTF-8
    // payload skips the major-switch and `readLength` chain.
    if head >= 0x60 && head < 0x78 then
      val len = head & 0x1F
      val end = pos + 1 + len
      if end > data.length then abort(CborError(Reason.Truncated(pos.toLong)))
      val str = new String(data, pos + 1, len, java.nio.charset.StandardCharsets.UTF_8)
      offset = end
      return CborAst(str)

    // Fast path for short byte strings (major 2, info 0–23, head 0x40–0x57).
    if head >= 0x40 && head < 0x58 then
      val len = head & 0x1F
      val end = pos + 1 + len
      if end > data.length then abort(CborError(Reason.Truncated(pos.toLong)))
      val out = new Array[Byte](len)
      System.arraycopy(data, pos + 1, out, 0, len)
      offset = end
      return CborAst(out.asInstanceOf[IArray[Byte]])

    val headOffset = pos.toLong
    val major = head >>> 5
    val info = head & 0x1F

    (major: @scala.annotation.switch) match
      case 0 =>
        val len = readLength(info, headOffset)
        if len < 0 then abort(CborError(Reason.Reserved(headOffset, head)))
        CborAst(len)

      case 1 =>
        val len = readLength(info, headOffset)
        if len < 0 then abort(CborError(Reason.Reserved(headOffset, head)))
        if len == Long.MinValue then abort(CborError(Reason.Overflow(headOffset)))
        CborAst(-1L - len)

      case 2 =>
        if info == 31 then CborAst(readIndefiniteByteString())
        else
          val len = boundedLength(readLength(info, headOffset), headOffset)
          CborAst(readBytes(len))

      case 3 =>
        if info == 31 then CborAst(readIndefiniteTextString())
        else
          val len = boundedLength(readLength(info, headOffset), headOffset)
          val str = decodeUtf8(data, offset, len, headOffset)
          offset += len
          CborAst(str)

      case 4 =>
        if info == 31 then
          val items = ArrayBuffer.empty[Any]
          var done = false
          while !done do
            need(1)
            if (data(offset) & 0xFF) == Break then { offset += 1; done = true }
            else items += readValue()
          CborAst.arr(IArray.from(items))
        else
          val len = readLength(info, headOffset)
          if len < 0 || len > Int.MaxValue
          then abort(CborError(Reason.Overflow(headOffset)))
          val n = len.toInt
          // Allocate directly in the parity-padded shape used by `CborAst.arr`
          // (odd length, with sentinel pad if logical n is even). One allocation
          // instead of two; no separate IArray.from copy.
          val padded = (n & 1) == 0
          val items = new Array[Any](if padded then n + 1 else n)
          var i = 0
          while i < n do { items(i) = readValue(); i += 1 }
          if padded then items(n) = CborAst.arrayPad
          CborAst(items.asInstanceOf[IArray[Any]])

      case 5 =>
        if info == 31 then
          val keys = ArrayBuffer.empty[Any]
          val values = ArrayBuffer.empty[Any]
          var done = false
          while !done do
            need(1)
            if (data(offset) & 0xFF) == Break then { offset += 1; done = true }
            else
              keys += readValue()
              values += readValue()
          CborAst.map(IArray.from(keys), IArray.from(values))
        else
          val len = readLength(info, headOffset)
          if len < 0 || len > Int.MaxValue
          then abort(CborError(Reason.Overflow(headOffset)))
          val n = len.toInt
          // Allocate the interleaved key/value array directly. One allocation
          // instead of three (keys + values + concatenated result).
          val items = new Array[Any](n*2)
          var i = 0
          while i < n do
            items(i*2) = readValue()
            items(i*2 + 1) = readValue()
            i += 1
          CborAst(items.asInstanceOf[IArray[Any]])

      case 6 =>
        val tag = readLength(info, headOffset)
        if tag < 0 then abort(CborError(Reason.Reserved(headOffset, head)))
        val inner = readValue()
        CborAst(CborTag(tag, inner))

      case 7 =>
        info match
          case 20 => CborAst(false)
          case 21 => CborAst(true)
          case 22 => CborAst(null)
          case 23 => CborAst(vacuous.Unset)
          case 25 => CborAst(halfToDouble(readUInt16()))
          case 26 => CborAst(java.lang.Float.intBitsToFloat(readUInt32().toInt).toDouble)
          case 27 => CborAst(java.lang.Double.longBitsToDouble(readUInt64()))
          case 24 => abort(CborError(Reason.BadSimpleValue(headOffset, readUInt8())))
          case 31 => abort(CborError(Reason.UnexpectedBreak(headOffset)))
          case _  => abort(CborError(Reason.BadSimpleValue(headOffset, info)))

      case _ => abort(CborError(Reason.Reserved(headOffset, head)))
