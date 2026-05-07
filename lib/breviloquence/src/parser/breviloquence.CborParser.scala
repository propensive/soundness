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
    if parser.offset < source.length then abort(CborError(Reason.Trailing(parser.offset)))
    result

private[breviloquence] final class CborParser(bytes: IArray[Byte]):
  import CborParser.Break

  var offset: Int = 0

  private inline def remaining: Int = bytes.length - offset

  private def need(count: Int): Unit raises CborError =
    if remaining < count then abort(CborError(Reason.Truncated(offset.toLong)))

  private def readByte(): Int =
    val b = bytes(offset) & 0xFF
    offset += 1
    b

  private def readUInt8(): Int raises CborError =
    need(1)
    readByte()

  private def readUInt16(): Int raises CborError =
    need(2)
    val b0 = readByte()
    val b1 = readByte()
    (b0 << 8) | b1

  private def readUInt32(): Long raises CborError =
    need(4)
    val b0 = readByte().toLong
    val b1 = readByte().toLong
    val b2 = readByte().toLong
    val b3 = readByte().toLong
    (b0 << 24) | (b1 << 16) | (b2 << 8) | b3

  private def readUInt64(): Long raises CborError =
    need(8)
    val b0 = readByte().toLong
    val b1 = readByte().toLong
    val b2 = readByte().toLong
    val b3 = readByte().toLong
    val b4 = readByte().toLong
    val b5 = readByte().toLong
    val b6 = readByte().toLong
    val b7 = readByte().toLong
    (b0 << 56) | (b1 << 48) | (b2 << 40) | (b3 << 32) | (b4 << 24) | (b5 << 16) | (b6 << 8) | b7

  // Decodes the additional-info length field, returning the unsigned value as
  // a `Long`. A negative result means indefinite length.
  private def readLength(info: Int, headOffset: Long): Long raises CborError =
    if info < 24 then info.toLong
    else if info == 24 then readUInt8().toLong
    else if info == 25 then readUInt16().toLong
    else if info == 26 then readUInt32()
    else if info == 27 then
      val v = readUInt64()
      // Bit 63 set means the value > Long.MaxValue; CBOR allows this for
      // major types 0/1 but breviloquence rejects it.
      if v < 0 then abort(CborError(Reason.Overflow(headOffset)))
      v
    else if info == 31 then -1L
    else abort(CborError(Reason.Reserved(headOffset, info)))

  private def readBytes(length: Int): IArray[Byte] =
    val result = new Array[Byte](length)
    System.arraycopy(bytes.asInstanceOf[Array[Byte]], offset, result, 0, length)
    offset += length
    result.asInstanceOf[IArray[Byte]]

  private def boundedLength(length: Long, headOffset: Long): Int raises CborError =
    if length < 0 || length > Int.MaxValue then abort(CborError(Reason.Overflow(headOffset)))
    val n = length.toInt
    need(n)
    n

  // Reads an indefinite-length byte string by concatenating its definite-
  // length chunks (each prefixed with major type 2) until a Break stop code.
  private def readIndefiniteByteString(): IArray[Byte] raises CborError =
    val buffer = ArrayBuffer.empty[Byte]
    var done = false
    while !done do
      need(1)
      val head = bytes(offset) & 0xFF
      if head == Break then
        offset += 1
        done = true
      else
        val major = head >>> 5
        val info = head & 0x1F
        if major != 2 then abort(CborError(Reason.Reserved(offset.toLong, head)))
        val chunkOffset = offset.toLong
        offset += 1
        val length = boundedLength(readLength(info, chunkOffset), chunkOffset)
        var i = 0
        while i < length do { buffer += bytes(offset + i); i += 1 }
        offset += length

    val out = new Array[Byte](buffer.length)
    var i = 0
    while i < buffer.length do { out(i) = buffer(i); i += 1 }
    out.asInstanceOf[IArray[Byte]]

  private def readIndefiniteTextString(): String raises CborError =
    val buffer = ArrayBuffer.empty[Byte]
    var done = false
    while !done do
      need(1)
      val head = bytes(offset) & 0xFF
      if head == Break then
        offset += 1
        done = true
      else
        val major = head >>> 5
        val info = head & 0x1F
        if major != 3 then abort(CborError(Reason.Reserved(offset.toLong, head)))
        val chunkOffset = offset.toLong
        offset += 1
        val length = boundedLength(readLength(info, chunkOffset), chunkOffset)
        var i = 0
        while i < length do { buffer += bytes(offset + i); i += 1 }
        offset += length

    val out = new Array[Byte](buffer.length)
    var i = 0
    while i < buffer.length do { out(i) = buffer(i); i += 1 }
    decodeUtf8(out, 0L)

  private def decodeUtf8(input: Array[Byte], errorOffset: Long): String raises CborError =
    try new String(input, "UTF-8")
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
    need(1)
    val headOffset = offset.toLong
    val head = bytes(offset) & 0xFF
    offset += 1
    val major = head >>> 5
    val info = head & 0x1F

    major match
      case 0 =>
        val length = readLength(info, headOffset)
        if length < 0 then abort(CborError(Reason.Reserved(headOffset, head)))
        CborAst(length)

      case 1 =>
        val length = readLength(info, headOffset)
        if length < 0 then abort(CborError(Reason.Reserved(headOffset, head)))
        // -1 - n: when n > Long.MaxValue, the result overflows.
        if length == Long.MinValue then abort(CborError(Reason.Overflow(headOffset)))
        CborAst(-1L - length)

      case 2 =>
        if info == 31 then CborAst(readIndefiniteByteString())
        else
          val length = boundedLength(readLength(info, headOffset), headOffset)
          CborAst(readBytes(length))

      case 3 =>
        if info == 31 then CborAst(readIndefiniteTextString())
        else
          val length = boundedLength(readLength(info, headOffset), headOffset)
          val raw = readBytes(length).asInstanceOf[Array[Byte]]
          CborAst(decodeUtf8(raw, headOffset))

      case 4 =>
        if info == 31 then
          val items = ArrayBuffer.empty[Any]
          var done = false
          while !done do
            need(1)
            if (bytes(offset) & 0xFF) == Break then { offset += 1; done = true }
            else items += readValue()
          CborAst.arr(IArray.from(items))
        else
          val length = readLength(info, headOffset)
          if length < 0 || length > Int.MaxValue
          then abort(CborError(Reason.Overflow(headOffset)))
          val n = length.toInt
          val items = new Array[Any](n)
          var i = 0
          while i < n do { items(i) = readValue(); i += 1 }
          CborAst.arr(items.asInstanceOf[IArray[Any]])

      case 5 =>
        if info == 31 then
          val keys = ArrayBuffer.empty[Any]
          val values = ArrayBuffer.empty[Any]
          var done = false
          while !done do
            need(1)
            if (bytes(offset) & 0xFF) == Break then { offset += 1; done = true }
            else
              keys += readValue()
              values += readValue()
          CborAst.map(IArray.from(keys), IArray.from(values))
        else
          val length = readLength(info, headOffset)
          if length < 0 || length > Int.MaxValue
          then abort(CborError(Reason.Overflow(headOffset)))
          val n = length.toInt
          val keys = new Array[Any](n)
          val values = new Array[Any](n)
          var i = 0
          while i < n do
            keys(i) = readValue()
            values(i) = readValue()
            i += 1
          CborAst.map(keys.asInstanceOf[IArray[Any]], values.asInstanceOf[IArray[Any]])

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
