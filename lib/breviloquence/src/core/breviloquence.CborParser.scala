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
import rudiments.*

import CborError.Reason

private[breviloquence] object CborParser:

  // The break stop code (0xFF) terminates an indefinite-length item.
  private inline val Break = 0xFF

  // Boxed-Long cache covering CBOR's uint16 range. The JDK's `Long.valueOf`
  // only caches -128..127; corpus payloads dominated by small unsigned
  // integers (timestamps, ids, counts) routinely fall outside that window
  // and pay a fresh `java.lang.Long` allocation per value. A flat array
  // lookup is two-to-three times cheaper than allocation in steady state.
  private inline val LongCacheSize = 65536

  private val longCache: Array[AnyRef] =
    val out = new Array[AnyRef](LongCacheSize)
    var index = 0

    while index < LongCacheSize do
      out(index) = java.lang.Long.valueOf(index.toLong).nn
      index += 1

    out

  private inline def boxLong(value: Long): AnyRef =
    if value >= 0L && value < LongCacheSize then longCache(value.toInt)
    else java.lang.Long.valueOf(value).nn

  def parse(source: IArray[Byte]): Cbor.Ast raises CborError =
    val parser = new CborParser(source)
    val result = parser.value()

    if parser.offset < parser.data.length
    then abort(CborError(Reason.Trailing(parser.offset.toLong)))

    result

private[breviloquence] final class CborParser(input: IArray[Byte]):
  import CborParser.{Break, boxLong}

  // Cache the underlying primitive array so reads compile to BALOAD rather
  // than going through `IArray$.apply`. `data.length` is constant-folded by
  // the JIT and cheaper than going through a separate `length` accessor.
  private[breviloquence] val data: Array[Byte] = input.asInstanceOf[Array[Byte]]

  // `offset` is exposed only to the package-private parse() entry point so it
  // can detect trailing bytes after a successful parse. All hot-path reads
  // mutate it directly through the JVM PUTFIELD/GETFIELD.
  var offset: Int = 0

  private inline def expect(count: Int): Unit raises CborError =
    if data.length - offset < count then abort(CborError(Reason.Truncated(offset.toLong)))

  private inline def readByte(): Int =
    (data(offset)&0xFF).also(offset += 1)

  private inline def readUInt8(): Int raises CborError =
    expect(1)
    readByte()

  private inline def readUInt16(): Int raises CborError =
    expect(2)
    val pos = offset
    offset = pos + 2
    ((data(pos) & 0xFF) << 8) | (data(pos + 1) & 0xFF)

  private inline def readUInt32(): Long raises CborError =
    expect(4)
    val pos = offset
    offset = pos + 4
    ((data(pos) & 0xFFL) << 24)
    | ((data(pos + 1) & 0xFFL) << 16)
    | ((data(pos + 2) & 0xFFL) << 8)
    | (data(pos + 3) & 0xFFL)

  private inline def readUInt64(): Long raises CborError =
    expect(8)
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

  private inline def readBytes(length: Int): IArray[Byte] =
    val result = new Array[Byte](length)
    System.arraycopy(data, offset, result, 0, length)
    offset += length
    result.asInstanceOf[IArray[Byte]]

  private inline def boundedLength(length: Long, headOffset: Long): Int raises CborError =
    if length < 0 || length > Int.MaxValue then abort(CborError(Reason.Overflow(headOffset)))
    val count = length.toInt
    expect(count)
    count

  // Reads an indefinite-length byte string by concatenating its definite-
  // length chunks (each prefixed with major type 2) until a Break stop code.
  // Uses `ByteArrayOutputStream` so chunk bytes flow through bulk `write`
  // (≈ `System.arraycopy`) without per-byte boxing into `java.lang.Byte`.
  private def readIndefiniteByteString(): IArray[Byte] raises CborError =
    val buffer = new java.io.ByteArrayOutputStream
    var done = false

    while !done do
      expect(1)
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
        val length = boundedLength(readLength(info, chunkOffset), chunkOffset)
        buffer.write(data, offset, length)
        offset += length

    buffer.toByteArray.nn.asInstanceOf[IArray[Byte]]

  private def readIndefiniteTextString(): String raises CborError =
    val buffer = new java.io.ByteArrayOutputStream
    var done = false

    while !done do
      expect(1)
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
        val length = boundedLength(readLength(info, chunkOffset), chunkOffset)
        buffer.write(data, offset, length)
        offset += length

    val bytes = buffer.toByteArray.nn
    decodeUtf8(bytes, 0, bytes.length, 0L)

  private inline def decodeUtf8
    ( bytes: Array[Byte], start: Int, length: Int, errorOffset: Long )
  :   String raises CborError =

    try new String(bytes, start, length, java.nio.charset.StandardCharsets.UTF_8)
    catch case _: Throwable => abort(CborError(Reason.InvalidUtf8(errorOffset)))

  // IEEE 754 half precision (16-bit) → Double, per RFC 8949 §3.3.
  // Assembles the 64-bit pattern directly rather than going through
  // `scala.math.pow` and a multiplication: half-floats have only 65 536 possible
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

  def value(): Cbor.Ast raises CborError =
    val pos = offset
    if pos >= data.length then abort(CborError(Reason.Truncated(pos.toLong)))
    val head = data(pos) & 0xFF
    offset = pos + 1

    // Fast paths for in-head small integers — by far the most common CBOR
    // head bytes in real workloads. Returning early skips the major/info
    // split, the `readLength` dispatch and the `headOffset` capture. Boxing
    // routes through the shared `boxLong` cache so the resulting
    // `java.lang.Long` is reused on the next parse.
    //   head 0x00–0x17 : major 0, info 0–23  → value is head itself
    //   head 0x20–0x37 : major 1, info 0–23  → value is -1 - (head & 0x1F)
    if head < 0x18 then return Cbor.Ast.fromRef(boxLong(head.toLong))

    if head >= 0x20 && head < 0x38 then
      return Cbor.Ast.fromRef(boxLong(-1L - (head & 0x1F).toLong))

    // Fast path for short text strings (major 3, info 0–23, head 0x60–0x77).
    // These dominate map keys and short literals; a length-prefixed UTF-8
    // payload skips the major-switch and `readLength` chain.
    if head >= 0x60 && head < 0x78 then
      val length = head & 0x1F
      val end = pos + 1 + length
      if end > data.length then abort(CborError(Reason.Truncated(pos.toLong)))
      val str = new String(data, pos + 1, length, java.nio.charset.StandardCharsets.UTF_8)
      offset = end
      return Cbor.Ast(str)

    // Fast path for short byte strings (major 2, info 0–23, head 0x40–0x57).
    if head >= 0x40 && head < 0x58 then
      val length = head & 0x1F
      val end = pos + 1 + length
      if end > data.length then abort(CborError(Reason.Truncated(pos.toLong)))
      val out = new Array[Byte](length)
      System.arraycopy(data, pos + 1, out, 0, length)
      offset = end
      return Cbor.Ast(out.asInstanceOf[IArray[Byte]])

    val headOffset = pos.toLong
    val major = head >>> 5
    val info = head & 0x1F

    (major: @scala.annotation.switch) match
      case 0 =>
        val length = readLength(info, headOffset)
        if length < 0 then abort(CborError(Reason.Reserved(headOffset, head)))
        Cbor.Ast.fromRef(boxLong(length))

      case 1 =>
        val length = readLength(info, headOffset)
        if length < 0 then abort(CborError(Reason.Reserved(headOffset, head)))
        if length == Long.MinValue then abort(CborError(Reason.Overflow(headOffset)))
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
          val str = decodeUtf8(data, offset, length, headOffset)
          offset += length
          Cbor.Ast(str)

      case 4 =>
        if info == 31 then
          // Build directly into an `Array[Any]`; flip to parity-padded shape
          // once the Break is seen rather than copying through `IArray.from`
          // and then re-allocating in `Ast.array`.
          val items = ArrayBuffer.empty[Any]
          var done = false

          while !done do
            expect(1)

            if (data(offset) & 0xFF) == Break then
              offset += 1
              done = true
            else
              items += value()

          val count = items.length
          val padded = (count&1) == 0
          val out = new Array[Any](if padded then count + 1 else count)
          var index = 0

          while index < count do
            out(index) = items(index)
            index += 1

          if padded then out(count) = Cbor.Ast.Sentinel
          Cbor.Ast(out.asInstanceOf[IArray[Any]])
        else
          val length = readLength(info, headOffset)

          if length < 0 || length > Int.MaxValue
          then abort(CborError(Reason.Overflow(headOffset)))
          val count = length.toInt
          // Allocate directly in the parity-padded shape used by `Cbor.Ast.array`
          // (odd length, with sentinel pad if logical count is even). One allocation
          // instead of two; no separate IArray.from copy.
          val padded = (count&1) == 0
          val items = new Array[Any](if padded then count + 1 else count)
          var index = 0

          while index < count do
            items(index) = value()
            index += 1

          if padded then items(count) = Cbor.Ast.Sentinel
          Cbor.Ast(items.asInstanceOf[IArray[Any]])

      case 5 =>
        if info == 31 then
          // Build directly into one interleaved `Array[Any]`. The previous
          // shape (two `ArrayBuffer`s + `IArray.from` twice + `Ast.map`'s
          // own `new Array[Any](count*2)` copy) was four allocations and
          // three full passes; one buffer + one `arraycopy`-equivalent loop
          // is enough.
          val items = ArrayBuffer.empty[Any]
          var done = false

          while !done do
            expect(1)

            if (data(offset) & 0xFF) == Break then
              offset += 1
              done = true
            else
              items += value()
              items += value()

          val out = new Array[Any](items.length)
          var index = 0
          while index < items.length do { out(index) = items(index); index += 1 }
          Cbor.Ast(out.asInstanceOf[IArray[Any]])

        else
          val length = readLength(info, headOffset)

          if length < 0 || length > Int.MaxValue
          then abort(CborError(Reason.Overflow(headOffset)))

          val count = length.toInt
          val items = new Array[Any](count*2)
          var index = 0

          while index < count do
            items(index*2) = value()
            items(index*2 + 1) = value()
            index += 1

          Cbor.Ast(items.asInstanceOf[IArray[Any]])

      case 6 =>
        val tag = readLength(info, headOffset)
        if tag < 0 then abort(CborError(Reason.Reserved(headOffset, head)))
        val inner = value()
        Cbor.Ast(Cbor.Tag(tag, inner))

      case 7 =>
        info match
          case 20 => Cbor.Ast(false)
          case 21 => Cbor.Ast(true)
          case 22 => Cbor.Ast(null)
          case 23 => Cbor.Ast(vacuous.Unset)
          case 25 => Cbor.Ast(halfToDouble(readUInt16()))
          case 26 => Cbor.Ast(java.lang.Float.intBitsToFloat(readUInt32().toInt).toDouble)
          case 27 => Cbor.Ast(java.lang.Double.longBitsToDouble(readUInt64()))
          case 24 => abort(CborError(Reason.BadSimpleValue(headOffset, readUInt8())))
          case 31 => abort(CborError(Reason.UnexpectedBreak(headOffset)))
          case _  => abort(CborError(Reason.BadSimpleValue(headOffset, info)))

      case _ => abort(CborError(Reason.Reserved(headOffset, head)))
