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
package pneumatic

import scala.collection.mutable as scm

import proscenium.compat.*
import rudiments.*
import vacuous.*

// The `.xz` stream container: a 12-byte header (6-byte magic, 2-byte flags naming the check type, a
// CRC-32 of the flags), then one or more blocks, then an index and a footer. Each block has a
// self-describing header (its filter chain — here always the single LZMA2 filter, whose one
// property byte encodes the dictionary size), the LZMA2 payload, four-byte-aligned padding, and the
// integrity check over the block's uncompressed data. Sizes are LEB128 variable-length integers.
//
// Decoding buffers the whole compressed stream, then walks the blocks. Only the single-LZMA2-filter
// chain is understood; delta/BCJ filters and multi-filter chains are rejected.
private[pneumatic] object XzContainer:
  val magic: Array[Byte]^{} =
    Array.unsafeFrozen:
      scala.Array(0xfd.toByte, '7', 'z', 'X', 'Z', 0x00)
  inline val Lzma2FilterId = 0x21
  inline val IndexIndicator = 0x00

  private def readVli(buffer: scala.Array[Byte], position: Int): (Long, Int) =
    var result = 0L
    var shift = 0
    var pos = position
    var continue = true

    while continue do
      if pos >= buffer.length then throw IllegalStateException("the XZ data is corrupt: truncated")
      val byte = buffer(pos) & 0xff
      pos += 1
      result |= (byte & 0x7f).toLong << shift
      if (byte & 0x80) == 0 then continue = false else shift += 7
      if shift > 63 then throw IllegalStateException("the XZ data is corrupt: oversized integer")

    (result, pos)

  // Decode a complete `.xz` stream, appending its uncompressed bytes to `sink`.
  def decode(buffer: scala.Array[Byte], sink: scm.ArrayBuffer[Byte]): Unit =
    if buffer.length < 12 then
      throw IllegalStateException("the XZ data is corrupt: truncated header")

    var i = 0

    while i < 6 do
      if buffer(i) != magic.readUnchecked(i) then
        throw IllegalStateException("the data is not in XZ format: bad magic bytes")

      i += 1

    if buffer(6) != 0 then throw IllegalStateException("the XZ data is corrupt: reserved flag set")
    val checkType = buffer(7) & 0xff
    val checkSize = XzCheck.size(checkType)

    var pos = 12

    while pos < buffer.length && (buffer(pos) & 0xff) != IndexIndicator do
      val headerSizeByte = buffer(pos) & 0xff
      val headerSize = (headerSizeByte + 1)*4
      val flags = buffer(pos + 1) & 0xff
      val filterCount = (flags & 0x03) + 1
      val compressedSizePresent = (flags & 0x40) != 0
      val uncompressedSizePresent = (flags & 0x80) != 0

      if (flags & 0x3c) != 0 then
        throw IllegalStateException("the XZ data is corrupt: reserved block flags set")

      var cursor = pos + 2
      if compressedSizePresent then cursor = readVli(buffer, cursor)(1)
      if uncompressedSizePresent then cursor = readVli(buffer, cursor)(1)

      var dictSizeByte = -1
      var filter = 0

      while filter < filterCount do
        val (filterId, afterId) = readVli(buffer, cursor)
        val (propsSize, afterProps) = readVli(buffer, afterId)

        if filterId != Lzma2FilterId then
          throw IllegalStateException("this XZ stream uses an unsupported filter")

        if propsSize != 1L then
          throw IllegalStateException("the XZ data is corrupt: bad LZMA2 properties size")

        dictSizeByte = buffer(afterProps) & 0xff
        cursor = afterProps + 1
        filter += 1

      if dictSizeByte < 0 then
        throw IllegalStateException("the XZ data is corrupt: missing LZMA2 filter")

      val blockDataStart = pos + headerSize
      val dictSize = Lzma2Options.byteToDictSize(dictSizeByte)

      val decompressor: Lzma2Decompressor^ = Lzma2Decompressor(dictSize)
      // `buffer` reaches `decode` from `BufferedEngine.transform`, which builds it fresh from
      // its accumulated input, so nothing else holds it.
      decompressor.accept
       ( Array.unsafeFrozen(buffer), blockDataStart, buffer.length - blockDataStart )
      decompressor.finish()

      if !decompressor.ended then
        throw IllegalStateException("the XZ data is corrupt: block did not terminate")

      val blockOutputStart = sink.length
      sink ++= decompressor.output
      val compressedLength = decompressor.consumed
      val padding = (-compressedLength) & 3
      val checkStart = blockDataStart + compressedLength + padding

      // Verify the block's integrity check over its uncompressed output.
      if checkSize > 0 then
        val checker: XzChecker^ = XzCheck.checker(checkType)
        val decoded: scala.Array[Byte]^ = new scala.Array[Byte](sink.length - blockOutputStart)
        var d = 0
        while d < decoded.length do { decoded(d) = sink(blockOutputStart + d); d += 1 }
        checker.absorb(decoded, 0, decoded.length)
        val expected: scala.Array[Byte]^ = checker.bytes
        var c = 0

        while c < checkSize do
          if buffer(checkStart + c) != expected(c) then
            throw IllegalStateException("the XZ data is corrupt: integrity check failed")

          c += 1

      pos = checkStart + checkSize

    // The index and stream footer that follow are not needed to reproduce the payload.

  private def appendBytes(buffer: scm.ArrayBuffer[Byte], bytes: scala.Array[Byte]): Unit =
    var i = 0
    while i < bytes.length do { buffer += bytes(i); i += 1 }

  private def crc32Bytes(bytes: scala.Array[Byte], offset: Int, length: Int): scala.Array[Byte] =
    val crc = Crc32()
    crc.update(bytes, offset, length)
    val value = crc.value
    val out: scala.Array[Byte]^ = new scala.Array[Byte](4)
    var i = 0
    while i < 4 do { out(i) = ((value >>> (i*8)) & 0xff).toByte; i += 1 }
    out

  private def writeVli(buffer: scm.ArrayBuffer[Byte], value: Long): Unit =
    var v = value

    while v >= 0x80 do
      buffer += ((v & 0x7f) | 0x80).toByte
      v >>>= 7

    buffer += (v & 0x7f).toByte

  private def blockHeader(dictSizeByte: Int): scala.Array[Byte] =
    // A 12-byte header: size byte, flags (one filter, no explicit sizes), the LZMA2 filter with its
    // one dictionary-size property byte, zero padding, then a CRC-32 over the first eight bytes.
    val header: scala.Array[Byte]^ = new scala.Array[Byte](12)
    header(0) = 0x02 // (12 / 4) - 1
    header(1) = 0x00 // one filter, no compressed/uncompressed size fields
    header(2) = XzContainer.Lzma2FilterId.toByte
    header(3) = 0x01 // property size
    header(4) = dictSizeByte.toByte
    val crc = crc32Bytes(header, 0, 8)
    System.arraycopy(crc, 0, header, 8, 4)
    header

  // The 12-byte stream header (magic, flags naming the check type, a CRC-32 of the flags).
  def streamHeader(checkType: Int): scala.Array[Byte] =
    val out = scm.ArrayBuffer[Byte]()
    appendBytes(out, magic.readable.to(scala.Array))
    val flags = scala.Array[Byte](0x00, checkType.toByte)
    appendBytes(out, flags)
    appendBytes(out, crc32Bytes(flags, 0, 2))
    out.toArray

  // One complete block (header, LZMA2 payload, 4-byte-aligned padding, integrity check) for `data`,
  // paired with its unpadded size for the index. Used both whole-value and per-segment when
  // streaming, so each block bounds the compressor's working memory.
  def block(data: scala.Array[Byte], checkType: Int, options: Lzma2Options): (scala.Array[Byte], Long) =
    val out = scm.ArrayBuffer[Byte]()
    val payload = Lzma2Compressor(data, options).compress()
    val header = blockHeader(Lzma2Options.dictSizeToByte(options.dictSize))
    appendBytes(out, header)
    appendBytes(out, payload)

    var padding = (-payload.length) & 3
    while padding > 0 do { out += 0.toByte; padding -= 1 }

    val checker: XzChecker^ = XzCheck.encoder(checkType)
    checker.absorb(data, 0, data.length)
    val checkBytes: scala.Array[Byte]^ = checker.bytes
    appendBytes(out, checkBytes)

    (out.toArray, (header.length + payload.length + checkBytes.length).toLong)

  // The index and stream footer that close a stream, given each block's (unpadded, uncompressed)
  // sizes in order.
  def indexAndFooter(records: scm.ArrayBuffer[(Long, Long)], checkType: Int): scala.Array[Byte] =
    val out = scm.ArrayBuffer[Byte]()

    val index = scm.ArrayBuffer[Byte]()
    index += IndexIndicator.toByte
    writeVli(index, records.length.toLong)

    records.foreach: (unpadded, uncompressed) =>
      writeVli(index, unpadded)
      writeVli(index, uncompressed)

    while index.length % 4 != 0 do index += 0.toByte
    val indexBody = index.toArray
    appendBytes(out, indexBody)
    appendBytes(out, crc32Bytes(indexBody, 0, indexBody.length))

    val indexSize = indexBody.length + 4
    val footer: scala.Array[Byte]^ = new scala.Array[Byte](12)
    val backward = indexSize/4 - 1
    footer(4) = (backward & 0xff).toByte
    footer(5) = ((backward >>> 8) & 0xff).toByte
    footer(6) = ((backward >>> 16) & 0xff).toByte
    footer(7) = ((backward >>> 24) & 0xff).toByte
    footer(8) = 0x00
    footer(9) = checkType.toByte
    val footerCrc = crc32Bytes(footer, 4, 6)
    System.arraycopy(footerCrc, 0, footer, 0, 4)
    footer(10) = 'Y'
    footer(11) = 'Z'
    appendBytes(out, footer)

    out.toArray

  // Encode `data` as a complete single-block `.xz` stream (empty input yields a block-less stream).
  def encode(data: scala.Array[Byte], checkType: Int, options: Lzma2Options): scala.Array[Byte] =
    val out = scm.ArrayBuffer[Byte]()
    appendBytes(out, streamHeader(checkType))
    val records = scm.ArrayBuffer[(Long, Long)]()

    if data.length > 0 then
      val (blockBytes, unpadded) = block(data, checkType, options)
      appendBytes(out, blockBytes)
      records += ((unpadded, data.length.toLong))

    appendBytes(out, indexAndFooter(records, checkType))
    out.toArray
