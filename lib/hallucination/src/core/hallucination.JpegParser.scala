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
package hallucination

import contingency.*
import proscenium.compat.*
import vacuous.*

import RasterError.Reason

// JPEG marker codes (the byte following an `0xFF`), from Table B.1. Only the subset needed to
// decode baseline and progressive Huffman-coded images is named here.
private[hallucination] object JpegMarker:
  inline val Soi = 0xd8
  inline val Eoi = 0xd9
  inline val Sos = 0xda
  inline val Dqt = 0xdb
  inline val Dht = 0xc4
  inline val Dri = 0xdd
  inline val Dac = 0xcc
  inline val Dnl = 0xdc
  inline val Dhp = 0xde
  inline val Exp = 0xdf
  inline val Com = 0xfe

  def isSof(code: Int): Boolean =
    code >= 0xc0 && code <= 0xcf && code != Dht && code != 0xc8 && code != Dac

  def isApp(code: Int): Boolean = code >= 0xe0 && code <= 0xef
  def isRst(code: Int): Boolean = code >= 0xd0 && code <= 0xd7

  // Whether the marker segment carries a two-byte length; standalone markers (RST, SOI, EOI, TEM)
  // do not.
  def hasLength(code: Int): Boolean =
    !(isRst(code) || code == Soi || code == Eoi || code == 0x01)

// The coding process of a frame. Hierarchical, lossless and arithmetic-coded frames are rejected
// during parsing, so only these two Huffman-coded DCT processes reach the decoder.
private[hallucination] enum JpegCoding:
  case Sequential, Progressive

private[hallucination] final case class JpegDimensions(width: Int, height: Int)

// One colour component (plane) of the frame. Sampling factors and the quantization-table index
// come from the frame header; the pixel and block dimensions are derived once all components are
// known, so `parseSof` builds every component fully-initialized.
private[hallucination] final class JpegComponent
  ( val identifier:               Int,
    val horizontalSamplingFactor: Int,
    val verticalSamplingFactor:   Int,
    val quantizationTableIndex:   Int,
    val sizeWidth:                Int,
    val sizeHeight:               Int,
    val blockWidth:               Int,
    val blockHeight:              Int ):

  // The full-resolution IDCT is always used (no thumbnail scaling), so the DCT scale is fixed at 8.
  val dctScale: Int = 8

private[hallucination] final class JpegFrame
  ( val isBaseline:    Boolean,
    val coding:        JpegCoding,
    val precision:     Int,
    val imageWidth:    Int,
    val imageHeight:   Int,
    val mcuWidth:      Int,
    val mcuHeight:     Int,
    val components:    scala.IArray[JpegComponent] )

private[hallucination] final class JpegScan
  ( val componentIndices: scala.IArray[Int],
    val dcTableIndices:   scala.IArray[Int],
    val acTableIndices:   scala.IArray[Int],
    val spectralStart:     Int,
    val spectralEnd:       Int, // exclusive
    val successiveHigh:    Int,
    val successiveLow:     Int )

// The result of an APP marker relevant to colour interpretation; other application data (ICC,
// Exif, XMP) is skipped.
private[hallucination] enum JpegApp:
  case None, Jfif, Avi1
  case Adobe(transform: Int)

private[hallucination] object JpegParser:
  private def bad(): Nothing raises RasterError =
    abort(RasterError(Jpeg(), Reason.Bitstream))

  private def unsupported(): Nothing raises RasterError =
    abort(RasterError(Jpeg(), Reason.UnsupportedVariant))

  private def readLength(reader: JpegReader^)(using Tactic[RasterError]): Int =
    val length = reader.u16()
    if length < 2 then bad() else length - 2

  private def ceilDiv(x: Int, y: Int): Int raises RasterError =
    if x <= 0 || y <= 0 then bad() else 1 + (x - 1)/y

  // Section B.2.2: the Start Of Frame header.
  def parseSof(reader: JpegReader^, code: Int)(using Tactic[RasterError]): JpegFrame =
    val length = readLength(reader)
    if length <= 6 then bad()

    val sofNumber = code - 0xc0

    val coding = sofNumber match
      case 0 | 1 => JpegCoding.Sequential
      case 2     => JpegCoding.Progressive
      case _     => unsupported() // lossless, differential, hierarchical or arithmetic coding

    val isBaseline = sofNumber == 0
    val precision = reader.u8()
    if precision != 8 then unsupported()

    val height = reader.u16()
    val width = reader.u16()
    if height == 0 then unsupported() // DNL-defined height
    if width == 0 then bad()

    val componentCount = reader.u8()
    if componentCount == 0 then bad()
    if coding == JpegCoding.Progressive && componentCount > 4 then bad()
    if length != 6 + 3*componentCount then bad()
    if componentCount != 1 && componentCount != 3 && componentCount != 4 then unsupported()

    // First read the raw per-component parameters; the pixel and block dimensions can only be
    // derived once the maximum sampling factors across all components are known.
    val identifiers = new scala.Array[Int](componentCount)
    val horizontals = new scala.Array[Int](componentCount)
    val verticals = new scala.Array[Int](componentCount)
    val quantizationIndices = new scala.Array[Int](componentCount)
    var index = 0

    while index < componentCount do
      val identifier = reader.u8()

      var duplicate = 0

      while duplicate < index do
        if identifiers(duplicate) == identifier then bad()
        duplicate += 1

      val sampling = reader.u8()
      val horizontal = sampling >> 4
      val vertical = sampling & 0x0f
      if horizontal == 0 || horizontal > 4 then bad()
      if vertical == 0 || vertical > 4 then bad()

      val quantizationIndex = reader.u8()
      if quantizationIndex > 3 then bad()

      identifiers(index) = identifier
      horizontals(index) = horizontal
      verticals(index) = vertical
      quantizationIndices(index) = quantizationIndex
      index += 1

    var hMax = 0
    var vMax = 0
    index = 0

    while index < componentCount do
      hMax = hMax.max(horizontals(index))
      vMax = vMax.max(verticals(index))
      index += 1

    val mcuWidth = ceilDiv(width, hMax*8)
    val mcuHeight = ceilDiv(height, vMax*8)

    val components = new scala.Array[JpegComponent](componentCount)
    index = 0

    while index < componentCount do
      // The DCT scale is fixed at 8 (`JpegComponent.dctScale`).
      components(index) =
        JpegComponent
          ( identifiers(index), horizontals(index), verticals(index), quantizationIndices(index),
            sizeWidth   = ceilDiv(width*horizontals(index)*8, hMax*8),
            sizeHeight  = ceilDiv(height*verticals(index)*8, vMax*8),
            blockWidth  = mcuWidth*horizontals(index),
            blockHeight = mcuHeight*verticals(index) )

      index += 1

    // The freshly-built components array is frozen zero-copy.
    JpegFrame
      ( isBaseline, coding, precision, width, height, mcuWidth, mcuHeight,
        components.asInstanceOf[scala.IArray[JpegComponent]] )

  // Section B.2.3: the Start Of Scan header.
  def parseSos(reader: JpegReader^, frame: JpegFrame)(using Tactic[RasterError]): JpegScan =
    val length = readLength(reader)
    if length == 0 then bad()

    val componentCount = reader.u8()
    if componentCount == 0 || componentCount > 4 then bad()
    if length != 4 + 2*componentCount then bad()

    val componentIndices = new scala.Array[Int](componentCount)
    val dcTableIndices = new scala.Array[Int](componentCount)
    val acTableIndices = new scala.Array[Int](componentCount)
    var index = 0

    while index < componentCount do
      val identifier = reader.u8()

      var componentIndex = -1
      var search = 0

      while search < frame.components.length do
        if frame.components(search).identifier == identifier then componentIndex = search
        search += 1

      if componentIndex == -1 then bad()

      var duplicate = 0

      while duplicate < index do
        if componentIndices(duplicate) == componentIndex then bad()
        duplicate += 1

      val tables = reader.u8()
      val dcTableIndex = tables >> 4
      val acTableIndex = tables & 0x0f
      if dcTableIndex > 3 || (frame.isBaseline && dcTableIndex > 1) then bad()
      if acTableIndex > 3 || (frame.isBaseline && acTableIndex > 1) then bad()

      componentIndices(index) = componentIndex
      dcTableIndices(index) = dcTableIndex
      acTableIndices(index) = acTableIndex
      index += 1

    val spectralStart = reader.u8()
    var spectralEnd = reader.u8()
    val approximation = reader.u8()
    val successiveHigh = approximation >> 4
    val successiveLow = approximation & 0x0f

    if frame.coding == JpegCoding.Progressive then
      if spectralEnd > 63 || spectralStart > spectralEnd ||
        (spectralStart == 0 && spectralEnd != 0)
      then bad()

      if spectralStart != 0 && componentCount != 1 then bad()
      if successiveHigh > 13 || successiveLow > 13 then bad()
      if successiveHigh != 0 && successiveHigh != successiveLow + 1 then bad()
    else
      if spectralEnd == 0 then spectralEnd = 63
      if spectralStart != 0 || spectralEnd != 63 then bad()
      if successiveHigh != 0 || successiveLow != 0 then bad()

    // As `parseSof`: the freshly-built index arrays are frozen zero-copy.
    JpegScan
      ( componentIndices.asInstanceOf[scala.IArray[Int]], dcTableIndices.asInstanceOf[scala.IArray[Int]],
        acTableIndices.asInstanceOf[scala.IArray[Int]], spectralStart, spectralEnd + 1,
        successiveHigh, successiveLow )

  // Section B.2.4.1: quantization tables, each returned in the file's zigzag order (unzigzagged by
  // the decoder). The four slots are indexed by the table's destination identifier.
  def parseDqt(reader: JpegReader^)(using Tactic[RasterError]): scala.Array[Optional[scala.Array[Int]]] =
    var length = readLength(reader)
    val tables: scala.Array[Optional[scala.Array[Int]]] = scala.Array(Unset, Unset, Unset, Unset)

    while length > 0 do
      val byte = reader.u8()
      val precision = byte >> 4
      val index = byte & 0x0f
      if precision > 1 then bad()
      if index > 3 then bad()
      if length < 65 + 64*precision then bad()

      val table = new scala.Array[Int](64)
      var item = 0

      while item < 64 do
        table(item) = if precision == 0 then reader.u8() else reader.u16()
        if table(item) == 0 then bad()
        item += 1

      writable(tables)(index) = table
      length -= 65 + 64*precision

    tables

  // Section B.2.4.2: Huffman tables. Returns the DC tables then the AC tables, four slots each.
  def parseDht(reader: JpegReader^, isBaseline: Boolean)(using Tactic[RasterError])
  :   (scala.Array[Optional[JpegHuffmanTable]], scala.Array[Optional[JpegHuffmanTable]]) =

    var length = readLength(reader)
    val dcTables: scala.Array[Optional[JpegHuffmanTable]]^ = scala.Array(Unset, Unset, Unset, Unset)
    val acTables: scala.Array[Optional[JpegHuffmanTable]]^ = scala.Array(Unset, Unset, Unset, Unset)

    while length > 17 do
      val byte = reader.u8()
      val tableClass = byte >> 4
      val index = byte & 0x0f
      if tableClass != 0 && tableClass != 1 then bad()
      if isBaseline && index > 1 then bad()
      if index > 3 then bad()

      val counts = new scala.Array[Int](16)
      var size = 0
      var count = 0

      while count < 16 do
        counts(count) = reader.u8()
        size += counts(count)
        count += 1

      if size == 0 then bad()
      if size > 256 then bad()
      if size > length - 17 then bad()

      val values = new scala.Array[Int](size)
      var value = 0

      while value < size do
        values(value) = reader.u8()
        value += 1

      val table = JpegHuffmanTable(counts, values, tableClass == 1)
      if tableClass == 0 then dcTables(index) = table
      else acTables(index) = table

      length -= 17 + size

    if length != 0 then bad()
    // The tables privately own their freshly-built arrays; laundered to the pure result.
    scala.caps.unsafe.unsafeAssumePure((dcTables, acTables))

  // Skips a length-prefixed segment whose contents are not needed (e.g. a comment).
  def skipSegment(reader: JpegReader^)(using Tactic[RasterError]): Unit =
    reader.skip(readLength(reader))

  // Section B.2.4.4: the restart interval.
  def parseDri(reader: JpegReader^)(using Tactic[RasterError]): Int =
    val length = readLength(reader)
    if length != 2 then bad()
    reader.u16()

  // Section B.2.4.6: application data. Only the JFIF, AVI1 and Adobe segments, which influence
  // colour interpretation, are recognised; the rest are skipped.
  def parseApp(reader: JpegReader^, code: Int)(using Tactic[RasterError]): JpegApp =
    val length = readLength(reader)
    val buffer = new scala.Array[Byte](length)
    reader.readExact(buffer)

    def matches(prefix: String): Boolean =
      prefix.length <= length && (0 until prefix.length).forall: index =>
        (buffer(index) & 0xff) == prefix.charAt(index).toInt

    if code == 0xe0 && matches("JFIF\u0000") then JpegApp.Jfif
    else if code == 0xe0 && matches("AVI1\u0000") then JpegApp.Avi1
    else if code == 0xee && length >= 12 && matches("Adobe\u0000") then
      val transform = buffer(11) & 0xff
      if transform > 2 then bad()
      JpegApp.Adobe(transform)
    else
      JpegApp.None
