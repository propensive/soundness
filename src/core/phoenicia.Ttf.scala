/*
    Phoenicia, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package phoenicia

import anticipation.*
import bifurcate.*
import contingency.*
import fulminate.*
import gossamer.*
import prepositional.*
import hypotenuse.*
import quantitative.*
import rudiments.*
import turbulence.*
import vacuous.*

object Ttf:
  def apply[SourceType: Readable by Bytes](source: SourceType): Ttf =
    val data = source.read[Bytes]
    Ttf(data)

  enum PlatformId:
    case Unicode, Macintosh, Windows, Custom

  enum EncodingId:
    case Unicode1, Unicode1_1, IsoIec10646, Unicode2Bmp, Unicode2Full, UnicodeVariation, UnicodeFull

case class Ttf(data: Bytes):
  ttf =>

  case class TableOffset(id: TableTag, checksum: B32, offset: Int, length: Int)

  lazy val numTables = B16(data, 4).u16.int
  lazy val searchRange = B16(data, 6).u16.int
  lazy val entrySelector = B16(data, 8).u16.int
  lazy val rangeShift = B16(data, 10).u16.int

  // FIXME: Don't just assume encoding 0
  def glyph(char: Char): Glyph[ttf.type] raises FontError =
    cmap.glyphEncodings(0).format.glyph(char)

  def advanceWidth(char: Char): Int raises FontError = hmtx.metrics(glyph(char).id).advanceWidth

  def width(text: Text): Quantity[Ems[1]] raises FontError =
    text.chars.sumBy(advanceWidth)*Em/head.unitsPerEm.int

  def leftSideBearing(char: Char): Int raises FontError =
    hmtx.metrics(glyph(char).id).leftSideBearing

  lazy val tables: Map[TableTag, TableOffset] =
    (0 until numTables).flatMap: n =>
      val start = 12 + n*16
      val tableTag = String(data.mutable(using Unsafe), start, 4, "ASCII").tt
      val checksum = B32(data, start + 4)
      val offset = B32(data, start + 8).s32.int
      val length = B32(data, start + 12).s32.int

      tableTag match
        case OtfTag(tag) => Some(tag -> TableOffset(tag, checksum, offset, length))
        case TtfTag(tag) => Some(tag -> TableOffset(tag, checksum, offset, length))
        case _           => None
    .to(Map)

  def head: HeadTable raises FontError =
    tables.at(TtfTag.Head).let: ref =>
      data.unpackFrom[HeadTable](ref.offset).tap: table =>
        if table.magicNumber != 0x5f0f3cf5.bits
        then raise(FontError(FontError.Reason.MagicNumber))

    .or(abort(FontError(FontError.Reason.MissingTable(TtfTag.Head))))

  def cmap: CmapTable raises FontError =
    tables.at(TtfTag.Cmap).let: ref =>
      CmapTable(ref.offset)
    .or(abort(FontError(FontError.Reason.MissingTable(TtfTag.Cmap))))

  def hhea: HheaTable raises FontError =
    tables.at(TtfTag.Hhea).let: ref =>
      data.unpackFrom[HheaTable](ref.offset)
    .or(abort(FontError(FontError.Reason.MissingTable(TtfTag.Hhea))))

  def hmtx: HmtxTable raises FontError =
    tables.at(TtfTag.Hmtx).let: ref =>
      HmtxTable(ref.offset, hhea.numberOfHMetrics.int)
    .or(abort(FontError(FontError.Reason.MissingTable(TtfTag.Hmtx))))

  case class HeadTable
     (majorVersion:       U16,
      minorVersion:       U16,
      fontRevisionHigh:   U16,
      fontRevisionLow:    U16,
      checksumAdjustment: B32,
      magicNumber:        B32,
      flags:              B16,
      unitsPerEm:         U16)

  case class HheaTable
     (majorVersion:        U16,
      minorVersion:        U16,
      ascender:            S16,
      descender:           S16,
      lineGap:             S16,
      advanceWidthMax:     U16,
      minLeftSideBearing:  S16,
      minRightSideBearing: S16,
      xMaxExtent:          S16,
      caretSlopeRise:      S16,
      caretSlopeRun:       S16,
      caretOffset:         S16,
      reserved0:           U16,
      reserved1:           U16,
      reserved2:           U16,
      reserved4:           U16,
      metricDataFormat:    S16,
      numberOfHMetrics:    U16)

  case class HmtxTable(offset: Int, count: Int):
    lazy val metrics: IArray[HMetrics] =
      IArray.from:
        (0 until count).map: index =>
          HMetrics(B16(data, offset + index*4).u16.int, B16(data, offset + index*4 + 2).s16.int)

    case class HMetrics(advanceWidth: Int, leftSideBearing: Int)

  case class CmapTable(offset: Int):
    case class GlyphEncoding(platformId: Int, encodingId: Int, offset: Int):
      val formatId: Int = B16(data, offset).u16.int

      private var formatMemo: Optional[Format] = Unset

      def format: Format raises FontError = synchronized:
        formatMemo.or:
          val format = formatId match
            case 0 =>
              val length = B16(data, offset + 2).u16.int
              val language = B16(data, offset + 4).u16.int
              Format0(length, language)

            case 4 =>
              val length = B16(data, offset + 2).u16.int
              val language = B16(data, offset + 4).u16.int
              val segCount = B16(data, offset + 6).u16.int/2
              val searchRange = B16(data, offset + 8).u16.int/2
              val entrySelector = B16(data, offset + 10).u16.int
              val rangeShift = B16(data, offset + 12).u16.int
              val endCodesStart = offset + 14
              val startCodesStart = endCodesStart + segCount*2 + 2
              val idDeltaStart = startCodesStart + segCount*2
              val idRangeOffsetsStart = startCodesStart + segCount*2

              val segments = (0 until segCount).map: n =>
                Segment
                 (B16(data, startCodesStart + n*2).u16.int.toChar,
                  B16(data, endCodesStart + n*2).u16.int.toChar,
                  B16(data, idDeltaStart).s16.int,
                  B16(data, idRangeOffsetsStart).u16.int)

              Format4
               (length,
                language,
                segCount,
                searchRange,
                entrySelector,
                rangeShift,
                IArray.from(segments))

            case 12 =>
              val length = B32(data, offset + 2).s32.int
              val language = B32(data, offset + 6).s32.int
              val nGroups = B32(data, offset + 10).s32.int
              Format12(length, language, nGroups)

            case other =>
              abort(FontError(FontError.Reason.UnknownFormat))

          format.also:
            formatMemo = format

      case class Segment(start: Char, end: Char, delta: Int, rangeOffset: Int)

      sealed trait Format:
        def glyph(char: Char): Glyph[ttf.type]

      case class Format0(length: Int, language: Int) extends Format:
        def glyph(char: Char): Glyph[ttf.type] = ???

      case class Format4
         (length:        Int,
          language:      Int,
          segCount:      Int,
          searchRange:   Int,
          entrySelector: Int,
          rangeShift:    Int,
          segments:      IArray[Segment])
      extends Format:

        def glyph(char: Char): Glyph[ttf.type] =
          val segment = segments(segments.indexWhere(_.start > char) - 1)

          // FIXME: Understand why we need to add a `+1` here to fix an off-by-one error
          Glyph(ttf, segment.rangeOffset/2 + (char - segment.start) + segment.rangeOffset +
              segment.delta + 1)

      case class Format12(length: Int, language: Int, nGroups: Int) extends Format:
        def glyph(char: Char): Glyph[ttf.type] = ???

    lazy val version = B16(data, offset).u16.int
    lazy val numTables = B16(data, offset + 2).u16.int

    lazy val glyphEncodings: Seq[GlyphEncoding] = (0 until numTables).map: n =>
      val platformId = B16(data, offset + 4 + n*8).u16.int
      val encodingId = B16(data, offset + 6 + n*8).u16.int
      val subOffset = B32(data, offset + 8 + n*8).s32.int

      GlyphEncoding(platformId, encodingId, offset + subOffset)
