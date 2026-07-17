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
package phoenicia

import anticipation.*
import contingency.*
import gossamer.*
import hypotenuse.*
import polaris.*
import prepositional.*
import quantitative.*
import rudiments.*
import symbolism.*
import turbulence.*
import vacuous.*
import zephyrine.*

object Ttf:
  def apply[source: Streamable by Data over Credit](source: source): Ttf =
    Ttf(source.read[Data])

  enum PlatformId:
    case Unicode, Macintosh, Windows, Custom

  enum EncodingId:
    case Unicode1, Unicode1_1, IsoIec10646, Unicode2Bmp, Unicode2Full, UnicodeVariation, UnicodeFull

case class Ttf(data: Data):
  ttf =>

  case class TableOffset(id: TableTag, checksum: B32, offset: Int, length: Int)

  lazy val numTables = B16(data, 4).u16.int
  lazy val searchRange = B16(data, 6).u16.int
  lazy val entrySelector = B16(data, 8).u16.int
  lazy val rangeShift = B16(data, 10).u16.int

  // The glyph for a character in the font's preferred character mapping, or glyph 0 — the
  // missing-glyph — for a character the font does not map.
  def glyph(char: Char): Glyph[ttf.type] raises FontError = cmap.glyph(char)

  def advanceWidth(char: Char): Int raises FontError = hmtx.advanceWidth(glyph(char).id)

  def width(text: Text): Quantity[Ems[1]] raises FontError =
    text.chars.sumBy(advanceWidth).toDouble*Em/head.unitsPerEm.int.toDouble

  def leftSideBearing(char: Char): Int raises FontError =
    hmtx.leftSideBearing(glyph(char).id)

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

    . to(Map)

  def head: HeadTable raises FontError =
    tables.at(TtfTag.Head).let: ref =>
      data.unpackFrom[HeadTable](ref.offset).tap: table =>
        if table.magicNumber != 0x5f0f3cf5.bits
        then raise(FontError(FontError.Reason.MagicNumber))

    . lest(FontError(FontError.Reason.MissingTable(TtfTag.Head)))

  def cmap: CmapTable raises FontError =
    tables.at(TtfTag.Cmap).let: ref =>
      CmapTable(ref.offset)

    . lest(FontError(FontError.Reason.MissingTable(TtfTag.Cmap)))

  def hhea: HheaTable raises FontError =
    tables.at(TtfTag.Hhea).let: ref =>
      data.unpackFrom[HheaTable](ref.offset)

    . lest(FontError(FontError.Reason.MissingTable(TtfTag.Hhea)))

  def hmtx: HmtxTable raises FontError =
    tables.at(TtfTag.Hmtx).let: ref =>
      HmtxTable(ref.offset, hhea.numberOfHMetrics.int)

    . lest(FontError(FontError.Reason.MissingTable(TtfTag.Hmtx)))

  case class HeadTable
    ( majorVersion:       U16,
      minorVersion:       U16,
      fontRevisionHigh:   U16,
      fontRevisionLow:    U16,
      checksumAdjustment: B32,
      magicNumber:        B32,
      flags:              B16,
      unitsPerEm:         U16 )

  case class HheaTable
    ( majorVersion:        U16,
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
      numberOfHMetrics:    U16 )

  // The horizontal metrics: one (advance, bearing) pair per glyph up to `count`, after which
  // the last advance repeats — a monospaced tail — and bearings continue in their own array.
  case class HmtxTable(offset: Int, count: Int):
    lazy val metrics: IArray[HMetrics] =
      IArray.from:
        (0 until count).map: index =>
          HMetrics(B16(data, offset + index*4).u16.int, B16(data, offset + index*4 + 2).s16.int)

    def advanceWidth(glyphId: Int): Int =
      metrics(if glyphId < count then glyphId else count - 1).advanceWidth

    def leftSideBearing(glyphId: Int): Int =
      if glyphId < count then metrics(glyphId).leftSideBearing
      else B16(data, offset + count*4 + (glyphId - count)*2).s16.int

    case class HMetrics(advanceWidth: Int, leftSideBearing: Int)

  case class CmapTable(offset: Int):
    case class GlyphEncoding(platformId: Int, encodingId: Int, offset: Int):
      val formatId: Int = B16(data, offset).u16.int

      private val mutex: Mutex = Mutex()
      private var formatMemo: Optional[Format] = Unset

      def format: Format raises FontError = mutex:
        formatMemo.or:
          val format = formatId match
            case 0 =>
              Format0(offset + 6)

            case 4 =>
              val segCount = B16(data, offset + 6).u16.int/2
              val endCodesStart = offset + 14
              val startCodesStart = endCodesStart + segCount*2 + 2 // a reserved pad intervenes
              val idDeltasStart = startCodesStart + segCount*2
              val idRangeOffsetsStart = idDeltasStart + segCount*2

              val segments = (0 until segCount).map: n =>
                Segment
                  ( B16(data, startCodesStart + n*2).u16.int.toChar,
                    B16(data, endCodesStart + n*2).u16.int.toChar,
                    B16(data, idDeltasStart + n*2).s16.int,
                    B16(data, idRangeOffsetsStart + n*2).u16.int )

              Format4(idRangeOffsetsStart, IArray.from(segments))

            case 6 =>
              val first = B16(data, offset + 6).u16.int
              val count = B16(data, offset + 8).u16.int
              Format6(first, count, offset + 10)

            case 12 =>
              Format12(B32(data, offset + 12).s32.int, offset + 16)

            case other =>
              abort(FontError(FontError.Reason.UnknownFormat(other)))

          format.also:
            formatMemo = format

      case class Segment(start: Char, end: Char, delta: Int, rangeOffset: Int)

      sealed trait Format:
        def glyph(char: Char): Glyph[ttf.type]

      // A byte-indexed array of glyph ids, for the first 256 character codes only.
      case class Format0(start: Int) extends Format:
        def glyph(char: Char): Glyph[ttf.type] =
          Glyph(ttf, if char < 256 then data(start + char) & 0xff else 0)

      // Segmented ranges over the Basic Multilingual Plane. Each segment maps by a delta, or
      // — when its range offset is nonzero — indirects into the glyph-id array which follows
      // the range offsets, addressed relative to the segment's own range-offset word.
      case class Format4(idRangeOffsetsStart: Int, segments: IArray[Segment]) extends Format:
        def glyph(char: Char): Glyph[ttf.type] =
          val index = segments.indexWhere(char <= _.end)

          if index < 0 || char < segments(index).start then Glyph(ttf, 0) else
            val segment = segments(index)

            val id =
              if segment.rangeOffset == 0 then char + segment.delta
              else
                val position =
                  idRangeOffsetsStart + index*2 + segment.rangeOffset + (char - segment.start)*2

                val indirect = B16(data, position).u16.int
                if indirect == 0 then 0 else indirect + segment.delta

            Glyph(ttf, id & 0xffff)

      // A dense run of glyph ids for a contiguous range of character codes.
      case class Format6(first: Int, count: Int, start: Int) extends Format:
        def glyph(char: Char): Glyph[ttf.type] =
          if char >= first && char < first + count
          then Glyph(ttf, B16(data, start + (char - first)*2).u16.int)
          else Glyph(ttf, 0)

      // Groups of sequential character-to-glyph mappings, covering all of Unicode. Groups are
      // ordered by start code, permitting binary search.
      case class Format12(groupCount: Int, start: Int) extends Format:
        def glyph(char: Char): Glyph[ttf.type] =
          def search(low: Int, high: Int): Int =
            if low > high then 0 else
              val middle = (low + high)/2
              val groupStart = B32(data, start + middle*12).s32.int
              val groupEnd = B32(data, start + middle*12 + 4).s32.int

              if char < groupStart then search(low, middle - 1)
              else if char > groupEnd then search(middle + 1, high)
              else B32(data, start + middle*12 + 8).s32.int + char - groupStart

          Glyph(ttf, search(0, groupCount - 1))

    lazy val version = B16(data, offset).u16.int
    lazy val numTables = B16(data, offset + 2).u16.int

    lazy val glyphEncodings: Seq[GlyphEncoding] = (0 until numTables).map: n =>
      val platformId = B16(data, offset + 4 + n*8).u16.int
      val encodingId = B16(data, offset + 6 + n*8).u16.int
      val subOffset = B32(data, offset + 8 + n*8).s32.int

      GlyphEncoding(platformId, encodingId, offset + subOffset)

    // The preferred character mapping: full Unicode before the Basic Multilingual Plane,
    // Unicode before Windows symbol and legacy Macintosh encodings.
    lazy val best: Optional[GlyphEncoding] =
      def rank(encoding: GlyphEncoding): Int = (encoding.platformId, encoding.encodingId) match
        case (3, 10)         => 0
        case (0, 4) | (0, 6) => 1
        case (3, 1)          => 2
        case (0, 3)          => 3
        case (0, _)          => 4
        case (3, 0)          => 5
        case (1, 0)          => 6
        case _               => 7

      if glyphEncodings.isEmpty then Unset else glyphEncodings.minBy(rank)

    def glyph(char: Char): Glyph[ttf.type] raises FontError =
      best.lest(FontError(FontError.Reason.MissingEncoding)).format.glyph(char)
