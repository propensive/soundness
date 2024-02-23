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

import rudiments.*
import hypotenuse.*
import anticipation.*
import contingency.*
import gossamer.*
import fulminate.*
import vacuous.*
import turbulence.*
import bifurcate.*
import quantitative.*

erased trait FontSize extends Dimension
erased given fontSize: PhysicalQuantity[Units[1, FontSize], "font size"] = ###

object Ems:
  given UnitName[Ems[1]] = () => t"ems"

erased trait Ems[Power <: Nat] extends Units[Power, FontSize]
val Em: Quantity[Ems[1]] = Quantity(1.0)

object Ttf:
  def apply[SourceType](source: SourceType)(using Readable[SourceType, Bytes]): Ttf =
    val data = source.readAs[Bytes]
    println(B32(data).hex)
    Ttf(data)

  enum PlatformId:
    case Unicode, Macintosh, Windows, Custom
  
  enum EncodingId:
    case Unicode1, Unicode1_1, IsoIec10646, Unicode2Bmp, Unicode2Full, UnicodeVariation, UnicodeFull

object FontError:
  enum Reason:
    case MissingTable(tag: TableTag)
    case UnknownFormat
  
  given communicable: Communicable[Reason] =
    case Reason.MissingTable(tag) => msg"the table ${tag.text} was not found"
    case Reason.UnknownFormat     => msg"the table contains data in an unknown format"

case class FontError(reason: FontError.Reason)
extends Error(msg"the font could not be read because $reason")

object Phoenicia:
  opaque type Glyph[TtfType <: Ttf & Singleton] = Int

  object Glyph:
    def apply(ttf: Ttf, id: Int): Glyph[ttf.type] = id

  extension [TtfType <: Ttf & Singleton](glyph: Glyph[TtfType])
    def id: Int = glyph

export Phoenicia.Glyph

sealed trait TableTag:
  def text: Text

enum TtfTag extends TableTag:
  case Avar, Cmap, Cvar, Cvt, Fpgm, Fvar, Gasp, Glyf, Gvar, Hdmx, Head, Hhea, Hmtx, Kern, Loca, Maxp, Meta,
      Name, Post, Prep, Sbix, Vhea, Vmtx

  def text: Text = this match
    case Cvt   => t"cvt "
    case table => table.toString.tt.lower

object TtfTag:
  def unapply(text: Text): Option[TtfTag] = text match
    case t"cvt " => Some(Cvt)
    case other   => try Some(TtfTag.valueOf(other.lower.capitalize.s)) catch case error: Exception => None

enum OtfTag extends TableTag:
  case Base, Cbdt, Cblc, Cff, Cff2, Colr, Cpal, Dsig, Ebdt, Eblc, Ebsc, Gdef, Gpos, Gsub, Hvar, Jstf, Ltsh,
      Math, Merg, Mvar, Os2, Pclt, Stat, Svg, Vdmx, Vorg, Vvar

  def text: Text = this match
    case Os2   => t"OS/2"
    case Cff   => t"CFF "
    case table => table.toString.tt.upper

object OtfTag:
  def unapply(text: Text): Option[OtfTag] = text match
    case t"OS/2" => Some(Os2)
    case t"CFF " => Some(Cff)
    case other   => try Some(OtfTag.valueOf(other.lower.capitalize.s)) catch case error: Exception => None

case class Ttf(data: Bytes):
  ttf =>

  case class TableOffset(id: TableTag, checksum: B32, offset: Int, length: Int)

  lazy val numTables = B16(data, 4).u16.int
  lazy val searchRange = B16(data, 6).u16.int
  lazy val entrySelector = B16(data, 8).u16.int
  lazy val rangeShift = B16(data, 10).u16.int

  def glyph(char: Char): Glyph[ttf.type] raises FontError = cmap.glyphEncodings(0).format.glyph(char)
  def advanceWidth(char: Char): Int raises FontError = hmtx.metrics(glyph(char).id).advanceWidth
  def width(text: Text): Quantity[Ems[1]] raises FontError = text.chars.sumBy(advanceWidth)*Em/head.unitsPerEm.int
  def leftSideBearing(char: Char): Int raises FontError = hmtx.metrics(glyph(char).id).leftSideBearing

  lazy val tables: Map[TableTag, TableOffset] =
    (0 until numTables).flatMap: n =>
      val start = 12 + n*16
      val tableTag = String(data.mutable(using Unsafe), start, 4, "ASCII").tt
      val checksum = B32(data, start + 4)
      val offset = B32(data, start + 8).i32.int
      val length = B32(data, start + 12).i32.int
      
      tableTag match
        case OtfTag(tag) => Some(tag -> TableOffset(tag, checksum, offset, length))
        case TtfTag(tag) => Some(tag -> TableOffset(tag, checksum, offset, length))
        case _           => None
    .to(Map)

  def head: HeadTable raises FontError =
    tables.get(TtfTag.Head).map: ref =>
      data.deserialize[HeadTable](ref.offset)
    .getOrElse(abort(FontError(FontError.Reason.MissingTable(TtfTag.Head))))

  def cmap: CmapTable raises FontError =
    tables.get(TtfTag.Cmap).map: ref =>
      CmapTable(ref.offset)
    .getOrElse(abort(FontError(FontError.Reason.MissingTable(TtfTag.Cmap))))
  
  def hhea: HheaTable raises FontError =
    tables.get(TtfTag.Hhea).map: ref =>
      data.deserialize[HheaTable](ref.offset)
    .getOrElse(abort(FontError(FontError.Reason.MissingTable(TtfTag.Hhea))))
  
  def hmtx: HmtxTable raises FontError =
    tables.get(TtfTag.Hmtx).map: ref =>
      HmtxTable(ref.offset, hhea.numberOfHMetrics.int)
    .getOrElse(abort(FontError(FontError.Reason.MissingTable(TtfTag.Hmtx))))

  case class HeadTable(majorVersion: U16, minorVersion: U16, fontRevisionHigh: U16, fontRevisionLow: U16,
      checksumAdjustment: B32, magicNumber: B32, flags: B16, unitsPerEm: U16)

  case class HheaTable(majorVersion: U16, minorVersion: U16, ascender: I16, descender: I16, lineGap: I16,
      advanceWidthMax: U16, minLeftSideBearing: I16, minRightSideBearing: I16, xMaxExtent: I16,
      caretSlopeRise: I16, caretSlopeRun: I16, caretOffset: I16, reserved0: U16, reserved1: U16,
      reserved2: U16, reserved4: U16, metricDataFormat: I16, numberOfHMetrics: U16)

  case class HmtxTable(offset: Int, count: Int):
    lazy val metrics: IArray[HMetrics] =
      IArray.from:
        (0 until count).map: index =>
          HMetrics(B16(data, offset + index*4).u16.int, B16(data, offset + index*4 + 2).i16.int)

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
                Segment(B16(data, startCodesStart + n*2).u16.int.toChar,
                    B16(data, endCodesStart + n*2).u16.int.toChar,
                    B16(data, idDeltaStart).i16.int,
                    B16(data, idRangeOffsetsStart).u16.int)
              
              Format4(length, language, segCount, searchRange, entrySelector, rangeShift, IArray.from(segments))

            case 12 =>
              val length = B32(data, offset + 2).i32.int
              val language = B32(data, offset + 6).i32.int
              val nGroups = B32(data, offset + 10).i32.int
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
          (length: Int, language: Int, segCount: Int, searchRange: Int, entrySelector: Int, rangeShift: Int,
              segments: IArray[Segment])
      extends Format:
        def glyph(char: Char): Glyph[ttf.type] =
          val segment = segments(segments.indexWhere(_.start > char) - 1)
          // FIXME: Understand why we need to add a `+1` here to fix an off-by-one error
          Glyph(ttf, segment.rangeOffset/2 + (char - segment.start) + segment.rangeOffset + segment.delta + 1)
      
      case class Format12(length: Int, language: Int, nGroups: Int) extends Format:
        def glyph(char: Char): Glyph[ttf.type] = ???


    lazy val version = B16(data, offset).u16.int
    lazy val numTables = B16(data, offset + 2).u16.int
    
    lazy val glyphEncodings: Seq[GlyphEncoding] = (0 until numTables).map: n =>
      val platformId = B16(data, offset + 4 + n*8).u16.int
      val encodingId = B16(data, offset + 6 + n*8).u16.int
      val subOffset = B32(data, offset + 8 + n*8).i32.int
      
      GlyphEncoding(platformId, encodingId, offset + subOffset)
