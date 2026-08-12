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
package phoenicia

import proscenium.compat.*

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


object Sfnt:
  // Which specialisation a font is, is a property of its tables: a `CFF ` table means the
  // outlines are PostScript, and the font is OpenType. Reading the directory is the only way
  // to tell, but it must not make construction fail — wrapping bytes in a font has always
  // been total, and the tables are parsed lazily thereafter — so an unreadable directory
  // yields a `Truetype`, exactly as it did before this distinction existed.
  def apply(data: Data): Sfnt =
    val probe = Truetype(data)
    if safely(probe.tables.at(Sfnt.Table.Otf.Cff)).present then Opentype(data) else probe

  def apply[source: Streamable by Data over Credit](source: source): Sfnt =
    Sfnt(source.read[Data])

  enum PlatformId:
    case Unicode, Macintosh, Windows, Custom

  enum EncodingId:
    case Unicode1, Unicode1_1, IsoIec10646, Unicode2Bmp, Unicode2Full, UnicodeVariation, UnicodeFull

  // Naming-table name identifiers (OpenType §name); each case's ordinal is its name id.
  enum NameId:
    case Copyright, Family, Subfamily, UniqueId, FullName, Version, PostScriptName, Trademark,
      Manufacturer, Designer, Description, VendorUrl, DesignerUrl, License, LicenseUrl, Reserved,
      TypographicFamily, TypographicSubfamily, CompatibleFullName, SampleText, PostScriptCidName,
      WwsFamily, WwsSubfamily, LightBackgroundPalette, DarkBackgroundPalette,
      VariationsPostScriptNamePrefix

  // TableTag → Truetype.Table.Tag, TtfTag → Truetype.Table.Ttf, OtfTag → Truetype.Table.Otf.
  // A table tag belongs to the font format, and escritoire owns the `Table*` prefix at
  // toplevel; `Tag`'s two subtypes are sealed to its file, so all three move together.
  object Table:
    sealed trait Tag:
      def text: Text

    object Otf extends Extractor[Text, Otf]:
      def extract(text: Text): Optional[Otf] = text match
        case t"OS/2" => Os2
        case t"CFF " => Cff
        case other   => safely(Otf.valueOf(other.lower.capitalize.s))

    object Ttf extends Extractor[Text, Ttf]:
      def extract(text: Text): Optional[Ttf] = text match
        case t"cvt " => Cvt
        case other   => safely(Ttf.valueOf(other.lower.capitalize.s))

    enum Ttf extends Tag:
      case
        Avar, Cmap, Cvar, Cvt, Fpgm, Fvar, Gasp, Glyf, Gvar, Hdmx, Head, Hhea, Hmtx, Kern, Loca, Maxp,
        Meta, Name, Post, Prep, Sbix, Vhea, Vmtx

      def text: Text = this match
        case Cvt   => t"cvt "
        case table => table.toString.tt.lower

    enum Otf extends Tag:
      case
        Base, Cbdt, Cblc, Cff, Cff2, Colr, Cpal, Dsig, Ebdt, Eblc, Ebsc, Gdef, Gpos, Gsub, Hvar, Jstf,
        Ltsh, Math, Merg, Mvar, Os2, Pclt, Stat, Svg, Vdmx, Vorg, Vvar

      def text: Text = this match
        case Os2   => t"OS/2"
        case Cff   => t"CFF "
        case table => table.toString.tt.upper

  // Serialises a table set as an sfnt font file: the header, a directory sorted by tag, and the
  // tables themselves, four-byte aligned and zero-padded, with per-table checksums computed and
  // head's checksum adjustment set so the whole file sums to the specified constant.
  def assemble(version: Data, tables: List[(Text, Data)]): Data =
    def padded(length: Int): Int = (length + 3)/4*4

    val sorted = tables.stdlib.sortBy(_(0).s)
    val count = sorted.length
    val entrySelector = 31 - Integer.numberOfLeadingZeros(count)
    val searchRange = (1 << entrySelector)*16
    val tablesStart = 12 + count*16

    val total = tablesStart + sorted.sumBy: entry => padded(entry(1).length)

    val buffer = Array[Byte](total)

    def putU16(position: Int, value: Int): Unit =
      buffer(position) = (value >> 8).toByte
      buffer(position + 1) = value.toByte

    def putU32(position: Int, value: Long): Unit =
      buffer(position) = (value >> 24).toByte
      buffer(position + 1) = (value >> 16).toByte
      buffer(position + 2) = (value >> 8).toByte
      buffer(position + 3) = value.toByte

    // The sum of big-endian 32-bit words, over the length rounded up to a word boundary —
    // safe because tables are zero-padded in place.
    def checksum(start: Int, length: Int): Long =
      var sum = 0L
      var position = start
      val end = start + padded(length)

      while position < end do
        val word =
          ((buffer(position) & 0xffL) << 24) | ((buffer(position + 1) & 0xffL) << 16) |
            ((buffer(position + 2) & 0xffL) << 8) | (buffer(position + 3) & 0xffL)

        sum = (sum + word) & 0xffffffffL
        position += 4

      sum

    (0 until 4).each: index => buffer(index) = version.readUnchecked(index)

    putU16(4, count)
    putU16(6, searchRange)
    putU16(8, entrySelector)
    putU16(10, count*16 - searchRange)

    var offset = tablesStart
    var headOffset = -1

    sorted.indices.each: index =>
      val (tag, table) = sorted(index)
      val directory = 12 + index*16
      val tagBytes = tag.s.getBytes("US-ASCII").nn

      (0 until 4).each: position => buffer(directory + position) = tagBytes(position)

      buffer.copyFrom(table, 0, offset, table.length)
      putU32(directory + 4, checksum(offset, table.length))
      putU32(directory + 8, offset.toLong)
      putU32(directory + 12, table.length.toLong)
      if tag == t"head" then headOffset = offset
      offset += padded(table.length)

    // The caller supplies head with its adjustment zeroed, so the directory checksum above
    // is the spec's zero-adjusted one; the adjustment is then patched in afterwards.
    if headOffset >= 0 then putU32(headOffset + 8, (0xb1b0afbaL - checksum(0, total)) & 0xffffffffL)

    Array.freeze(buffer)

// The sfnt container format: a table directory and the tables every scalable font carries,
// whatever outline format it uses. TrueType and OpenType are both sfnt files, differing in
// how they store outlines — `glyf`/`loca` against `CFF ` — so everything else lives here.
trait Sfnt:
  sfnt: Sfnt =>

  def data: Data

  case class TableOffset(id: Sfnt.Table.Tag, checksum: B32, offset: Int, length: Int)

  lazy val numTables = B16(data, 4).u16.int
  lazy val searchRange = B16(data, 6).u16.int
  lazy val entrySelector = B16(data, 8).u16.int
  lazy val rangeShift = B16(data, 10).u16.int

  // The glyph for a character in the font's preferred character mapping, or glyph 0 — the
  // missing-glyph — for a character the font does not map.
  def glyph(char: Char): Glyph[sfnt.type] raises FontError = cmap.glyph(char)

  def advanceWidth(char: Char): Int raises FontError = hmtx.advanceWidth(glyph(char).id)

  def width(text: Text): Quantity[Ems[1]] raises FontError =
    text.chars.readable.sumBy(advanceWidth).toDouble*Em/head.unitsPerEm.int.toDouble

  def leftSideBearing(char: Char): Int raises FontError =
    hmtx.leftSideBearing(glyph(char).id)

  lazy val tables: Map[Sfnt.Table.Tag, TableOffset] =
    (0 until numTables).flatMap: n =>
      val start = 12 + n*16
      val tableTag = String(Array.unsafeJvm(data), start, 4, "ASCII").tt
      val checksum = B32(data, start + 4)
      val offset = B32(data, start + 8).s32.int
      val length = B32(data, start + 12).s32.int

      tableTag match
        case Sfnt.Table.Otf(tag) => Some(tag -> TableOffset(tag, checksum, offset, length))
        case Sfnt.Table.Ttf(tag) => Some(tag -> TableOffset(tag, checksum, offset, length))
        case _           => None

    . pipe(Map.from(_))

  def head: HeadTable raises FontError =
    tables(Sfnt.Table.Ttf.Head).let: ref =>
      data.unpackFrom[HeadTable](ref.offset).tap: table =>
        if table.magicNumber != 0x5f0f3cf5.bits then raise(FontError(FontError.Reason.MagicNumber))

    . lest(FontError(FontError.Reason.MissingTable(Sfnt.Table.Ttf.Head)))

  def cmap: CmapTable raises FontError =
    tables(Sfnt.Table.Ttf.Cmap).let: ref => CmapTable(ref.offset)
    . lest(FontError(FontError.Reason.MissingTable(Sfnt.Table.Ttf.Cmap)))

  def hhea: HheaTable raises FontError =
    tables(Sfnt.Table.Ttf.Hhea).let: ref => data.unpackFrom[HheaTable](ref.offset)
    . lest(FontError(FontError.Reason.MissingTable(Sfnt.Table.Ttf.Hhea)))

  def hmtx: HmtxTable raises FontError =
    tables(Sfnt.Table.Ttf.Hmtx).let: ref => HmtxTable(ref.offset, hhea.numberOfHMetrics.int)
    . lest(FontError(FontError.Reason.MissingTable(Sfnt.Table.Ttf.Hmtx)))

  def maxp: MaxpTable raises FontError =
    tables(Sfnt.Table.Ttf.Maxp).let: ref => MaxpTable(ref.offset)
    . lest(FontError(FontError.Reason.MissingTable(Sfnt.Table.Ttf.Maxp)))

  def post: PostTable raises FontError =
    tables(Sfnt.Table.Ttf.Post).let: ref => PostTable(ref.offset)
    . lest(FontError(FontError.Reason.MissingTable(Sfnt.Table.Ttf.Post)))

  def os2: Os2Table raises FontError =
    tables(Sfnt.Table.Otf.Os2).let: ref => Os2Table(ref.offset)
    . lest(FontError(FontError.Reason.MissingTable(Sfnt.Table.Otf.Os2)))

  def name: NameTable raises FontError =
    tables(Sfnt.Table.Ttf.Name).let: ref => NameTable(ref.offset)
    . lest(FontError(FontError.Reason.MissingTable(Sfnt.Table.Ttf.Name)))

  // The font's PostScript name, by which PDF and PostScript documents reference it.
  def fontName: Optional[Text] = safely(name(Sfnt.NameId.PostScriptName))

  def familyName: Optional[Text] = safely(name(Sfnt.NameId.Family))

  case class HeadTable
    ( majorVersion:       U16,
      minorVersion:       U16,
      fontRevisionHigh:   U16,
      fontRevisionLow:    U16,
      checksumAdjustment: B32,
      magicNumber:        B32,
      flags:              B16,
      unitsPerEm:         U16,
      created:            S64,
      modified:           S64,
      xMin:               S16,
      yMin:               S16,
      xMax:               S16,
      yMax:               S16,
      macStyle:           B16,
      lowestRecPpem:      U16,
      fontDirectionHint:  S16,
      indexToLocFormat:   S16,
      glyphDataFormat:    S16 )

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
    lazy val metrics: Array[HMetrics]^{} =
      Array.from:
        (0 until count).map: index =>
          HMetrics(B16(data, offset + index*4).u16.int, B16(data, offset + index*4 + 2).s16.int)

    def advanceWidth(glyphId: Int): Int =
      metrics.readUnchecked(if glyphId < count then glyphId else count - 1).advanceWidth

    def leftSideBearing(glyphId: Int): Int =
      if glyphId < count then metrics.readUnchecked(glyphId).leftSideBearing
      else B16(data, offset + count*4 + (glyphId - count)*2).s16.int

    case class HMetrics(advanceWidth: Int, leftSideBearing: Int)

  // The maximum-profile table; only the glyph count is of interest here.
  case class MaxpTable(offset: Int):
    lazy val glyphCount: Int = B16(data, offset + 4).u16.int

  // PostScript-related metadata.
  case class PostTable(offset: Int):
    lazy val italicAngle: Double = B32(data, offset + 4).s32.int/65536.0
    lazy val underlinePosition: Int = B16(data, offset + 8).s16.int
    lazy val underlineThickness: Int = B16(data, offset + 10).s16.int
    lazy val monospaced: Boolean = B32(data, offset + 12).s32.int != 0

  // OS/2 and Windows metrics. Later versions of the table append fields; those which the
  // font's version predates are absent.
  case class Os2Table(offset: Int):
    lazy val version: Int = B16(data, offset).u16.int
    lazy val weightClass: Int = B16(data, offset + 4).u16.int
    lazy val widthClass: Int = B16(data, offset + 6).u16.int
    lazy val fsType: Int = B16(data, offset + 8).u16.int
    lazy val familyClass: Int = B16(data, offset + 30).s16.int
    lazy val selection: Int = B16(data, offset + 62).u16.int
    lazy val typoAscender: Int = B16(data, offset + 68).s16.int
    lazy val typoDescender: Int = B16(data, offset + 70).s16.int
    lazy val typoLineGap: Int = B16(data, offset + 72).s16.int
    lazy val winAscent: Int = B16(data, offset + 74).u16.int
    lazy val winDescent: Int = B16(data, offset + 76).u16.int

    lazy val xHeight: Optional[Int] =
      if version >= 2 then B16(data, offset + 86).s16.int else Unset

    lazy val capHeight: Optional[Int] =
      if version >= 2 then B16(data, offset + 88).s16.int else Unset

    // Installable embedding is 0; of the restriction bits, only bit 1 forbids embedding
    // outright.
    def embeddable: Boolean = (fsType & 0x000f) != 0x0002

  // The naming table: localized, per-platform strings such as the font's family and
  // PostScript names.
  case class NameTable(offset: Int):
    lazy val count: Int = B16(data, offset + 2).u16.int
    private lazy val storageStart: Int = offset + B16(data, offset + 4).u16.int

    case class Record
      ( platformId: Int, encodingId: Int, languageId: Int, nameId: Int, length: Int, start: Int ):

      def decode: Text =
        val bytes = Array.unsafeJvm(data)

        platformId match
          case 0 | 3 =>
            String(bytes, start, length, "UTF-16BE").tt

          case _ =>
            try String(bytes, start, length, "x-MacRoman").tt
            catch case _: Exception => String(bytes, start, length, "ISO-8859-1").tt

    lazy val records: Array[Record]^{} =
      Array.from:
        (0 until count).map: n =>
          val base = offset + 6 + n*12

          Record
            ( B16(data, base).u16.int,
              B16(data, base + 2).u16.int,
              B16(data, base + 4).u16.int,
              B16(data, base + 6).u16.int,
              B16(data, base + 8).u16.int,
              storageStart + B16(data, base + 10).u16.int )

    // The best record for a name: Windows US English first, then other Unicode records, then
    // legacy Macintosh.
    def apply(nameId: Sfnt.NameId): Optional[Text] =
      val candidates = records.filter(_.nameId == nameId.ordinal)

      def rank(record: Record): Int = (record.platformId, record.encodingId) match
        case (3, 1)  => if record.languageId == 0x409 then 0 else 1
        case (3, 10) => 2
        case (0, _)  => 3
        case (1, 0)  => 4
        case _       => 5

      if candidates.isEmpty then Unset else candidates.readable.minBy(rank).decode

  case class CmapTable(offset: Int):
    case class GlyphEncoding(platformId: Int, encodingId: Int, offset: Int):
      val formatId: Int = B16(data, offset).u16.int

      private val mutex: Mutex = Mutex()
      @scala.caps.unsafe.untrackedCaptures
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

              Format4(idRangeOffsetsStart, Array.from(segments))

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
        def glyph(char: Char): Glyph[sfnt.type]

      // A byte-indexed array of glyph ids, for the first 256 character codes only.
      case class Format0(start: Int) extends Format:
        def glyph(char: Char): Glyph[sfnt.type] =
          Glyph(sfnt, if char < 256 then data.readUnchecked(start + char) & 0xff else 0)

      // Segmented ranges over the Basic Multilingual Plane. Each segment maps by a delta, or
      // — when its range offset is nonzero — indirects into the glyph-id array which follows
      // the range offsets, addressed relative to the segment's own range-offset word.
      case class Format4(idRangeOffsetsStart: Int, segments: Array[Segment]^{}) extends Format:
        def glyph(char: Char): Glyph[sfnt.type] =
          val index = segments.indexWhere(char <= _.end)

          if index < 0 || char < segments.readUnchecked(index).start then Glyph(sfnt, 0) else
            val segment = segments.readUnchecked(index)

            val id =
              if segment.rangeOffset == 0 then char + segment.delta
              else
                val position =
                  idRangeOffsetsStart + index*2 + segment.rangeOffset + (char - segment.start)*2

                val indirect = B16(data, position).u16.int
                if indirect == 0 then 0 else indirect + segment.delta

            Glyph(sfnt, id & 0xffff)

      // A dense run of glyph ids for a contiguous range of character codes.
      case class Format6(first: Int, count: Int, start: Int) extends Format:
        def glyph(char: Char): Glyph[sfnt.type] =
          if char >= first && char < first + count
          then Glyph(sfnt, B16(data, start + (char - first)*2).u16.int)
          else Glyph(sfnt, 0)

      // Groups of sequential character-to-glyph mappings, covering all of Unicode. Groups are
      // ordered by start code, permitting binary search.
      case class Format12(groupCount: Int, start: Int) extends Format:
        def glyph(char: Char): Glyph[sfnt.type] =
          def search(low: Int, high: Int): Int =
            if low > high then 0 else
              val middle = (low + high)/2
              val groupStart = B32(data, start + middle*12).s32.int
              val groupEnd = B32(data, start + middle*12 + 4).s32.int

              if char < groupStart then search(low, middle - 1)
              else if char > groupEnd then search(middle + 1, high)
              else B32(data, start + middle*12 + 8).s32.int + char - groupStart

          Glyph(sfnt, search(0, groupCount - 1))

    lazy val version = B16(data, offset).u16.int
    lazy val numTables = B16(data, offset + 2).u16.int

    lazy val glyphEncodings: List[GlyphEncoding] = (0 until numTables).to(List).map: n =>
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

    def glyph(char: Char): Glyph[sfnt.type] raises FontError =
      best.lest(FontError(FontError.Reason.MissingEncoding)).format.glyph(char)
