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

object Ttf:
  def apply[source: Streamable by Data over Credit](source: source): Ttf =
    Ttf(source.read[Data])

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

    . pipe(Map.from(_))

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

  def maxp: MaxpTable raises FontError =
    tables.at(TtfTag.Maxp).let: ref =>
      MaxpTable(ref.offset)

    . lest(FontError(FontError.Reason.MissingTable(TtfTag.Maxp)))

  def post: PostTable raises FontError =
    tables.at(TtfTag.Post).let: ref =>
      PostTable(ref.offset)

    . lest(FontError(FontError.Reason.MissingTable(TtfTag.Post)))

  def os2: Os2Table raises FontError =
    tables.at(OtfTag.Os2).let: ref =>
      Os2Table(ref.offset)

    . lest(FontError(FontError.Reason.MissingTable(OtfTag.Os2)))

  def name: NameTable raises FontError =
    tables.at(TtfTag.Name).let: ref =>
      NameTable(ref.offset)

    . lest(FontError(FontError.Reason.MissingTable(TtfTag.Name)))

  def loca: LocaTable raises FontError =
    tables.at(TtfTag.Loca).let: ref =>
      LocaTable(ref.offset, maxp.glyphCount, head.indexToLocFormat.int == 1)

    . lest(FontError(FontError.Reason.MissingTable(TtfTag.Loca)))

  def glyf: GlyfTable raises FontError =
    tables.at(TtfTag.Glyf).let: ref =>
      GlyfTable(ref.offset, loca)

    . lest(FontError(FontError.Reason.MissingTable(TtfTag.Glyf)))

  // A new font containing only the outlines needed to render the given characters — plus any
  // composite components they reference — with the original glyph numbering retained: unused
  // glyphs keep empty outlines, so character mappings, metrics and glyph references remain
  // valid. Every other table is carried over unchanged.
  def subset(chars: Set[Char]): Ttf raises FontError =
    val retained = glyphClosure(Set.of(chars.stdlib.map(glyph(_).id) + 0))
    val glyphs = glyf
    val count = maxp.glyphCount

    val offsets = new Array[Int](count + 1)
    val parts = scala.collection.immutable.List.newBuilder[Data]
    var position = 0

    (0 until count).each: id =>
      offsets(id) = position

      if retained.contains(id) then
        val bytes = glyphs(id).bytes
        parts += bytes
        position += bytes.length

    offsets(count) = position

    val newGlyf = new Array[Byte](position)
    var written = 0

    parts.result().each: part =>
      System.arraycopy(part.mutable(using Unsafe), 0, newGlyf, written, part.length)
      written += part.length

    // The rebuilt loca always uses the long format, so head's format field must agree.
    val newLoca = new Array[Byte]((count + 1)*4)

    (0 to count).each: id =>
      newLoca(id*4) = (offsets(id) >> 24).toByte
      newLoca(id*4 + 1) = (offsets(id) >> 16).toByte
      newLoca(id*4 + 2) = (offsets(id) >> 8).toByte
      newLoca(id*4 + 3) = offsets(id).toByte

    val headRef = tables.at(TtfTag.Head).lest(FontError(FontError.Reason.MissingTable(TtfTag.Head)))
    val newHead = data.slice(headRef.offset, headRef.offset + headRef.length).mutable(using Unsafe)
    (8 to 11).each { index => newHead(index) = 0 } // adjustment is recomputed on assembly
    newHead(50) = 0
    newHead(51) = 1

    val carried = tables.values.to(List).bind: ref =>
      if ref.id == TtfTag.Glyf || ref.id == TtfTag.Loca || ref.id == TtfTag.Head then Nil
      else List(ref.id.text -> data.slice(ref.offset, ref.offset + ref.length))

    val entries =
      (t"glyf", newGlyf.immutable(using Unsafe)) ::
        (t"loca", newLoca.immutable(using Unsafe)) ::
        (t"head", newHead.immutable(using Unsafe)) :: (carried: List[(Text, Data)])

    Ttf(Sfnt.assemble(data.slice(0, 4), List.of(entries)))

  def subset(text: Text): Ttf raises FontError = subset(Set.from(text.chars))

  // The transitive closure of a set of glyphs under composite-glyph components: every glyph
  // needed to render the given ones.
  def glyphClosure(glyphIds: Set[Int]): Set[Int] raises FontError =
    val table = glyf

    def expand(pending: List[Int], seen: Set[Int]): Set[Int] = pending match
      case Nil =>
        seen

      case head :: tail =>
        val fresh = table(head).components.filter(!seen.contains(_))
        expand(List.of(fresh.stdlib ++ tail.stdlib), Set.of(seen.stdlib ++ fresh.stdlib))

    expand(glyphIds.toList, glyphIds)

  // The font's PostScript name, by which PDF and PostScript documents reference it.
  def fontName: Optional[Text] = safely(name(Ttf.NameId.PostScriptName))

  def familyName: Optional[Text] = safely(name(Ttf.NameId.Family))

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

  // The glyph-location index: for each glyph, the extent of its data within glyf. In the
  // short format, offsets are stored halved in sixteen bits.
  case class LocaTable(offset: Int, glyphCount: Int, longFormat: Boolean):
    lazy val offsets: IArray[Int] =
      IArray.from:
        (0 to glyphCount).map: index =>
          if longFormat then B32(data, offset + index*4).s32.int
          else B16(data, offset + index*2).u16.int*2

  case class GlyfTable(offset: Int, loca: LocaTable):
    def apply(glyphId: Int): GlyphRecord =
      GlyphRecord(offset + loca.offsets(glyphId), loca.offsets(glyphId + 1) - loca.offsets(glyphId))

    // One glyph's raw data. A glyph with no outline — a space — has zero extent; a composite
    // glyph has a negative contour count and a list of component glyphs.
    case class GlyphRecord(start: Int, length: Int):
      def empty: Boolean = length == 0
      def bytes: Data = data.slice(start, start + length)
      def contourCount: Int = if empty then 0 else B16(data, start).s16.int
      def composite: Boolean = !empty && contourCount < 0

      lazy val components: List[Int] =
        if !composite then Nil else
          val builder = scala.collection.immutable.List.newBuilder[Int]
          var position = start + 10
          var more = true

          while more do
            val flags = B16(data, position).u16.int
            builder += B16(data, position + 2).u16.int
            position += (if (flags & 0x0001) != 0 then 8 else 6) // words or bytes for the args

            if (flags & 0x0008) != 0 then position += 2      // a single scale
            else if (flags & 0x0040) != 0 then position += 4 // separate x and y scales
            else if (flags & 0x0080) != 0 then position += 8 // a 2×2 transformation

            more = (flags & 0x0020) != 0

          List.of(builder.result())

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
        val bytes = data.mutable(using Unsafe)

        platformId match
          case 0 | 3 =>
            String(bytes, start, length, "UTF-16BE").tt

          case _ =>
            try String(bytes, start, length, "x-MacRoman").tt
            catch case _: Exception => String(bytes, start, length, "ISO-8859-1").tt

    lazy val records: IArray[Record] =
      IArray.from:
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
    def apply(nameId: Ttf.NameId): Optional[Text] =
      val candidates = records.filter(_.nameId == nameId.ordinal)

      def rank(record: Record): Int = (record.platformId, record.encodingId) match
        case (3, 1)  => if record.languageId == 0x409 then 0 else 1
        case (3, 10) => 2
        case (0, _)  => 3
        case (1, 0)  => 4
        case _       => 5

      if candidates.isEmpty then Unset else candidates.minBy(rank).decode

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

    def glyph(char: Char): Glyph[ttf.type] raises FontError =
      best.lest(FontError(FontError.Reason.MissingEncoding)).format.glyph(char)
