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

import anticipation.*
import denominative.*
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


object Truetype:
  def apply[source: Streamable by Data over Credit](source: source): Truetype =
    Truetype(source.read[Data])

// A TrueType font: an sfnt file whose outlines are quadratic, stored in `glyf` and indexed
// by `loca`. Subsetting is defined here because it rewrites exactly those two tables.
case class Truetype(data: Data) extends Sfnt:
  ttf =>

  def loca: LocaTable raises Font.Error =
    tables(Sfnt.Table.Ttf.Loca).let: ref =>
      LocaTable(ref.offset, maxp.glyphCount, head.indexToLocFormat.int == 1)

    . lest(Font.Error(Font.Error.Reason.MissingTable(Sfnt.Table.Ttf.Loca)))

  def glyf: GlyfTable raises Font.Error =
    tables(Sfnt.Table.Ttf.Glyf).let: ref => GlyfTable(ref.offset, loca)
    . lest(Font.Error(Font.Error.Reason.MissingTable(Sfnt.Table.Ttf.Glyf)))

  // A new font containing only the outlines needed to render the given characters — plus any
  // composite components they reference — with the original glyph numbering retained: unused
  // glyphs keep empty outlines, so character mappings, metrics and glyph references remain
  // valid. Every other table is carried over unchanged.
  def subset(chars: Set[Char]): Truetype raises Font.Error =
    val retained = glyphClosure(chars.map(glyph(_).id) :+ 0)
    val glyphs = glyf
    val count = maxp.glyphCount

    val offsets = new scala.Array[Int](count + 1)
    val parts = scala.collection.immutable.List.newBuilder[Data]
    var position = 0

    (0 until count).each: id =>
      offsets(id) = position

      if retained.has(id) then
        val bytes = glyphs(id).bytes
        parts += bytes
        position += bytes.length

    offsets(count) = position

    val newGlyf = Array.allocate[Byte](position)
    var written = 0

    parts.result().each: part =>
      newGlyf.place(part, 0, written, part.length)
      written += part.length

    // The rebuilt loca always uses the long format, so head's format field must agree.
    val newLoca = Array.allocate[Byte]((count + 1)*4)

    (0 to count).each: id =>
      newLoca(id*4) = (offsets(id) >> 24).toByte
      newLoca(id*4 + 1) = (offsets(id) >> 16).toByte
      newLoca(id*4 + 2) = (offsets(id) >> 8).toByte
      newLoca(id*4 + 3) = offsets(id).toByte

    val headRef = tables(Sfnt.Table.Ttf.Head).lest(Font.Error(Font.Error.Reason.MissingTable(Sfnt.Table.Ttf.Head)))
    val headData = data.segment((headRef.offset).z till (headRef.offset + headRef.length).z)
    val newHead = Array.allocate[Byte](headData.length)
    newHead.place(headData)
    (8 to 11).each { index => newHead(index) = 0 } // adjustment is recomputed on assembly
    newHead(50) = 0
    newHead(51) = 1

    val carried = tables.values.bind: ref =>
      if ref.id == Sfnt.Table.Ttf.Glyf || ref.id == Sfnt.Table.Ttf.Loca || ref.id == Sfnt.Table.Ttf.Head then Nil
      else List(ref.id.text -> data.segment((ref.offset).z till (ref.offset + ref.length).z))

    val entries =
      (t"glyf", Array.freeze(newGlyf)) ::
        (t"loca", Array.freeze(newLoca)) ::
        (t"head", Array.freeze(newHead)) :: (carried: List[(Text, Data)])

    Truetype(Sfnt.assemble(data.segment((0).z till (4).z), entries))

  def subset(text: Text): Truetype raises Font.Error = subset(Set.from(text.chars.readable))

  // The transitive closure of a set of glyphs under composite-glyph components: every glyph
  // needed to render the given ones.
  def glyphClosure(glyphIds: Set[Int]): Set[Int] raises Font.Error =
    val table = glyf

    def expand(pending: List[Int], seen: Set[Int]): Set[Int] = pending match
      case Nil =>
        seen

      case head :: tail =>
        val fresh = table(head).components.filter(!seen.has(_))
        expand(fresh + tail, seen + fresh.to[Set])

    expand(glyphIds.to[List], glyphIds)

  // The glyph-location index: for each glyph, the extent of its data within glyf. In the
  // short format, offsets are stored halved in sixteen bits.
  case class LocaTable(offset: Int, glyphCount: Int, longFormat: Boolean):
    lazy val offsets: Array[Int]^{} =
      Array.from:
        (0 to glyphCount).map: index =>
          if longFormat then B32(data, offset + index*4).s32.int
          else B16(data, offset + index*2).u16.int*2

  case class GlyfTable(offset: Int, loca: LocaTable):
    def apply(glyphId: Int): GlyphRecord =
      GlyphRecord(offset + loca.offsets.readUnchecked(glyphId), loca.offsets.readUnchecked(glyphId + 1) - loca.offsets.readUnchecked(glyphId))

    // One glyph's raw data. A glyph with no outline — a space — has zero extent; a composite
    // glyph has a negative contour count and a list of component glyphs.
    case class GlyphRecord(start: Int, length: Int):
      def empty: Boolean = length == 0
      def bytes: Data = data.segment((start).z till (start + length).z)
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

          builder.result().to(List)
