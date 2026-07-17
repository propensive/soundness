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

import soundness.*

import strategies.throwUnsafely

object Tests extends Suite(m"Phoenicia Tests"):

  // Big-endian byte assembly for hand-built font fixtures.
  def u16(values: Int*): Data =
    IArray.from(values.flatMap { value => Seq((value >> 8).toByte, value.toByte) })

  def u32(values: Long*): Data =
    IArray.from:
      values.flatMap: value =>
        Seq((value >> 24).toByte, (value >> 16).toByte, (value >> 8).toByte, value.toByte)

  // Assembles tables into an sfnt container: the header, a table directory, then the tables
  // themselves, four-byte aligned. Checksums are left zero: the parser does not verify them.
  def sfnt(tables: (Text, Data)*): Ttf =
    val count = tables.length
    val entrySelector = 31 - Integer.numberOfLeadingZeros(count)
    val searchRange = (1 << entrySelector)*16
    val header = u32(0x00010000L) ++ u16(count, searchRange, entrySelector, count*16 - searchRange)

    var offset = 12 + count*16
    val directory = List.newBuilder[Data]
    val body = List.newBuilder[Data]

    tables.each: (tag, table) =>
      val padding = if table.length%4 == 0 then 0 else 4 - table.length%4
      val tagBytes = IArray.from(tag.s.getBytes("US-ASCII").nn)
      directory += tagBytes ++ u32(0L, offset.toLong, table.length.toLong)
      body += table ++ IArray.fill[Byte](padding)(0)
      offset += table.length + padding

    Ttf(header ++ directory.result().reduce(_ ++ _) ++ body.result().reduce(_ ++ _))

  val headTable: Data =
    u16(1, 0, 1, 0)              // versions and font revision
    ++ u32(0L, 0x5f0f3cf5L)      // checksum adjustment and magic number
    ++ u16(0, 1000)              // flags and unitsPerEm
    ++ u32(0L, 0L, 0L, 0L)       // created and modified
    ++ u16(-50, -200, 1000, 800) // glyph bounding box
    ++ u16(0, 8)                 // macStyle and lowestRecPPEM
    ++ u16(2, 0, 0)              // direction hint, indexToLocFormat and glyphDataFormat

  val hheaTable: Data =
    u16(1, 0)                    // version
    ++ u16(800, -200, 90)        // ascender, descender and line gap
    ++ u16(600)                  // maximum advance width
    ++ u16(10, 10, 1000)         // minimum bearings and maximum extent
    ++ u16(1, 0, 0)              // caret slope and offset
    ++ u16(0, 0, 0, 0)           // reserved
    ++ u16(0, 3)                 // metric data format and number of metrics

  // Three full metric pairs, then a bearing-only entry for the monospaced tail.
  val hmtxTable: Data = u16(600, 10, 500, 20, 400, 30) ++ u16(25)

  // Three segments: A–C mapping by delta, a–c indirecting through the glyph id array in
  // rotated order, and the terminal sentinel.
  val format4Subtable: Data =
    u16(4, 46, 0)                // format, length and language
    ++ u16(6, 4, 1, 2)           // segment count and search hints
    ++ u16(0x43, 0x63, 0xffff)   // end codes
    ++ u16(0)                    // reserved pad
    ++ u16(0x41, 0x61, 0xffff)   // start codes
    ++ u16(-64, 0, 1)            // id deltas
    ++ u16(0, 4, 0)              // id range offsets
    ++ u16(2, 3, 1)              // glyph id array

  val cmapFormat4: Data = u16(0, 1) ++ u16(3, 1) ++ u32(12L) ++ format4Subtable

  val cmapFormat0: Data =
    val glyphIds = Array.fill[Byte](256)(0)
    glyphIds(0x41) = 7
    u16(0, 1) ++ u16(1, 0) ++ u32(12L) ++ u16(0, 262, 0) ++ glyphIds.immutable(using Unsafe)

  val cmapFormat6: Data =
    u16(0, 1) ++ u16(1, 0) ++ u32(12L)
    ++ u16(6, 16, 0)             // format, length and language
    ++ u16(0x30, 3)              // first code and count
    ++ u16(11, 12, 13)           // glyph ids

  // A Windows BMP Format 4 subtable alongside a full-Unicode Format 12 subtable which maps
  // the same character differently; the latter should be preferred.
  val cmapPreferring12: Data =
    u16(0, 2)
    ++ u16(3, 1) ++ u32(20L)
    ++ u16(3, 10) ++ u32(66L)
    ++ format4Subtable
    ++ u16(12, 0) ++ u32(28L, 0L, 1L) ++ u32(0x41L, 0x41L, 5L)

  def font(cmap: Data = cmapFormat4): Ttf =
    sfnt(t"cmap" -> cmap, t"head" -> headTable, t"hhea" -> hheaTable, t"hmtx" -> hmtxTable)

  def run(): Unit =
    suite(m"Character mapping"):
      val ttf = font()

      test(m"Format 4 delta segment maps letters"):
        List('A', 'B', 'C').map(ttf.glyph(_).id)
      . assert(_ == List(1, 2, 3))

      test(m"Format 4 indirect segment maps through the glyph id array"):
        List('a', 'b', 'c').map(ttf.glyph(_).id)
      . assert(_ == List(2, 3, 1))

      test(m"An unmapped character maps to the missing-glyph"):
        ttf.glyph('z').id
      . assert(_ == 0)

      test(m"Format 0 maps byte codes"):
        font(cmapFormat0).glyph('A').id
      . assert(_ == 7)

      test(m"Format 0 maps characters beyond bytes to the missing-glyph"):
        font(cmapFormat0).glyph('β').id
      . assert(_ == 0)

      test(m"Format 6 maps a dense range and nothing else"):
        List('0', '1', '2', '3').map(font(cmapFormat6).glyph(_).id)
      . assert(_ == List(11, 12, 13, 0))

      test(m"A full-Unicode subtable is preferred to the BMP"):
        font(cmapPreferring12).glyph('A').id
      . assert(_ == 5)

    suite(m"Horizontal metrics"):
      val ttf = font()

      test(m"Advance widths follow the character mapping"):
        List(ttf.advanceWidth('A'), ttf.advanceWidth('B'))
      . assert(_ == List(500, 400))

      test(m"A glyph beyond the metrics count repeats the last advance"):
        ttf.advanceWidth('C')
      . assert(_ == 400)

      test(m"A glyph beyond the metrics count reads the bearing tail"):
        ttf.leftSideBearing('C')
      . assert(_ == 25)

      test(m"Text width scales advances by the em size"):
        ttf.width(t"AB")
      . assert(_ == 0.9*Em)
