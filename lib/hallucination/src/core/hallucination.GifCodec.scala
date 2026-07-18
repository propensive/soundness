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
package hallucination

import scala.collection.mutable as scm

import anticipation.*
import contingency.*
import rudiments.*
import vacuous.*

import Binary.*
import RasterError.Reason

// A pure-Scala GIF codec. Decoding reads GIF87a and GIF89a, with global and local colour
// tables, interlacing and transparency, compositing the first frame onto the logical screen
// (later animation frames are ignored). Encoding writes a single GIF89a frame, quantizing to at
// most 256 colours by median cut when necessary, with one palette slot reserved for
// transparency when the raster's layout has alpha.
private[hallucination] object GifCodec:
  def decode(data: Data): Raster raises RasterError =
    try
      if data.length < 13 || u8(data, 0) != 'G' || u8(data, 1) != 'I' || u8(data, 2) != 'F'
      then abort(RasterError(Gif(), Reason.BadSignature))

      val width = u16le(data, 6)
      val height = u16le(data, 8)
      val packed = u8(data, 10)

      var position = 13

      val global: IArray[Int] =
        if (packed&0x80) != 0 then
          val size = 2 << (packed&7)
          val table = readTable(data, position, size)
          position += size*3
          table
        else
          IArray()

      var transparentIndex = -1
      val screen = new Array[Long](width*height)

      def skipBlocks(): Unit =
        while u8(data, position) != 0 do position += u8(data, position) + 1
        position += 1

      var finished = false

      while !finished do
        u8(data, position) match
          case 0x21 =>
            val label = u8(data, position + 1)
            position += 2

            if label == 0xf9 then
              val size = u8(data, position)
              if (u8(data, position + 1)&1) == 1 then transparentIndex = u8(data, position + 4)
              position += size + 1

            skipBlocks()

          case 0x2c =>
            val left = u16le(data, position + 1)
            val top = u16le(data, position + 3)
            val frameWidth = u16le(data, position + 5)
            val frameHeight = u16le(data, position + 7)
            val framePacked = u8(data, position + 9)
            position += 10

            val palette: IArray[Int] =
              if (framePacked&0x80) != 0 then
                val size = 2 << (framePacked&7)
                val table = readTable(data, position, size)
                position += size*3
                table
              else
                global

            val interlaced = (framePacked&0x40) != 0
            val minimum = u8(data, position)
            position += 1

            val compressed = scm.ArrayBuilder.ofByte()

            while u8(data, position) != 0 do
              val size = u8(data, position)
              var index = 0

              while index < size do
                compressed += data(position + 1 + index).toByte
                index += 1

              position += size + 1

            position += 1

            val pixels = frameWidth*frameHeight

            val indices =
              GifLzw.decode(minimum, compressed.result().immutable(using Unsafe), pixels)

            // Interlaced frames deliver their rows in four passes.
            val rows: Array[Int] = new Array[Int](frameHeight)

            if interlaced then
              var row = 0

              for (start, step) <- List((0, 8), (4, 8), (2, 4), (1, 2)) do
                var y = start

                while y < frameHeight do
                  rows(row) = y
                  row += 1
                  y += step
            else
              for y <- 0 until frameHeight do rows(y) = y

            for row <- 0 until frameHeight do
              val y = top + rows(row)
              var x = 0

              while x < frameWidth do
                val entry = indices(row*frameWidth + x)&0xff

                if entry != transparentIndex && y < height && left + x < width then
                  screen((left + x) + y*width) = palette(entry).toLong << 8 | 0xff

                x += 1

            finished = true

          case 0x3b =>
            finished = true

          case _ =>
            abort(RasterError(Gif(), Reason.UnsupportedVariant))

      Raster.build(width, height, Descriptor.rgba)(screen(_))

    catch case _: IndexOutOfBoundsException => abort(RasterError(Gif(), Reason.Truncated))

  private def readTable(data: Data, position: Int, size: Int): IArray[Int] =
    IArray.tabulate(size): index =>
      u8(data, position + index*3) << 16 |
        u8(data, position + index*3 + 1) << 8 |
        u8(data, position + index*3 + 2)

  def encode(raster: Raster): Data =
    val width = raster.width
    val height = raster.height
    val counts = scm.HashMap[Int, Int]()
    val opacity = new Array[Boolean](width*height)
    val colors = new Array[Int](width*height)
    var transparent = false

    for index <- 0 until width*height do
      val word = raster.word(index)
      val visible = raster.descriptor.alpha(word) >= 0.5
      opacity(index) = visible

      if visible then
        val color = raster.descriptor.chroma(word).underlying
        colors(index) = color
        counts(color) = counts.getOrElse(color, 0) + 1
      else
        transparent = true

    val limit = if transparent then 255 else 256
    val (palette, assignment) = Quantization(counts, limit)
    val transparentIndex = palette.length

    val indices = new Array[Byte](width*height)

    for index <- 0 until width*height do
      indices(index) =
        if opacity(index) then assignment(colors(index)).toByte else transparentIndex.toByte

    val entries = palette.length + (if transparent then 1 else 0)

    var bits = 1
    while (1 << bits) < entries do bits += 1

    val minimum = bits.max(2)
    val output = scm.ArrayBuilder.ofByte()

    def write8(value: Int): Unit = output += value.toByte

    def write16(value: Int): Unit =
      write8(value&0xff)
      write8(value >> 8)

    "GIF89a".tt.s.foreach: char =>
      write8(char.toInt)

    write16(width)
    write16(height)
    write8(0x80 | 0x70 | (bits - 1))
    write8(0)
    write8(0)

    for index <- 0 until (1 << bits) do
      val color = if index < palette.length then palette(index) else 0
      write8(color >> 16)
      write8((color >> 8)&0xff)
      write8(color&0xff)

    if transparent then
      write8(0x21)
      write8(0xf9)
      write8(4)
      write8(1)
      write16(0)
      write8(transparentIndex)
      write8(0)

    write8(0x2c)
    write16(0)
    write16(0)
    write16(width)
    write16(height)
    write8(0)
    write8(minimum)

    val compressed = GifLzw.encode(minimum, indices)
    var offset = 0

    while offset < compressed.length do
      val size = (compressed.length - offset).min(255)
      write8(size)
      var index = 0

      while index < size do
        output += compressed(offset + index)
        index += 1

      offset += size

    write8(0)
    write8(0x3b)
    output.result().immutable(using Unsafe)
