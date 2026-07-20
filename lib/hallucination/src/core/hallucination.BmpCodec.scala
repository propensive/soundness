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

import scala.math

import anticipation.*
import contingency.*
import rudiments.*
import vacuous.*

import Binary.*
import RasterError.Reason

// A pure-Scala BMP codec: decodes 1-, 4- and 8-bit palettes and 16-, 24- and 32-bit pixels,
// `BI_RGB` or `BI_BITFIELDS`, in either row order; encodes bottom-up 24-bit `BI_RGB`, the most
// widely-supported form. Run-length-encoded variants are not supported.
private[hallucination] object BmpCodec:
  def decode(data: Data): Raster raises RasterError =
    try
      if data.length < 2 || u8(data, 0) != 'B' || u8(data, 1) != 'M'
      then abort(RasterError(Bmp(), Reason.BadSignature))

      val dataOffset = u32le(data, 10)
      val headerSize = u32le(data, 14)

      if headerSize < 40 then abort(RasterError(Bmp(), Reason.UnsupportedVariant))

      val width = u32le(data, 18)
      val rawHeight = u32le(data, 22)
      val topDown = rawHeight < 0
      val height = math.abs(rawHeight)
      val bitCount = u16le(data, 28)
      val compression = u32le(data, 30)
      val colorCount = u32le(data, 46)

      if compression != 0 && compression != 3
      then abort(RasterError(Bmp(), Reason.UnsupportedVariant))

      // Default channel masks for `BI_RGB`; explicit masks for `BI_BITFIELDS`, which live
      // after a 40-byte header (RGB only) or inside a V4/V5 header (with alpha).
      val masks: (Int, Int, Int, Int) =
        if compression == 3 then
          val alphaMask = if headerSize >= 108 then u32le(data, 66) else 0
          (u32le(data, 54), u32le(data, 58), u32le(data, 62), alphaMask)
        else
          bitCount match
            case 16 => (0x7c00, 0x03e0, 0x001f, 0)
            case _  => (0xff0000, 0x00ff00, 0x0000ff, 0)

      val paletteOffset = 14 + headerSize + (if headerSize == 40 && compression == 3 then 12 else 0)

      val palette: IArray[Int] =
        if bitCount > 8 then IArray()
        else
          val size = if colorCount == 0 then 1 << bitCount else colorCount

          IArray.tabulate(size): index =>
            u32le(data, paletteOffset + index*4)&0xffffff

      val rowSize = ((bitCount*width + 31)/32)*4
      val alpha = masks(3) != 0
      val descriptor = if alpha then Descriptor.rgba else Descriptor.rgb

      def rescale(value: Int, mask: Int): Int =
        if mask == 0 then 0 else
          val shift = java.lang.Integer.numberOfTrailingZeros(mask)
          val maximum = mask >>> shift
          ((value&mask) >>> shift)*255/maximum

      Raster.build(width, height, descriptor): index =>
        val x = index%width
        val y = index/width
        val row = dataOffset + (if topDown then y else height - 1 - y)*rowSize

        bitCount match
          case 1 =>
            val bit = (u8(data, row + x/8) >> (7 - x%8))&1
            palette(bit).toLong

          case 4 =>
            val pair = u8(data, row + x/2)
            val entry = if x%2 == 0 then pair >> 4 else pair&15
            palette(entry).toLong

          case 8 =>
            palette(u8(data, row + x)).toLong

          case 16 =>
            val value = u16le(data, row + x*2)

            val opaque =
              pack(rescale(value, masks(0)), rescale(value, masks(1)), rescale(value, masks(2)))

            if alpha then opaque << 8 | rescale(value, masks(3)) else opaque

          case 24 =>
            val offset = row + x*3
            pack(u8(data, offset + 2), u8(data, offset + 1), u8(data, offset))

          case 32 =>
            val value = u32le(data, row + x*4)

            val opaque =
              pack(rescale(value, masks(0)), rescale(value, masks(1)), rescale(value, masks(2)))

            if alpha then opaque << 8 | rescale(value, masks(3)) else opaque

          case _ =>
            abort(RasterError(Bmp(), Reason.UnsupportedVariant))

    catch case _: (IndexOutOfBoundsException | NegativeArraySizeException) =>
      abort(RasterError(Bmp(), Reason.Truncated))

  def encode(raster: Raster): Data =
    val rowSize = (raster.width*3 + 3)& -4
    val imageSize = rowSize*raster.height
    val buffer = new Array[Byte](54 + imageSize)

    def put16(offset: Int, value: Int): Unit =
      buffer(offset) = value.toByte
      buffer(offset + 1) = (value >> 8).toByte

    def put32(offset: Int, value: Int): Unit =
      put16(offset, value&0xffff)
      put16(offset + 2, value >>> 16)

    buffer(0) = 'B'
    buffer(1) = 'M'
    put32(2, buffer.length)
    put32(10, 54)
    put32(14, 40)
    put32(18, raster.width)
    put32(22, raster.height)
    put16(26, 1)
    put16(28, 24)
    put32(34, imageSize)
    put32(38, 2835)
    put32(42, 2835)

    var y = 0

    while y < raster.height do
      val row = 54 + (raster.height - 1 - y)*rowSize
      var x = 0

      while x < raster.width do
        val chroma = raster(x, y)
        buffer(row + x*3) = chroma.blue.toByte
        buffer(row + x*3 + 1) = chroma.green.toByte
        buffer(row + x*3 + 2) = chroma.red.toByte
        x += 1

      y += 1

    buffer.immutable(using Unsafe)

  private def pack(red: Int, green: Int, blue: Int): Long =
    red.toLong << 16 | green << 8 | blue
