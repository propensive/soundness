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

// The four VP8L reverse transforms, ported from image-rs/image-webp
// (`src/lossless/decoder/reverse_transform.rs`, MIT/Apache-2.0). The reference uses chunked,
// autovectorized loops; these are the equivalent scalar forms. All arithmetic on pixel bytes is
// modulo 256 (`toByte`), and every buffer is RGBA, four bytes per pixel.
private[hallucination] object WebpTransform:
  private inline def u(byte: Byte): Int = byte & 0xff

  // floor((a + b)/2) for two bytes.
  private inline def average2(a: Byte, b: Byte): Int = (u(a) + u(b)) >> 1

  private inline def clampAddSubtractFull(a: Int, b: Int, c: Int): Int =
    (a + b - c).max(0).min(255)

  private inline def clampAddSubtractHalf(a: Int, b: Int): Int =
    (a + (a - b)/2).max(0).min(255)

  // Signed t·c arithmetic-shifted right by 5; only the low 8 bits of the sum it feeds ever matter.
  private inline def colorDelta(t: Byte, c: Byte): Int = (t.toInt*c.toInt) >> 5

  def predictor
    ( data: Array[Byte], width: Int, height: Int, sizeBits: Int, predictorData: Array[Byte] )
  :   Unit =

    val blockWidth = subsampleSize(width, sizeBits)
    val stride = width*4

    // The top-left pixel is inverse-predicted against opaque black (only its alpha changes); the
    // rest of the first row uses the left predictor, and the first column of every row the top.
    data(3) = (u(data(3)) + 255).toByte
    left(data, 4, stride)

    var y = 1

    while y < height do
      var i = 0

      while i < 4 do
        data(y*stride + i) = (u(data(y*stride + i)) + u(data((y - 1)*stride + i))).toByte
        i += 1

      y += 1

    y = 1

    while y < height do
      var blockX = 0

      while blockX < blockWidth do
        val blockIndex = (y >> sizeBits)*blockWidth + blockX
        val mode = u(predictorData(blockIndex*4 + 1))
        val start = (y*width + ((blockX << sizeBits).max(1)))*4
        val end = (y*width + (((blockX + 1) << sizeBits).min(width)))*4
        dispatch(mode, data, start, end, stride)
        blockX += 1

      y += 1

  private def dispatch(mode: Int, data: Array[Byte], start: Int, end: Int, stride: Int): Unit =
    mode match
      case 0  => opaque(data, start, end)
      case 1  => left(data, start, end)
      case 2  => top(data, start, end, stride)
      case 3  => topRight(data, start, end, stride)
      case 4  => topLeft(data, start, end, stride)
      case 5  => select5(data, start, end, stride)
      case 6  => select6(data, start, end, stride)
      case 7  => select7(data, start, end, stride)
      case 8  => select8(data, start, end, stride)
      case 9  => select9(data, start, end, stride)
      case 10 => select10(data, start, end, stride)
      case 11 => select11(data, start, end, stride)
      case 12 => select12(data, start, end, stride)
      case 13 => select13(data, start, end, stride)
      case _  => ()

  private def opaque(data: Array[Byte], start: Int, end: Int): Unit =
    var i = start + 3

    while i < end do
      data(i) = (u(data(i)) + 0xff).toByte
      i += 4

  private def left(data: Array[Byte], start: Int, end: Int): Unit =
    var i = start

    while i < end do
      data(i) = (u(data(i)) + u(data(i - 4))).toByte
      i += 1

  private def top(data: Array[Byte], start: Int, end: Int, stride: Int): Unit =
    var i = start

    while i < end do
      data(i) = (u(data(i)) + u(data(i - stride))).toByte
      i += 1

  private def topRight(data: Array[Byte], start: Int, end: Int, stride: Int): Unit =
    var i = start

    while i < end do
      data(i) = (u(data(i)) + u(data(i - stride + 4))).toByte
      i += 1

  private def topLeft(data: Array[Byte], start: Int, end: Int, stride: Int): Unit =
    var i = start

    while i < end do
      data(i) = (u(data(i)) + u(data(i - stride - 4))).toByte
      i += 1

  private def select5(data: Array[Byte], start: Int, end: Int, stride: Int): Unit =
    var i = start

    while i < end do
      val a = average2(data(i - 4).toByte, data(i - stride + 4).toByte)
      data(i) = (u(data(i)) + ((a + u(data(i - stride))) >> 1)).toByte
      i += 1

  private def select6(data: Array[Byte], start: Int, end: Int, stride: Int): Unit =
    var i = start

    while i < end do
      data(i) = (u(data(i)) + average2(data(i - 4), data(i - stride - 4))).toByte
      i += 1

  private def select7(data: Array[Byte], start: Int, end: Int, stride: Int): Unit =
    var i = start

    while i < end do
      data(i) = (u(data(i)) + average2(data(i - 4), data(i - stride))).toByte
      i += 1

  private def select8(data: Array[Byte], start: Int, end: Int, stride: Int): Unit =
    var i = start

    while i < end do
      data(i) = (u(data(i)) + average2(data(i - stride - 4), data(i - stride))).toByte
      i += 1

  private def select9(data: Array[Byte], start: Int, end: Int, stride: Int): Unit =
    var i = start

    while i < end do
      data(i) = (u(data(i)) + average2(data(i - stride), data(i - stride + 4))).toByte
      i += 1

  private def select10(data: Array[Byte], start: Int, end: Int, stride: Int): Unit =
    var i = start

    while i < end do
      val left = average2(data(i - 4), data(i - stride - 4))
      val topRight = average2(data(i - stride), data(i - stride + 4))
      data(i) = (u(data(i)) + ((left + topRight) >> 1)).toByte
      i += 1

  // The select predictor: per pixel, choose the left or top neighbour by which is nearer to the
  // gradient L + T - TL, summed across all four channels.
  private def select11(data: Array[Byte], start: Int, end: Int, stride: Int): Unit =
    var i = start

    while i < end do
      var predictLeft = 0
      var predictTop = 0
      var c = 0

      while c < 4 do
        val l = u(data(i + c - 4))
        val t = u(data(i + c - stride))
        val tl = u(data(i + c - stride - 4))
        val predict = l + t - tl
        predictLeft += math.abs(predict - l)
        predictTop += math.abs(predict - t)
        c += 1

      val neighbour = if predictLeft < predictTop then i - 4 else i - stride
      c = 0

      while c < 4 do
        data(i + c) = (u(data(i + c)) + u(data(neighbour + c))).toByte
        c += 1

      i += 4

  private def select12(data: Array[Byte], start: Int, end: Int, stride: Int): Unit =
    var i = start

    while i < end do
      val a = clampAddSubtractFull(u(data(i - 4)), u(data(i - stride)), u(data(i - stride - 4)))
      data(i) = (u(data(i)) + a).toByte
      i += 1

  private def select13(data: Array[Byte], start: Int, end: Int, stride: Int): Unit =
    var i = start

    while i < end do
      val a =
        clampAddSubtractHalf((u(data(i - 4)) + u(data(i - stride)))/2, u(data(i - stride - 4)))

      data(i) = (u(data(i)) + a).toByte
      i += 1

  def color(data: Array[Byte], width: Int, sizeBits: Int, transformData: Array[Byte]): Unit =
    val blockWidth = subsampleSize(width, sizeBits)
    val stride = width*4
    val height = data.length/stride
    var y = 0

    while y < height do
      val rowTransform = (y >> sizeBits)*blockWidth*4
      var x = 0

      while x < width do
        val block = rowTransform + (x >> sizeBits)*4
        val redToBlue = transformData(block).toByte
        val greenToBlue = transformData(block + 1).toByte
        val greenToRed = transformData(block + 2).toByte
        val pixel = (y*width + x)*4
        val green = data(pixel + 1).toByte
        val red = (u(data(pixel)) + colorDelta(greenToRed, green)) & 0xff

        val blue =
          ( u(data(pixel + 2)) + colorDelta(greenToBlue, green) +
            colorDelta(redToBlue, red.toByte) ) & 0xff

        data(pixel) = red.toByte
        data(pixel + 2) = blue.toByte
        x += 1

      y += 1

  def subtractGreen(data: Array[Byte]): Unit =
    var i = 0

    while i < data.length do
      data(i) = (u(data(i)) + u(data(i + 1))).toByte
      data(i + 2) = (u(data(i + 2)) + u(data(i + 1))).toByte
      i += 4

  // Expands packed palette indices (stored in the green channel) back to full colours, in place.
  // The image was decoded at a subsampled width, several indices packed into each byte for small
  // palettes; rows are processed bottom-up so the wider output never overwrites unread input.
  def colorIndexing
    ( data: Array[Byte], width: Int, height: Int, tableSize: Int, table: Array[Byte] )
  :   Unit =

    inline def lookup(index: Int, pixel: Int): Unit =
      if index < tableSize then System.arraycopy(table, index*4, data, pixel, 4)
      else
        data(pixel) = 0; data(pixel + 1) = 0; data(pixel + 2) = 0; data(pixel + 3) = 0

    if tableSize > 16 then
      var i = width*height - 1

      while i >= 0 do
        lookup(u(data(i*4 + 1)), i*4)
        i -= 1
    else
      val widthBits = if tableSize <= 2 then 3 else if tableSize <= 4 then 2 else 1
      val perByte = 1 << widthBits
      val bitsPerEntry = 8/perByte
      val mask = (1 << bitsPerEntry) - 1
      val packedWidth = (width + perByte - 1)/perByte
      val packed = new Array[Int](packedWidth)
      var y = height - 1

      while y >= 0 do
        var bx = 0

        while bx < packedWidth do
          packed(bx) = u(data((y*packedWidth + bx)*4 + 1))
          bx += 1

        var x = width - 1

        while x >= 0 do
          val index = (packed(x/perByte) >>> ((x%perByte)*bitsPerEntry)) & mask
          lookup(index, (y*width + x)*4)
          x -= 1

        y -= 1

  // ceil(size / 2^bits): the dimension of a transform's subsampled block grid.
  def subsampleSize(size: Int, bits: Int): Int = (size + (1 << bits) - 1) >> bits
