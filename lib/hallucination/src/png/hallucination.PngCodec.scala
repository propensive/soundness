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
package hallucination

import java.io as ji
import java.nio.charset.StandardCharsets
import proscenium.compat.*

import scala.collection.mutable as scm
import scala.math

import anticipation.*
import contingency.*
import pneumatic.*
import rudiments.*

import Binary.*
import Raster.Error.Reason

// A pure-Scala PNG codec over pneumatic's `Zlib` (which selects `java.util.zip` on the JVM and
// the pure DEFLATE port elsewhere). Decoding covers all critical chunks and colour types, bit
// depths 1 to 16 (16-bit samples are reduced to 8), all five filters, `tRNS` transparency and
// Adam7 interlacing, with CRC verification. Encoding writes 8-bit truecolour — with alpha when
// the raster's layout has it — choosing each scanline's filter by the minimum-sum-of-absolute-
// differences heuristic.
private[hallucination] object PngCodec:
  private val signature: Array[Int]^{} = Array.of(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a)

  def decode(data: Data): Raster raises Raster.Error =
    try
      val signed =
        data.length >= 8 && signature.indices.forall: index => u8(data, index) == signature.readUnchecked(index)

      if !signed then abort(Raster.Error(Png(), Reason.BadSignature))

      var position = 8
      var width = 0
      var height = 0
      var depth = 0
      var colorType = 0
      var interlace = 0
      var palette: Array[Int]^{} = Array.of()
      var transparency: Array[Int]^{} = Array.of()
      val idat = scm.ArrayBuilder.ofByte()
      var finished = false

      while !finished do
        val length = u32be(data, position)
        // `slice` yields a fresh array and `String`'s constructor copies it.
        val chunkType = new String(Array.unsafeJvm(data.slice(position + 4, position + 8)))
        val body = data.slice(position + 8, position + 8 + length)

        val storedCrc = u32be(data, position + 8 + length)

        if corpuscular.Crc32.checksum(data.slice(position + 4, position + 8), body) != storedCrc
        then abort(Raster.Error(Png(), Reason.BadCrc))

        chunkType match
          case "IHDR" =>
            width = u32be(data, position + 8)
            height = u32be(data, position + 12)
            depth = u8(data, position + 16)
            colorType = u8(data, position + 17)
            interlace = u8(data, position + 20)

            val legal = colorType match
              case 0 => List(1, 2, 4, 8, 16).has(depth)
              case 2 => depth == 8 || depth == 16
              case 3 => List(1, 2, 4, 8).has(depth)
              case 4 => depth == 8 || depth == 16
              case 6 => depth == 8 || depth == 16
              case _ => false

            val supported =
              legal && u8(data, position + 18) == 0 && u8(data, position + 19) == 0 &&
                interlace <= 1

            if !supported then abort(Raster.Error(Png(), Reason.UnsupportedVariant))

          case "PLTE" =>
            palette = Array.tabulate(length/3): index =>
              u8(data, position + 8 + index*3) << 16 |
                u8(data, position + 9 + index*3) << 8 |
                u8(data, position + 10 + index*3)

          case "tRNS" =>
            transparency = Array.tabulate(length): index =>
              u8(data, position + 8 + index)

          case "IDAT" =>
            idat.addAll(Array.unsafeJvm(body), 0, length)

          case "IEND" =>
            finished = true

          case _ =>
            ()

        position += length + 12

      val inflated: Array[Byte]^{} =
        val deflated = Array.unsafeFrozen(idat.result())

        // The frozen form keeps the `try` result free of the fresh read capability
        // a raw array result would carry.
        try
          concatenate(Zlib.compression.decompress(Chain(deflated)))
          . asInstanceOf[Array[Byte]^{}]
        catch case _: IllegalStateException => abort(Raster.Error(Png(), Reason.Truncated))

      val channels = colorType match
        case 0 => 1
        case 2 => 3
        case 3 => 1
        case 4 => 2
        case _ => 4

      val alpha = colorType == 4 || colorType == 6 || !transparency.isEmpty
      val maximum = (1 << depth) - 1

      // The `tRNS` samples for greyscale and truecolour are full-depth values.
      def transparencySample(index: Int): Int =
        if transparency.length < index*2 + 2 then -1
        else transparency.readUnchecked(index*2) << 8 | transparency.readUnchecked(index*2 + 1)

      val words = new scala.Array[Long](width*height)

      // Decodes one (possibly interlaced) pass, whose scanlines are `passWidth` pixels wide,
      // placing pixel (x, y) of the pass at `locate(x, y)` in the image.
      def decodePass(offset: Int, passWidth: Int, passHeight: Int)(locate: (Int, Int) => Int)
      :   Int =

        if passWidth == 0 || passHeight == 0 then offset else
          val rowBytes = (passWidth*channels*depth + 7)/8
          val unit = ((channels*depth + 7)/8).max(1)
          // Pure-typed rows (see `pureBytes`): writes route through `writable`.
          var previous: scala.Array[Byte] = pureBytes(rowBytes)
          var current: scala.Array[Byte] = pureBytes(rowBytes)
          var position = offset

          def sample(x: Int, channel: Int): Int =
            if depth == 8 then current(x*channels + channel)&0xff
            else if depth == 16 then
              (current((x*channels + channel)*2)&0xff) << 8 |
                current((x*channels + channel)*2 + 1)&0xff
            else
              val bit = x*depth
              (current(bit >> 3)&0xff) >> (8 - depth - (bit&7))&maximum

          def scaled(value: Int): Int =
            if depth == 16 then value >> 8 else value*255/maximum

          for y <- 0 until passHeight do
            val filter = inflated.readUnchecked(position)&0xff
            position += 1

            for index <- 0 until rowBytes do
              val raw = inflated.readUnchecked(position + index)&0xff
              val left = if index >= unit then current(index - unit)&0xff else 0
              val up = previous(index)&0xff
              val upLeft = if index >= unit then previous(index - unit)&0xff else 0

              val defiltered = filter match
                case 0 => raw
                case 1 => raw + left
                case 2 => raw + up
                case 3 => raw + (left + up)/2

                case 4 =>
                  val estimate = left + up - upLeft
                  val leftDelta = math.abs(estimate - left)
                  val upDelta = math.abs(estimate - up)
                  val upLeftDelta = math.abs(estimate - upLeft)

                  if leftDelta <= upDelta && leftDelta <= upLeftDelta then raw + left
                  else if upDelta <= upLeftDelta then raw + up
                  else raw + upLeft

                case _ => abort(Raster.Error(Png(), Reason.UnsupportedVariant))

              writable(current)(index) = defiltered.toByte

            position += rowBytes

            for x <- 0 until passWidth do
              val word: Long = colorType match
                case 0 =>
                  val grey = sample(x, 0)
                  val value = scaled(grey)
                  val opacity = if transparencySample(0) == grey then 0 else 255
                  pack(value, value, value, opacity)

                case 2 =>
                  val red = sample(x, 0)
                  val green = sample(x, 1)
                  val blue = sample(x, 2)

                  val transparent =
                    transparencySample(0) == red && transparencySample(1) == green &&
                      transparencySample(2) == blue

                  val opacity = if transparent then 0 else 255

                  pack(scaled(red), scaled(green), scaled(blue), opacity)

                case 3 =>
                  val entry = sample(x, 0)
                  val color = if entry < palette.length then palette.readUnchecked(entry) else 0

                  val opacity =
                    if entry < transparency.length then transparency.readUnchecked(entry) else 255

                  pack(color >> 16, (color >> 8)&0xff, color&0xff, opacity)

                case 4 =>
                  val value = scaled(sample(x, 0))
                  pack(value, value, value, scaled(sample(x, 1)))

                case _ =>
                  pack(scaled(sample(x, 0)), scaled(sample(x, 1)), scaled(sample(x, 2)),
                       scaled(sample(x, 3)))

              words(locate(x, y)) = word

            val swap = previous
            previous = current
            current = swap

          position

      if interlace == 0
      then
        decodePass(0, width, height): (x, y) => y*width + x
      else
        // Adam7: seven passes, each a subsampling of the image.
        val passes = List((0, 0, 8, 8), (4, 0, 8, 8), (0, 4, 4, 8), (2, 0, 4, 4), (0, 2, 2, 4),
                          (1, 0, 2, 2), (0, 1, 1, 2))

        passes.fold(0):
          case (offset, (startX, startY, stepX, stepY)) =>
            val passWidth = (width - startX + stepX - 1)/stepX
            val passHeight = (height - startY + stepY - 1)/stepY

            decodePass(offset, passWidth, passHeight): (x, y) =>
              (startY + y*stepY)*width + startX + x*stepX

      val descriptor = if alpha then Descriptor.rgba else Descriptor.rgb

      Raster.build(width, height, descriptor): index =>
        if alpha then words(index) else words(index) >>> 8

    catch case _: IndexOutOfBoundsException => abort(Raster.Error(Png(), Reason.Truncated))

  def encode(raster: Raster): Data =
    val width = raster.width
    val height = raster.height
    val alpha = raster.descriptor.hasAlpha
    val channels = if alpha then 4 else 3
    val rowBytes = width*channels
    val raw = new scala.Array[Byte]((rowBytes + 1)*height)
    var previous = new scala.Array[Byte](rowBytes)
    val current = new scala.Array[Byte](rowBytes)
    val filtered = new scala.Array[Byte](rowBytes)
    val best = new scala.Array[Byte](rowBytes)

    for y <- 0 until height do
      for x <- 0 until width do
        val word = raster.word(y*width + x)
        val chroma = raster.descriptor.chroma(word)
        current(x*channels) = chroma.red.toByte
        current(x*channels + 1) = chroma.green.toByte
        current(x*channels + 2) = chroma.blue.toByte

        if alpha then
          current(x*channels + 3) = (raster.descriptor.alpha(word)*255 + 0.5).toInt.toByte

      // Choose the filter with the least sum of absolute differences.
      var bestFilter = 0
      var bestScore = Long.MaxValue

      for filter <- 0 to 4 do
        var score = 0L

        for index <- 0 until rowBytes do
          val raw0 = current(index)&0xff
          val left = if index >= channels then current(index - channels)&0xff else 0
          val up = previous(index)&0xff
          val upLeft = if index >= channels then previous(index - channels)&0xff else 0

          val value = filter match
            case 0 => raw0
            case 1 => raw0 - left
            case 2 => raw0 - up
            case 3 => raw0 - (left + up)/2

            case _ =>
              val estimate = left + up - upLeft
              val leftDelta = math.abs(estimate - left)
              val upDelta = math.abs(estimate - up)
              val upLeftDelta = math.abs(estimate - upLeft)

              if leftDelta <= upDelta && leftDelta <= upLeftDelta then raw0 - left
              else if upDelta <= upLeftDelta then raw0 - up
              else raw0 - upLeft

          filtered(index) = value.toByte
          score += math.abs(value.toByte)

        if score < bestScore then
          bestScore = score
          bestFilter = filter
          System.arraycopy(filtered, 0, best, 0, rowBytes)

      raw(y*(rowBytes + 1)) = bestFilter.toByte
      System.arraycopy(best, 0, raw, y*(rowBytes + 1) + 1, rowBytes)
      System.arraycopy(current, 0, previous, 0, rowBytes)

    val compressed =
      concatenate(Zlib.compression.compress(Chain(Array.unsafeFrozen(raw))))

    val output = ji.ByteArrayOutputStream()
    signature.foreach(output.write(_))

    // Takes the frozen form: every chunk body outlives the call (the CRC reads it after the
    // stream has), and `OutputStream.write` only reads the array it is given.
    def chunk(chunkType: String, body: Data): Unit =
      writeInt(output, body.length)
      val typeBytes = chunkType.getBytes(StandardCharsets.UTF_8).nn
      output.write(typeBytes)
      output.write(Array.unsafeJvm(body))
      writeInt(output, corpuscular.Crc32.checksum(Array.unsafeFrozen(typeBytes), body))

    val header = Array[Byte](13)
    header(0) = (width >> 24).toByte
    header(1) = (width >> 16).toByte
    header(2) = (width >> 8).toByte
    header(3) = width.toByte
    header(4) = (height >> 24).toByte
    header(5) = (height >> 16).toByte
    header(6) = (height >> 8).toByte
    header(7) = height.toByte
    header(8) = 8
    header(9) = if alpha then 6 else 2

    chunk("IHDR", Array.freeze(header))
    chunk("IDAT", compressed)
    chunk("IEND", Array.of[Byte]())
    Array.unsafeFrozen(output.toByteArray.nn)

  private def pack(red: Int, green: Int, blue: Int, alpha: Int): Long =
    red.toLong << 24 | green << 16 | blue << 8 | alpha

  private def writeInt(output: ji.ByteArrayOutputStream, value: Int): Unit =
    output.write(value >> 24)
    output.write((value >> 16)&0xff)
    output.write((value >> 8)&0xff)
    output.write(value&0xff)

  private def concatenate(stream: Chain[Data]): Data =
    val output = ji.ByteArrayOutputStream()

    stream.each: data =>
      output.write(Array.unsafeJvm(data))

    Array.unsafeFrozen(output.toByteArray.nn)
