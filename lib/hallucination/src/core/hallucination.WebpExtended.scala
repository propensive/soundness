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

import anticipation.*
import contingency.*
import vacuous.*

import Binary.*
import RasterError.Reason

// The extended (VP8X) WebP container, ported from image-rs/image-webp (`src/extended.rs` and
// `src/decoder.rs`, MIT/Apache-2.0). Handles alpha for lossy images (the ALPH chunk, filtered and
// optionally lossless-compressed) and, for animations, decodes and composites the first frame.
private[hallucination] object WebpExtended:
  private final class Chunks
    ( var alph: Optional[(Int, Int)], var vp8: Optional[(Int, Int)],
      var vp8l: Optional[(Int, Int)], var anmf: Optional[(Int, Int)] )

  def decode(data: Data, start: Int, end: Int, riffEnd: Int): Raster raises RasterError =
    val flags = u8(data, start)
    val hasAlpha = (flags & 0x10) != 0
    val animation = (flags & 0x02) != 0
    val canvasWidth = read3(data, start + 4) + 1
    val canvasHeight = read3(data, start + 7) + 1

    // Scan the chunks following VP8X, recording the first of each kind.
    val chunks = Chunks(Unset, Unset, Unset, Unset)
    scan(data, start + 10, riffEnd, chunks)

    if animation then
      val (frameStart, frameEnd) =
        chunks.anmf.or(abort(RasterError(Webp(), Reason.UnsupportedVariant)))

      decodeAnmf(data, frameStart, frameEnd, canvasWidth, canvasHeight)
    else
      chunks.vp8l.lay(decodeLossy(data, chunks, hasAlpha)): (lossStart, lossEnd) =>
        // A lossless image in an extended container.
        val reader = WebpBitReader(data, lossStart, lossEnd)
        val (width, height, rgba) = WebpLossless.decode(reader)
        rasterRgba(width, height, rgba)

  // Scans chunk headers in [position, end), recording the first ALPH/VP8/VP8L/ANMF ranges.
  private def scan(data: Data, position0: Int, end: Int, chunks: Chunks): Unit =
    var position = position0

    while position + 8 <= end do
      val name = fourcc(data, position)
      val size = u32le(data, position + 4)
      val payload = (position + 8, position + 8 + size)

      name match
        case "ALPH" => if chunks.alph.absent then chunks.alph = payload
        case "VP8 " => if chunks.vp8.absent then chunks.vp8 = payload
        case "VP8L" => if chunks.vp8l.absent then chunks.vp8l = payload
        case "ANMF" => if chunks.anmf.absent then chunks.anmf = payload
        case _      => ()

      position += 8 + size + (size & 1)

  // Decodes a lossy VP8 image, applying the ALPH alpha channel if present.
  private def decodeLossy(data: Data, chunks: Chunks, hasAlpha: Boolean)
  :   Raster raises RasterError =

    val (vp8Start, vp8End) = chunks.vp8.or(abort(RasterError(Webp(), Reason.UnsupportedVariant)))
    val frame = Vp8Decoder.decode(data, vp8Start, vp8End)
    val rgb = Vp8Yuv.toRgb(frame)

    if !hasAlpha then
      Raster.build(frame.width, frame.height, Descriptor.rgb)(rgb(_).toLong & 0xffffff)
    else
      val (alphStart, alphEnd) =
        chunks.alph.or(abort(RasterError(Webp(), Reason.UnsupportedVariant)))

      val alpha = readAlpha(data, alphStart, alphEnd, frame.width, frame.height)
      combine(frame.width, frame.height, rgb, alpha)

  // Reads and un-filters the ALPH chunk into one alpha byte per pixel.
  private def readAlpha(data: Data, start: Int, end: Int, width: Int, height: Int)
  :   Array[Int] raises RasterError =

    val info = u8(data, start)
    val filtering = (info & 0x0c) >> 2
    val compression = info & 0x03

    // The stored (still filtered) alpha samples.
    val filtered: Array[Int] =
      if compression == 1 then
        // Lossless-compressed alpha: a VP8L L8 image whose green channel carries the alpha.
        val reader = WebpBitReader(data, start + 1, end)
        val rgba = WebpLossless.decodeRaw(reader, width, height)

        Array.tabulate(width*height): i =>
          rgba(i*4 + 1) & 0xff
      else if compression == 0 then
        Array.tabulate(width*height): i =>
          u8(data, start + 1 + i)
      else
        abort(RasterError(Webp(), Reason.UnsupportedVariant))

    // Un-filter: each sample is added to its predictor (from already-decoded neighbours).
    val alpha = new Array[Int](width*height)
    var y = 0

    while y < height do
      var x = 0

      while x < width do
        val predictor = alphaPredictor(x, y, width, filtering, alpha)
        alpha(y*width + x) = (predictor + filtered(y*width + x)) & 0xff
        x += 1

      y += 1

    alpha

  // The alpha predictor for pixel (x, y) under the given filtering method, reading previously
  // un-filtered neighbours from `alpha`.
  private def alphaPredictor(x: Int, y: Int, width: Int, filtering: Int, alpha: Array[Int]): Int =
    inline def at(px: Int, py: Int): Int = alpha(py*width + px)

    filtering match
      case 1 => // horizontal
        if x == 0 && y == 0 then 0 else if x == 0 then at(x, y - 1) else at(x - 1, y)

      case 2 => // vertical
        if x == 0 && y == 0 then 0 else if y == 0 then at(x - 1, y) else at(x, y - 1)

      case 3 => // gradient
        val (left, top, topLeft) =
          if x == 0 && y == 0 then (0, 0, 0)
          else if x == 0 then (at(x, y - 1), at(x, y - 1), at(x, y - 1))
          else if y == 0 then (at(x - 1, y), at(x - 1, y), at(x - 1, y))
          else (at(x - 1, y), at(x, y - 1), at(x - 1, y - 1))

        (left + top - topLeft).max(0).min(255)

      case _ => 0 // none

  private def decodeAnmf(data: Data, start: Int, end: Int, canvasWidth: Int, canvasHeight: Int)
  :   Raster raises RasterError =

    // The 16-byte ANMF frame header: x, y, width−1, height−1 (all ×2 for x/y), duration, flags.
    val frameX = read3(data, start)*2
    val frameY = read3(data, start + 3)*2
    val frameWidth = read3(data, start + 6) + 1
    val frameHeight = read3(data, start + 9) + 1

    val chunks = Chunks(Unset, Unset, Unset, Unset)
    scan(data, start + 16, end, chunks)

    // Decode the frame's own image (lossless, or lossy with optional alpha).
    val frame: Raster =
      chunks.vp8l.lay(decodeLossy(data, chunks, chunks.alph.present)): (lossStart, lossEnd) =>
        val reader = WebpBitReader(data, lossStart, lossEnd)
        val (w, h, rgba) = WebpLossless.decode(reader)
        rasterRgba(w, h, rgba)

    // Composite onto a transparent canvas at the frame's offset.
    if frameX == 0 && frameY == 0 && frameWidth == canvasWidth && frameHeight == canvasHeight
    then frame
    else
      Raster.build(canvasWidth, canvasHeight, Descriptor.rgba): index =>
        val x = index % canvasWidth
        val y = index / canvasWidth

        if x >= frameX && x < frameX + frameWidth && y >= frameY && y < frameY + frameHeight
        then frame.word((y - frameY)*frameWidth + (x - frameX))
        else 0L

  private def combine(width: Int, height: Int, rgb: Array[Int], alpha: Array[Int]): Raster =
    Raster.build(width, height, Descriptor.rgba): index =>
      (rgb(index).toLong & 0xffffff) << 8 | (alpha(index) & 0xff)

  private def rasterRgba(width: Int, height: Int, rgba: Array[Byte]): Raster =
    Raster.build(width, height, Descriptor.rgba): index =>
      (rgba(index*4) & 0xffL) << 24 | (rgba(index*4 + 1) & 0xffL) << 16 |
        (rgba(index*4 + 2) & 0xffL) << 8 | (rgba(index*4 + 3) & 0xffL)

  private def read3(data: Data, offset: Int): Int =
    u8(data, offset) | (u8(data, offset + 1) << 8) | (u8(data, offset + 2) << 16)

  private def fourcc(data: Data, offset: Int): String =
    String(Array(data(offset), data(offset + 1), data(offset + 2), data(offset + 3)), "UTF-8").nn
