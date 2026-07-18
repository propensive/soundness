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

import anticipation.*
import contingency.*

import Binary.*
import RasterError.Reason

// A pure-Scala WebP codec, ported from image-rs/image-webp (MIT/Apache-2.0). This phase decodes
// simple (non-extended) lossless VP8L images; lossy VP8 and extended VP8X images raise
// `UnsupportedVariant` until later phases. Unlike the other formats, WebP has no `javax.imageio`
// support, so this codec is selected on every platform, including the JVM.
private[hallucination] object WebpCodec:
  // The RIFF container signature: "RIFF", a size, then "WEBP".
  def isWebp(data: Data): Boolean =
    data.length >= 12 && fourcc(data, 0) == "RIFF" && fourcc(data, 8) == "WEBP"

  def decode(data: Data): Raster raises RasterError =
    try
      if data.length < 12 || fourcc(data, 0) != "RIFF" || fourcc(data, 8) != "WEBP"
      then abort(RasterError(Webp(), Reason.BadSignature))

      // The first chunk after the "WEBP" fourcc determines the image kind.
      val chunk = fourcc(data, 12)
      val chunkStart = 20

      chunk match
        case "VP8L" =>
          val chunkSize = u32le(data, 16)
          val reader = WebpBitReader(data, chunkStart, chunkStart + chunkSize)
          val (width, height, rgba) = WebpLossless.decode(reader)
          raster(width, height, rgba)

        case "VP8 " | "VP8X" =>
          abort(RasterError(Webp(), Reason.UnsupportedVariant))

        case _ =>
          abort(RasterError(Webp(), Reason.BadSignature))

    catch case _: (IndexOutOfBoundsException | NegativeArraySizeException) =>
      abort(RasterError(Webp(), Reason.Truncated))

  // Builds an RGBA raster from the decoded, un-transformed byte buffer (in RGBA order).
  private def raster(width: Int, height: Int, rgba: Array[Byte]): Raster =
    Raster.build(width, height, Descriptor.rgba): index =>
      (rgba(index*4) & 0xffL) << 24 | (rgba(index*4 + 1) & 0xffL) << 16 |
        (rgba(index*4 + 2) & 0xffL) << 8 | (rgba(index*4 + 3) & 0xffL)

  private def fourcc(data: Data, offset: Int): String =
    String(Array(data(offset), data(offset + 1), data(offset + 2), data(offset + 3)), "UTF-8").nn
