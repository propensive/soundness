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

import java.awt.image as jai
import java.io as ji2
import javax.imageio as ji

import anticipation.*
import contingency.*
import rudiments.*
import vacuous.*


// The JVM backend: decoding and encoding through `javax.imageio`, whose native codecs outperform
// any pure implementation. The pure-Scala codecs in `core` remain compiled (and tested) on the
// JVM; they are simply not selected here.
// The `javax.imageio` bridge, whose native codecs outperform any pure implementation. Each
// format component's JVM backend calls this; the formats `javax.imageio` cannot read (WebP has no
// reader on any standard JRE) simply do not, and use their pure codec on every platform.
private[hallucination] object ImageIo:
  def decode(format: Rasterizable, data: Data): Raster raises Raster.Error =
    val reader: ji.ImageReader =
      ji.ImageIO.getImageReadersByFormatName(format.name.s).nn.next().nn

    reader.setInput(ji.ImageIO.createImageInputStream(data.javaInputStream).nn)
    val image = try reader.read(0).nn catch case _: ji.IIOException => abort(Raster.Error(format))

    convert(image).also(reader.dispose())

  def encode(format: Rasterizable, raster: Raster): Data =
    val alpha = format.alpha && raster.descriptor.hasAlpha

    val imageType =
      if alpha then jai.BufferedImage.TYPE_INT_ARGB else jai.BufferedImage.TYPE_INT_RGB

    val image = jai.BufferedImage(raster.width, raster.height, imageType)
    val argb = new scala.Array[Int](raster.width*raster.height)

    for index <- 0 until argb.length do
      val word = raster.word(index)
      val opacity = if alpha then (raster.descriptor.alpha(word)*255 + 0.5).toInt else 255
      argb(index) = opacity << 24 | raster.descriptor.chroma(word).underlying

    image.setRGB(0, 0, raster.width, raster.height, argb, 0, raster.width)
    val out = ji2.ByteArrayOutputStream()
    ji.ImageIO.write(image, format.name.s, out)

    Array.unsafeFrozen(out.toByteArray.nn)

  // Reads out the pixels in one bulk `getRGB`, as `Rgb` or `Rgba` according to the image's
  // colour model.
  private def convert(image: jai.BufferedImage): Raster =
    val width = image.getWidth
    val height = image.getHeight
    val argb = image.getRGB(0, 0, width, height, null, 0, width).nn

    if image.getColorModel.nn.hasAlpha
    then Raster.build(width, height, Descriptor.rgba): index =>
      (argb(index).toLong&0xffffff) << 8 | (argb(index) >>> 24)
    else Raster.build(width, height, Descriptor.rgb)(argb(_).toLong&0xffffff)
