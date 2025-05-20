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
┃    Soundness, version 0.27.0.                                                                    ┃
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
import gossamer.*
import iridescence.*
import prepositional.*
import proscenium.*
import rudiments.*
import turbulence.*
import vacuous.*

import java.awt.image as jai
import java.awt as ja
import javax.imageio as ji

case class Raster(private[hallucination] val image: jai.BufferedImage):
  type Format
  def width: Int = image.getWidth
  def height: Int = image.getHeight

  def apply(x: Int, y: Int): Rgb24 =
    val color: ja.Color = ja.Color(image.getRGB(x, y), true)
    Rgb24(color.getRed, color.getGreen, color.getBlue)

  def to[format: Rasterizable as rasterizable]: Raster in format = Raster[format](image)

  def portrait: Boolean = height > width
  def square: Boolean = width == height
  def landscape: Boolean = width > height

  def rotate(angle: 90 | 180 | 270): Raster =
    val image2 = angle match
      case 180 => jai.BufferedImage(width, height, image.getType())
      case _   => jai.BufferedImage(height, width, image.getType())

    angle match
      case 90 =>
        for y <- 0 until height; x <- 0 until width
        do image2.setRGB(y, width - 1 - x, image.getRGB(x, y))

      case 180 =>
        for y <- 0 until height; x <- 0 until width
        do image2.setRGB(width - 1 - x, height - 1 - y, image.getRGB(x, y))

        image2

      case 270 =>
        for y <- 0 until height; x <- 0 until width
        do image2.setRGB(height - 1 - y, x, image.getRGB(x, y))

      new Raster(image2)

object Raster:
  def apply[readable: Readable by Bytes](input: readable): Raster =
    new Raster(ji.ImageIO.read(input.read[Bytes].javaInputStream).nn)

  def apply[format: Rasterizable as rasterizable](image: jai.BufferedImage): Raster in format =
    new Raster(image):
      type Format = format

  given readable: [format: Rasterizable] => (Raster in format) is Readable by Bytes = raster =>
    val out = StreamOutputStream()
    ji.ImageIO.write(raster.image, format.name.s, out)
    out.close()
    out.stream

  given abstractable: [format: Rasterizable] => (Raster in format) is Abstractable:
    type Domain = HttpStreams
    type Result = HttpStreams.Content

    def genericize(image: Raster in format): HttpStreams.Content =
      (format.mediaType.basic, image.read[Stream[Bytes]])

  given graphical: Raster is Graphical:
    def pixel(raster: Raster, x: Int, y: Int): Int = raster(x, y).asInt
    def width(raster: Raster): Int = raster.width
    def height(raster: Raster): Int = raster.height

  given aggregable: [format: Rasterizable as rasterizable] => Tactic[RasterError]
        => (Raster in format) is Aggregable by Bytes =
    rasterizable.read(_)

  given aggregable2: Tactic[RasterError] => Raster is Aggregable by Bytes = Raster(_)
