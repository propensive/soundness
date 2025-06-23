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
┃    Soundness, version 0.34.0.                                                                    ┃
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
  type Form
  def width: Int = image.getWidth
  def height: Int = image.getHeight

  def apply(x: Int, y: Int): Rgb24 =
    val color: ja.Color = ja.Color(image.getRGB(x, y), true)
    Rgb24(color.getRed, color.getGreen, color.getBlue)

  def to[format: Rasterizable as rasterizable]: Raster in format = Raster[format](image)

  def crop(left: Int = 0, bottom: Int = 0, top: Int = 0, right: Int = 0): Raster =
    Raster(width - left - right, height - top - bottom) { (x, y) => apply(x + left, y + bottom) }

  def flipX: Raster = Raster(width, height) { (x, y) => apply(width - 1 - x, y) }
  def flipY: Raster = Raster(width, height) { (x, y) => apply(x, height - 1 - y) }

  def rotate(angle: 90 | 180 | 270): Raster = angle match
    case 90  => Raster(height, width) { (x, y) => apply(width - 1 - y, x) }
    case 180 => Raster(width, height) { (x, y) => apply(width - 1 - x, height - 1 - y) }
    case 270 => Raster(height, width) { (x, y) => apply(y, height - 1 - x) }

  def portrait: Boolean = height > width
  def square: Boolean = width == height
  def landscape: Boolean = width > height

object Raster:
  def apply(width: Int, height: Int)(pixel: (Int, Int) => Rgb24): Raster =
   val image = jai.BufferedImage(width, height, jai.BufferedImage.TYPE_INT_RGB)
   for x <- 0 until width; y <- 0 until height do image.setRGB(x, y, pixel(x, y).asInt)
   new Raster(image)

  def apply[readable: Readable by Bytes](input: readable): Raster =
    new Raster(ji.ImageIO.read(input.read[Bytes].javaInputStream).nn)

  def apply[form: Rasterizable as rasterizable](image: jai.BufferedImage): Raster in form =
    new Raster(image):
      type Form = form

  given readable: [form: Rasterizable] => (Raster in form) is Readable by Bytes = raster =>
    val out = StreamOutputStream()
    ji.ImageIO.write(raster.image, form.name.s, out)
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
