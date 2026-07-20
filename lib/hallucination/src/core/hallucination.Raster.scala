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

import scala.compiletime.*

import anticipation.*
import contingency.*
import fulminate.*
import iridescence.*
import prepositional.*
import turbulence.*
import zephyrine.*

object Raster:
  def apply(width: Int, height: Int)(pixel: (Int, Int) => Chroma): Raster by Rgb =
    Raster[Rgb](width, height): (x, y) =>
      iridescence.pixel(pixel(x, y))

  @annotation.targetName("applyLayout")
  inline def apply[layout <: Tuple](width: Int, height: Int)
    ( pixel: (Int, Int) => Pixel[layout] )
  :   Raster by layout =

    val descriptor = Descriptor.of[layout]

    inline erasedValue[Channel.Storage[layout]] match
      case _: Byte =>
        val buffer = new Array[Byte](width*height)

        fill(width, height): (x, y, index) =>
          buffer(index) = Pixel.value(pixel(x, y)).toByte

        make[layout](width, height, buffer, descriptor)

      case _: Short =>
        val buffer = new Array[Short](width*height)

        fill(width, height): (x, y, index) =>
          buffer(index) = Pixel.value(pixel(x, y)).toShort

        make[layout](width, height, buffer, descriptor)

      case _: Int =>
        val buffer = new Array[Int](width*height)

        fill(width, height): (x, y, index) =>
          buffer(index) = Pixel.value(pixel(x, y)).toInt

        make[layout](width, height, buffer, descriptor)

      case _: Long =>
        val buffer = new Array[Long](width*height)

        fill(width, height): (x, y, index) =>
          buffer(index) = Pixel.value(pixel(x, y))

        make[layout](width, height, buffer, descriptor)

  def apply[streamable: Streamable by Data over zephyrine.Credit](input: streamable)
  :   Raster raises RasterError =

    RasterBackend.decode(input.read[Data])

  private def fill(width: Int, height: Int)(set: (Int, Int, Int) => Unit): Unit =
    var index = 0
    var y = 0

    while y < height do
      var x = 0

      while x < width do
        set(x, y, index)
        index += 1
        x += 1

      y += 1

  private[hallucination] def make[layout <: Tuple]
    ( width: Int, height: Int, buffer: Array[?], descriptor: Descriptor )
  :   Raster by layout =

    new Raster(width, height, buffer, descriptor).asInstanceOf[Raster by layout]

  // Builds a raster from per-index pixel words, in the storage primitive `descriptor` demands.
  private[hallucination] def build(width: Int, height: Int, descriptor: Descriptor)
    ( pixel: Int => Long )
  :   Raster =

    val length = width*height

    val buffer: Array[?] = descriptor.storageBits match
      case 8 =>
        val buffer = new Array[Byte](length)
        for index <- 0 until length do buffer(index) = pixel(index).toByte
        buffer

      case 16 =>
        val buffer = new Array[Short](length)
        for index <- 0 until length do buffer(index) = pixel(index).toShort
        buffer

      case 32 =>
        val buffer = new Array[Int](length)
        for index <- 0 until length do buffer(index) = pixel(index).toInt
        buffer

      case _ =>
        val buffer = new Array[Long](length)
        for index <- 0 until length do buffer(index) = pixel(index)
        buffer

    new Raster(width, height, buffer, descriptor)

  private[hallucination] def repack(raster: Raster, descriptor2: Descriptor): Raster =
    if raster.descriptor == descriptor2 then raster
    else
      build(raster.width, raster.height, descriptor2): index =>
        val word = raster.word(index)
        descriptor2.pack(raster.descriptor.srgb(word), raster.descriptor.alpha(word))

  given streamable: [form: Rasterizable as rasterizable]
  =>  (Raster in form) is Streamable by Data over zephyrine.Credit =
    raster => zephyrine.Stream(RasterBackend.encode(rasterizable, raster))

  given abstractable: [format: Rasterizable] => (Raster in format) is Abstractable:
    type Domain = HttpStreams
    type Result = HttpStreams.Content

    def genericize(image: Raster in format): HttpStreams.Content =
      (format.mediaType.basic, HttpStreams.Body(image.source[Data].toLazyList.iterator))

  given graphical: Raster is Graphical:
    def pixel(raster: Raster, x: Int, y: Int): Chroma = raster(x, y)
    def width(raster: Raster): Int = raster.width
    def height(raster: Raster): Int = raster.height


  given aggregable: [format: Rasterizable as rasterizable] => (tactic: Tactic[RasterError])
  =>  ( ((Raster in format) is Aggregable by Data)^{tactic} ) =

    rasterizable.read(_)


  given aggregable2: (tactic: Tactic[RasterError])
  =>  ( (Raster is Aggregable by Data)^{tactic} ) = Raster(_)

// A platform-neutral pixel store: `buffer`'s element type is the storage primitive of the
// raster's layout (`Channel.Storage[Operand]`), held unparameterised and recovered statically at
// each inline access site. The `Form` phantom carries the image format (`Raster in Png`) and the
// `Operand` phantom the pixel layout (`Raster by Rgba`); both are optional refinements.
class Raster private[hallucination]
  ( val width:  Int,
    val height: Int,
    private[hallucination] val buffer: Array[?],
    val descriptor: Descriptor )
extends Formal, Operable:
  type Operand <: Tuple

  def apply(x: Int, y: Int): Chroma = descriptor.chroma(word(y*width + x))

  private[hallucination] def word(index: Int): Long = buffer.asMatchable match
    case buffer: Array[Byte]  => buffer(index)&0xffL
    case buffer: Array[Short] => buffer(index)&0xffffL
    case buffer: Array[Int]   => buffer(index)&0xffffffffL
    case buffer: Array[Long]  => buffer(index)
    case _                    => panic(m"raster buffer has an unexpected element type")

  def to[format: Rasterizable]: Raster in format = asInstanceOf[Raster in format]

  def crop(left: Int = 0, bottom: Int = 0, top: Int = 0, right: Int = 0): Raster =
    remap(width - left - right, height - top - bottom): (x, y) =>
      (x + left, y + top)

  def flipX: Raster = remap(width, height): (x, y) => (width - 1 - x, y)
  def flipY: Raster = remap(width, height): (x, y) => (x, height - 1 - y)

  def rotate(angle: 90 | 180 | 270): Raster = angle match
    case 90  => remap(height, width): (x, y) => (width - 1 - y, x)
    case 180 => remap(width, height): (x, y) => (width - 1 - x, height - 1 - y)
    case _   => remap(height, width): (x, y) => (y, height - 1 - x)

  def portrait: Boolean = height > width
  def square: Boolean = width == height
  def landscape: Boolean = width > height

  // Builds a same-layout raster whose pixel at (x, y) is this raster's pixel at `source(x, y)`.
  private def remap(width2: Int, height2: Int)(source: (Int, Int) => (Int, Int)): Raster =
    def index(x: Int, y: Int): Int =
      val (x2, y2) = source(x, y)
      y2*width + x2

    buffer.asMatchable match
      case buffer: Array[Byte] =>
        val buffer2 = new Array[Byte](width2*height2)

        Raster.fill(width2, height2): (x, y, index2) =>
          buffer2(index2) = buffer(index(x, y))

        new Raster(width2, height2, buffer2, descriptor)

      case buffer: Array[Short] =>
        val buffer2 = new Array[Short](width2*height2)

        Raster.fill(width2, height2): (x, y, index2) =>
          buffer2(index2) = buffer(index(x, y))

        new Raster(width2, height2, buffer2, descriptor)

      case buffer: Array[Int] =>
        val buffer2 = new Array[Int](width2*height2)

        Raster.fill(width2, height2): (x, y, index2) =>
          buffer2(index2) = buffer(index(x, y))

        new Raster(width2, height2, buffer2, descriptor)

      case buffer: Array[Long] =>
        val buffer2 = new Array[Long](width2*height2)

        Raster.fill(width2, height2): (x, y, index2) =>
          buffer2(index2) = buffer(index(x, y))

        new Raster(width2, height2, buffer2, descriptor)

      case _ =>
        panic(m"raster buffer has an unexpected element type")
