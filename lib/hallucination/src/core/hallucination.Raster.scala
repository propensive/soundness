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
import scala.annotation.targetName

import anticipation.*
import contingency.*
import fulminate.*
import iridescence.*
import prepositional.*
import turbulence.*
import vacuous.*
import zephyrine.*

object Raster:
  def apply(width: Int, height: Int)(pixel: (Int, Int) => Chroma): Raster by Rgb =
    // Routed through `build` (whose writes are already exclusive) rather than the inline
    // layout `apply`, whose expanded closure write the separation checker rejects here.
    build(width, height, Descriptor.of[Rgb]): index =>
      Pixel.value(iridescence.packed(pixel(index%width, index/width)))
    . asInstanceOf[Raster by Rgb]

  @targetName("applyLayout")
  inline def apply[layout <: Tuple](width: Int, height: Int)
    ( pixel: (Int, Int) => Pixel[layout] )
  :   Raster by layout =

    val descriptor = Descriptor.of[layout]

    // Routed through `build` (which selects the storage primitive from the descriptor at
    // runtime) rather than closure writes into a per-storage array: an expanded closure
    // write is rejected by the separation checker at each inline site.
    build(width, height, descriptor) { index => Pixel.value(pixel(index%width, index/width)) }
    . asInstanceOf[Raster by layout]

  // Recognises the format from its opening magic bytes, among the formats the caller has named.
  // `hallucination.formats` supplies every format this library implements.
  def apply[streamable: Streamable by Data over zephyrine.Credit](input: streamable)
    (using formats: Raster.Formats)
  :   Raster raises Raster.Error =

    val data = input.read[Data]
    formats.recognise(data).lay(abort(Raster.Error(Unset)))(_.decode(data))

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
    ( width: Int, height: Int, buffer: scala.Array[?], descriptor: Descriptor )
  :   Raster by layout =

    new Raster(width, height, buffer, descriptor).asInstanceOf[Raster by layout]

  // Builds a raster from per-index pixel words, in the storage primitive `descriptor` demands.
  private[hallucination] def build(width: Int, height: Int, descriptor: Descriptor)
    ( pixel: Int => Long )
  :   Raster =

    val length = width*height

    val buffer: scala.Array[?] = descriptor.storageBits match
      case 8 =>
        val buffer = new scala.Array[Byte](length)
        var index = 0
        while index < length do
          writable(buffer)(index) = pixel(index).toByte
          index += 1
        buffer

      case 16 =>
        val buffer = new scala.Array[Short](length)
        var index = 0
        while index < length do
          writable(buffer)(index) = pixel(index).toShort
          index += 1
        buffer

      case 32 =>
        val buffer = new scala.Array[Int](length)
        var index = 0
        while index < length do
          writable(buffer)(index) = pixel(index).toInt
          index += 1
        buffer

      case _ =>
        val buffer = new scala.Array[Long](length)
        var index = 0
        while index < length do
          writable(buffer)(index) = pixel(index)
          index += 1
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
    raster => zephyrine.Stream(rasterizable.encode(raster))

  given abstractable: [format: Rasterizable] => (Raster in format) is Abstractable:
    type Domain = HttpStreams
    type Result = HttpStreams.Content

    def genericize(image: Raster in format): HttpStreams.Content =
      // `.stdlib`: `HttpStreams.Body` takes a stdlib `Iterator`, which the opaque `Chain`
      // has no member to yield.
      (format.mediaType.basic, HttpStreams.Body(image.source[Data].chain))

  given graphical: Raster is Graphical:
    def pixel(raster: Raster, x: Int, y: Int): Chroma = raster(x, y)
    def width(raster: Raster): Int = raster.width
    def height(raster: Raster): Int = raster.height

  given aggregable: [format: Rasterizable as rasterizable] => (tactic: Tactic[Raster.Error])
  =>  ( ((Raster in format) is Aggregable by Data)^{tactic} ) =

    rasterizable.read(_)

  // Aggregating a stream into a `Raster` without naming its format recognises it by magic bytes,
  // so it needs the candidate formats just as `Raster(data)` does.
  given aggregable2: (tactic: Tactic[Raster.Error]) => (formats: Raster.Formats)
  =>  ( (Raster is Aggregable by Data)^{tactic} ) = Raster(_)

  // RasterError → Raster.Error
  object Error:
    enum Reason:
      case BadSignature, BadCrc, Truncated, UnsupportedVariant, Bitstream, Huffman, InvalidTransform

  case class Error
    ( rasterizable: Optional[Rasterizable], reason: Optional[Raster.Error.Reason] = Unset )
    ( using Diagnostics )
  extends fulminate.Error
    ( m"unable to read the raster image in ${rasterizable.lay("unspecified".tt)(_.name)} format" )

  // RasterFormats → Raster.Formats
  // The formats `Raster` will try when asked to decode data whose format it was not told. Because
  // each codec now lives in its own component, the candidates are whatever the caller has linked
  // and named, rather than every format the library implements: recognising a format you did not
  // compile is not something this can do. `hallucination.formats` supplies every format at once,
  // for consumers that want the old behaviour in a single import.
  case class Formats(candidates: List[Rasterizable]):
    def recognise(data: Data): Optional[Rasterizable] =
      def next(remaining: List[Rasterizable]): Optional[Rasterizable] = remaining match
        case head :: tail => if head.sniff(data) then head else next(tail)
        case _            => Unset

      next(candidates)

  object Formats:
    def apply(formats: Rasterizable*): Raster.Formats =
      Raster.Formats(formats.to(List))

// A platform-neutral pixel store: `buffer`'s element type is the storage primitive of the
// raster's layout (`Channel.Storage[Operand]`), held unparameterised and recovered statically at
// each inline access site. The `Form` phantom carries the image format (`Raster in Png`) and the
// `Operand` phantom the pixel layout (`Raster by Rgba`); both are optional refinements.
class Raster private[hallucination]
  ( val width:  Int,
    val height: Int,
    // Not frozen: a `Write`-granted `CanvasHandle` mutates the buffer in place, so it is
    // untracked instead, keeping the class type free of a capture variable.
    @scala.caps.unsafe.untrackedCaptures private[hallucination] val buffer: scala.Array[?],
    val descriptor: Descriptor )
extends Formal, Operable:
  type Operand <: Tuple

  def apply(x: Int, y: Int): Chroma = descriptor.chroma(word(y*width + x))

  private[hallucination] def word(index: Int): Long = buffer.asMatchable match
    case buffer: scala.Array[Byte]  => buffer(index)&0xffL
    case buffer: scala.Array[Short] => buffer(index)&0xffffL
    case buffer: scala.Array[Int]   => buffer(index)&0xffffffffL
    case buffer: scala.Array[Long]  => buffer(index)
    case _                    => panic(m"raster buffer has an unexpected element type")

  def to[format: Rasterizable]: Raster in format = asInstanceOf[Raster in format]

  def crop(left: Int = 0, bottom: Int = 0, top: Int = 0, right: Int = 0): Raster =
    remap(width - left - right, height - top - bottom): (x, y) => (x + left, y + top)

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
      case buffer: scala.Array[Byte] =>
        val buffer2 = new scala.Array[Byte](width2*height2)

        Raster.fill(width2, height2): (x, y, index2) => buffer2(index2) = buffer(index(x, y))

        new Raster(width2, height2, buffer2, descriptor)

      case buffer: scala.Array[Short] =>
        val buffer2 = new scala.Array[Short](width2*height2)

        Raster.fill(width2, height2): (x, y, index2) => buffer2(index2) = buffer(index(x, y))

        new Raster(width2, height2, buffer2, descriptor)

      case buffer: scala.Array[Int] =>
        val buffer2 = new scala.Array[Int](width2*height2)

        Raster.fill(width2, height2): (x, y, index2) => buffer2(index2) = buffer(index(x, y))

        new Raster(width2, height2, buffer2, descriptor)

      case buffer: scala.Array[Long] =>
        val buffer2 = new scala.Array[Long](width2*height2)

        Raster.fill(width2, height2): (x, y, index2) => buffer2(index2) = buffer(index(x, y))

        new Raster(width2, height2, buffer2, descriptor)

      case _ =>
        panic(m"raster buffer has an unexpected element type")
