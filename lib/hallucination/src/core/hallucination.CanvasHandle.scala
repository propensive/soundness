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
import scala.caps

import aperture.*
import iridescence.*
import prepositional.*

object CanvasHandle:
  // The pixel operations are `Granting`-gated extensions rather than methods, so that a
  // read-only canvas simply has no `update`, and the mismatch is a compile-time error.
  extension [layout <: Tuple](canvas: (CanvasHandle[layout] & Granting[Grant.Read])^)
    inline def apply(x: Int, y: Int): Pixel[layout] = hallucination.pixel(canvas.raster)(x, y)

  extension [layout <: Tuple](canvas: (CanvasHandle[layout] & Granting[Grant.Write])^)
    inline def update(x: Int, y: Int, pixel: Pixel[layout]): Unit =
      val index = y*canvas.raster.width + x

      inline erasedValue[Channel.Storage[layout]] match
        case _: Byte =>
          canvas.raster.buffer.asInstanceOf[Array[Byte]](index) = Pixel.value(pixel).toByte

        case _: Short =>
          canvas.raster.buffer.asInstanceOf[Array[Short]](index) = Pixel.value(pixel).toShort

        case _: Int =>
          canvas.raster.buffer.asInstanceOf[Array[Int]](index) = Pixel.value(pixel).toInt

        case _: Long =>
          canvas.raster.buffer.asInstanceOf[Array[Long]](index) = Pixel.value(pixel)

// The scoped capability for pixel access to an open raster. Writes through a `Write`-granted
// canvas mutate the raster's buffer in place, with the same semantics as writing through an
// open file handle; `snapshot` takes an independent copy for deriving new rasters instead.
class CanvasHandle[layout <: Tuple] private[hallucination]
  ( private[hallucination] val raster: Raster by layout )
extends caps.ExclusiveCapability:
  def width: Int = raster.width
  def height: Int = raster.height

  def snapshot: Raster by layout =
    Raster.build(raster.width, raster.height, raster.descriptor)(raster.word(_))
    . asInstanceOf[Raster by layout]
