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
package iridescence

import scala.annotation.nowarn
import scala.compiletime.*

import anticipation.*
import prepositional.*
import rudiments.*

object PixelOpaque:
  // One pixel of the layout `layout`, packed into the low `Channel.TotalBits[layout]` bits of a
  // `Long`, first channel most significant. The channel accessors are `inline` so that for any
  // concrete layout each compiles to a single shift-and-mask with constant operands; they exist
  // only for layouts which contain the channel, since `Channel.Shift` fails to reduce otherwise.
  // Code which is itself generic in `layout` cannot use them; it should work with the runtime
  // channel descriptions instead.
  opaque type Pixel[layout <: Tuple] = Long

  object Pixel:
    inline given underlying: [layout <: Tuple] => Underlying[Pixel[layout], Long] = !!

    @nowarn("msg=New anonymous class definition will be duplicated at each inline site")
    inline given chromatic: [layout <: Tuple] => Pixel[layout] is Chromatic =
      new Chromatic:
        type Self = Pixel[layout]
        def convert(pixel: Pixel[layout]): Chroma = PixelOpaque.chroma(pixel)

    def make[layout <: Tuple](value: Long): Pixel[layout] = value
    def value[layout <: Tuple](pixel: Pixel[layout]): Long = pixel

    // Scales a colour into whichever channels the layout has: RGB (with any alpha channel set
    // fully opaque), CMYK or greyscale (by Rec. 601 luma).
    inline def apply[layout <: Tuple](color: Color in Srgb): Pixel[layout] =
      val srgb = color.to[Srgb]

      inline erasedValue[Channel.Has[layout, "red"]] match
        case _: true =>
          val red = pack[layout, "red"](srgb.red)
          val green = pack[layout, "green"](srgb.green)
          val blue = pack[layout, "blue"](srgb.blue)

          inline erasedValue[Channel.Has[layout, "alpha"]] match
            case _: true  => red | green | blue | pack[layout, "alpha"](1.0)

            case _: false => red | green | blue

        case _: false => inline erasedValue[Channel.Has[layout, "cyan"]] match
          case _: true =>
            val key = 1.0 - srgb.red.max(srgb.green).max(srgb.blue)
            val white = 1.0 - key
            val cyan = if white == 0.0 then 0.0 else (white - srgb.red)/white
            val magenta = if white == 0.0 then 0.0 else (white - srgb.green)/white
            val yellow = if white == 0.0 then 0.0 else (white - srgb.blue)/white

            pack[layout, "cyan"](cyan) | pack[layout, "magenta"](magenta) |
              pack[layout, "yellow"](yellow) | pack[layout, "key"](key)

          case _: false =>
            pack[layout, "grey"](srgb.red*0.299 + srgb.green*0.587 + srgb.blue*0.114)

    // Scales a proportion to the labelled channel's depth and shifts it into position.
    private inline def pack[layout <: Tuple, label <: String & Singleton](proportion: Double)
    :   Long =

      scale(proportion, constValue[Channel.Depth[layout, label]]) <<
        constValue[Channel.Shift[layout, label]]

    private def limit(depth: Int): Long = (1L << depth) - 1

    private def scale(proportion: Double, depth: Int): Long =
      (proportion*limit(depth) + 0.5).toLong

  extension [layout <: Tuple](pixel: Pixel[layout])
    // The kernel behind every channel accessor: for a concrete layout, both `constValue`s fold
    // to literals, leaving a shift and a mask.
    inline def channel[label <: String & Singleton]: Int =
      val mask = (1L << constValue[Channel.Depth[layout, label]]) - 1
      ((pixel >>> constValue[Channel.Shift[layout, label]])&mask).toInt

    inline def red: Int = pixel.channel["red"]
    inline def green: Int = pixel.channel["green"]
    inline def blue: Int = pixel.channel["blue"]
    inline def alpha: Int = pixel.channel["alpha"]
    inline def cyan: Int = pixel.channel["cyan"]
    inline def magenta: Int = pixel.channel["magenta"]
    inline def yellow: Int = pixel.channel["yellow"]
    inline def key: Int = pixel.channel["key"]
    inline def grey: Int = pixel.channel["grey"]

    // The proportion of the labelled channel's full depth, in the unit interval.
    inline def proportion[label <: String & Singleton]: Double =
      pixel.channel[label].toDouble/((1 << constValue[Channel.Depth[layout, label]]) - 1)

    private inline def srgbColor: Srgb =
      Srgb(pixel.proportion["red"], pixel.proportion["green"], pixel.proportion["blue"])

    private inline def cmykColor: Cmyk =
      Cmyk
        ( pixel.proportion["cyan"],
          pixel.proportion["magenta"],
          pixel.proportion["yellow"],
          pixel.proportion["key"] )

    private inline def greyColor: Srgb =
      val luma = pixel.proportion["grey"]
      Srgb(luma, luma, luma)

    // The colour of the pixel in the most natural model for its layout: `Srgb` for RGB and
    // greyscale layouts, or `Cmyk` for CMYK layouts, exact at any channel depth.
    transparent inline def color: Color =
      inline erasedValue[Channel.Has[layout, "red"]] match
        case _: true  => pixel.srgbColor

        case _: false => inline erasedValue[Channel.Has[layout, "cyan"]] match
          case _: true  => pixel.cmykColor
          case _: false => pixel.greyColor

    // The colour of the pixel as `Srgb`, whatever its layout's colour model.
    inline def srgb: Srgb =
      inline erasedValue[Channel.Has[layout, "red"]] match
        case _: true  => pixel.srgbColor

        case _: false => inline erasedValue[Channel.Has[layout, "cyan"]] match
          case _: true  => pixel.cmykColor.to[Srgb]
          case _: false => pixel.greyColor

    // Rescales the pixel to 24-bit RGB, rounding as `Rgb32.chroma` does; layouts without RGB
    // channels are converted through their colour model.
    inline def chroma: Chroma =
      inline erasedValue[Channel.Has[layout, "red"]] match
        case _: true =>
          val redMax = (1 << constValue[Channel.Depth[layout, "red"]]) - 1
          val greenMax = (1 << constValue[Channel.Depth[layout, "green"]]) - 1
          val blueMax = (1 << constValue[Channel.Depth[layout, "blue"]]) - 1

          Chroma
            ( (pixel.red*255 + redMax/2)/redMax,
              (pixel.green*255 + greenMax/2)/greenMax,
              (pixel.blue*255 + blueMax/2)/blueMax )

        case _: false =>
          val srgb = pixel.srgb

          Chroma
            ( (srgb.red*255 + 0.5).toInt,
              (srgb.green*255 + 0.5).toInt,
              (srgb.blue*255 + 0.5).toInt )
