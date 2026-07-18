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

import soundness.*

import strategies.throwUnsafely
import errorDiagnostics.stackTracesDiagnostics

object Tests extends Suite(m"Hallucination Tests"):

  val jpeg = hex"""ffd8ffe000104a46494600010101012c012c0000ffdb0043000302020302020303030304030304050
                   805050404050a070706080c0a0c0c0b0a0b0b0d0e12100d0e110e0b0b1016101113141515150c0f17
                   1816141812141514ffdb00430103040405040509050509140d0b0d141414141414141414141414141
                   4141414141414141414141414141414141414141414141414141414141414141414141414ffc00011
                   080001000103011100021101031101ffc40014000100000000000000000000000000000009ffc4001
                   4100100000000000000000000000000000000ffc40014010100000000000000000000000000000000
                   ffc40014110100000000000000000000000000000000ffda000c03010002110311003f002a81ffd9
                   """

  val png = hex"""89504e470d0a1a0a0000000d4948445200000001000000010802000000907753de0000000173524742
                  01d9c92c7f0000000467414d410000b18f0bfc6105000000206348524d00007a26000080840000fa00
                  000080e8000075300000ea6000003a98000017709cba513c0000000c4944415408d763606060000000
                  0400012734270a0000000049454e44ae426082"""

  val broken = hex"00001080200000907753de0000000173524742"

  def run(): Unit =
    test(m"Read a JPEG's width"):
      val raster = jpeg.read[Raster in Jpeg]
      raster.width
    . assert(_ == 1)

    test(m"Read a JPEG's height"):
      val raster = jpeg.read[Raster in Jpeg]
      raster.height
    . assert(_ == 1)

    test(m"Read a PNG's width"):
      val raster = png.read[Raster in Png]
      raster.width
    . assert(_ == 1)

    test(m"Read a PNG's height"):
      val raster = png.read[Raster in Png]
      raster.height
    . assert(_ == 1)

    test(m"Read a JPEG as a PNG fails"):
      capture[RasterError](png.read[Raster in Jpeg])
    . assert(_ == RasterError(Jpeg()))

    test(m"Read a PNG as a JPEG fails"):
      capture[RasterError](jpeg.read[Raster in Png])
    . assert(_ == RasterError(Png()))

    test(m"Convert a PNG to a JPEG"):
      val jpeg2 = png.read[Raster in Png].to[Jpeg].read[Data]
      jpeg2.read[Raster in Jpeg].width
    . assert(_ == 1)

    test(m"Convert a JPEG to a BMP"):
      val bmp = jpeg.read[Raster in Jpeg].to[Bmp].read[Data]
      bmp.read[Raster in Bmp].width
    . assert(_ == 1)

    test(m"Convert a JPEG to a GIF"):
      val gif = jpeg.read[Raster in Jpeg].to[Gif].read[Data]
      gif.read[Raster in Gif].width
    . assert(_ == 1)

    test(m"Convert a PNG to a BMP"):
      val bmp = png.read[Raster in Png].to[Bmp].read[Data]
      bmp.read[Raster in Bmp].width
    . assert(_ == 1)

    test(m"Convert a PNG to a GIF"):
      val gif = png.read[Raster in Png].to[Gif].read[Data]
      gif.read[Raster in Gif].width
    . assert(_ == 1)

    // Each row carries its own y coordinate in the red channel so that crop()
    // can be checked structurally.
    def stripes: Raster = Raster(2, 4)((_, y) => Chroma(y*10, 0, 0))

    test(m"crop(top = n) drops rows from the top"):
      val cropped = stripes.crop(top = 2)
      (cropped.height, cropped(0, 0).red, cropped(0, 1).red)
    . assert(_ == (2, 20, 30))

    test(m"crop(bottom = n) drops rows from the bottom"):
      val cropped = stripes.crop(bottom = 2)
      (cropped.height, cropped(0, 0).red, cropped(0, 1).red)
    . assert(_ == (2, 0, 10))

    test(m"crop with top and bottom keeps the middle rows"):
      val cropped = stripes.crop(top = 1, bottom = 1)
      (cropped.height, cropped(0, 0).red, cropped(0, 1).red)
    . assert(_ == (2, 10, 20))

    test(m"a layout-typed raster gives typed pixel access"):
      val raster = Raster[Rgba](2, 2): (x, y) =>
        Pixel[Rgba](Srgb(x.toDouble, y.toDouble, 1.0))

      (raster.pixel(1, 0).red, raster.pixel(1, 0).alpha, raster.pixel(0, 1).green)
    . assert(_ == (255, 255, 255))

    test(m"a decoded PNG has an eight-bit RGB or RGBA layout"):
      png.read[Raster in Png].descriptor.entries.map(_.depth)
    . assert(_.forall(_ == 8))

    test(m"repacking to the same layout returns the same raster"):
      val raster = stripes
      raster.repack[Rgb] eq raster
    . assert(_ == true)

    test(m"repacking to RGBA adds a fully-opaque alpha channel"):
      val raster = stripes.repack[Rgba]
      (raster.pixel(0, 2).red, raster.pixel(0, 2).alpha)
    . assert(_ == (20, 255))

    test(m"repacking to a 16-bit layout scales the channels"):
      Raster(1, 1)((x, y) => Chroma(255, 0, 51)).repack[(Red[5], Green[6], Blue[5])].pixel(0, 0)
      . pipe: pixel =>
        (pixel.red, pixel.green, pixel.blue)
    . assert(_ == (31, 0, 6))

    test(m"rotation transposes the dimensions and moves the pixels"):
      stripes.rotate(90).pipe: rotated =>
        (rotated.width, rotated.height, rotated(3, 1).red)
    . assert(_ == (4, 2, 30))

    test(m"a raster opened as a canvas reads pixels"):
      val raster = Raster(2, 2)((x, y) => Chroma(x*100 + y*10, 0, 0))

      raster.open[Canvas](): canvas ?=>
        canvas(1, 1).red
    . assert(_ == 110)

    test(m"writing through a canvas mutates the raster in place"):
      val raster = Raster(2, 2)((x, y) => Chroma(0, 0, 0))

      raster.open[Canvas](Read & Write): canvas ?=>
        canvas(0, 0) = Pixel[Rgb](Srgb(1.0, 0.0, 0.0))

      raster(0, 0).red
    . assert(_ == 255)

    test(m"a snapshot is unaffected by later writes"):
      val raster = Raster(1, 1)((x, y) => Chroma(1, 2, 3))

      raster.open[Canvas](Read & Write): canvas ?=>
        val before = canvas.snapshot
        canvas(0, 0) = Pixel[Rgb](Srgb(1.0, 1.0, 1.0))
        before(0, 0).red
    . assert(_ == 1)
