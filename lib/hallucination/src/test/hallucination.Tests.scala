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
┃    Soundness, version 0.42.0.                                                                    ┃
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
import errorDiagnostics.stackTraces

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
      val jpeg2 = png.read[Raster in Png].to[Jpeg].read[Bytes]
      jpeg2.read[Raster in Jpeg].width
    . assert(_ == 1)

    test(m"Convert a JPEG to a BMP"):
      val bmp = jpeg.read[Raster in Jpeg].to[Bmp].read[Bytes]
      bmp.read[Raster in Bmp].width
    . assert(_ == 1)

    test(m"Convert a JPEG to a GIF"):
      val gif = jpeg.read[Raster in Jpeg].to[Gif].read[Bytes]
      gif.read[Raster in Gif].width
    . assert(_ == 1)

    test(m"Convert a PNG to a BMP"):
      val bmp = png.read[Raster in Png].to[Bmp].read[Bytes]
      bmp.read[Raster in Bmp].width
    . assert(_ == 1)

    test(m"Convert a PNG to a GIF"):
      val gif = png.read[Raster in Png].to[Gif].read[Bytes]
      gif.read[Raster in Gif].width
    . assert(_ == 1)
