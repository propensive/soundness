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

import scala.math

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

  // A 23x17 photo-like gradient (odd dimensions exercise block padding), encoded by libjpeg via
  // Pillow: a baseline 4:2:0 image, the same image as a progressive scan, and a grayscale image.
  val jpg420 = hex"""ffd8ffe000104a46494600010100000100010000ffdb00430003020202020203020202030303
                     0304060404040404080606050609080a0a090809090a0c0f0c0a0b0e0b09090d110d0e0f1010
                     11100a0c12131210130f101010ffdb00430103030304030408040408100b090b101010101010
                     1010101010101010101010101010101010101010101010101010101010101010101010101010
                     101010101010ffc00011080011001703012200021101031101ffc4001f000001050101010101
                     0100000000000000000102030405060708090a0bffc400b51000020103030204030505040400
                     00017d01020300041105122131410613516107227114328191a1082342b1c11552d1f0243362
                     7282090a161718191a25262728292a3435363738393a434445464748494a535455565758595a
                     636465666768696a737475767778797a838485868788898a92939495969798999aa2a3a4a5a6
                     a7a8a9aab2b3b4b5b6b7b8b9bac2c3c4c5c6c7c8c9cad2d3d4d5d6d7d8d9dae1e2e3e4e5e6e7
                     e8e9eaf1f2f3f4f5f6f7f8f9faffc4001f010003010101010101010101000000000000010203
                     0405060708090a0bffc400b51100020102040403040705040400010277000102031104052131
                     061241510761711322328108144291a1b1c109233352f0156272d10a162434e125f11718191a
                     262728292a35363738393a434445464748494a535455565758595a636465666768696a737475
                     767778797a82838485868788898a92939495969798999aa2a3a4a5a6a7a8a9aab2b3b4b5b6b7
                     b8b9bac2c3c4c5c6c7c8c9cad2d3d4d5d6d7d8d9dae2e3e4e5e6e7e8e9eaf2f3f4f5f6f7f8f9
                     faffda000c03010002110311003f00f93be1f7c1ff00f55fe8be9fc35f4cfc3ef83ffeabfd17
                     d3f86bbcf02fc36d32d8299a484142032820b039c63039af59d760d23c1fe14ff42f35eff51c
                     db5b7949b5a3c8f9e4e59586d5ce197386299eb59712ff00abf83adf57c56329426fa39c79b5
                     76bf2a7ccd2d6f65a59f667a5937174323cb6ae6989bfb3a51727e76d92f36ec979b47cdde20
                     f0a47e2dd752decadf3a7e96b2410b7cac25727e79548fe16daa072785078c91457a84fe37f8
                     5df0b4411f8d356b5d3dee154c56ec5a5b96560d87f2224770998dc6fc6dc8c67240a2b2a39b
                     f0b5382a7839ce708e89c70f88945d9ead4a349a777d53699f864aa717f1a559e7b3c1d5aaeb
                     3bf3461271b68924d2b5a2928af248e8fe1f758bf0aecfe24f5f0d7fdbd7fed1a28afe3ec3ff
                     00c8d29ffdbdff00a4b3eb38effe484c67fdc2ff00d3d4cfc9ef01ff00aa5ff77fa514515fdc
                     d8afe2b3fac72dff007689ffd9
                     """

  val jpgProg = hex"""ffd8ffe000104a46494600010100000100010000ffdb00430003020202020203020202030303
                     0304060404040404080606050609080a0a090809090a0c0f0c0a0b0e0b09090d110d0e0f1010
                     11100a0c12131210130f101010ffdb00430103030304030408040408100b090b101010101010
                     1010101010101010101010101010101010101010101010101010101010101010101010101010
                     101010101010ffc20011080011001703012200021101031101ffc40018000100030100000000
                     00000000000000000005060708ffc40018010002030000000000000000000000000002070405
                     06ffda000c03010002100310000001c9b4d9eb6049cdd680e1646684f5b727079b67ffc4001a
                     100003010101010000000000000000000000040503010231ffda00080101000105029f1c9f1c
                     615e37ba3373f26fcc9357aecb964f29087cffc4002311000104010109000000000000000000
                     000100020405031112313233354182b2f0ffda0008010301013f01876e20c674ac9c2d1aa2eb
                     7ba719c70b9db7dc03a7c372bde859bc7ddaa372c2ffc400251100020102020b000000000000
                     000000000103020005049111121315313234517181b1ffda0008010201013f01b96ef4cf66d7
                     441f234e5c6a18bb5c46aa4920765b08cc4697d547dfca6f357fffc400251000010204040700
                     0000000000000000000100020311122204103161212341718291c1ffda0008010100063f0216
                     a16aa58de5c2981beeb890ac9d712d6cba6e80c6c56c3ab46eaef43b20b0de5f32ffc4001f10
                     00020202010500000000000000000000011100312141f11051617191ffda0008010100013f21
                     e06703023002f05b61e0a1f22aab6372da5411eeb0381b1b51ecc018c80bca895966ba8747a9
                     ffda000c030100020003000000109818feffc4001a1101010100030100000000000000000000
                     01116100102131ffda0008010301013f10b4b52d9f035606a712f6ad28f00109000c0ef45fff
                     c4001c11010003000203000000000000000000000100112131415161f0ffda0008010201013f
                     10d81fa798d5d1b06dd194f860dc3c2829d448dbd8a33e0f69cb3fffc4001e10010003000202
                     0300000000000000000001001121314151a16181f0ffda0008010100013f10f551eaa2d75810
                     7771e00d7039690740580221baaa3677dff3e0ee0695652bf30e60860c52bfb8e7455da139fd
                     67393f27c4ffd9
                     """

  val jpgGray = hex"""ffd8ffe000104a46494600010100000100010000ffdb00430003020202020203020202030303
                     0304060404040404080606050609080a0a090809090a0c0f0c0a0b0e0b09090d110d0e0f1010
                     11100a0c12131210130f101010ffc0000b080011001701011100ffc4001f0000010501010101
                     010100000000000000000102030405060708090a0bffc400b510000201030302040305050404
                     0000017d01020300041105122131410613516107227114328191a1082342b1c11552d1f02433
                     627282090a161718191a25262728292a3435363738393a434445464748494a53545556575859
                     5a636465666768696a737475767778797a838485868788898a92939495969798999aa2a3a4a5
                     a6a7a8a9aab2b3b4b5b6b7b8b9bac2c3c4c5c6c7c8c9cad2d3d4d5d6d7d8d9dae1e2e3e4e5e6
                     e7e8e9eaf1f2f3f4f5f6f7f8f9faffda0008010100003f00f8b3e16f847fd4feebd3b57d81f0
                     b7c23fea7f75e9dabebdf867e11fdcafeebf87d3dabf3b7e16f847fd4feebd3b57d81f0b7c23
                     fea7f75e9dabd4be38fc6fd07f64af8293fc55d5f40fedbbb7beb5d2b4ad23ed0f6df6fba949
                     668fcf58a5116d823b8972cbb4f95b321996be29f85bff002c7f0afb03e16ffcb1fc2be67ff8
                     2c27fa9f821feef897f969b5ffd9
                     """

  // libjpeg's own decode of `jpgGray`: one luma byte per pixel, row-major.
  val grayRef = hex"""0009121a242c363f47515a626c747e878f99a1abb4bcc6050e172029323b444d565f68717a83
                      8c959fa6b0b9c2cb0a131c252e374049525b646d767f88919aa4abb5bec7d00e182129333b45
                      4e566069717b838d969ea8b0bac3cbd5141d262f38414a535c656e778089929ba4aeb5bfc9d1
                      da19222b343d464f58616a737c858e97a0a8b3bac4cdd6df1e273039424b545d666f78818a93
                      9ca5aeb8bfc9d2dbe4232c353e475059626b747d868f98a1aab3bdc4ced8e0e928313a424c54
                      5e677079828a949ca6afb5c3cbd1d9e4f02d363f48515a636c757e879099a2abb4bec0cfd6e8
                      e7f2323b444d565f68717a838c959ea7b0b9c2cfd4dde1f5f2374049515b636d767f889199a3
                      abb5bec4d3d4e1edf4fe3c454f576169727b848d979fa9b1bac3cfd5dde8f3fb00414a535c65
                      6e778089929ba4adb6bfc8ced6e7eef2fc0d464f58616a737c858e97a0a9b2bbc4cdd6e7e0f1
                      fe030b4b545e667078818a939ca6aeb8c0c9d2dce0eef7fe070f5059626b747d868f98a1aab3
                      bcc5ced7e0e9f2fb050c17
                      """

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
    . assert(_.stdlib.forall(_ == 8))

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

    // The pure codecs compile on every platform but are only *selected* off the JVM; here they
    // are exercised directly, and differentially against the `javax.imageio` backend.

    def same(left: Raster, right: Raster): Boolean =
      left.width == right.width && left.height == right.height &&
        (0 until left.width*left.height).forall: index =>
          val leftWord = left.word(index)
          val rightWord = right.word(index)

          left.descriptor.chroma(leftWord) == right.descriptor.chroma(rightWord) &&
            left.descriptor.alpha(leftWord) == right.descriptor.alpha(rightWord)

    def gradient: Raster = Raster(16, 16)((x, y) => Chroma(x*16, y*16, (x + y)*8))

    def translucent: Raster =
      Raster[Rgba](8, 8): (x, y) =>
        Pixel.make[Rgba](x.toLong*30 << 24 | y.toLong*30 << 16 | 128L << 8 | (x*9 + y*9))

    // Mean and maximum absolute per-channel difference between two same-sized rasters, for
    // comparing the pure (lossy) JPEG decoder against `javax.imageio` — they agree closely but not
    // bit-for-bit, since each uses a different inverse-DCT implementation.
    def jpegClose(left: Raster, right: Raster, meanTolerance: Double, maxTolerance: Int): Boolean =
      left.width == right.width && left.height == right.height && {
        var sum = 0L
        var maximum = 0
        var count = 0

        for index <- 0 until left.width*left.height do
          val a = left.descriptor.chroma(left.word(index))
          val b = right.descriptor.chroma(right.word(index))

          for (p, q) <- List((a.red, b.red), (a.green, b.green), (a.blue, b.blue)) do
            val d = math.abs(p - q)
            sum += d
            maximum = maximum.max(d)
            count += 1

        sum.toDouble/count <= meanTolerance && maximum <= maxTolerance
      }

    test(m"the pure JPEG decoder reads a baseline image's dimensions"):
      val raster = JpegCodec.decode(jpg420)
      (raster.width, raster.height)
    . assert(_ == (23, 17))

    test(m"the pure and ImageIO decoders agree on a baseline 4:2:0 JPEG"):
      jpegClose(JpegCodec.decode(jpg420), jpg420.read[Raster in Jpeg], 2.0, 16)
    . assert(_ == true)

    test(m"the pure and ImageIO decoders agree on a progressive JPEG"):
      jpegClose(JpegCodec.decode(jpgProg), jpgProg.read[Raster in Jpeg], 2.0, 16)
    . assert(_ == true)

    // ImageIO decodes grayscale JPEGs through a linear color space and gamma-shifts the samples in
    // `getRGB`, so it is not a fair reference; instead the decode is pinned to libjpeg's own output
    // (`grayRef`, one luma byte per pixel, row-major).
    test(m"the pure JPEG decoder matches libjpeg on a grayscale image"):
      val raster = JpegCodec.decode(jpgGray)
      var sum = 0L
      for index <- 0 until 23*17 do
        sum += math.abs(raster(index%23, index/23).red - (grayRef(index) & 0xff))

      sum.toDouble/(23*17)
    . assert(_ < 2.0)

    test(m"the pure JPEG decoder reads what ImageIO writes"):
      val encoded = gradient.to[Jpeg].read[Data]
      jpegClose(JpegCodec.decode(encoded), encoded.read[Raster in Jpeg], 2.5, 20)
    . assert(_ == true)

    test(m"a truncated JPEG fails cleanly"):
      capture[RasterError](JpegCodec.decode(jpg420.slice(0, 200))).reason
    . assert(_ == RasterError.Reason.Truncated)

    // A smooth gradient whose channels stay within 0..255 (no wrap-around discontinuity that
    // chroma subsampling would otherwise amplify).
    def gradient32: Raster = Raster(32, 24)((x, y) => Chroma(x*7, y*10, (x + y)*4))

    test(m"the pure JPEG encoder produces a valid JPEG signature"):
      val encoded = JpegEncoder.encode(gradient32, 90)
      (encoded(0) & 0xff, encoded(1) & 0xff)
    . assert(_ == (0xff, 0xd8))

    test(m"a high-quality (4:4:4) JPEG round-trips through the pure codec"):
      val encoded = JpegEncoder.encode(gradient32, 95)
      jpegClose(JpegCodec.decode(encoded), gradient32, 4.0, 40)
    . assert(_ == true)

    test(m"a 4:2:0 JPEG round-trips through the pure codec"):
      val encoded = JpegEncoder.encode(gradient32, 80)
      jpegClose(JpegCodec.decode(encoded), gradient32, 6.0, 64)
    . assert(_ == true)

    test(m"ImageIO reads what the pure JPEG encoder writes"):
      val encoded = JpegEncoder.encode(gradient32, 90)
      jpegClose(encoded.read[Raster in Jpeg], gradient32, 6.0, 50)
    . assert(_ == true)

    test(m"a pure-PNG round trip preserves every pixel"):
      same(PngCodec.decode(PngCodec.encode(gradient)), gradient)
    . assert(_ == true)

    test(m"a pure-PNG round trip preserves alpha"):
      same(PngCodec.decode(PngCodec.encode(translucent)), translucent)
    . assert(_ == true)

    test(m"ImageIO reads what the pure PNG encoder writes"):
      same(PngCodec.encode(gradient).read[Raster in Png], gradient)
    . assert(_ == true)

    test(m"the pure PNG decoder reads what ImageIO writes"):
      same(PngCodec.decode(gradient.to[Png].read[Data]), gradient)
    . assert(_ == true)

    test(m"the pure and ImageIO PNG decoders agree on a fixture"):
      same(PngCodec.decode(png), png.read[Raster in Png])
    . assert(_ == true)

    test(m"a corrupted PNG fails its checksum"):
      val corrupted = IArray.tabulate(png.length): index =>
        if index == 40 then (png(index) ^ 1).toByte else png(index)

      capture[RasterError](PngCodec.decode(corrupted)).reason
    . assert(_ == RasterError.Reason.BadCrc)

    test(m"a pure-BMP round trip preserves every pixel"):
      same(BmpCodec.decode(BmpCodec.encode(gradient)), gradient)
    . assert(_ == true)

    test(m"ImageIO reads what the pure BMP encoder writes"):
      same(BmpCodec.encode(gradient).read[Raster in Bmp], gradient)
    . assert(_ == true)

    test(m"the pure BMP decoder reads what ImageIO writes"):
      same(BmpCodec.decode(gradient.to[Bmp].read[Data]), gradient)
    . assert(_ == true)

    test(m"a pure-GIF round trip preserves 256 or fewer colours"):
      same(GifCodec.decode(GifCodec.encode(gradient)), gradient)
    . assert(_ == true)

    test(m"ImageIO reads what the pure GIF encoder writes"):
      same(GifCodec.encode(gradient).read[Raster in Gif], gradient)
    . assert(_ == true)

    test(m"the pure GIF decoder reads what ImageIO writes"):
      same(GifCodec.decode(gradient.to[Gif].read[Data]), gradient)
    . assert(_ == true)

    val grey4 = hex"""89504e470d0a1a0a0000000d49484452000000040000000204000000009f33cfbe0000000e494441
                      54789c63605dcff02b0000051f01ffda29ceba0000000049454e44ae426082"""

    test(m"a four-bit greyscale PNG decodes with scaled samples"):
      val raster = PngCodec.decode(grey4)
      (0 until 4).map(raster(_, 0).red).to(List)
    . assert(_ == List(0, 85, 170, 255))

    val pal2 = hex"""89504e470d0a1a0a0000000d494844520000000400000002020300000002c695f00000000c504c54
                     45ff000000ff000000fffffffffb0060f60000000374524e53ff80007f6d68780000000c49444154
                     789c639066780200013901004564919b0000000049454e44ae426082"""

    test(m"a two-bit palette PNG decodes with tRNS alpha"):
      val raster = PngCodec.decode(pal2)

      (raster(0, 0).red, raster(2, 0).blue, raster.descriptor.alpha(raster.word(1)),
       raster.descriptor.alpha(raster.word(2)))
    . assert(_ == (255, 255, 128.0/255, 0.0))

    val rgb16 = hex"""89504e470d0a1a0a0000000d4948445200000002000000021002000000ad44463000000023494441
                      54789c63103209ab98b5e7ff7f06860606064626661656b67f7776cd280b0100715908f45d82bec6
                      0000000049454e44ae426082"""

    test(m"a sixteen-bit PNG reduces to its high bytes"):
      val raster = PngCodec.decode(rgb16)
      (raster(0, 0), raster(1, 0), raster(0, 1), raster(1, 1))
    . assert(_ == (Chroma(0x12, 0x56, 0x9a), Chroma(0xff, 0x00, 0x80), Chroma(0x01, 0x03, 0x05),
                   Chroma(0xfe, 0xba, 0x76)))

    val adam7 = hex"""89504e470d0a1a0a0000000d49484452000000050000000308060000012c31f56e00000045494441
                      54789c05c1a101c0201004c1d5d1e8e8d7d168f4e9e814f11d7c1b67a8844aa884cc001c8c0a110b
                      dc8edc97fd1541db499f8463a73541d709dd5b7a566a4cebad1fdca017bc004594d5000000004945
                      4e44ae426082"""

    test(m"an Adam7-interlaced PNG deinterlaces correctly"):
      val raster = PngCodec.decode(adam7)

      (0 until 3).forall: y =>
        (0 until 5).forall: x =>
          raster(x, y) == Chroma(x*40, y*80, x*20 + y*10) &&
            raster.descriptor.alpha(raster.word(y*5 + x)) == (255 - x*30)/255.0
    . assert(_ == true)

    val bmp4 = hex"""424d4e0000000000000046000000280000000400000002000000010004000000000008000000130b
                     0000130b000004000000000000000000ff0000ff0000ff000000ffffff003210000001230000"""

    test(m"a four-bit palette BMP decodes bottom-up"):
      val raster = BmpCodec.decode(bmp4)
      (raster(0, 0), raster(3, 0), raster(0, 1), raster(3, 1))
    . assert(_ == (Chroma(255, 0, 0), Chroma(255, 255, 255), Chroma(255, 255, 255),
                   Chroma(255, 0, 0)))

    val bmp16 = hex"""424d5200000000000000420000002800000002000000feffffff010010000300000010000000130b
                      0000130b0000000000000000000000f80000e00700001f00000000f8e0071f00ffff"""

    test(m"a sixteen-bit bitfields BMP decodes top-down"):
      val raster = BmpCodec.decode(bmp16)
      (raster(0, 0), raster(1, 0), raster(0, 1), raster(1, 1))
    . assert(_ == (Chroma(255, 0, 0), Chroma(0, 255, 0), Chroma(0, 0, 255),
                   Chroma(255, 255, 255)))

    val gifInterlaced = hex"""47494638396104000400f10000ff000000ff000000ff00000021f90401000003
                              002c00000000040004004002065c047807d158003b"""

    test(m"an interlaced GIF with transparency decodes correctly"):
      val raster = GifCodec.decode(gifInterlaced)

      def color(x: Int, y: Int): Chroma = (x + y)%3 match
        case 0 => Chroma(255, 0, 0)
        case 1 => Chroma(0, 255, 0)
        case _ => Chroma(0, 0, 255)

      val opaque =
        (0 until 4).forall: y =>
          (0 until 4).forall: x =>
            (x == 0 && y == 0) || raster(x, y) == color(x, y)

      (opaque, raster.descriptor.alpha(raster.word(0)))
    . assert(_ == (true, 0.0))

    // WebP has no `javax.imageio` support, so the pure lossless decoder is exercised against the
    // same images encoded as PNG (`cwebp -lossless -exact` from libwebp produced the fixtures).
    // Each pairing checks a different part of the VP8L pipeline: predictors, alpha, the colour
    // indexing transform, backward references with the colour cache, and plain literals.

    val webpGradient = hex"""524946462e000000574542505650384c220000002f0fc00200b93244f43f7651ffe8
                             7f8048dba622eedfeed8f1404c404c005c07eb3f"""

    val pngGradient = hex"""89504e470d0a1a0a0000000d49484452000000100000000c0802000000e485aad6
                            0000004849444154789c95cb3b1280200c05c017780a6a63e1fdcf6ac50c9f10c2
                            ccb62b00de1dc40740fc5842706215a207dbc0250ee1b0510ba781939066
                            8c905576b846cb70773ce1a9fd90fd04fcf5b3fcea0000000049454e44ae426082"""

    val webpAlpha = hex"""524946462e000000574542505650384c210000002f09400210b98ce87fec220adeff
                          80908070c3ffad9a01933180cb300188b61d0300"""

    val webpFew = hex"""5249464636000000574542505650384c2a0000002f13c001001f201048da1f7a8df9
                        171014f93fdafc075f24802010c464ce0de69c258988fee75cc55c05"""

    val pngFew = hex"""89504e470d0a1a0a0000000d494844520000001400000008080200000076ff48ba
                       0000002849444154789c63f8cfc0c000c6ffff3390ca66205be77f06060a74fe0731
                       c9d5c94081cea1eb67001642c7390ee4f2a30000000049454e44ae426082"""

    val webpTiles = hex"""524946465e000000574542505650384c520000002f17c005101f201020546a97214720
                          401c69d2a74e82403609e2fe0b552040f8332e4644bcc2963850130048c30840245ae0
                          88600aa3785ac8289edcce0011fdff814399bcd490be94365baa665fea9bbf03"""

    val pngTiles = hex"""89504e470d0a1a0a0000000d4948445200000018000000180806000000e0773df8
                         0000006d49444154789c633c21a7f19f010ac256316c85b1b9c21678c3d8272ef99c
                         84b16fe8899893a29e8981c68089d61630ca9dd0d842ed60b981a47e18049186dc89
                         ffd40e963024f5c320883e7c103941ed60e142523f0c82e8c4685944008c964504c1
                         685944108c96450c840000257473599904196f0000000049454e44ae426082"""

    val webpNoise = hex"""5249464660020000574542505650384c540200002f0bc002007fe44800c0b249d246b3
                          6d9be7eddbbaf1053dceb6ed6f380e00c06c52dbb66d6ed0493b4afab36d9b17cbad6d
                          dba6335e9efd5e6c3ba5f1312ef397a9d3dab671ef0de73f8e226d1c413e9c9d5e0a10
                          cddd472d712194b3c040ae23a9e4049d6503fb301e2541a0741e5c147f214c6b62ccfc
                          0981933bff08db9c3819f3d3a834aa23b55715e1292eb80e98296ee89e409f6c66062f
                          70b2ebff55650439d4a6d2bfd4853870c4c8d13a8960fd055d04195a113b9f8b49db4e
                          ba8a21f891dd945aa6117027107142006bdac6c628eaff9e629afa7515fe9cfdb66f48
                          d880f1bb9e86456a02ce4f99c881f7f103272dbeb405011e1ac0d9f0544b6a4364a69e
                          13dc0eb27bf6719fff81057fe2d4db5b17e7a19ef475e8fb16fc850aeded44001f5d68
                          4e67bec3106259bed9ed66763d02583f66bf7aeb6fe67a1262cf2e2a9d7f8b76ff919c
                          fb3095577028434ed0a721f4182838fa5936157448b1ee3ac0c7d7d2c8fad4f49b5278
                          f430043d8d56d801d55000ddfe06e2bfde6ae6ea6e2a0962ce02d217a6459921d6b593
                          7f7044b8a31000e2538b631ac063167c3c50cf5e2b9f9037e415b03775fc2785231ab8
                          ca5c956d966e934a8ae4ac01c400000826b36ddbb66ddbb6916dbbf723fa1f972f8f68
                          36b4769b58b935ff4a93b67a77d096de0cc1a12c9395971b9ba702c7726f7d27f403be
                          422a8a50e32bb1badef2e9d9bae8b02306c8cb758ef85725e0f3c9c7349ccca4b13fe5
                          27d2e17791a68853e0172d3260267c383fdb613a92f51b4f753fb6f97373f1c2706c6e
                          fc1283d7b208a447ddd85f39dd81088744e9befe26"""

    val pngNoise = hex"""89504e470d0a1a0a0000000d494844520000000c0000000c0802000000d917cbb0
                         000001c749444154789c01bc0143fe00e7eee7615ef35f30e49b482e15cae75007201
                         e12617b0feda7e1647796ff022bea8ed02a049bb38e32b1aea4be1435f8f4f3368
                         61b1ad070b1e6659a9d314b3b33dbd23a3dada535f4026c58e5cdd63e0c098da705
                         b564001d30f07b9ffe5df0e5e3365865fe90f4e9e2b03777e901bcbee885395d846
                         609da1731c2dde565a6c411210d60fb4d43b5015d33138228aa1bd2e5008f6c6808
                         2389d2e47f1e175a90bc432fb946e6a9471109f3b79f110a26f6229fa345252600e
                         7bc1642aeb42bf227d50fff07c3c20624292e3b83d5a9c6eae1ec2a0f9e2cf60b75
                         39fe01f88205c41844abbc667ba9ccc5d005be0de608e80a35a7d5a2936ed022da4
                         219599b538702db41f7e54d5bbfbadf33903af73ac56a0cfb9fc409a6adf6a03909
                         1345d99d4a751465a5015b738b56de3ec1db0954161422221dd84beae7b94598054
                         a88bf22dd5a6a0ea40b2f85c4018cf90ee327cda238d03698a19a0ce090e468e3a3
                         0b6fe7fc8521631e473c9cefd4140c3000fb2f5521ead3ce897ef2fc41addef3a23
                         762d60f85420b12634f740691a4b57dff35ff3e018065df8b5bf446a89c1ddd0843
                         172af007cec2ced51712e90a83045a110f88f7c9cb98ae8e6ad46b42fe99840000
                         000049454e44ae426082"""

    def webpMatchesPng(webp: Data, png: Data): Boolean =
      same(WebpCodec.decode(webp), png.read[Raster in Png])

    test(m"a lossless WebP with predictor transforms decodes correctly"):
      webpMatchesPng(webpGradient, pngGradient)
    . assert(_ == true)

    test(m"a lossless WebP with an alpha channel decodes correctly"):
      val raster = WebpCodec.decode(webpAlpha)

      def expected(x: Int, y: Int): (Chroma, Double) =
        (Chroma(x*25, y*25, 128), ((x + y)*12 % 256)/255.0)

      def actual(x: Int, y: Int): (Chroma, Double) =
        (raster(x, y), raster.descriptor.alpha(raster.word(y*raster.width + x)))

      ( (raster.width, raster.height),
        actual(0, 0) == expected(0, 0),
        actual(9, 9) == expected(9, 9),
        actual(3, 5) == expected(3, 5) )
    . assert(_ == ((10, 10), true, true, true))

    test(m"a lossless WebP using the colour indexing transform decodes correctly"):
      webpMatchesPng(webpFew, pngFew)
    . assert(_ == true)

    test(m"a lossless WebP with backward references decodes correctly"):
      webpMatchesPng(webpTiles, pngTiles)
    . assert(_ == true)

    test(m"a lossless WebP of noise decodes correctly"):
      webpMatchesPng(webpNoise, pngNoise)
    . assert(_ == true)

    test(m"a WebP raster is read through the public decoding API"):
      webpGradient.read[Raster in Webp].pipe: raster =>
        (raster.width, raster.height)
    . assert(_ == (16, 12))

    // WebP lossy (VP8) decode. The reference images were produced by libwebp's `dwebp -nofancy`,
    // which uses simple chroma upsampling to match this decoder; the comparison is byte-exact.

    val ls1Webp = hex"""
                        524946464a00000057454250565038203e000000f001009d012a1000100002003425b00274010f0c12f2ca8000fe
                        fd6f1460c7655b6ab5f0b984b5667097ee72ec0207e62ffff917ba9fff3c7e97e2e40000"""
    val ls1Ref = hex"""
                        89504e470d0a1a0a0000000d494844520000001000000010080200000090916836000001b24944415428913d90cd
                        6e144110c6a6fb7355ffcc2e482104097144bcffa341a29064a6ab9643809b6f965dbadf23ee03c4d797fa9f7fc4
                        1dd2e7d855f53d1f247d8eab0ab4d6102d408ccd0cf630497b4cc13ca7c4cc8998e70ee2f85442e5881a2adbada5
                        9461455ae99b9447afb0b26f224f4f84ff3598a459a7602e80bf06db91f6dc85f6734af0eba1406959c7e8cf3793
                        745926e9770af47c56de597a3e252abd7777bf96e1ee2307622c043387c4644a9a39240d1b121c9f9c398ef09bd9
                        9116f0ba4ce8253ae8ed30d05b0af1760ac4f57a75f7b1a892a523ac027838926f2ee4e1425e1ce0e9dbc5ccfaa2
                        b5f6eb7508b5b3493ce647842f077ee604f97249f4decd6c6673f7a961d8be1ad2252e4297d50497dc117b3882e3
                        ae95d65ec302dbb60f8962b54dca9805e56a55e4ad27e42281b9ef063d4cb50e86a4b19ad088f7630e8c1cc00c93
                        c4e3971bb6f52873f4c7d8a08c7343e52925d51692ead34d521dabfe6b70bf96d17b9fc7c4d8cf26346302733930
                        7302f33410c7bd69f4e3e69bf376284d2f4b88234a508e558272640dcaeb2aa270bd5e5a6b7d19e09b9be1d5912c
                        1cf0ea127e33815713fa0359d4a4b40a26462e0000000049454e44ae426082"""
    val ls2Webp = hex"""
                        524946464e0000005745425056503820420000003003009d012a180010003e51208c4423a221180400380504a009
                        d328478183f3110000fee64f1edf049ee9c31cc4511367e1c3d7ad04950f36dbbbc6427968000000"""
    val ls2Ref = hex"""
                        89504e470d0a1a0a0000000d4948445200000018000000100802000000834628c2000000be49444154388dadd241
                        7284300c05d12703434e99336493e3c7cac20634c9d65eb87e0955a9dd283ebfbe7164e0ec0d7b838c7ee7d70fec
                        a327022de16c1b22a15974f6e640c4c0f0dcfa9d43949e2c9ddb9dd711c917647b2697cf33cff90d2d4abd3f1ceb
                        88323e90c21f45591d0d96e0dd5131bb8e281cf7b4acef8f8e8861edb93312a19bd244f6a54469472f8e3ad814ba
                        710670ed9a52d71285d3fb068d3cc140cb6a2a5d5eaeca5aa26bb3c78e984cc4f83b558b7f8e4665ada35fd90943
                        dfbe51f5fa0000000049454e44ae426082"""

    def lossyMatches(webp: Data, ref: Data): Boolean = same(WebpCodec.decode(webp), ref.read[Raster in Png])

    test(m"a lossy WebP decodes byte-exactly against libwebp (dwebp -nofancy)"):
      lossyMatches(ls1Webp, ls1Ref)
    . assert(_ == true)

    test(m"a smooth-gradient lossy WebP decodes byte-exactly against libwebp"):
      lossyMatches(ls2Webp, ls2Ref)
    . assert(_ == true)

    test(m"a lossy WebP decodes to an opaque RGB raster of the right size"):
      val raster = WebpCodec.decode(ls1Webp)
      (raster.width, raster.height, raster.descriptor.hasAlpha)
    . assert(_ == (16, 16, false))

    test(m"a lossy WebP is read through the public decoding API"):
      ls2Webp.read[Raster in Webp].pipe: raster =>
        (raster.width, raster.height)
    . assert(_ == (24, 16))


    // Extended (VP8X) WebP: lossy-with-alpha and animation (first frame).

    val vp8xLossy = hex"""
                        524946469c00000057454250565038580a000000100000001300000f0000414c50481200000011b98ce87f404890
                        10fdbfdc18310169ee0156503820640000009002009d012a1400100001402625b00274ca11c00167463ba69201bf
                        6800fefe1dccfc63ac5a0385fe4eae42e136eebf89893de9bfc12aa892aeb3608c1610461bdd4f26bdd4ff9a7b55
                        b7d6137d665a08b2c239b39f3eb2967a019e11f4af870f000000"""
    val vp8xLossyRef = hex"""
                        89504e470d0a1a0a0000000d494844520000001400000010080600000016185f1b0000028249444154388d5d92c1
                        6e1c451445abdf7d3575c6310c4496b3c52c5820f1a57ca2178994150b0832b2c73355af1e8b6a278255a9d4addb
                        a7efb9dbafbffd5e1e1f1ff9be775a6bbc3f9f31334eb353ddf9b954cc8cf799988c0fbd23777e9cebbc8f2beece
                        5d0899e11f3f7ec24cdc7e07b556da9c78addce6c06ba5be0672a7cec04c7829b81ccf824b544b24a76e864978dc
                        9d69ad717ef98773299c8e46a91ba504a304d7cb22e91198c4e841ca19d949893e3b29a7872109bf7d774b3d54bc
                        07a7d38977970ba514acbf524ae17873839971cc44661c3d7039c77424e7665e913b3721cc0c7fbafb824cfc700a
                        fe56e770bd62320e19988c6b1866c665eff0d20743e2350732e3252e48ce4b0893e1ad35dc1df68edab66166b4b2
                        02db1ed8f6c06642122d97841605c9696f81797f256c306761d324afab9b99834d2266a1989199a419d983f424e7
                        7a2fc758f798a4190e20339865916c1bf64620d166592499ebb92deb6d2e094d05b916a119fed7870b45c69813af
                        4eed036962259136febc4cdcc5968199517b471eccecc8c4a15f703987b035abda1aee8239d797de3a2ab99f2bb0
                        ed81cd6c11a62113cdd62edb5b60dc778a263302ab95ec7d75928374272f41ba1339491331064549ce41da24fb60
                        7a9243a4267e3c829920f66ebe12689d2570775aceb502edddcdddb26debcfb45b7ebefb82491c66e05e791e7dcd
                        a04c24e7f9b202c9819971ee03b9789e03c9e825e9a5d0b7b6a4b4d6d0be43c977826f81df08b577a845348564f8
                        0c6aade85a389fcf78dc0f8a261181d524fb20956409d283bc4ca682286b87d13be56d87267206e778e5d3e73ff8
                        e9e101e7b876788cff5b8e752ff3abf5ff589ecbb2aeaf3c3dbdf0cbc30337c723ff023d1e5058068b0d9f000000
                        0049454e44ae426082"""
    val anim = hex"""
                        524946468400000057454250565038580a000000020000000f00000f0000414e494d06000000ffffffff0000414e
                        4d46280000000000000000000f00000f0000640000025650384c0f0000002f0fc003000710fd8ffe0722a2ff0100
                        414e4d46280000000000000000000f00000f0000640000005650384c0f0000002f0fc0030007d0ff88fe0722a2ff
                        0100"""

    test(m"a lossy WebP with an alpha channel (VP8X + ALPH) decodes byte-exactly"):
      same(WebpCodec.decode(vp8xLossy), vp8xLossyRef.read[Raster in Png])
    . assert(_ == true)

    test(m"a lossy alpha WebP decodes to an RGBA raster"):
      WebpCodec.decode(vp8xLossy).descriptor.hasAlpha
    . assert(_ == true)

    test(m"an animated WebP decodes its first frame"):
      val raster = WebpCodec.decode(anim)
      (raster.width, raster.height, raster(5, 5))
    . assert(_ == (16, 16, Chroma(255, 0, 0)))

    // WebP lossy (VP8) encoding. The output is valid VP8: verified out of band to decode in
    // libwebp's `dwebp`, and here round-tripped through the decoder to confirm good fidelity
    // (the decoder is itself byte-exact against `dwebp -nofancy`).

    def meanChannelDiff(left: Raster, right: Raster): Double =
      var total = 0L

      for i <- 0 until left.width*left.height do
        val a = left(i % left.width, i / left.width)
        val b = right(i % right.width, i / right.width)
        total += math.abs(a.red - b.red) + math.abs(a.green - b.green) + math.abs(a.blue - b.blue)

      total.toDouble/(3*left.width*left.height)

    def lossySource: Raster =
      Raster(48, 32)((x, y) => Chroma((x*5) % 256, (y*7) % 256, ((x + y)*3) % 256))

    test(m"a lossy-encoded WebP round-trips through the decoder with good fidelity"):
      val decoded = WebpCodec.decode(WebpCodec.encodeLossy(lossySource, 90))
      (decoded.width, decoded.height, meanChannelDiff(decoded, lossySource) < 12)
    . assert(_ == (48, 32, true))

    test(m"lossy encoding produces a valid RIFF/WEBP/VP8 container"):
      val encoded = WebpCodec.encodeLossy(lossySource, 80)

      ( String(IArray.genericWrapArray(encoded.slice(0, 4)).toArray, "UTF-8").tt,
        String(IArray.genericWrapArray(encoded.slice(8, 12)).toArray, "UTF-8").tt,
        String(IArray.genericWrapArray(encoded.slice(12, 16)).toArray, "UTF-8").tt )
    . assert(_ == (t"RIFF", t"WEBP", t"VP8 "))

    test(m"a smaller image also lossy-encodes and round-trips"):
      val src = Raster(16, 16)((x, y) => Chroma(x*16, y*16, 128))
      val decoded = WebpCodec.decode(WebpCodec.encodeLossy(src, 85))
      (decoded.width, decoded.height, meanChannelDiff(decoded, src) < 15)
    . assert(_ == (16, 16, true))

    test(m"a non-WebP byte stream is rejected"):
      capture[RasterError](WebpCodec.decode(pngGradient)).reason
    . assert(_ == RasterError.Reason.BadSignature)

    // The lossless encoder round-trips through the decoder for every layout and image structure.

    def roundtrips(raster: Raster): Boolean =
      same(WebpCodec.decode(WebpCodec.encode(raster)), raster)

    test(m"an RGB raster round-trips through the WebP lossless encoder"):
      roundtrips(gradient)
    . assert(_ == true)

    test(m"an RGBA raster round-trips through the WebP lossless encoder"):
      roundtrips(translucent)
    . assert(_ == true)

    test(m"a raster with long runs round-trips (run-length coding)"):
      roundtrips(Raster(40, 30)((_, _) => Chroma(90, 40, 200)))
    . assert(_ == true)

    test(m"a large raster round-trips (multiple predictor blocks)"):
      roundtrips(Raster(600, 40)((x, y) => Chroma(x % 256, y*6, (x + y) % 256)))
    . assert(_ == true)

    test(m"encoded WebP is a valid RIFF/WEBP/VP8L container"):
      val encoded = WebpCodec.encode(gradient)

      ( String(IArray.genericWrapArray(encoded.slice(0, 4)).toArray, "UTF-8").tt,
        String(IArray.genericWrapArray(encoded.slice(8, 12)).toArray, "UTF-8").tt,
        String(IArray.genericWrapArray(encoded.slice(12, 16)).toArray, "UTF-8").tt )
    . assert(_ == (t"RIFF", t"WEBP", t"VP8L"))
