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
