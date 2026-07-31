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

// A decoded VP8 keyframe: the luma and chroma planes (samples as `Int` in 0–255). The luma plane
// has stride `bufferWidth` (the macroblock-padded width); chroma planes have stride
// `bufferWidth/2`. `width`/`height` are the displayed dimensions.
private[hallucination] final class Vp8Frame
  ( val width: Int, val height: Int, val bufferWidth: Int, val luma: scala.Array[Int],
    val chromaU: scala.Array[Int], val chromaV: scala.Array[Int] )

// YUV→RGB conversion, ported from image-rs/image-webp (`src/lossy/yuv.rs`, MIT/Apache-2.0). This
// uses simple (nearest-neighbour) chroma upsampling — each chroma sample covers a 2×2 luma block —
// matching libwebp's `dwebp -nofancy`.
private[hallucination] object Vp8Yuv:
  private inline def mulhi(value: Int, coeff: Int): Int = (value*coeff) >> 8

  private inline def clip(value: Int): Int =
    val v = value >> 6

    if v < 0 then 0 else if v > 255 then 255 else v

  // Fills `rgb(index) = packed 0xRRGGBB` for the displayed image, one entry per pixel.
  def toRgb(frame: Vp8Frame): scala.Array[Int] =
    val width = frame.width
    val height = frame.height
    val chromaStride = frame.bufferWidth/2
    val rgb = new scala.Array[Int](width*height)
    var y = 0

    while y < height do
      var x = 0

      while x < width do
        val luma = frame.luma(y*frame.bufferWidth + x)
        val u = frame.chromaU((y/2)*chromaStride + x/2)
        val v = frame.chromaV((y/2)*chromaStride + x/2)
        val base = mulhi(luma, 19077)
        val red = clip(base + mulhi(v, 26149) - 14234)
        val green = clip(base - mulhi(u, 6419) - mulhi(v, 13320) + 8708)
        val blue = clip(base + mulhi(u, 33050) - 17685)
        rgb(y*width + x) = (red << 16) | (green << 8) | blue
        x += 1

      y += 1

    rgb
