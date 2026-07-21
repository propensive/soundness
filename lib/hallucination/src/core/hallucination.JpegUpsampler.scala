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

import contingency.*

import RasterError.Reason

// Chroma upsampling and row interleaving, ported from image-rs/jpeg-decoder (`src/upsampler.rs`,
// MIT/Apache-2.0). Each component plane is upsampled to the full image width with the fancy
// (triangle) filter for the common 4:2:2 and 4:2:0 ratios, and nearest-neighbour otherwise; the
// upsampled component rows are then handed to a colour-conversion function to produce interleaved
// output.
private[hallucination] object JpegUpsampler:
  // Upsampler kinds; `Generic` additionally carries integer scaling factors.
  inline val H1V1 = 0
  inline val H2V1 = 1
  inline val H1V2 = 2
  inline val H2V2 = 3
  inline val Generic = 4

private[hallucination] final class JpegUpsampler
  ( components:   Array[JpegComponent],
    outputWidth:  Int,
    outputHeight: Int )
  ( using Tactic[RasterError] ):

  import JpegUpsampler.*

  private val count = components.length
  private val kinds = new Array[Int](count)
  private val widths = new Array[Int](count)
  private val heights = new Array[Int](count)
  private val rowStrides = new Array[Int](count)
  private val hScales = new Array[Int](count)
  private val vScales = new Array[Int](count)

  private var hMax = 0
  private var vMax = 0

  locally:
    var index = 0

    while index < count do
      hMax = hMax.max(components(index).horizontalSamplingFactor)
      vMax = vMax.max(components(index).verticalSamplingFactor)
      index += 1

  locally:
    var index = 0

    while index < count do
      val component = components(index)
      val h = component.horizontalSamplingFactor
      val v = component.verticalSamplingFactor
      val h1 = h == hMax || outputWidth == 1
      val v1 = v == vMax || outputHeight == 1
      val h2 = h*2 == hMax
      val v2 = v*2 == vMax

      kinds(index) =
        if h1 && v1 then H1V1
        else if h2 && v1 then H2V1
        else if h1 && v2 then H1V2
        else if h2 && v2 then H2V2
        else if hMax % h != 0 || vMax % v != 0 then
          abort(RasterError(Jpeg(), Reason.UnsupportedVariant))
        else
          Generic

      hScales(index) = if h == 0 then 1 else hMax/h
      vScales(index) = if v == 0 then 1 else vMax/v
      widths(index) = component.sizeWidth
      heights(index) = component.sizeHeight
      rowStrides(index) = component.blockWidth*component.dctScale
      index += 1

  private val lineBufferSize =
    var maximum = 0
    var index = 0
    while index < count do { maximum = maximum.max(widths(index)); index += 1 }
    maximum*hMax

  private val lineBuffers = Array.fill(count)(new Array[Byte](lineBufferSize))

  // Upsamples row `row` of every component to full width and interleaves via `colorConvert`.
  def upsampleAndInterleaveRow
    ( componentData: Array[Array[Byte]],
      row:           Int,
      output:        Array[Byte],
      colorConvert:  (Array[Array[Byte]], Array[Byte]) => Unit )
  :   Unit =

    var index = 0

    while index < count do
      upsampleRow
        ( kinds(index), componentData(index), widths(index), heights(index), rowStrides(index),
          hScales(index), vScales(index), row, lineBuffers(index) )

      index += 1

    colorConvert(lineBuffers, output)

  private def upsampleRow
    ( kind:      Int,
      input:     Array[Byte],
      width:     Int,
      height:    Int,
      rowStride: Int,
      hScale:    Int,
      vScale:    Int,
      row:       Int,
      output:    Array[Byte] )
  :   Unit =

    inline def sample(offset: Int): Int = input(offset) & 0xff

    kind match
      case H1V1 =>
        val base = row*rowStride
        var i = 0
        while i < outputWidth do { output(i) = input(base + i); i += 1 }

      case H2V1 =>
        val base = row*rowStride

        if width == 1 then
          output(0) = input(base); output(1) = input(base)
        else
          output(0) = input(base)
          output(1) = ((sample(base)*3 + sample(base + 1) + 2) >> 2).toByte
          var i = 1

          while i < width - 1 do
            val s = 3*sample(base + i) + 2
            output(i*2) = ((s + sample(base + i - 1)) >> 2).toByte
            output(i*2 + 1) = ((s + sample(base + i + 1)) >> 2).toByte
            i += 1

          output((width - 1)*2) =
            ((sample(base + width - 1)*3 + sample(base + width - 2) + 2) >> 2).toByte

          output((width - 1)*2 + 1) = input(base + width - 1)

      case H1V2 =>
        val rowNear = row/2.0
        val frac = rowNear - rowNear.toInt
        val rowFar = (rowNear + frac*3.0 - 0.25).min((height - 1).toDouble)
        val near = rowNear.toInt*rowStride
        val far = rowFar.toInt*rowStride
        var i = 0

        while i < outputWidth do
          output(i) = ((3*sample(near + i) + sample(far + i) + 2) >> 2).toByte
          i += 1

      case H2V2 =>
        val rowNear = row/2.0
        val frac = rowNear - rowNear.toInt
        val rowFar = (rowNear + frac*3.0 - 0.25).min((height - 1).toDouble)
        val near = rowNear.toInt*rowStride
        val far = rowFar.toInt*rowStride

        if width == 1 then
          val value = ((3*sample(near) + sample(far) + 2) >> 2).toByte
          output(0) = value; output(1) = value
        else
          var t1 = 3*sample(near) + sample(far)
          output(0) = ((t1 + 2) >> 2).toByte
          var i = 1

          while i < width do
            val t0 = t1
            t1 = 3*sample(near + i) + sample(far + i)
            output(i*2 - 1) = ((3*t0 + t1 + 8) >> 4).toByte
            output(i*2) = ((3*t1 + t0 + 8) >> 4).toByte
            i += 1

          output(width*2 - 1) = ((t1 + 2) >> 2).toByte

      case _ =>
        val start = (row/vScale)*rowStride
        var index = 0
        var i = 0

        while i < width do
          var repeat = 0

          while repeat < hScale do
            output(index) = input(start + i)
            index += 1
            repeat += 1

          i += 1
