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

// The 8x8 inverse DCT with dequantization, ported from image-rs/jpeg-decoder (`src/idct.rs`,
// MIT/Apache-2.0), which is in turn based on stb_image's `stbi__idct_block`. Only the
// full-resolution transform is ported (Hallucination does not do thumbnail downscaling).
// Arithmetic is 32-bit and wrapping — malicious inputs can overflow, which is harmless here since
// Scala's `Int` wraps silently, matching the reference's `Wrapping<i32>`.
private[hallucination] object JpegIdct:
  // Fixed-point constants scaled by 4096 (`stbi_f2f`), truncated toward zero as the reference does.
  private def f2f(x: Double): Int = (x*4096.0 + 0.5).toInt

  private val c0_541 = f2f(0.541196100)
  private val c1_847 = f2f(-1.847759065)
  private val c0_765 = f2f(0.765366865)
  private val c1_175 = f2f(1.175875602)
  private val c0_298 = f2f(0.298631336)
  private val c2_053 = f2f(2.053119869)
  private val c3_072 = f2f(3.072711026)
  private val c1_501 = f2f(1.501321110)
  private val cn0_899 = f2f(-0.899976223)
  private val cn2_562 = f2f(-2.562915447)
  private val cn1_961 = f2f(-1.961570560)
  private val cn0_390 = f2f(-0.390180644)

  private def clamp(value: Int): Byte = value.max(0).min(255).toByte

  // Dequantizes the 64 zig-unzagged coefficients at `coeffBase` with `quant`, applies the inverse
  // DCT, and writes the 8x8 block of samples into `output` at `outputBase`, one row every `stride`
  // bytes.
  def dequantizeAndIdct8x8
    ( coefficients: Array[Int],
      coeffBase:    Int,
      quant:        Array[Int],
      output:       Array[Byte],
      outputBase:   Int,
      stride:       Int )
  :   Unit =

    val temp = new Array[Int](64)

    // Pass 1: the columns.
    var i = 0

    while i < 8 do
      if coefficients(coeffBase + i + 8) == 0 && coefficients(coeffBase + i + 16) == 0 &&
        coefficients(coeffBase + i + 24) == 0 && coefficients(coeffBase + i + 32) == 0 &&
        coefficients(coeffBase + i + 40) == 0 && coefficients(coeffBase + i + 48) == 0 &&
        coefficients(coeffBase + i + 56) == 0
      then
        val dcterm = (coefficients(coeffBase + i)*quant(i)) << 2
        temp(i) = dcterm; temp(i + 8) = dcterm; temp(i + 16) = dcterm; temp(i + 24) = dcterm
        temp(i + 32) = dcterm; temp(i + 40) = dcterm; temp(i + 48) = dcterm; temp(i + 56) = dcterm
      else
        val s0 = coefficients(coeffBase + i)*quant(i)
        val s1 = coefficients(coeffBase + i + 8)*quant(i + 8)
        val s2 = coefficients(coeffBase + i + 16)*quant(i + 16)
        val s3 = coefficients(coeffBase + i + 24)*quant(i + 24)
        val s4 = coefficients(coeffBase + i + 32)*quant(i + 32)
        val s5 = coefficients(coeffBase + i + 40)*quant(i + 40)
        val s6 = coefficients(coeffBase + i + 48)*quant(i + 48)
        val s7 = coefficients(coeffBase + i + 56)*quant(i + 56)

        // The constants scale up by 1<<12; 512 keeps two extra bits of precision for pass 2.
        val p1e = (s2 + s6)*c0_541
        val t2 = p1e + s6*c1_847
        val t3 = p1e + s2*c0_765
        val t0 = (s0 + s4) << 12
        val t1 = (s0 - s4) << 12
        val x0 = t0 + t3 + 512
        val x3 = t0 - t3 + 512
        val x1 = t1 + t2 + 512
        val x2 = t1 - t2 + 512

        var o0 = s7; var o1 = s5; var o2 = s3; var o3 = s1
        val p3 = o0 + o2
        val p4 = o1 + o3
        val p1 = o0 + o3
        val p2 = o1 + o2
        val p5 = (p3 + p4)*c1_175
        o0 *= c0_298; o1 *= c2_053; o2 *= c3_072; o3 *= c1_501
        val q1 = p5 + p1*cn0_899
        val q2 = p5 + p2*cn2_562
        val q3 = p3*cn1_961
        val q4 = p4*cn0_390
        o3 += q1 + q4
        o2 += q2 + q3
        o1 += q2 + q4
        o0 += q1 + q3

        temp(i)      = (x0 + o3) >> 10
        temp(i + 56) = (x0 - o3) >> 10
        temp(i + 8)  = (x1 + o2) >> 10
        temp(i + 48) = (x1 - o2) >> 10
        temp(i + 16) = (x2 + o1) >> 10
        temp(i + 40) = (x2 - o1) >> 10
        temp(i + 24) = (x3 + o0) >> 10
        temp(i + 32) = (x3 - o0) >> 10

      i += 1

    // Pass 2: the rows. 1<<12 from the constants, 1<<2 from pass 1, and 1<<3 from the two sqrt(8)
    // scalings give 1<<17 to remove; the offset both rounds and re-biases -128..127 to 0..255.
    val xScale = 65536 + (128 << 17)
    var row = 0

    while row < 8 do
      val base = row*8
      val out = outputBase + row*stride
      val s0 = temp(base)

      if temp(base + 1) == 0 && temp(base + 2) == 0 && temp(base + 3) == 0 &&
        temp(base + 4) == 0 && temp(base + 5) == 0 && temp(base + 6) == 0 && temp(base + 7) == 0
      then
        val dcterm = clamp(((s0 << 12) + xScale) >> 17)
        var c = 0
        while c < 8 do { output(out + c) = dcterm; c += 1 }
      else
        val s1 = temp(base + 1); val s2 = temp(base + 2); val s3 = temp(base + 3)
        val s4 = temp(base + 4); val s5 = temp(base + 5); val s6 = temp(base + 6)
        val s7 = temp(base + 7)

        val p1e = (s2 + s6)*c0_541
        val t2 = p1e + s6*c1_847
        val t3 = p1e + s2*c0_765
        val t0 = (s0 + s4) << 12
        val t1 = (s0 - s4) << 12
        val x0 = t0 + t3 + xScale
        val x3 = t0 - t3 + xScale
        val x1 = t1 + t2 + xScale
        val x2 = t1 - t2 + xScale

        var o0 = s7; var o1 = s5; var o2 = s3; var o3 = s1
        val p3 = o0 + o2
        val p4 = o1 + o3
        val p1 = o0 + o3
        val p2 = o1 + o2
        val p5 = (p3 + p4)*c1_175
        o0 *= c0_298; o1 *= c2_053; o2 *= c3_072; o3 *= c1_501
        val q1 = p5 + p1*cn0_899
        val q2 = p5 + p2*cn2_562
        val q3 = p3*cn1_961
        val q4 = p4*cn0_390
        o3 += q1 + q4
        o2 += q2 + q3
        o1 += q2 + q4
        o0 += q1 + q3

        output(out)     = clamp((x0 + o3) >> 17)
        output(out + 7) = clamp((x0 - o3) >> 17)
        output(out + 1) = clamp((x1 + o2) >> 17)
        output(out + 6) = clamp((x1 - o2) >> 17)
        output(out + 2) = clamp((x2 + o1) >> 17)
        output(out + 5) = clamp((x2 - o1) >> 17)
        output(out + 3) = clamp((x3 + o0) >> 17)
        output(out + 4) = clamp((x3 - o0) >> 17)

      row += 1
