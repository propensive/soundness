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

// The forward DCT (slow but accurate integer "islow" variant), ported from jpeg-encoder's
// `fdct.rs` (Apache-2.0/MIT), itself ported from mozjpeg/libjpeg-turbo (IJG). A 2-D DCT is done as
// 1-D DCTs on rows then columns. Results are left scaled up by an overall factor of 8, which the
// quantization step divides out. Operates in place on a 64-element block of level-shifted samples.
private[hallucination] object JpegFdct:
  private inline val ConstBits = 13
  private inline val Pass1Bits = 2

  private inline val Fix0_298631336 = 2446
  private inline val Fix0_390180644 = 3196
  private inline val Fix0_541196100 = 4433
  private inline val Fix0_765366865 = 6270
  private inline val Fix0_899976223 = 7373
  private inline val Fix1_175875602 = 9633
  private inline val Fix1_501321110 = 12299
  private inline val Fix1_847759065 = 15137
  private inline val Fix1_961570560 = 16069
  private inline val Fix2_053119869 = 16819
  private inline val Fix2_562915447 = 20995
  private inline val Fix3_072711026 = 25172

  private inline def descale(value: Int, bits: Int): Int = (value + (1 << (bits - 1))) >> bits

  def fdct(data: Array[Int]): Unit =
    val temp = new Array[Int](64)

    // Pass 1: rows, scaled up by 2**Pass1Bits.
    var y = 0

    while y < 8 do
      val offset = y*8
      val tmp0 = data(offset) + data(offset + 7)
      val tmp7 = data(offset) - data(offset + 7)
      val tmp1 = data(offset + 1) + data(offset + 6)
      val tmp6 = data(offset + 1) - data(offset + 6)
      val tmp2 = data(offset + 2) + data(offset + 5)
      val tmp5 = data(offset + 2) - data(offset + 5)
      val tmp3 = data(offset + 3) + data(offset + 4)
      val tmp4 = data(offset + 3) - data(offset + 4)

      val tmp10 = tmp0 + tmp3
      val tmp13 = tmp0 - tmp3
      val tmp11 = tmp1 + tmp2
      val tmp12 = tmp1 - tmp2

      temp(offset) = (tmp10 + tmp11) << Pass1Bits
      temp(offset + 4) = (tmp10 - tmp11) << Pass1Bits

      val z1e = (tmp12 + tmp13)*Fix0_541196100
      temp(offset + 2) = descale(z1e + tmp13*Fix0_765366865, ConstBits - Pass1Bits)
      temp(offset + 6) = descale(z1e + tmp12*(-Fix1_847759065), ConstBits - Pass1Bits)

      val a1 = tmp4 + tmp7
      val a2 = tmp5 + tmp6
      val a3 = tmp4 + tmp6
      val a4 = tmp5 + tmp7
      val a5 = (a3 + a4)*Fix1_175875602

      val o4 = tmp4*Fix0_298631336
      val o5 = tmp5*Fix2_053119869
      val o6 = tmp6*Fix3_072711026
      val o7 = tmp7*Fix1_501321110
      val b1 = a1*(-Fix0_899976223)
      val b2 = a2*(-Fix2_562915447)
      val b3 = a3*(-Fix1_961570560) + a5
      val b4 = a4*(-Fix0_390180644) + a5

      temp(offset + 7) = descale(o4 + b1 + b3, ConstBits - Pass1Bits)
      temp(offset + 5) = descale(o5 + b2 + b4, ConstBits - Pass1Bits)
      temp(offset + 3) = descale(o6 + b2 + b3, ConstBits - Pass1Bits)
      temp(offset + 1) = descale(o7 + b1 + b4, ConstBits - Pass1Bits)
      y += 1

    // Pass 2: columns, removing the Pass1Bits scaling and leaving an overall factor of 8.
    var x = 0

    while x < 8 do
      val tmp0 = temp(x) + temp(56 + x)
      val tmp7 = temp(x) - temp(56 + x)
      val tmp1 = temp(8 + x) + temp(48 + x)
      val tmp6 = temp(8 + x) - temp(48 + x)
      val tmp2 = temp(16 + x) + temp(40 + x)
      val tmp5 = temp(16 + x) - temp(40 + x)
      val tmp3 = temp(24 + x) + temp(32 + x)
      val tmp4 = temp(24 + x) - temp(32 + x)

      val tmp10 = tmp0 + tmp3
      val tmp13 = tmp0 - tmp3
      val tmp11 = tmp1 + tmp2
      val tmp12 = tmp1 - tmp2

      data(x) = descale(tmp10 + tmp11, Pass1Bits)
      data(32 + x) = descale(tmp10 - tmp11, Pass1Bits)

      val z1e = (tmp12 + tmp13)*Fix0_541196100
      data(16 + x) = descale(z1e + tmp13*Fix0_765366865, ConstBits + Pass1Bits)
      data(48 + x) = descale(z1e + tmp12*(-Fix1_847759065), ConstBits + Pass1Bits)

      val a1 = tmp4 + tmp7
      val a2 = tmp5 + tmp6
      val a3 = tmp4 + tmp6
      val a4 = tmp5 + tmp7
      val a5 = (a3 + a4)*Fix1_175875602

      val o4 = tmp4*Fix0_298631336
      val o5 = tmp5*Fix2_053119869
      val o6 = tmp6*Fix3_072711026
      val o7 = tmp7*Fix1_501321110
      val b1 = a1*(-Fix0_899976223)
      val b2 = a2*(-Fix2_562915447)
      val b3 = a3*(-Fix1_961570560) + a5
      val b4 = a4*(-Fix0_390180644) + a5

      data(56 + x) = descale(o4 + b1 + b3, ConstBits + Pass1Bits)
      data(40 + x) = descale(o5 + b2 + b4, ConstBits + Pass1Bits)
      data(24 + x) = descale(o6 + b2 + b3, ConstBits + Pass1Bits)
      data(8 + x) = descale(o7 + b1 + b4, ConstBits + Pass1Bits)
      x += 1
