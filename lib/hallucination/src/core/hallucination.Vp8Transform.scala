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

// The VP8 inverse transforms, ported from image-rs/image-webp (`src/lossy/transform.rs`,
// MIT/Apache-2.0), per RFC 6386 §14. Both operate in place on a 16-element (4×4) coefficient
// block; intermediate products are computed as `Long` to avoid overflow.
private[hallucination] object Vp8Transform:
  private val Const1: Long = 20091 // cos(pi/8)·sqrt(2) − 1, 16-bit fixed point
  private val Const2: Long = 35468 // sin(pi/8)·sqrt(2), 16-bit fixed point

  // Forward discrete cosine transform (encoder), inverse of `idct4x4`.
  def dct4x4(block: Array[Int]): Unit =
    var i = 0

    while i < 4 do
      val a = (block(i*4).toLong + block(i*4 + 3))*8
      val b = (block(i*4 + 1).toLong + block(i*4 + 2))*8
      val c = (block(i*4 + 1).toLong - block(i*4 + 2))*8
      val d = (block(i*4).toLong - block(i*4 + 3))*8

      block(i*4) = (a + b).toInt
      block(i*4 + 2) = (a - b).toInt
      block(i*4 + 1) = ((c*2217 + d*5352 + 14500) >> 12).toInt
      block(i*4 + 3) = ((d*2217 - c*5352 + 7500) >> 12).toInt
      i += 1

    i = 0

    while i < 4 do
      val a = block(i).toLong + block(i + 12)
      val b = block(i + 4).toLong + block(i + 8)
      val c = block(i + 4).toLong - block(i + 8)
      val d = block(i).toLong - block(i + 12)

      block(i) = ((a + b + 7) >> 4).toInt
      block(i + 8) = ((a - b + 7) >> 4).toInt
      block(i + 4) = (((c*2217 + d*5352 + 12000) >> 16) + (if d != 0 then 1 else 0)).toInt
      block(i + 12) = ((d*2217 - c*5352 + 51000) >> 16).toInt
      i += 1

  // Forward DCT on a 16-element subblock at `offset` within a larger array.
  def dct4x4Slice(block: Array[Int], offset: Int): Unit =
    val temp = new Array[Int](16)
    System.arraycopy(block, offset, temp, 0, 16)
    dct4x4(temp)
    System.arraycopy(temp, 0, block, offset, 16)

  // Forward Walsh-Hadamard transform (encoder) for the Y2 block, inverse of `iwht4x4`.
  def wht4x4(block: Array[Int]): Unit =
    var i = 0

    while i < 4 do
      val a = block(i*4).toLong + block(i*4 + 3)
      val b = block(i*4 + 1).toLong + block(i*4 + 2)
      val c = block(i*4 + 1).toLong - block(i*4 + 2)
      val d = block(i*4).toLong - block(i*4 + 3)

      block(i*4) = (a + b).toInt
      block(i*4 + 1) = (c + d).toInt
      block(i*4 + 2) = (a - b).toInt
      block(i*4 + 3) = (d - c).toInt
      i += 1

    i = 0

    while i < 4 do
      val a1 = block(i).toLong + block(i + 12)
      val b1 = block(i + 4).toLong + block(i + 8)
      val c1 = block(i + 4).toLong - block(i + 8)
      val d1 = block(i).toLong - block(i + 12)
      val a2 = a1 + b1
      val b2 = c1 + d1
      val c2 = a1 - b1
      val d2 = d1 - c1

      block(i) = ((a2 + (if a2 > 0 then 1 else 0))/2).toInt
      block(i + 4) = ((b2 + (if b2 > 0 then 1 else 0))/2).toInt
      block(i + 8) = ((c2 + (if c2 > 0 then 1 else 0))/2).toInt
      block(i + 12) = ((d2 + (if d2 > 0 then 1 else 0))/2).toInt
      i += 1

  // Inverse discrete cosine transform.
  def idct4x4(block: Array[Int], offset: Int = 0): Unit =
    inline def b(i: Int): Int = block(offset + i)
    inline def set(i: Int, v: Int): Unit = block(offset + i) = v
    var i = 0

    while i < 4 do
      val a1 = b(i).toLong + b(8 + i)
      val b1 = b(i).toLong - b(8 + i)
      val t1 = (b(4 + i).toLong*Const2) >> 16
      val t2 = b(12 + i).toLong + ((b(12 + i).toLong*Const1) >> 16)
      val c1 = t1 - t2
      val t3 = b(4 + i).toLong + ((b(4 + i).toLong*Const1) >> 16)
      val t4 = (b(12 + i).toLong*Const2) >> 16
      val d1 = t3 + t4

      set(i, (a1 + d1).toInt)
      set(4 + i, (b1 + c1).toInt)
      set(12 + i, (a1 - d1).toInt)
      set(8 + i, (b1 - c1).toInt)
      i += 1

    i = 0

    while i < 4 do
      val a1 = b(4*i).toLong + b(4*i + 2)
      val b1 = b(4*i).toLong - b(4*i + 2)
      val t1 = (b(4*i + 1).toLong*Const2) >> 16
      val t2 = b(4*i + 3).toLong + ((b(4*i + 3).toLong*Const1) >> 16)
      val c1 = t1 - t2
      val t3 = b(4*i + 1).toLong + ((b(4*i + 1).toLong*Const1) >> 16)
      val t4 = (b(4*i + 3).toLong*Const2) >> 16
      val d1 = t3 + t4

      set(4*i, ((a1 + d1 + 4) >> 3).toInt)
      set(4*i + 3, ((a1 - d1 + 4) >> 3).toInt)
      set(4*i + 1, ((b1 + c1 + 4) >> 3).toInt)
      set(4*i + 2, ((b1 - c1 + 4) >> 3).toInt)
      i += 1

  // Inverse Walsh-Hadamard transform, used for the Y2 (DC-of-DC) block.
  def iwht4x4(block: Array[Int], offset: Int = 0): Unit =
    inline def b(i: Int): Int = block(offset + i)
    inline def set(i: Int, v: Int): Unit = block(offset + i) = v
    var i = 0

    while i < 4 do
      val a1 = b(i) + b(12 + i)
      val b1 = b(4 + i) + b(8 + i)
      val c1 = b(4 + i) - b(8 + i)
      val d1 = b(i) - b(12 + i)

      set(i, a1 + b1)
      set(4 + i, c1 + d1)
      set(8 + i, a1 - b1)
      set(12 + i, d1 - c1)
      i += 1

    i = 0

    while i < 4 do
      val base = i*4
      val a1 = b(base) + b(base + 3)
      val b1 = b(base + 1) + b(base + 2)
      val c1 = b(base + 1) - b(base + 2)
      val d1 = b(base) - b(base + 3)

      set(base, (a1 + b1 + 3) >> 3)
      set(base + 1, (c1 + d1 + 3) >> 3)
      set(base + 2, (a1 - b1 + 3) >> 3)
      set(base + 3, (d1 - c1 + 3) >> 3)
      i += 1
