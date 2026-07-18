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

import Vp8Tables.*

// VP8 intra prediction, ported from image-rs/image-webp (`src/lossy/prediction.rs`,
// MIT/Apache-2.0), per RFC 6386 §12. Working blocks carry a one-pixel top/left border (luma also
// four top-right pixels); all samples are held as `Int` in 0–255. `predict4x4` handles the ten
// B_PRED subblock modes; the whole-block V/H/DC/TM predictors serve 16×16 luma and 8×8 chroma.
private[hallucination] object Vp8Predict:
  val LumaStride: Int = 1 + 16 + 4
  val LumaBlockSize: Int = (1 + 16 + 4)*(1 + 16)
  val ChromaStride: Int = 8 + 1
  val ChromaBlockSize: Int = (8 + 1)*(8 + 1)

  private inline def avg2(a: Int, b: Int): Int = (a + b + 1) >> 1
  private inline def avg3(a: Int, b: Int, c: Int): Int = (a + 2*b + c + 2) >> 2

  // Builds the luma working block with its borders drawn from the neighbouring macroblocks'
  // saved edges (or the frame-edge defaults 127 above and 129 left).
  def createBorderLuma(mbx: Int, mby: Int, mbw: Int, top: Array[Int], left: Array[Int])
  :   Array[Int] =

    val stride = LumaStride
    val ws = new Array[Int](LumaBlockSize)

    if mby == 0 then
      var i = 1

      while i < stride do
        ws(i) = 127
        i += 1
    else
      var i = 0

      while i < 16 do
        ws(1 + i) = top(mbx*16 + i)
        i += 1

      if mbx == mbw - 1 then
        i = 16

        while i < stride - 1 do
          ws(1 + i) = top(mbx*16 + 15)
          i += 1
      else
        i = 16

        while i < stride - 1 do
          ws(1 + i) = top(mbx*16 + i)
          i += 1

    var i = 17

    while i < stride do
      ws(4*stride + i) = ws(i)
      ws(8*stride + i) = ws(i)
      ws(12*stride + i) = ws(i)
      i += 1

    if mbx == 0 then
      i = 0

      while i < 16 do
        ws((i + 1)*stride) = 129
        i += 1
    else
      i = 0

      while i < 16 do
        ws((i + 1)*stride) = left(1 + i)
        i += 1

    ws(0) = if mby == 0 then 127 else if mbx == 0 then 129 else left(0)
    ws

  def createBorderChroma(mbx: Int, mby: Int, top: Array[Int], left: Array[Int]): Array[Int] =
    val stride = ChromaStride
    val ws = new Array[Int](ChromaBlockSize)

    if mby == 0 then
      var i = 1

      while i < stride do
        ws(i) = 127
        i += 1
    else
      var i = 0

      while i < 8 do
        ws(1 + i) = top(mbx*8 + i)
        i += 1

    if mbx == 0 then
      var y = 0

      while y < 8 do
        ws((y + 1)*stride) = 129
        y += 1
    else
      var y = 0

      while y < 8 do
        ws((y + 1)*stride) = left(1 + y)
        y += 1

    ws(0) = if mby == 0 then 127 else if mbx == 0 then 129 else left(0)
    ws

  // Adds a decoded 4×4 residual block to the predicted pixels, clamping to 0–255.
  def addResidue
    ( pblock: Array[Int], rblock: Array[Int], rOffset: Int, y0: Int, x0: Int, stride: Int )
  :   Unit =

    var pos = y0*stride + x0
    var row = 0

    while row < 4 do
      var col = 0

      while col < 4 do
        val value = rblock(rOffset + row*4 + col) + pblock(pos + col)
        pblock(pos + col) = if value < 0 then 0 else if value > 255 then 255 else value
        col += 1

      pos += stride
      row += 1

  def predictVpred(a: Array[Int], size: Int, x0: Int, y0: Int, stride: Int): Unit =
    var y = 0

    while y < size do
      var x = 0

      while x < size do
        a((y0 + y)*stride + x0 + x) = a((y0 - 1)*stride + x0 + x)
        x += 1

      y += 1

  def predictHpred(a: Array[Int], size: Int, x0: Int, y0: Int, stride: Int): Unit =
    var y = 0

    while y < size do
      val left = a((y0 + y)*stride + x0 - 1)
      var x = 0

      while x < size do
        a((y0 + y)*stride + x0 + x) = left
        x += 1

      y += 1

  def predictDcpred(a: Array[Int], size: Int, stride: Int, above: Boolean, left: Boolean): Unit =
    var sum = 0
    var shift = if size == 8 then 2 else 3

    if left then
      var y = 0

      while y < size do
        sum += a((y + 1)*stride)
        y += 1

      shift += 1

    if above then
      var x = 0

      while x < size do
        sum += a(1 + x)
        x += 1

      shift += 1

    val dc = if !left && !above then 128 else (sum + (1 << (shift - 1))) >> shift
    var y = 0

    while y < size do
      var x = 0

      while x < size do
        a(1 + stride*(y + 1) + x) = dc
        x += 1

      y += 1

  // The "TrueMotion" predictor: X_ij = clamp(L_i + A_j − P).
  def predictTmpred(a: Array[Int], size: Int, x0: Int, y0: Int, stride: Int): Unit =
    val p = a((y0 - 1)*stride + x0 - 1)
    var y = 0

    while y < size do
      val leftMinusP = a((y0 + y)*stride + x0 - 1) - p
      var x = 0

      while x < size do
        val value = leftMinusP + a((y0 - 1)*stride + x0 + x)
        a((y0 + y)*stride + x0 + x) = if value < 0 then 0 else if value > 255 then 255 else value
        x += 1

      y += 1

  // Dispatches each of a macroblock's sixteen 4×4 subblocks to its B_PRED mode, adding residue.
  def predict4x4(ws: Array[Int], stride: Int, modes: Array[Int], resdata: Array[Int]): Unit =
    var sby = 0

    while sby < 4 do
      var sbx = 0

      while sbx < 4 do
        val i = sbx + sby*4
        val y0 = sby*4 + 1
        val x0 = sbx*4 + 1

        modes(i) match
          case `BTmPred` => predictTmpred(ws, 4, x0, y0, stride)
          case `BVePred` => bvepred(ws, x0, y0, stride)
          case `BHePred` => bhepred(ws, x0, y0, stride)
          case `BDcPred` => bdcpred(ws, x0, y0, stride)
          case `BLdPred` => bldpred(ws, x0, y0, stride)
          case `BRdPred` => brdpred(ws, x0, y0, stride)
          case `BVrPred` => bvrpred(ws, x0, y0, stride)
          case `BVlPred` => bvlpred(ws, x0, y0, stride)
          case `BHdPred` => bhdpred(ws, x0, y0, stride)
          case _         => bhupred(ws, x0, y0, stride)

        addResidue(ws, resdata, i*16, y0, x0, stride)
        sbx += 1

      sby += 1

  private inline def topLeft(a: Array[Int], x0: Int, y0: Int, stride: Int): Int =
    a((y0 - 1)*stride + x0 - 1)

  private inline def top(a: Array[Int], x0: Int, y0: Int, stride: Int, k: Int): Int =
    a((y0 - 1)*stride + x0 + k)

  private inline def leftPixel(a: Array[Int], x0: Int, y0: Int, stride: Int, k: Int): Int =
    a((y0 + k)*stride + x0 - 1)

  // The nine edge pixels around a subblock, ordered bottom-left (e0) up the left column, through
  // the top-left corner (e4), along the top row to e8.
  private inline def edge(a: Array[Int], x0: Int, y0: Int, stride: Int, k: Int): Int =
    val corner = (y0 - 1)*stride + x0 - 1

    k match
      case 0 => a(corner + 4*stride)
      case 1 => a(corner + 3*stride)
      case 2 => a(corner + 2*stride)
      case 3 => a(corner + stride)
      case _ => a(corner + (k - 4))

  private def bdcpred(a: Array[Int], x0: Int, y0: Int, stride: Int): Unit =
    var v = 4
    var k = 0

    while k < 4 do
      v += top(a, x0, y0, stride, k)
      v += a((y0 + k)*stride + x0 - 1)
      k += 1

    v >>= 3
    var y = 0

    while y < 4 do
      var x = 0

      while x < 4 do
        a((y0 + y)*stride + x0 + x) = v
        x += 1

      y += 1

  private def bvepred(a: Array[Int], x0: Int, y0: Int, stride: Int): Unit =
    val p = topLeft(a, x0, y0, stride)

    val avg = Array(avg3(p, top(a, x0, y0, stride, 0), top(a, x0, y0, stride, 1)),
        avg3(top(a, x0, y0, stride, 0), top(a, x0, y0, stride, 1), top(a, x0, y0, stride, 2)),
        avg3(top(a, x0, y0, stride, 1), top(a, x0, y0, stride, 2), top(a, x0, y0, stride, 3)),
        avg3(top(a, x0, y0, stride, 2), top(a, x0, y0, stride, 3), top(a, x0, y0, stride, 4)))

    var y = 0

    while y < 4 do
      var x = 0

      while x < 4 do
        a((y0 + y)*stride + x0 + x) = avg(x)
        x += 1

      y += 1

  private def bhepred(a: Array[Int], x0: Int, y0: Int, stride: Int): Unit =
    val p = topLeft(a, x0, y0, stride)
    val l0 = leftPixel(a, x0, y0, stride, 0); val l1 = leftPixel(a, x0, y0, stride, 1)
    val l2 = leftPixel(a, x0, y0, stride, 2); val l3 = leftPixel(a, x0, y0, stride, 3)
    val avg = Array(avg3(p, l0, l1), avg3(l0, l1, l2), avg3(l1, l2, l3), avg3(l2, l3, l3))

    var y = 0

    while y < 4 do
      var x = 0

      while x < 4 do
        a((y0 + y)*stride + x0 + x) = avg(y)
        x += 1

      y += 1

  private def bldpred(a: Array[Int], x0: Int, y0: Int, stride: Int): Unit =
    val t = Array.tabulate(8)(top(a, x0, y0, stride, _))

    val avg = Array(avg3(t(0), t(1), t(2)), avg3(t(1), t(2), t(3)), avg3(t(2), t(3), t(4)),
        avg3(t(3), t(4), t(5)), avg3(t(4), t(5), t(6)), avg3(t(5), t(6), t(7)),
        avg3(t(6), t(7), t(7)))

    var y = 0

    while y < 4 do
      var x = 0

      while x < 4 do
        a((y0 + y)*stride + x0 + x) = avg(y + x)
        x += 1

      y += 1

  private def brdpred(a: Array[Int], x0: Int, y0: Int, stride: Int): Unit =
    val e = Array.tabulate(9)(edge(a, x0, y0, stride, _))

    val avg = Array(avg3(e(0), e(1), e(2)), avg3(e(1), e(2), e(3)), avg3(e(2), e(3), e(4)),
        avg3(e(3), e(4), e(5)), avg3(e(4), e(5), e(6)), avg3(e(5), e(6), e(7)),
        avg3(e(6), e(7), e(8)))

    var y = 0

    while y < 4 do
      var x = 0

      while x < 4 do
        a((y0 + y)*stride + x0 + x) = avg(3 - y + x)
        x += 1

      y += 1

  private def bvrpred(a: Array[Int], x0: Int, y0: Int, stride: Int): Unit =
    val e = Array.tabulate(9)(edge(a, x0, y0, stride, _))
    inline def set(dy: Int, dx: Int, v: Int): Unit = a((y0 + dy)*stride + x0 + dx) = v

    set(3, 0, avg3(e(1), e(2), e(3)))
    set(2, 0, avg3(e(2), e(3), e(4)))
    set(3, 1, avg3(e(3), e(4), e(5))); set(1, 0, avg3(e(3), e(4), e(5)))
    set(2, 1, avg2(e(4), e(5))); set(0, 0, avg2(e(4), e(5)))
    set(3, 2, avg3(e(4), e(5), e(6))); set(1, 1, avg3(e(4), e(5), e(6)))
    set(2, 2, avg2(e(5), e(6))); set(0, 1, avg2(e(5), e(6)))
    set(3, 3, avg3(e(5), e(6), e(7))); set(1, 2, avg3(e(5), e(6), e(7)))
    set(2, 3, avg2(e(6), e(7))); set(0, 2, avg2(e(6), e(7)))
    set(1, 3, avg3(e(6), e(7), e(8)))
    set(0, 3, avg2(e(7), e(8)))

  private def bvlpred(a: Array[Int], x0: Int, y0: Int, stride: Int): Unit =
    val t = Array.tabulate(8)(top(a, x0, y0, stride, _))
    inline def set(dy: Int, dx: Int, v: Int): Unit = a((y0 + dy)*stride + x0 + dx) = v

    set(0, 0, avg2(t(0), t(1)))
    set(1, 0, avg3(t(0), t(1), t(2)))
    set(2, 0, avg2(t(1), t(2))); set(0, 1, avg2(t(1), t(2)))
    set(1, 1, avg3(t(1), t(2), t(3))); set(3, 0, avg3(t(1), t(2), t(3)))
    set(2, 1, avg2(t(2), t(3))); set(0, 2, avg2(t(2), t(3)))
    set(3, 1, avg3(t(2), t(3), t(4))); set(1, 2, avg3(t(2), t(3), t(4)))
    set(2, 2, avg2(t(3), t(4))); set(0, 3, avg2(t(3), t(4)))
    set(3, 2, avg3(t(3), t(4), t(5))); set(1, 3, avg3(t(3), t(4), t(5)))
    set(2, 3, avg3(t(4), t(5), t(6)))
    set(3, 3, avg3(t(5), t(6), t(7)))

  private def bhdpred(a: Array[Int], x0: Int, y0: Int, stride: Int): Unit =
    val e = Array.tabulate(9)(edge(a, x0, y0, stride, _))
    inline def set(dy: Int, dx: Int, v: Int): Unit = a((y0 + dy)*stride + x0 + dx) = v

    set(3, 0, avg2(e(0), e(1)))
    set(3, 1, avg3(e(0), e(1), e(2)))
    set(2, 0, avg2(e(1), e(2))); set(3, 2, avg2(e(1), e(2)))
    set(2, 1, avg3(e(1), e(2), e(3))); set(3, 3, avg3(e(1), e(2), e(3)))
    set(2, 2, avg2(e(2), e(3))); set(1, 0, avg2(e(2), e(3)))
    set(2, 3, avg3(e(2), e(3), e(4))); set(1, 1, avg3(e(2), e(3), e(4)))
    set(1, 2, avg2(e(3), e(4))); set(0, 0, avg2(e(3), e(4)))
    set(1, 3, avg3(e(3), e(4), e(5))); set(0, 1, avg3(e(3), e(4), e(5)))
    set(0, 2, avg3(e(4), e(5), e(6)))
    set(0, 3, avg3(e(5), e(6), e(7)))

  private def bhupred(a: Array[Int], x0: Int, y0: Int, stride: Int): Unit =
    val l0 = leftPixel(a, x0, y0, stride, 0); val l1 = leftPixel(a, x0, y0, stride, 1)
    val l2 = leftPixel(a, x0, y0, stride, 2); val l3 = leftPixel(a, x0, y0, stride, 3)
    inline def set(dy: Int, dx: Int, v: Int): Unit = a((y0 + dy)*stride + x0 + dx) = v

    set(0, 0, avg2(l0, l1))
    set(0, 1, avg3(l0, l1, l2))
    set(0, 2, avg2(l1, l2)); set(1, 0, avg2(l1, l2))
    set(0, 3, avg3(l1, l2, l3)); set(1, 1, avg3(l1, l2, l3))
    set(1, 2, avg2(l2, l3)); set(2, 0, avg2(l2, l3))
    set(1, 3, avg3(l2, l3, l3)); set(2, 1, avg3(l2, l3, l3))
    set(2, 2, l3); set(2, 3, l3); set(3, 0, l3); set(3, 1, l3); set(3, 2, l3); set(3, 3, l3)
