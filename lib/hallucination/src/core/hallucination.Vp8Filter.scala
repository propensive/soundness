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

// The VP8 loop filter, ported from image-rs/image-webp (`src/lossy/loop_filter.rs`,
// MIT/Apache-2.0), per RFC 6386 §15. Buffers hold samples as `Int` in 0–255. Horizontal filters
// act on eight consecutive samples `pixels(base + 0..7)` spanning a vertical edge; vertical
// filters act on `pixels(point + k*stride)` for k in −4..3 spanning a horizontal edge.
private[hallucination] object Vp8Filter:
  private inline def clamp(value: Int): Int =
    if value < -128 then -128 else if value > 127 then 127 else value

  private inline def signed(value: Int): Int = value - 128
  private inline def unsigned(value: Int): Int = clamp(value) + 128
  private inline def diff(a: Int, b: Int): Int = math.abs(a - b)

  // Adjusts the two middle samples of an edge; returns the primary adjustment `a`.
  private def commonAdjust(useOuter: Boolean, p: Array[Int], p1i: Int, p0i: Int, q0i: Int, q1i: Int)
  :   Int =

    val p1 = signed(p(p1i)); val p0 = signed(p(p0i))
    val q0 = signed(p(q0i)); val q1 = signed(p(q1i))
    val outer = if useOuter then clamp(p1 - q1) else 0
    val a0 = clamp(outer + 3*(q0 - p0))
    val b = clamp(a0 + 3) >> 3
    val a = clamp(a0 + 4) >> 3

    p(q0i) = unsigned(q0 - a)
    p(p0i) = unsigned(p0 + b)
    a

  private inline def simpleThreshold(limit: Int, p0: Int, q0: Int, p1: Int, q1: Int): Boolean =
    diff(p0, q0)*2 + diff(p1, q1)/2 <= limit

  private def shouldFilter(interior: Int, edge: Int, get: Int => Int): Boolean =
    simpleThreshold(edge, get(3), get(4), get(2), get(5)) &&
      diff(get(0), get(1)) <= interior && diff(get(1), get(2)) <= interior &&
      diff(get(2), get(3)) <= interior && diff(get(7), get(6)) <= interior &&
      diff(get(6), get(5)) <= interior && diff(get(5), get(4)) <= interior

  private inline def highVariance(threshold: Int, p1: Int, p0: Int, q0: Int, q1: Int): Boolean =
    diff(p1, p0) > threshold || diff(q1, q0) > threshold

  // Horizontal filters (vertical edge, eight consecutive samples at `base`).

  def simpleSegmentHorizontal(edge: Int, p: Array[Int], base: Int): Unit =
    if simpleThreshold(edge, p(base + 3), p(base + 4), p(base + 2), p(base + 5))
    then commonAdjust(true, p, base + 2, base + 3, base + 4, base + 5)

  def subblockFilterHorizontal(hev: Int, interior: Int, edge: Int, p: Array[Int], base: Int): Unit =
    if shouldFilter(interior, edge, k => p(base + k)) then
      val hv = highVariance(hev, p(base + 2), p(base + 3), p(base + 4), p(base + 5))
      val a = (commonAdjust(hv, p, base + 2, base + 3, base + 4, base + 5) + 1) >> 1

      if !hv then
        p(base + 5) = unsigned(signed(p(base + 5)) - a)
        p(base + 2) = unsigned(signed(p(base + 2)) + a)

  def macroblockFilterHorizontal(hev: Int, interior: Int, edge: Int, p: Array[Int], base: Int)
  :   Unit =

    if shouldFilter(interior, edge, k => p(base + k)) then
      if !highVariance(hev, p(base + 2), p(base + 3), p(base + 4), p(base + 5)) then
        val p2 = signed(p(base + 1)); val p1 = signed(p(base + 2)); val p0 = signed(p(base + 3))
        val q0 = signed(p(base + 4)); val q1 = signed(p(base + 5)); val q2 = signed(p(base + 6))
        val w = clamp(clamp(p1 - q1) + 3*(q0 - p0))
        val a1 = clamp((27*w + 63) >> 7)
        p(base + 4) = unsigned(q0 - a1); p(base + 3) = unsigned(p0 + a1)
        val a2 = clamp((18*w + 63) >> 7)
        p(base + 5) = unsigned(q1 - a2); p(base + 2) = unsigned(p1 + a2)
        val a3 = clamp((9*w + 63) >> 7)
        p(base + 6) = unsigned(q2 - a3); p(base + 1) = unsigned(p2 + a3)
      else
        commonAdjust(true, p, base + 2, base + 3, base + 4, base + 5)

  // Vertical filters (horizontal edge, eight samples down a column at `point`, spacing `stride`).

  def simpleSegmentVertical(edge: Int, p: Array[Int], point: Int, stride: Int): Unit =
    if simpleThreshold(edge, p(point - stride), p(point), p(point - 2*stride), p(point + stride))
    then commonAdjust(true, p, point - 2*stride, point - stride, point, point + stride)

  def subblockFilterVertical
    ( hev: Int, interior: Int, edge: Int, p: Array[Int], point: Int, stride: Int )
  :   Unit =

    if shouldFilter(interior, edge, k => p(point + (k - 4)*stride)) then
      val hv =
        highVariance(hev, p(point - 2*stride), p(point - stride), p(point), p(point + stride))

      val a =
        (commonAdjust(hv, p, point - 2*stride, point - stride, point, point + stride) + 1) >> 1

      if !hv then
        p(point + stride) = unsigned(signed(p(point + stride)) - a)
        p(point - 2*stride) = unsigned(signed(p(point - 2*stride)) + a)

  def macroblockFilterVertical
    ( hev: Int, interior: Int, edge: Int, p: Array[Int], point: Int, stride: Int )
  :   Unit =

    if shouldFilter(interior, edge, k => p(point + (k - 4)*stride)) then
      val variance = highVariance(hev, p(point - 2*stride), p(point - stride), p(point),
          p(point + stride))

      if !variance then
        val p2 = signed(p(point - 3*stride)); val p1 = signed(p(point - 2*stride))
        val p0 = signed(p(point - stride)); val q0 = signed(p(point))
        val q1 = signed(p(point + stride)); val q2 = signed(p(point + 2*stride))
        val w = clamp(clamp(p1 - q1) + 3*(q0 - p0))
        val a1 = clamp((27*w + 63) >> 7)
        p(point) = unsigned(q0 - a1); p(point - stride) = unsigned(p0 + a1)
        val a2 = clamp((18*w + 63) >> 7)
        p(point + stride) = unsigned(q1 - a2); p(point - 2*stride) = unsigned(p1 + a2)
        val a3 = clamp((9*w + 63) >> 7)
        p(point + 2*stride) = unsigned(q2 - a3); p(point - 3*stride) = unsigned(p2 + a3)
      else
        commonAdjust(true, p, point - 2*stride, point - stride, point, point + stride)
