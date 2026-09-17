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
package cartouche

import rudiments.*
import vacuous.*

object Obstacle:
  object Box:
    // The footprint of a label of `width`×`height` whose anchor point is (`x`, `y`) and which
    // lies on the `attachment` side of it: for `East` the anchor is the label's left-middle, for
    // `North` its bottom-centre, for `Center` its centre.
    def around
      ( x: Double, y: Double, width: Double, height: Double, attachment: Caption.Attachment )
    :   Box =

      Box(x - width*(1 - attachment.dx)/2.0, y - height*(1 - attachment.dy)/2.0, width, height)

  // An axis-aligned rectangle in a y-down frame, as SVG and every raster format has it.
  case class Box(left: Double, top: Double, width: Double, height: Double) extends Obstacle:
    def right: Double = left + width
    def bottom: Double = top + height
    def area: Double = width*height
    def centerX: Double = left + width/2.0
    def centerY: Double = top + height/2.0

    def pad(amount: Double): Box =
      Box(left - amount, top - amount, width + 2.0*amount, height + 2.0*amount)

    def overlap(box: Box): Double =
      val across = (right.min(box.right) - left.max(box.left)).max(0.0)
      val down = (bottom.min(box.bottom) - top.max(box.top)).max(0.0)
      across*down

    // How much of this box lies outside `canvas`.
    def excess(canvas: Box): Double = area - overlap(canvas)

    def contains(x: Double, y: Double): Boolean =
      x >= left && x <= right && y >= top && y <= bottom

    // The point on the perimeter nearest to (`x`, `y`): where a leader line meets the label.
    def nearest(x: Double, y: Double): (Double, Double) =
      if !contains(x, y) then (x.max(left).min(right), y.max(top).min(bottom))
      else
        val toLeft = x - left
        val toRight = right - x
        val toTop = y - top
        val toBottom = bottom - y
        val least = toLeft.min(toRight).min(toTop).min(toBottom)

        if least == toLeft then (left, y)
        else if least == toRight then (right, y)
        else if least == toTop then (x, top)
        else (x, bottom)

  // A stroked segment, such as one edge of a plotted polyline or a leader line. Its overlap with
  // a box is the length of the segment inside the box (Liang–Barsky clipping) times its width.
  case class Line(x1: Double, y1: Double, x2: Double, y2: Double, width: Double = 1.0)
  extends Obstacle:

    def length: Double =
      val dx = x2 - x1
      val dy = y2 - y1
      scala.math.sqrt(dx*dx + dy*dy)

    def overlap(box: Box): Double =
      val dx = x2 - x1
      val dy = y2 - y1

      // Each edge of the box constrains the parameter `t` of the segment to one side of a
      // threshold; the segment is inside the box where every constraint holds.
      val edges: List[(Double, Double)] =
        List((-dx, x1 - box.left), (dx, box.right - x1), (-dy, y1 - box.top), (dy, box.bottom - y1))

      val range: Optional[(Double, Double)] =
        edges.fold[Optional[(Double, Double)]]((0.0, 1.0)): (acc, edge) =>
          acc.let: span =>
            val (p, q) = edge

            if p == 0.0 then (if q < 0.0 then Unset else span)
            else
              val t = q/p

              if p < 0.0 then (if t > span(1) then Unset else (t.max(span(0)), span(1)))
              else (if t < span(0) then Unset else (span(0), t.min(span(1))))

      range.lay(0.0): span => if span(1) <= span(0) then 0.0 else (span(1) - span(0))*length*width

  // A filled circle, such as a scatter plot's marker. The zero test is exact: the box is clear
  // when its nearest point is at least a radius from the centre. The magnitude is the box's
  // overlap with the disc's bounding square scaled by the disc's share of that square.
  case class Disc(x: Double, y: Double, radius: Double) extends Obstacle:
    def overlap(box: Box): Double =
      val nearestX = x.max(box.left).min(box.right)
      val nearestY = y.max(box.top).min(box.bottom)
      val dx = nearestX - x
      val dy = nearestY - y

      if dx*dx + dy*dy >= radius*radius then 0.0
      else Box(x - radius, y - radius, 2.0*radius, 2.0*radius).overlap(box)*scala.math.Pi/4.0

// Something a label must not be set over: another label's footprint, a plotted line, a marker.
// Each shape answers with how much of a box it covers, in area units, and the answer is exactly
// zero when the two are disjoint. The magnitude only has to rank candidates against each other,
// so a disc's is an estimate; the zero is exact.
sealed trait Obstacle:
  def overlap(box: Obstacle.Box): Double
