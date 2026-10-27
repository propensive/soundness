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
package tasseomancy

import Framing.*
import acyclicity.*
import cartouche.*
import cataclysm.Css

import geodesy.*
import gossamer.*
import hypotenuse.*
import iridescence.*
import murmuration.sortingAlgorithms.timsort
import prepositional.*
import rudiments.*
import savagery.*
import symbolism.*
import vacuous.*

object Doughnut:
  // A node's place in its band: the angles its sector starts and ends at, clockwise from north in
  // radians, and the size it was laid out with.
  case class Sector[node](node: node, start: Double, end: Double, size: Double)

  // A rank's band, counted outward from the centre: the fractions of the distance between the
  // hole and the rim at which its edges lie, and its sectors in drawing order.
  case class Band[node](rank: Int, inner: Double, outer: Double, sectors: List[Sector[node]])

  // Everything that holds still across revisions: the bands. Completion is not here; it is
  // read from the nodes on every draw.
  case class Fit[node](bands: List[Band[node]])

  private val evenOdd: Css.Style = Css.Style.of(List(t"fill-rule" -> t"evenodd"))

  // The components of a doughnut: each node's sector, given as the path operations of the part
  // completed and of the remainder, in a colour shaded by where the sector lies and a subdued
  // tint of it. `hole` hollows the centre to that fraction of the radius, and `sectorGap` and
  // `bandGap` are the widths in pixels of the grout between neighbouring sectors and between
  // neighbouring bands. A doughnut has no labels and no legend.
  trait Style extends Chart.Style:
    def hole: Double
    def sectorGap: Double
    def bandGap: Double

    // The colour of the sector whose midpoint lies at (`x`, `y`), each from -1 at the left or
    // the bottom of the doughnut to 1 at the right or the top, from the palette's first colour:
    // its hue turns through thirty degrees from left to right, and from bottom to top it passes
    // from a fifth darker, through the pure hue at the middle, to a fifth lighter.
    def shade(x: Double, y: Double, base: Color in Srgb): Color in Srgb =
      val pure = base.to[Hsv].pure.rotate(Angle.degrees(15.0*x))
      (if y < 0.0 then pure.shade(-0.2*y) else pure.tint(0.2*y)).to[Srgb]

    def sector(ops: List[Stroke], color: Color in Srgb, rank: Int, index: Int): List[Figure] =
      List(Outline(ops, style = filled(color) + evenOdd, id = Svg.Id(t"sector-$index")))

    def remainder(ops: List[Stroke], color: Color in Srgb, rank: Int, index: Int): List[Figure] =
      List(Outline(ops, style = filled(color) + evenOdd, id = Svg.Id(t"sector-$index-rest")))

  given dag: [node: {Weighted as weighted, Progressive as progressive}] => (ranking: Ranking)
  =>  Dag[node] is Plottable in Doughnut to Doughnut.Fit[node] by Doughnut.Style =

    new Plottable:
      type Self = Dag[node]
      type Form = Doughnut
      type Result = Doughnut.Fit[node]
      type Operand = Doughnut.Style

      // Ranks come from the layering, which puts every node one rank outside its deepest
      // dependency; within a rank, the layering's crossing-minimised order is refined by one
      // pass outward, so that a node sits over the dependencies it was drawn to.
      def fit(form: Doughnut, data: Dag[node]): Doughnut.Fit[node] =
        val nodes = data.linearized

        if countOf(nodes) == 0 then Doughnut.Fit(Nil) else
          val layering = data.layered
          val depth = nodes.fold(0): (acc, node) => acc.max(layering.rank(node) + 1)
          def sizeOf(node: node): Double = weighted.weight(node).max(0.0)

          var middles: Map[node, Double] = Map()
          var bands: List[Band[node]] = Nil

          List.range(0, depth).each: rank =>
            val initial = nodes.filter(layering.rank(_) == rank).order(layering.position(_))
            val count = countOf(initial)

            // A node's key is the circular mean of the midpoints of its dependencies in the
            // ranks inside, which is where they were drawn; a node with none, or whose
            // dependencies lie on opposite sides and have no mean, keeps the layering's place,
            // scaled into the same turn.
            val ordered = if rank == 0 then initial else initial.order: node =>
              val dependencies = data.successors(node).to[List].filter(layering.rank(_) < rank)
              val fallback = 2.0*π*layering.position(node)/count

              if countOf(dependencies) == 0 then fallback else
                val (sines, cosines) = dependencies.fold((0.0, 0.0)): (acc, dependency) =>
                  val middle = Map.at(middles, dependency)
                  (acc(0) + sin(middle).double, acc(1) + cos(middle).double)

                if sines.abs + cosines.abs < Scale.tolerance then fallback else
                  val mean = Math.atan2(sines, cosines)
                  if mean < 0.0 then mean + 2.0*π else mean

            val total = ordered.fold(0.0): (acc, node) => acc + sizeOf(node)
            var start = 0.0

            val sectors = ordered.fold(List[Sector[node]]()): (acc, node) =>
              val size = sizeOf(node)
              val sweep = if total <= 0.0 then 2.0*π/count else 2.0*π*size/total
              val end = start + sweep
              middles = Map.define(middles, node, (start + end)/2.0)
              start = end
              Sector(node, end - sweep, end, size) :: acc

            bands = Band(rank, 0.0, 0.0, sectors.reverse) :: bands

          // A band's thickness is its rank's share of the total size; equal shares if there is
          // no size at all.
          def totalOf(band: Band[node]): Double =
            band.sectors.fold(0.0): (acc, sector) => acc + sector.size

          val grand = bands.fold(0.0): (acc, band) => acc + totalOf(band)
          var inner = 0.0

          val stacked = bands.reverse.fold(List[Band[node]]()): (acc, band) =>
            val thickness = if grand <= 0.0 then 1.0/depth else totalOf(band)/grand
            val placed = band.copy(inner = inner, outer = inner + thickness)
            inner += thickness
            placed :: acc

          Doughnut.Fit(stacked.reverse)

      // The layout depends on the graph's shape and the sizes alone, so a fit survives a graph
      // of the same shape whose nodes have only progressed, and then only the bands whose
      // sectors filled further are replaced.
      def accommodates(form: Doughnut, fit: Doughnut.Fit[node], data: Dag[node]): Boolean =
        def geometry(fit: Doughnut.Fit[node]) = fit.bands.map: band =>
          (band.rank, band.inner, band.outer, band.sectors.map { s => (s.start, s.end, s.size) })

        geometry(this.fit(form, data)) == geometry(fit)

      def draw(form: Doughnut, data: Dag[node], fit: Doughnut.Fit[node])
        ( using style:    Doughnut.Style,
                palette:  ChartPalette,
                metric:   FontMetric,
                arranger: Arranger )
      :   Chart.Drawing =

        val width = (style.width - style.inset*2).max(1.0)
        val height = (style.height - style.inset*2).max(1.0)
        val radius = (width.min(height)/2.0).max(1.0)
        val hole = radius*style.hole.max(0.0).min(0.95)
        val usable = radius - hole
        val cx = style.inset + width/2.0
        val cy = style.inset + height/2.0
        // The data's own layout, whose geometry is the fit's wherever the fit accommodates it,
        // and whose nodes carry the progress to draw.
        val current = this.fit(form, data)
        val depth = countOf(current.bands)

        def rim(angle: Double, distance: Double): Point =
          point(cx + distance*sin(angle).double, cy - distance*cos(angle).double)

        def arc(distance: Double, large: Boolean, sweep: Sweep, angle: Double): Stroke =
          val size = distance.toFloat
          Stroke.ArcTo(size, size, Angle(0.0), large, sweep, rim(angle, distance))

        // Half the grout: how far each edge of a sector moves, parallel to itself, into the
        // sector, so that the grout between neighbours is the same width along its length.
        val offset = style.sectorGap.max(0.0)/2.0

        // Where the edge at `angle`, moved by `offset` in the direction of `sign`, meets the
        // circle of radius `distance`; `Unset` where it passes the circle by.
        def corner(angle: Double, offset: Double, distance: Double, sign: Double)
        :   Optional[Double] =
          if offset > distance then Unset else angle + sign*asin(offset/distance).double

        // Where a sector's two edges, each moved into it by its offset, cross, as a distance and
        // an angle; a negative distance where they are parallel. With no offsets, they cross at
        // the centre.
        def crossing(start: Double, end: Double, offset1: Double, offset2: Double)
        :   (Double, Double) =
          val det = sin(end - start).double

          if det.abs < Scale.tolerance then (-1.0, 0.0) else
            val x = (offset1*sin(end).double + offset2*sin(start).double)/det
            val y = (offset2*cos(start).double + offset1*cos(end).double)/det
            (Math.hypot(x, y), Math.atan2(x, y))

        // The path operations of a sector between two radii, listed last to first as an
        // `Outline` holds them, with the grout taken from whichever edges `gapStart` and
        // `gapEnd` ask for. Where the edges cross before they reach the inner circle — always,
        // for the innermost band of a doughnut without a hole — the sector is a wedge to their
        // crossing; where they cross beyond the outer circle, there is nothing left to draw.
        def annular
          ( start: Double, end: Double, inner: Double, outer: Double, gapStart: Boolean,
            gapEnd: Boolean )
        :   List[Stroke] =
          val offset1 = if gapStart then offset else 0.0
          val offset2 = if gapEnd then offset else 0.0

          corner(start, offset1, outer, 1.0).lay(Nil): a =>
            corner(end, offset2, outer, -1.0).lay(Nil): b =>
              if a >= b then Nil else
                val outside: List[Stroke] =
                  List(arc(outer, b - a > π, Sweep.Clockwise, b), Stroke.MoveTo(rim(a, outer)))

                val (distance, angle) = crossing(start, end, offset1, offset2)

                if distance >= inner then
                  if distance >= outer then Nil
                  else List(Stroke.Close, Stroke.DrawTo(rim(angle, distance))) + outside
                else
                  corner(end, offset2, inner, -1.0).lay(Nil): c =>
                    corner(start, offset1, inner, 1.0).lay(Nil): d =>
                      if inner <= 0.0 || d >= c then List(Stroke.Close) + outside
                      else
                        List
                          ( Stroke.Close,
                            arc(inner, c - d > π, Sweep.Counterclockwise, d),
                            Stroke.DrawTo(rim(c, inner)) )
                        + outside

        // A whole ring cannot be one arc, whose ends would coincide: it is two half-turns, and
        // its hole is cut by the even-odd rule.
        def ring(inner: Double, outer: Double): List[Stroke] =
          val outside =
            List
              ( Stroke.Close,
                arc(outer, true, Sweep.Clockwise, 0.0),
                arc(outer, true, Sweep.Clockwise, π),
                Stroke.MoveTo(rim(0.0, outer)) )

          if inner <= 0.0 then outside else
            val inside =
              List
                ( Stroke.Close,
                  arc(inner, true, Sweep.Counterclockwise, π),
                  arc(inner, true, Sweep.Counterclockwise, 0.0),
                  Stroke.MoveTo(rim(π, inner)) )

            inside + outside

        var index = 0

        val parts = current.bands.fold(List[(Svg.Id, Figure)]()): (acc, band) =>
          val first = band.rank == 0
          val last = band.rank == depth - 1
          val inner = hole + band.inner*usable + (if first then 0.0 else style.bandGap/2.0)
          val rim = hole + band.outer*usable - (if last then 0.0 else style.bandGap/2.0)
          val outer = rim.max(inner + 1.0)

          val figures = band.sectors.fold(List[Figure]()): (acc, sector) =>
            val bearing = (sector.start + sector.end)/2.0
            val reach = (inner + outer)/2.0/radius
            val x = reach*sin(bearing).double
            val y = reach*cos(bearing).double
            val color = style.shade(x, y, palette.color(0))
            val muted = palette.subdue(color, 0.7)
            val whole = sector.end - sector.start >= 2.0*π - Scale.tolerance
            val start = sector.start
            val end = sector.end
            val completion = progressive.completion(sector.node).max(0.0).min(1.0)
            val middle = start + completion*(end - start)
            val done = completion >= 1.0 - Scale.tolerance
            val untouched = completion <= Scale.tolerance

            // The seam between the completed part and the remainder has no grout; a whole
            // ring has no grout at all.
            val completed: List[Figure] =
              if untouched then Nil
              else if whole && done then style.sector(ring(inner, outer), color, band.rank, index)
              else
                val ops = annular(start, middle, inner, outer, !whole, done && !whole)
                style.sector(ops, color, band.rank, index)

            val remaining: List[Figure] =
              if done then Nil
              else if whole && untouched
              then style.remainder(ring(inner, outer), muted, band.rank, index)
              else
                val ops = annular(middle, end, inner, outer, untouched && !whole, !whole)
                style.remainder(ops, muted, band.rank, index)

            index += 1
            remaining.reverse + (completed.reverse + acc)

          val id = Svg.Id(t"rank-${band.rank}")
          (id -> Group(figures.reverse, id = id)) :: acc

        Framing.drawing(parts.reverse)

// A dependency graph as concentric bands, one per rank from the nodes depending on nothing at
// the centre outward, each node a sector of its band proportional to its size, filled clockwise
// as far as it is complete.
case class Doughnut()
