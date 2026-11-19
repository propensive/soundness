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
package dendrology

import acyclicity.*
import anticipation.*
import denominative.*
import gossamer.*
import murmuration.sortingAlgorithms.timsort
import prepositional.*
import rudiments.*
import spectacular.*
import symbolism.*
import vacuous.*

import DagDiagram.Tile.*

object LayeredDagDiagram:
  import Layering.Vertex

  // A cell's edges and crossings as bit flags in an `Int`, so a row is a plain array built in
  // place: there is no mutable cell object for the checker to track. `VerticalPassThrough` and
  // `HorizontalPassThrough` distinguish a pure crossing (a horizontal lane passing over a
  // continuing vertical lane without sharing a node) from a junction where lanes meet.
  private object Cell:
    inline val Top                   = 1
    inline val Down                  = 2
    inline val Left                  = 4
    inline val Right                 = 8
    inline val VerticalPassThrough   = 16
    inline val HorizontalPassThrough = 32

    def tile(cell: Int): DagDiagram.Tile =
      inline def has(inline flag: Int): Boolean = (cell & flag) != 0

      if has(VerticalPassThrough) && has(HorizontalPassThrough) then Crossing
      else (has(Top), has(Down), has(Left), has(Right)) match
        case (false, false, false, false) => Space
        case (true, true, false, false)   => Vertical
        case (false, false, true, true)   => Horizontal
        case (true, false, false, true)   => CornerNe
        case (true, false, true, false)   => CornerNw
        case (false, true,  false, true)  => CornerSe
        case (false, true,  true,  false) => CornerSw
        case (true,  true,  false, true)  => TeeE
        case (true,  true,  true,  false) => TeeW
        case (true,  false, true,  true)  => TeeN
        case (false, true,  true,  true)  => TeeS
        case (true,  true,  true,  true)  => Junction
        case _                            => Space

  // Any graph that admits a topological order: a `Dag`, a frozen `Topology`, a `Hasse`. The
  // layering decides the order within each layer; this assigns columns to it. A vertex wants the
  // median column of its neighbours in the layer above (so a straight edge stays straight), and
  // is pushed right past the vertex before it when that column is taken.
  def apply[graph, node](dag: graph)
    ( using nodal:       graph is Nodal by node,
            topological: graph is Topological,
            ranking:     Ranking )
  :   LayeredDagDiagram[node] =

    val layering = dag.layered

    // Each layer beside the links which join it to the layer above; the first has none.
    val layers: List[(Sequence[Vertex[node]], List[(Int, Int)])] =
      layering.layers.to[List].zip(Nil :: layering.links.to[List])

    // The columns of a layer, from those of the layer `above` and the links between the two.
    def assign(layer: Sequence[Vertex[node]], above: Sequence[Int], links: List[(Int, Int)])
    :   Sequence[Int] =

      def desired(position: Int): Int =
        val upper: Sequence[Int] =
          links.filter(_(1) == position).map { link => above.at(link(0).z) }.compact
          . order(identity(_))
          . to[Sequence]

        upper.at((upper.limit.n0/2).z).or(-1)

      val assigned = layer.indexed.fold((List[Int](), 0)): (state, entry) =>
        val column = desired(entry(1).n0).max(state(1))
        (column :: state(0), column + 1)

      assigned(0).reverse.to[Sequence]

    val columns: List[Sequence[Int]] =
      val assigned = layers.fold((List[Sequence[Int]](), Sequence[Int]())): (state, next) =>
        val columns = assign(next(0), state(1), next(1))
        (columns :: state(0), columns)

      assigned(0).reverse

    val width: Int = columns.fold(0): (width, layer) =>
      layer.fold(width): (width, column) => width.max(column + 1)

    // Every vertex of a layer beside its column.
    val placed: List[Sequence[(Vertex[node], Int)]] =
      layers.zip(columns).map: (layer, columns) => layer(0).zip(columns)

    val rows = layers.zip(placed).zip(Sequence[(Vertex[node], Int)]() :: placed).bind: entry =>
      val ((layer, vertices), above) = entry

      val nodesAt: Map[Int, node] =
        vertices.sweep { case (Vertex.Real(node), column) => column -> node }.to[Map]

      val passing: Set[Int] =
        vertices.sweep { case (Vertex.Virtual(_, _), column) => column }.to[Set]

      val node = (nodeRow(nodesAt, passing, width), nodesAt)

      // A link names a position in each of the two layers it joins; one naming a position
      // that is not there draws nothing.
      val bends: List[(Int, Int, Boolean)] =
        layer(1).map: (upper, lower) =>
          above.at(upper.z).let: (source, entry) =>
            vertices.at(lower.z).let: (target, exit) =>
              val continuing = (source, target) match
                case (Vertex.Virtual(_, _), Vertex.Virtual(_, _)) => true
                case _                                            => false

              (entry, exit, continuing)

        . compact

      if above.nil then List(node) else List((connectorRow(bends, width), Map[Int, node]()), node)

    LayeredDagDiagram(rows)

  private def connectorRow(bends: List[(Int, Int, Boolean)], width: Int): List[DagDiagram.Tile] =
    val cells = new scala.Array[Int](width)

    def drawBend(topEntry: Int, bottomExit: Int, continuing: Boolean): Unit =
      if topEntry == bottomExit then
        cells(topEntry) |= Cell.Top
        cells(topEntry) |= Cell.Down
        if continuing then cells(topEntry) |= Cell.VerticalPassThrough
      else if topEntry < bottomExit then
        cells(topEntry) |= Cell.Top
        cells(topEntry) |= Cell.Right
        var c = topEntry + 1

        while c < bottomExit do
          cells(c) |= Cell.Left
          cells(c) |= Cell.Right
          cells(c) |= Cell.HorizontalPassThrough
          c += 1

        cells(bottomExit) |= Cell.Left
        cells(bottomExit) |= Cell.Down
      else
        cells(topEntry) |= Cell.Top
        cells(topEntry) |= Cell.Left
        var c = bottomExit + 1

        while c < topEntry do
          cells(c) |= Cell.Left
          cells(c) |= Cell.Right
          cells(c) |= Cell.HorizontalPassThrough
          c += 1

        cells(bottomExit) |= Cell.Right
        cells(bottomExit) |= Cell.Down

    bends.each(drawBend(_, _, _))

    List.from(cells.iterator.map(Cell.tile))

  private def nodeRow[node](nodes: Map[Int, node], passing: Set[Int], width: Int)
  :   List[DagDiagram.Tile] =

    List.range(0, width).map: column =>
      if nodes.at(column).present then Node else if passing.has(column) then Vertical else Space

  given printable: [node: Showable] => (style: LaneDagDiagram.Style[Text])
  =>  LayeredDagDiagram[node] is Printable =
    (diagram, termcap) => diagram.render[Text]{ node => t"● $node  " }.join(t"\n")

case class LayeredDagDiagram[node](rows: List[(List[DagDiagram.Tile], Map[Int, node])]):
  val size: Int = rows.fold(0): (count, _) => count + 1

  def render[line](glyph: node => line)(using style: LaneDagDiagram.Style[line]): List[line] =
    val columns: Int = rows.fold(0): (count, row) =>
      count.max(row(0).fold(0) { (count, _) => count + 1 })

    val widths = scala.Array.fill(columns)(2)

    rows.each: (_, nodesAt) =>
      nodesAt.each: (column, node) =>
        val width = style.width(glyph(node))
        if width > widths(column) then widths(column) = width

    val widthsList: List[Int] = List.from(widths.iterator)

    rows.map: (tiles, nodesAt) => style.serialize(tiles, nodesAt.map(glyph(_)), widthsList, Unset)

  def tiles: List[List[DagDiagram.Tile]] = rows.map(_(0))
  def nodesAt: List[Map[Int, node]] = rows.map(_(1))
