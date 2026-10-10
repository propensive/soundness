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

// Deliberate stdlib opt-out: these internals consume acyclicity's `Layering`, whose layers
// and links are on the stdlib `List` for now.
import scala.collection.immutable.{List, Map, Nil, Set, Vector}

import acyclicity.*
import anticipation.*
import gossamer.*
import prepositional.*
import spectacular.*
import vacuous.*

import DagTile.*

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

    def tile(cell: Int): DagTile =
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
    val layers: Vector[Vector[Vertex[node]]] = layering.layers.map(_.to(Vector)).to(Vector)
    val links: Vector[Vector[(Int, Int)]] = layering.links.map(_.to(Vector)).to(Vector)

    if layers.isEmpty then LayeredDagDiagram(Nil) else
      def assign(layer: Int, above: Vector[Int]): Vector[Int] =
        def desired(position: Int): Int =
          if layer == 0 then -1 else
            val upper = links(layer - 1).filter(_(1) == position).map: link => above(link(0))
            if upper.isEmpty then -1 else upper.sorted.apply(upper.length/2)

        layers(layer).indices.foldLeft(List.empty[Int]): (assigned, position) =>
          assigned :+ (desired(position) max assigned.lastOption.fold(0)(_ + 1))

        . to(Vector)

      val columns: Vector[Vector[Int]] =
        layers.indices.foldLeft(List.empty[Vector[Int]]): (above, layer) =>
          above :+ assign(layer, above.lastOption.getOrElse(Vector.empty))

        . to(Vector)

      val width: Int = columns.flatten.max + 1

      val rows = layers.indices.to(List).flatMap: layer =>
        val vertices = layers(layer)

        val nodesAt: Map[Int, node] =
          vertices.zip(columns(layer)).collect { case (Vertex.Real(n), column) => column -> n }
          . to(Map)

        val passing: Set[Int] =
          vertices.zip(columns(layer)).collect { case (Vertex.Virtual(_, _), column) => column }
          . to(Set)

        val node = (nodeRow(nodesAt.keySet, passing, width), nodesAt)

        if layer == 0 then List(node) else
          val bends = links(layer - 1).to(List).map: (upper, lower) =>
            val continuing = (layers(layer - 1)(upper), vertices(lower)) match
              case (Vertex.Virtual(_, _), Vertex.Virtual(_, _)) => true
              case _                                            => false

            (columns(layer - 1)(upper), columns(layer)(lower), continuing)

          List((connectorRow(bends, width), Map.empty[Int, node]), node)

      LayeredDagDiagram(rows)

  private def connectorRow(bends: List[(Int, Int, Boolean)], width: Int): List[DagTile] =
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

    bends.foreach(drawBend)

    cells.iterator.map(Cell.tile).to(List)

  private def nodeRow(nodes: Set[Int], passing: Set[Int], width: Int): List[DagTile] =
    (0 until width).to(List).map: column =>
      if nodes(column) then Node else if passing(column) then Vertical else Space

  given printable: [node: Showable] => (style: LaneDagStyle[Text])
  =>  LayeredDagDiagram[node] is Printable =
    (diagram, termcap) => diagram.render[Text]{ node => t"● $node  " }.join(t"\n")

case class LayeredDagDiagram[node](rows: List[(List[DagTile], Map[Int, node])]):
  val size: Int = rows.length

  def render[line](glyph: node => line)(using style: LaneDagStyle[line]): List[line] =
    val maxCol = rows.iterator.map(_(0).length).maxOption.getOrElse(0)
    val widths = scala.Array.fill(maxCol)(2)

    rows.foreach: (_, nodesAt) =>
      nodesAt.foreach: (col, n) =>
        val w = style.width(glyph(n))
        if w > widths(col) then widths(col) = w

    val widthsList = widths.iterator.to(List)

    rows.map: (tiles, nodesAt) =>
      val glyphs: Map[Int, line] = nodesAt.map: (col, n) => col -> glyph(n)
      style.serialize(tiles.to(proscenium.List), glyphs, widthsList.to(proscenium.List), Unset)

  def tiles: List[List[DagTile]] = rows.map(_(0))
  def nodesAt: List[Map[Int, node]] = rows.map(_(1))
