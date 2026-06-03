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
┃    Soundness, version 0.54.0.                                                                    ┃
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

import scala.collection.mutable as scm

import acyclicity.*
import anticipation.*
import contingency.*
import gossamer.*
import rudiments.*
import spectacular.*
import vacuous.*

import DagTile.*

object LayeredDagDiagram:
  private case class Lane[node](source: node, target: node, col: Int)

  private case class Layout[node]
    ( state:       Map[Int, Lane[node]],
      terminating: Map[Int, Lane[node]],
      continuing:  Map[Int, Lane[node]],
      nodeCol:     Map[node, Int],
      prevNodeCol: Map[node, Int] )

  private final class Cell:
    var top: Boolean = false
    var down: Boolean = false
    var left: Boolean = false
    var right: Boolean = false
    var verticalPassThrough: Boolean = false
    var horizontalPassThrough: Boolean = false

    def tile: DagTile =
      if verticalPassThrough && horizontalPassThrough then Crossing
      else (top, down, left, right) match
        case (false, false, false, false) => Space
        case (true,  true,  false, false) => Vertical
        case (false, false, true,  true)  => Horizontal
        case (true,  false, false, true)  => CornerNe
        case (true,  false, true,  false) => CornerNw
        case (false, true,  false, true)  => CornerSe
        case (false, true,  true,  false) => CornerSw
        case (true,  true,  false, true)  => TeeE
        case (true,  true,  true,  false) => TeeW
        case (true,  false, true,  true)  => TeeN
        case (false, true,  true,  true)  => TeeS
        case (true,  true,  true,  true)  => Junction
        case _                            => Space

  def apply[node](dag: Dag[node]): LayeredDagDiagram[node] raises DagError =
    val nodes: Vector[node] = dag.sorted.to[Vector]

    if nodes.isEmpty then LayeredDagDiagram(Nil) else
      val parents: Map[node, Set[node]] = dag.edgeMap
      val forward: Map[node, Set[node]] = dag.invert.edgeMap

      val level: scm.HashMap[node, Int] = scm.HashMap()

      for n <- nodes do
        val ps = parents.getOrElse(n, Set.empty)
        level(n) = if ps.scala.isEmpty then 0 else ps.iterator.map(level).max + 1

      val maxLevel: Int = level.values.max

      val byLevel: Vector[Vector[node]] =
        (0 to maxLevel).to(Vector).map: l =>
          nodes.filter(level(_) == l)

      val state: scm.HashMap[Int, Lane[node]] = scm.HashMap()
      val layouts = scm.ListBuffer[Layout[node]]()
      var prevNodeCols: Map[node, Int] = Map.empty

      for l <- 0 to maxLevel do
        val levelNodes = byLevel(l)

        val terminating = Map.from(state).filter: (_, lane) => level(lane.target) == l
        val continuing = Map.from(state) -- terminating.keys

        val incomingByNode: Map[node, Vector[Int]] =
          terminating
            . groupBy(_._2.target)
            . map: (n, m) =>
                n -> m.keys.to[Vector].sorted

        val desired: Map[node, Int] = levelNodes.map: n =>
          val incoming = incomingByNode.getOrElse(n, Vector.empty)

          val centre =
            if incoming.nonEmpty then incoming(incoming.length/2)
            else if continuing.nonEmpty then continuing.keys.max + 1
            else 0

          n -> centre

        .to(Map)

        val ordered = levelNodes.sortBy(desired(_))
        val nodeOccupied = scm.HashSet[Int]() ++ continuing.keys.scala
        val nodeCol = scm.LinkedHashMap[node, Int]()

        for n <- ordered do
          var col = desired(n)
          while nodeOccupied.contains(col) do col += 1
          nodeCol(n) = col
          nodeOccupied.add(col)

        layouts += Layout(Map.from(state), terminating, continuing, Map.from(nodeCol), prevNodeCols)

        terminating.keys.each(state.remove(_))

        val laneOccupied = scm.HashSet[Int]() ++ continuing.keys.scala

        for n <- ordered do
          val outgoing = forward.getOrElse(n, Set.empty).filter(level(_) > l)
          val sortedOut = outgoing.to[Vector].sortBy(level)

          for target <- sortedOut do
            var col = nodeCol(n)
            while laneOccupied.contains(col) do col += 1
            laneOccupied.add(col)
            state(col) = Lane(n, target, col)

        prevNodeCols = Map.from(nodeCol)

      val width: Int =
        val cols = layouts.iterator.flatMap: lay =>
          lay.state.keys.iterator
            ++ lay.nodeCol.values.iterator
            ++ lay.prevNodeCol.values.iterator
        cols.maxOption.fold(1)(_ + 1)

      val rows = scm.ListBuffer[(List[DagTile], Map[Int, node])]()

      layouts.iterator.zipWithIndex.foreach: (lay, l) =>
        if l > 0 then rows += ((connectorRow(lay, width), Map.empty[Int, node]))
        rows += ((nodeRow(lay, width), lay.nodeCol.iterator.map{ (n, c) => c -> n }.to(Map)))

      LayeredDagDiagram(rows.to(List))

  private def connectorRow[node](layout: Layout[node], width: Int): List[DagTile] =
    val cells = Array.fill(width)(LayeredDagDiagram.Cell())

    def drawBend(topEntry: Int, bottomExit: Int, continuing: Boolean): Unit =
      if topEntry == bottomExit then
        cells(topEntry).top = true
        cells(topEntry).down = true
        if continuing then cells(topEntry).verticalPassThrough = true
      else if topEntry < bottomExit then
        cells(topEntry).top = true
        cells(topEntry).right = true
        var c = topEntry + 1

        while c < bottomExit do
          cells(c).left = true
          cells(c).right = true
          cells(c).horizontalPassThrough = true
          c += 1

        cells(bottomExit).left = true
        cells(bottomExit).down = true
      else
        cells(topEntry).top = true
        cells(topEntry).left = true
        var c = bottomExit + 1

        while c < topEntry do
          cells(c).left = true
          cells(c).right = true
          cells(c).horizontalPassThrough = true
          c += 1

        cells(bottomExit).right = true
        cells(bottomExit).down = true

    layout.state.foreach: (col, lane) =>
      val justStarted = layout.prevNodeCol.contains(lane.source)
      val terminatingNow = layout.terminating.contains(col)

      (justStarted, terminatingNow) match
        case (true, true) =>
          drawBend(layout.prevNodeCol(lane.source), layout.nodeCol(lane.target), false)

        case (false, true)  => drawBend(col, layout.nodeCol(lane.target), false)
        case (true, false)  => drawBend(layout.prevNodeCol(lane.source), col, false)
        case (false, false) => drawBend(col, col, true)

    cells.iterator.map(_.tile).to(List)

  private def nodeRow[node](layout: Layout[node], width: Int): List[DagTile] =
    val nodeColSet = layout.nodeCol.values.to[Set]

    (0 until width).map: c =>
      if nodeColSet(c) then Node
      else if layout.continuing.contains(c) then Vertical
      else Space

    .to(List)

  given printable: [node: Showable] => (style: LaneDagStyle[Text])
  =>  LayeredDagDiagram[node] is Printable =
    (diagram, termcap) => diagram.render[Text]{ node => t"● $node  " }.join(t"\n")

case class LayeredDagDiagram[node](rows: List[(List[DagTile], Map[Int, node])]):
  val size: Int = rows.length

  def render[line](glyph: node => line)(using style: LaneDagStyle[line]): List[line] =
    val maxCol = rows.iterator.map(_(0).length).maxOption.getOrElse(0)
    val widths = Array.fill(maxCol)(2)

    rows.each: (_, nodesAt) =>
      nodesAt.foreach: (col, n) =>
        val w = style.width(glyph(n))
        if w > widths(col) then widths(col) = w

    val widthsList = widths.iterator.to(List)

    rows.map: (tiles, nodesAt) =>
      val glyphs: Map[Int, line] = nodesAt.map: (col, n) => col -> glyph(n)
      style.serialize(tiles, glyphs, widthsList, Unset)

  def tiles: List[List[DagTile]] = rows.map(_(0))
  def nodesAt: List[Map[Int, node]] = rows.map(_(1))
