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
import spectacular.*
import vacuous.*

import DagTile.*

object LaneDagDiagram:
  private case class Lane[node](source: node, target: node, col: Int)

  private final class Cell:
    var top: Boolean = false
    var down: Boolean = false
    var left: Boolean = false
    var right: Boolean = false

    // Distinguish pure crossings (where a horizontal lane passes over a continuing
    // vertical lane without sharing a node) from real junctions where lanes meet.
    var verticalPassThrough: Boolean = false
    var horizontalPassThrough: Boolean = false

    def tile: DagTile =
      if verticalPassThrough && horizontalPassThrough then Crossing
      else (top, down, left, right) match
        case (false, false, false, false) => Space
        case (true, true, false, false)   => Vertical
        case (false, false, true, true)   => Horizontal
        case (true, false, false, true)   => CornerNe
        case (true, false, true, false)   => CornerNw
        case (false, true, false, true)   => CornerSe
        case (false, true, true, false)   => CornerSw
        case (true, true, false, true)    => TeeE
        case (true, true, true, false)    => TeeW
        case (true, false, true, true)    => TeeN
        case (false, true, true, true)    => TeeS
        case (true, true, true, true)     => Junction
        case _                            => Space

  def apply[node](dag: Dag[node]): LaneDagDiagram[node] raises DagError =
    val nodes: Series[node] = dag.sorted.to(Series)
    val total: Int = nodes.length

    if total == 0 then LaneDagDiagram(Nil) else
      val rowOf: Map[node, Int] = nodes.zipWithIndex.to(Map)
      val forward: Map[node, Set[node]] = dag.invert.edgeMap

      val nodeCol: Array[Int] = new Array[Int](total)
      val laneState: Array[Map[Int, Lane[node]]] = Array.fill(total + 1)(Map.empty[Int, Lane[node]])
      val started: Array[Series[Lane[node]]] = Array.fill(total)(Series.empty[Lane[node]])
      val directOut: Array[Boolean] = new Array[Boolean](total)

      for r <- 0 until total do
        val current = nodes(r)
        val state = laneState(r)
        val terminating = state.filter: (_, lane) => lane.target == current
        val continuing = state -- terminating.keys
        val terminatingCols = terminating.keys.iterator.toArray
        java.util.Arrays.sort(terminatingCols)

        val chosenCol: Int =
          if terminatingCols.length > 0 then terminatingCols(terminatingCols.length / 2)
          else
            var c = 0
            while continuing.contains(c) do c += 1
            c

        nodeCol(r) = chosenCol

        val nextNode: Optional[node] = if r + 1 < total then nodes(r + 1) else Unset
        val targets: Series[node] = forward.getOrElse(current, Set.empty).to(Series).sortBy(rowOf)

        val (directs, indirects) = nextNode.lay((Series.empty[node], targets)): nx =>
          targets.partition(_ == nx)

        directOut(r) = directs.nonEmpty

        val occupied = scm.HashSet.from(continuing.keys)

        val newLanes = indirects.map: target =>
          val col = nearestFree(chosenCol, occupied)
          occupied.add(col)
          Lane(current, target, col)

        started(r) = newLanes
        laneState(r + 1) = continuing ++ newLanes.map: lane => lane.col -> lane

      val width: Int =
        val colsUsed = laneState.flatMap(_.keys) ++ nodeCol
        if colsUsed.isEmpty then 1 else colsUsed.iterator.max + 1

      val rows = scm.ListBuffer[(List[DagTile], Optional[node])]()

      for r <- 0 until total do
        if r > 0 then
          rows += ((connectorRow(
            laneState(r),
            started(r - 1),
            nodeCol(r - 1),
            nodeCol(r),
            directOut(r - 1),
            nodes(r),
            width), Unset))

        rows += ((nodeRow(laneState(r), nodeCol(r), nodes(r), width), nodes(r)))

      LaneDagDiagram(rows.to(List))

  private def nearestFree(center: Int, taken: scm.HashSet[Int]): Int =
    if !taken(center) then center else
      var i = 1
      var found = -1

      while found < 0 do
        val low = center - i

        if low >= 0 && !taken(low) then found = low
        else
          val high = center + i
          if !taken(high) then found = high

        i += 1

      found

  private def connectorRow[node]
    ( state:        Map[Int, Lane[node]],
      justStarted:  Series[Lane[node]],
      prevNodeCol:  Int,
      curNodeCol:   Int,
      directEdge:   Boolean,
      currentNode:  node,
      width:        Int )
  :   List[DagTile] =

    val cells = Array.fill(width)(LaneDagDiagram.Cell())
    val startedCols = justStarted.iterator.map(_.col).to(Set)

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

    state.foreach: (col, lane) =>
      if lane.target == currentNode then drawBend(col, curNodeCol, false)
      else if startedCols(col) then drawBend(prevNodeCol, col, false)
      else drawBend(col, col, true)

    if directEdge then drawBend(prevNodeCol, curNodeCol, false)

    cells.iterator.map(_.tile).to(List)

  private def nodeRow[node]
    ( state:    Map[Int, Lane[node]],
      col:      Int,
      current:  node,
      width:    Int )
  :   List[DagTile] =

    val continuing = state.filter{ (_, lane) => lane.target != current }.keys.to(Set)

    (0 until width).map: c =>
      if c == col then Node
      else if continuing(c) then Vertical
      else Space

    . to(List)

  given printable: [node: Showable] => (style: LaneDagStyle[Text])
  =>  LaneDagDiagram[node] is Printable =
    (diagram, termcap) => diagram.render[Text]{ node => t" $node" }.join(t"\n")

  private def keepRow[node](row: (List[DagTile], Optional[node])): Boolean =
    val (tiles, node) = row

    if node.present then true else
      val onlyPassThrough = tiles.forall: tile => tile == Vertical || tile == Space
      val verticalCount = tiles.count(_ == Vertical)
      !(onlyPassThrough && verticalCount != 1)

  private def defaultWidths(rows: Iterator[List[DagTile]]): List[Int] =
    val maxCol = rows.map(_.length).maxOption.getOrElse(0)
    List.fill(maxCol)(2)

  private def computeWidths[node, line]
    ( rows:  List[(List[DagTile], Optional[node])],
      glyph: node => line,
      style: LaneDagStyle[line] )
  :   List[Int] =

    val maxCol = rows.iterator.map(_(0).length).maxOption.getOrElse(0)
    val widths = Array.fill(maxCol)(2)

    rows.foreach: (tiles, optNode) =>
      if optNode.present then
        val nodeIdx = tiles.indexOf(Node)

        if nodeIdx >= 0 then
          val w = style.width(glyph(optNode.vouch))
          if w > widths(nodeIdx) then widths(nodeIdx) = w

    widths.iterator.to(List)

case class LaneDagDiagram[node](lines: List[(List[DagTile], Optional[node])]):
  val size: Int = lines.length

  def render[line](label: node => line)(using style: LaneDagStyle[line]): List[line] =
    val widths = LaneDagDiagram.defaultWidths(lines.iterator.map(_(0)))
    lines.map: (tiles, node) => style.serialize(tiles, Map.empty, widths, node.let(label))

  def render[line](glyph: node => line, label: node => line)(using style: LaneDagStyle[line])
  :   List[line] =

    val widths = LaneDagDiagram.computeWidths(lines, glyph, style)

    lines.map: (tiles, node) =>
      val nodeIdx = tiles.indexOf(Node)

      val glyphs: Map[Int, line] =
        if nodeIdx < 0 then Map.empty
        else node.let{ n => Map(nodeIdx -> glyph(n)) }.or(Map.empty)

      style.serialize(tiles, glyphs, widths, node.let(label))

  def compact: LaneDagDiagram[node] = LaneDagDiagram(lines.filter(LaneDagDiagram.keepRow))

  def nodes: List[node] = lines.flatMap(_(1).option)
  def tiles: List[List[DagTile]] = lines.map(_(0))
