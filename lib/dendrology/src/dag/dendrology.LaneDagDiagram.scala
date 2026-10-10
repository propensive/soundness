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

import scala.collection.immutable.Vector

// Deliberate stdlib opt-out: these internals consume acyclicity's `Dag`, whose set algebra
// remains on the stdlib `Set` for now.
import scala.collection.immutable.{List, Map, Nil, Set, ::}

import scala.collection.mutable as scm

import acyclicity.*
import anticipation.*
import contingency.*
import gossamer.*
import prepositional.*
import spectacular.*
import vacuous.*

import DagTile.*

object LaneDagDiagram:
  private case class Lane[node](source: node, target: node, col: Int)

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
        case (false, true, false, true)   => CornerSe
        case (false, true, true, false)   => CornerSw
        case (true, true, false, true)    => TeeE
        case (true, true, true, false)    => TeeW
        case (true, false, true, true)    => TeeN
        case (false, true, true, true)    => TeeS
        case (true, true, true, true)     => Junction
        case _                            => Space

  // Any graph that admits a topological order: a `Dag`, a frozen `Topology`, a `Hasse`.
  def apply[graph, node](dag: graph)
    ( using nodal:       graph is Nodal by node,
            topological: graph is Topological )
  :   LaneDagDiagram[node] =

    val nodes: Vector[node] = proscenium.List.iterator(dag.linearized).to(Vector)
    val total: Int = nodes.length

    if total == 0 then LaneDagDiagram(Nil) else
      val rowOf: Map[node, Int] = nodes.zipWithIndex.to(Map)
      // Dependants of each node, from the edges: `Dag` no longer exposes its adjacency.
      val forward: Map[node, Set[node]] =
        proscenium.Set.iterator(dag.edges).to(List).groupMap(_(1))(_(0)).view.mapValues(_.to(Set)).to(Map)

      val nodeCol: scala.Array[Int]^ = new scala.Array[Int](total)

      val laneState: scala.Array[Map[Int, Lane[node]]]^ =
        scala.Array.fill(total + 1)(Map.empty[Int, Lane[node]])

      val started: scala.Array[Vector[Lane[node]]]^ = scala.Array.fill(total)(Vector.empty[Lane[node]])
      val directOut: scala.Array[Boolean]^ = new scala.Array[Boolean](total)
      var r = 0

      while r < total do
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
        val targets: Vector[node] = forward.getOrElse(current, Set.empty).to(Vector).sortBy(rowOf)

        val (directs, indirects) = nextNode.lay((Vector.empty[node], targets)): nx =>
          targets.partition(_ == nx)

        directOut(r) = directs.nonEmpty

        val occupied = scm.HashSet.from(continuing.keys)

        val newLanes = indirects.map: target =>
          val col = nearestFree(chosenCol, occupied)
          occupied.add(col)
          Lane(current, target, col)

        started(r) = newLanes
        laneState(r + 1) = continuing ++ newLanes.map: lane => lane.col -> lane
        r += 1

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
      justStarted:  Vector[Lane[node]],
      prevNodeCol:  Int,
      curNodeCol:   Int,
      directEdge:   Boolean,
      currentNode:  node,
      width:        Int )
  :   List[DagTile] =

    val cells = new scala.Array[Int](width)
    val startedCols = justStarted.iterator.map(_.col).to(Set)

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

    state.foreach: (col, lane) =>
      if lane.target == currentNode then drawBend(col, curNodeCol, false)
      else if startedCols(col) then drawBend(prevNodeCol, col, false)
      else drawBend(col, col, true)

    if directEdge then drawBend(prevNodeCol, curNodeCol, false)

    cells.iterator.map(Cell.tile).to(List)

  private def nodeRow[node]
    ( state:    Map[Int, Lane[node]],
      col:      Int,
      current:  node,
      width:    Int )
  :   List[DagTile] =

    val continuing = state.filter{ (_, lane) => lane.target != current }.keys.to(Set)

    (0 until width).map: c => if c == col then Node else if continuing(c) then Vertical else Space
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
    val widths = scala.Array.fill(maxCol)(2)

    rows.foreach: (tiles, optNode) =>
      optNode.let: node =>
        val nodeIdx = tiles.indexOf(Node)

        if nodeIdx >= 0 then
          val w = style.width(glyph(node))
          if w > widths(nodeIdx) then widths(nodeIdx) = w

    widths.iterator.to(List)

case class LaneDagDiagram[node](lines: List[(List[DagTile], Optional[node])]):
  val size: Int = lines.length

  def render[line](label: node => line)(using style: LaneDagStyle[line]): List[line] =
    val widths = LaneDagDiagram.defaultWidths(lines.iterator.map(_(0)))

    lines.map: (tiles, node) =>
      style.serialize(tiles.to(proscenium.List), Map.empty, widths.to(proscenium.List), node.let(label))

  def render[line](glyph: node => line, label: node => line)(using style: LaneDagStyle[line])
  :   List[line] =

    val widths = LaneDagDiagram.computeWidths(lines, glyph, style)

    lines.map: (tiles, node) =>
      val nodeIdx = tiles.indexOf(Node)

      val glyphs: Map[Int, line] =
        if nodeIdx < 0 then Map.empty else node.let{ n => Map(nodeIdx -> glyph(n)) }.or(Map.empty)

      style.serialize
        ( tiles.to(proscenium.List), glyphs, widths.to(proscenium.List), node.let(label) )

  def compact: LaneDagDiagram[node] = LaneDagDiagram(lines.filter(LaneDagDiagram.keepRow))

  def nodes: List[node] = lines.flatMap(_(1).option)
  def tiles: List[List[DagTile]] = lines.map(_(0))
