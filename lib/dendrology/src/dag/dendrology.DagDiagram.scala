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

import scala.collection.immutable.List
import scala.collection.immutable.Vector

import acyclicity.*
import anticipation.*
import gossamer.*
import prepositional.*
import spectacular.*

object DagDiagram:
  // Any graph that admits a topological order: a `Dag`, a frozen `Topology`, a `Hasse`.
  def apply[graph, node](dag: graph)
    ( using nodal:       graph is Nodal by node,
            topological: graph is Topological )
  :   DagDiagram[node] =

    val nodes = proscenium.List.iterator(dag.linearized).to(Vector)
    val indexes: scala.collection.immutable.Map[node, Int] = nodes.zipWithIndex.toMap

    // A flat exclusive scratch array rather than a nested `Array[Array[Int]]`: writing
    // through an element read of a nested array is rejected by separation checking.
    val n = nodes.length
    val layout: scala.Array[Int]^ = new scala.Array[Int](n*n)
    var rest = proscenium.Set.iterator(dag.edges).to(List)

    while rest.nonEmpty do
      val (source, destination) = rest.head
      val si = indexes(source)
      val di = indexes(destination)

      layout(si*n + di) |= 1
      var i = di + 1

      while i < si do
        layout(i*n + di) |= 2
        layout(si*n + i) |= 4
        i += 1

      rest = rest.tail

    DagDiagram:
      List.tabulate(n): row =>
        val tiles = List.tabulate(row): col => DagDiagram.Tile.fromOrdinal(layout(row*n + col))
        (tiles, nodes(row))

  given printable: [node: Showable] => (style: Style[Text]) => DagDiagram[node] is Printable =
    (diagram, termcap) => (diagram.render[Text] { node => t"▪ $node" }).join(t"\n")

  enum Tile:
    case Space, CornerNe, Vertical, TeeE, Horizontal, TeeN, Crossing, Junction
    case CornerNw, CornerSe, CornerSw, TeeW, TeeS, Node

  trait Style[line] extends beneficence.Findable:
    def serialize(tiles: proscenium.List[Tile], node: line): line

  case class TextualStyle[line: Textual as textual]
    ( space:      Text,
      cornerNe:   Text,
      vertical:   Text,
      teeE:       Text,
      horizontal: Text,
      teeN:       Text,
      crossing:   Text,
      junction:   Text )
  extends Style[line]:
    import anticipation.*
    import gossamer.*
    import gossamer.Textual.concatenable
    import rudiments.*
    import symbolism.*
    import Tile.*

    def serialize(tiles: proscenium.List[Tile], node: line): line =
      textual(tiles.map(text(_)).join)+node

    def text(tile: Tile): Text = tile match
      case Space      => space
      case CornerNe   => cornerNe
      case Vertical   => vertical
      case TeeE       => teeE
      case Horizontal => horizontal
      case TeeN       => teeN
      case Crossing   => crossing
      case Junction   => junction
      case _          => space

    def followOnText(tile: Tile): Text = tile match
      case Space | Horizontal | CornerNe | TeeN | Junction => space
      case _                                               => vertical

case class DagDiagram[node](lines: List[(List[DagDiagram.Tile], node)]):
  val size: Int = lines.length

  def render[line](line: node => line)(using style: DagDiagram.Style[line]): List[line] =
    lines.map: (tiles, node) => style.serialize(tiles.to(proscenium.List), line(node))

  def nodes: List[node] = lines.map(_(1))
  def tiles: List[List[DagDiagram.Tile]] = lines.map(_(0))
