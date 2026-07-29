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

import scala.collection.immutable.{List, Nil, ::}
import scala.reflect.*

import acyclicity.*
import anticipation.*
import contingency.*
import gossamer.*
import spectacular.*

object DagDiagram:
  def apply[node](dag: Dag[node]): DagDiagram[node] raises DagError =
    val nodes = dag.sorted.to(Vector)
    val indexes: scala.collection.immutable.Map[node, Int] = nodes.zipWithIndex.toMap

    // A flat exclusive scratch array rather than a nested `Array[Array[Int]]`: writing
    // through an element read of a nested array is rejected by separation checking.
    val n = nodes.length
    val layout: Array[Int]^ = new scala.Array[Int](n*n)
    var rest = dag.edges.to(List)

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
        val tiles = List.tabulate(row) { col => DagTile.fromOrdinal(layout(row*n + col)) }
        (tiles, nodes(row))

  given printable: [node: Showable] => (style: DagStyle[Text]) => DagDiagram[node] is Printable =
    (diagram, termcap) => (diagram.render[Text] { node => t"▪ $node" }).join(t"\n")

case class DagDiagram[node](lines: List[(List[DagTile], node)]):
  val size: Int = lines.length

  def render[line](line: node => line)(using style: DagStyle[line]): List[line] =
    lines.map: (tiles, node) => style.serialize(proscenium.List.of(tiles), line(node))

  def nodes: List[node] = lines.map(_(1))
  def tiles: List[List[DagTile]] = lines.map(_(0))
