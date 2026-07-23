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

    val layout: Array[Array[Int]] = Array.from:
      nodes.indices.map: i =>
        Array.range(0, i).map(_ => 0)

    dag.edges.map: (source, destination) =>
      val si = indexes(source)
      val di = indexes(destination)

      layout(si)(di) |= 1

      for i <- (di + 1) until si do
        layout(i)(di) |= 2
        layout(si)(i) |= 4

    DagDiagram:
      layout.iterator.to(List).map: row =>
        val tiles = row.iterator.to(List).map(DagTile.fromOrdinal)
        (tiles, nodes(row.length))

  given printable: [node: Showable] => (style: DagStyle[Text]) => DagDiagram[node] is Printable =
    (diagram, termcap) => (diagram.render[Text] { node => t"▪ $node" }).join(t"\n")

case class DagDiagram[node](lines: List[(List[DagTile], node)]):
  val size: Int = lines.length

  def render[line](line: node => line)(using style: DagStyle[line]): List[line] =
    lines.map: (tiles, node) => style.serialize(proscenium.List.of(tiles), line(node))

  def nodes: List[node] = lines.map(_(1))
  def tiles: List[List[DagTile]] = lines.map(_(0))
