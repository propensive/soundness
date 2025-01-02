/*
    Dendrology, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package dendrology

import language.experimental.captureChecking

import scala.reflect.*

import acyclicity.*
import anticipation.*
import gossamer.*
import rudiments.*
import spectacular.*

import DagTile.*

object DagDiagram:
  def apply[NodeType: ClassTag](dag: Dag[NodeType]): DagDiagram[NodeType] =
    val nodes = Array.from(dag.sorted)
    val indexes: Map[NodeType, Int] = nodes.zipWithIndex.to(Map)

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
      layout.to(List).map: row =>
        val tiles = row.to(List).map(DagTile.fromOrdinal)
        (tiles, nodes(row.length))

  given [NodeType: Showable](using style: DagStyle[Text]) => DagDiagram[NodeType] is Printable =
    (diagram, termcap) => (diagram.render[Text] { node => t"▪ $node" }).join(t"\n")

case class DagDiagram[NodeType](lines: List[(List[DagTile], NodeType)]):
  val size: Int = lines.length
  def render[LineType](line: NodeType => LineType)(using style: DagStyle[LineType]): List[LineType] =
    lines.map { (tiles, node) => style.serialize(tiles, line(node)) }

  def nodes: List[NodeType] = lines.map(_(1))
  def tiles: List[List[DagTile]] = lines.map(_(0))
