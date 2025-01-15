/*
    Dendrology, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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

import rudiments.*
import anticipation.*
import gossamer.*
import spectacular.*

import TreeTile.*

object TreeDiagram:
  def apply[NodeType: Expandable](roots: NodeType*): TreeDiagram[NodeType] =
    by[NodeType](NodeType.children(_))(roots*)

  given [NodeType: Showable](using style: TreeStyle[Text]) => TreeDiagram[NodeType] is Printable =
    (diagram, termcap) =>
      (diagram.render[Text] { node => t"▪ $node" }).join(t"\n")

  def by[NodeType](getChildren: NodeType => Seq[NodeType])(roots: NodeType*): TreeDiagram[NodeType] =
    def recur(level: List[TreeTile], input: Seq[NodeType]): LazyList[(List[TreeTile], NodeType)] =
      val last = input.size - 1
      input.zipWithIndex.to(LazyList).flatMap: (item, idx) =>
        val tiles: List[TreeTile] = ((if idx == last then Last else Branch) :: level).reverse
        (tiles, item) #:: recur((if idx == last then Space else Extender) :: level, getChildren(item))

    new TreeDiagram(recur(Nil, roots))

case class TreeDiagram[NodeType](lines: LazyList[(List[TreeTile], NodeType)]):
  def render[LineType](line: NodeType => LineType)(using style: TreeStyle[LineType]): LazyList[LineType] =
    map[LineType] { node => style.serialize(tiles, line(node)) }

  def map[RowType](line: (tiles: List[TreeTile]) ?=> NodeType => RowType): LazyList[RowType] =
    lines.map(line(using _)(_))

  def nodes: LazyList[NodeType] = lines.map(_(1))
  def tiles: LazyList[List[TreeTile]] = lines.map(_(0))
