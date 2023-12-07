/*
    Dendrology, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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

import rudiments.*
import anticipation.*
import gossamer.*

import language.experimental.captureChecking

package treeStyles:
  given default[TextType: Textual]: TextualTreeStyle[TextType] =
    TextualTreeStyle(Text("  "), Text("└─"), Text("├─"), Text("│ "))
  
  given rounded[TextType: Textual]: TextualTreeStyle[TextType] =
    TextualTreeStyle(Text("  "), Text("╰─"), Text("├─"), Text("│ "))
  
  given ascii[TextType: Textual]: TextualTreeStyle[TextType] =
    TextualTreeStyle(Text("  "), Text("+-"), Text("|-"), Text("| "))

trait TreeStyle[LineType]:
  def serialize(tiles: List[TreeTile], node: LineType): LineType

case class TextualTreeStyle
    [LineType]
    (space: Text, last: Text, branch: Text, extender: Text)
    (using textual: Textual[LineType])
extends TreeStyle[LineType]:
  def serialize(tiles: List[TreeTile], node: LineType): LineType =
    textual.make(tiles.map(text(_)).join.s)+node
  
  def text(tile: TreeTile): Text = tile match
    case TreeTile.Space    => space
    case TreeTile.Last     => last
    case TreeTile.Branch   => branch
    case TreeTile.Extender => extender

  def followOnText(tile: TreeTile): Text = tile match
    case TreeTile.Space    => space
    case TreeTile.Last     => space
    case TreeTile.Branch   => extender
    case TreeTile.Extender => extender

enum TreeTile:
  case Space, Last, Branch, Extender

import TreeTile.*

object TreeDiagram:
  def apply[NodeType](roots: NodeType*)(using expandable: Expandable[NodeType]): TreeDiagram[NodeType] =
    by[NodeType](expandable.children(_))(roots*)

  def by
      [NodeType]
      (getChildren: NodeType => Seq[NodeType])
      (roots: NodeType*)
      : TreeDiagram[NodeType] =
    def recur(level: List[TreeTile], input: Seq[NodeType]): LazyList[(List[TreeTile], NodeType)] =
      val last = input.size - 1
      input.zipWithIndex.to(LazyList).flatMap: (item, idx) =>
        val tiles: List[TreeTile] = ((if idx == last then Last else Branch) :: level).reverse
        (tiles, item) #:: recur((if idx == last then Space else Extender) :: level, getChildren(item))
  
    new TreeDiagram(recur(Nil, roots))

case class TreeDiagram[NodeType](lines: LazyList[(List[TreeTile], NodeType)]):
  def render[LineType](line: NodeType => LineType)(using style: TreeStyle[LineType]): LazyList[LineType] =
    lines.map { (tiles, node) => style.serialize(tiles, line(node)) }
  
  def nodes: LazyList[NodeType] = lines.map(_(1))
  def tiles: LazyList[List[TreeTile]] = lines.map(_(0))

trait Expandable[NodeType]:
  def children(node: NodeType): List[NodeType]
