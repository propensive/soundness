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

import language.experimental.captureChecking

package treeStyles:
  given default: TreeStyle = TreeStyle(Text("  "), Text("└─"), Text("├─"), Text("│ "))
  given rounded: TreeStyle = TreeStyle(Text("  "), Text("╰─"), Text("├─"), Text("│ "))
  given ascii: TreeStyle   = TreeStyle(Text("  "), Text("+-"), Text("|-"), Text("| "))

case class TreeStyle(space: Text, last: Text, branch: Text, extender: Text)

enum TreeTile:
  case Space, Last, Branch, Extender

  def text(using style: TreeStyle): Text = this match
    case Space    => style.space
    case Last     => style.last
    case Branch   => style.branch
    case Extender => style.extender

import TreeTile.*

def drawTree
    [NodeType, LineType]
    (getChildren: NodeType => Seq[NodeType], line: (List[TreeTile], NodeType) => LineType)
    (top: Seq[NodeType])
    : LazyList[LineType] =
  def recur(level: List[TreeTile], input: Seq[NodeType]): LazyList[LineType] =
    val last = input.size - 1
    input.zipWithIndex.to(LazyList).flatMap: (item, idx) =>
      val current = line(((if idx == last then Last else Branch) :: level).reverse, item)
      current #:: recur((if idx == last then Space else Extender) :: level, getChildren(item))

  recur(Nil, top)

