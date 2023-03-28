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
import gossamer.*

package treeStyles:
  given default: TreeStyle = TreeStyle(t"  ", t"└─", t"├─", t"│ ")
  given rounded: TreeStyle = TreeStyle(t"  ", t"╰─", t"├─", t"│ ")
  given ascii: TreeStyle   = TreeStyle(t"  ", t"+-", t"|-", t"| ")

case class TreeStyle(space: Text, last: Text, branch: Text, extender: Text)

object TreeTile:
  given (using style: TreeStyle): Show[TreeTile] =
    case TreeTile.Space    => style.space
    case TreeTile.Last     => style.last
    case TreeTile.Branch   => style.branch
    case TreeTile.Extender => style.extender

enum TreeTile:
  case Space, Last, Branch, Extender

def drawTree[N, L](getChildren: N => Seq[N], mkLine: (List[TreeTile], N) => L)(top: Seq[N]): LazyList[L] =
  def recur(level: List[TreeTile], input: Seq[N]): LazyList[L] =
    val last = input.size - 1
    input.zipWithIndex.to(LazyList).flatMap: (item, idx) =>
      val current = mkLine(((if idx == last then TreeTile.Last else TreeTile.Branch) :: level).reverse, item)
      current #:: recur((if idx == last then TreeTile.Space else TreeTile.Extender) :: level, getChildren(item))

  recur(Nil, top)

