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

def textualizeTree[N, L](getChildren: N => Seq[N], mkLine: (List[TreeTile], N) => L)(top: Seq[N]): LazyList[L] =
  def recur(level: List[TreeTile], input: Seq[N]): LazyList[L] =
    val last = input.size - 1
    input.zipWithIndex.to(LazyList).flatMap: (item, idx) =>
      val current = mkLine(((if idx == last then TreeTile.Last else TreeTile.Branch) :: level).reverse, item)
      current #:: recur((if idx == last then TreeTile.Space else TreeTile.Extender) :: level, getChildren(item))

  recur(Nil, top)

