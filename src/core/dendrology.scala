package dendrology

import rudiments.*
import gossamer.*

object TreeTile:
  given Show[TreeTile] =
    case TreeTile.Space    => t"  "
    case TreeTile.Last     => t"└─"
    case TreeTile.Branch   => t"├─"
    case TreeTile.Extender => t"│ "

enum TreeTile:
  case Space, Last, Branch, Extender

def textualizeTree[S, T](getChildren: S => Seq[S], mkLine: (List[TreeTile], S) => T)(top: Seq[S]): LazyList[T] =
  def recur(level: List[TreeTile], input: Seq[S]): LazyList[T] =
    val last = input.size - 1
    input.zipWithIndex.to(LazyList).flatMap: (item, idx) =>
      val current = mkLine(((if idx == last then TreeTile.Last else TreeTile.Branch) :: level).reverse, item)
      current #:: recur((if idx == last then TreeTile.Space else TreeTile.Extender) :: level, getChildren(item))

  recur(Nil, top)
