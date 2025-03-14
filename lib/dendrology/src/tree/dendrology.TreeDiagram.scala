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
┃    Soundness, version 0.27.0.                                                                    ┃
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

import language.experimental.captureChecking

import anticipation.*
import gossamer.*
import proscenium.*
import spectacular.*

import TreeTile.*

object TreeDiagram:
  def apply[NodeType: Expandable](roots: NodeType*): TreeDiagram[NodeType] =
    by[NodeType](NodeType.children(_))(roots*)

  given [NodeType: Showable] => (style: TreeStyle[Text]) => TreeDiagram[NodeType] is Printable =
    (diagram, termcap) =>
      (diagram.render[Text] { node => t"▪ $node" }).join(t"\n")

  def by[NodeType](getChildren: NodeType => Seq[NodeType])(roots: NodeType*)
  :     TreeDiagram[NodeType] =
    def recur(level: List[TreeTile], input: Seq[NodeType]): Stream[(List[TreeTile], NodeType)] =
      val last = input.size - 1
      input.zipWithIndex.to(Stream).flatMap: (item, idx) =>
        val tiles: List[TreeTile] = ((if idx == last then Last else Branch) :: level).reverse

        (tiles, item)
        #:: recur((if idx == last then Space else Extender) :: level, getChildren(item))

    new TreeDiagram(recur(Nil, roots))

case class TreeDiagram[NodeType](lines: Stream[(List[TreeTile], NodeType)]):
  def render[LineType](line: NodeType => LineType)(using style: TreeStyle[LineType])
  :     Stream[LineType] =
    map[LineType] { node => style.serialize(tiles, line(node)) }

  def map[RowType](line: (tiles: List[TreeTile]) ?=> NodeType => RowType): Stream[RowType] =
    lines.map(line(using _)(_))

  def nodes: Stream[NodeType] = lines.map(_(1))
  def tiles: Stream[List[TreeTile]] = lines.map(_(0))
