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

import anticipation.*
import gossamer.*
import rudiments.*
import spectacular.*

import TreeTile.*

object TreeDiagram:
  def apply[node: Expandable](roots: node*): TreeDiagram[node] =
    by[node](node.children(_))(roots*)

  given printable: [node: Showable] => (style: TreeStyle[Text]) => TreeDiagram[node] is Printable =
    (diagram, termcap) =>
      (diagram.render[Text] { node => t"▪ $node" }).stdlib.join(t"\n")

  def by[node](getChildren: node => List[node])(roots: node*): TreeDiagram[node] =
    def recur(level: List[TreeTile], input: List[node]): Progression[(List[TreeTile], node)] =
      val last = input.stdlib.size - 1

      input.stdlib.zipWithIndex.transmute[Progression].flatMap: (item, index) =>
        val tiles: List[TreeTile] =
          List.of(((if index == last then Last else Branch) :: level).reverse)

        ((tiles, item) #::
          recur((if index == last then Space else Extender) :: level, getChildren(item)))
        : Progression[(List[TreeTile], node)]

    new TreeDiagram(recur(Nil, List.from(roots)))

case class TreeDiagram[node](lines: Progression[(List[TreeTile], node)]):
  def render[line](line: node => line)(using style: TreeStyle[line]): Progression[line] = map[line]:
    tiles => node => style.serialize(tiles, line(node))

  def map[row](line: List[TreeTile] => node => row): Progression[row] = lines.map(line(_)(_))
  def nodes: Progression[node] = lines.map(_(1))
  def tiles: Progression[List[TreeTile]] = lines.map(_(0))
