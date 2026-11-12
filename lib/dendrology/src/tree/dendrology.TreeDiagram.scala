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
import beneficence.*
import denominative.*
import denominative.dysasymptotics.linearSize
import gossamer.*
import gossamer.Textual.concatenable
import hieroglyph.*
import polysyllabic.*
import rudiments.*
import spectacular.*
import symbolism.*
import tessellate.*

import TreeDiagram.Tile.*

object TreeDiagram:
  def apply[node: Expandable](roots: node*): TreeDiagram[node] =
    by[node](node.children(_))(roots*)

  given printable: [node: Showable] => (style: Style[Text]) => TreeDiagram[node] is Printable =
    (diagram, termcap) => (diagram.render[Text] { node => t"▪ $node" }).join(t"\n")

  def by[node](getChildren: node => List[node])(roots: node*): TreeDiagram[node] =
    def recur(level: List[Tile], input: List[node]): Chain[(List[Tile], node)] =
      val last = input.size - 1

      input.indexed.to[Chain].flatMap: (item, index) =>
        val tiles: List[TreeDiagram.Tile] =
          ((if index.n0 == last then Last else Branch) :: level).reverse

        ((tiles, item) #::
          recur((if index.n0 == last then Space else Extender) :: level, getChildren(item)))
        : Chain[(List[TreeDiagram.Tile], node)]

    new TreeDiagram(recur(Nil, roots))

  enum Tile:
    case Space, Last, Branch, Extender

  trait Style[line] extends Findable:
    def serialize(tiles: List[Tile], node: line): line

    // A continuation row of a multi-line node: the same lanes, but with no branch glyph —
    // extenders continue and a last child's branch gives way to space. A style that draws no
    // distinct continuation prefix repeats its ordinary serialization.
    def followOn(tiles: List[Tile], node: line): line = serialize(tiles, node)

  case class TextualStyle[line: Textual as textual]
    ( space: Text, last: Text, branch: Text, extender: Text )
  extends Style[line]:
    def serialize(tiles: List[Tile], node: line): line =
      textual.apply(tiles.map(text(_)).join)+node

    override def followOn(tiles: List[Tile], node: line): line =
      textual.apply(tiles.map(followOnText(_)).join)+node

    def text(tile: Tile): Text = tile match
      case Tile.Space    => space
      case Tile.Last     => last
      case Tile.Branch   => branch
      case Tile.Extender => extender

    def followOnText(tile: Tile): Text = tile match
      case Tile.Space    => space
      case Tile.Last     => space
      case Tile.Branch   => extender
      case Tile.Extender => extender

case class TreeDiagram[node](lines: Chain[(List[TreeDiagram.Tile], node)]):
  def render[line](line: node => line)(using style: TreeDiagram.Style[line]): Chain[line] =
    map[line]: tiles => node => style.serialize(tiles, line(node))

  // Render with word wrapping: each node's content flows into rows no wider than `width`
  // (inclusive of its tile prefix), and continuation rows take the style's follow-on tiles —
  // extenders continue beneath a branch, and space follows a last child.
  def flow[line: Textual { type Result = Char }](width: Int)(line: node => line)
    ( using style: TreeDiagram.Style[line] )
    ( using Text is Measurable, Hyphenation )
  :   Chain[line] =

    lines.flatMap: (tiles, node) =>
      val content = line(node)
      val textual = summon[line is Textual]

      // Follow-on rows substitute tiles (a `Branch` becomes an `extender`, a `Last` becomes a
      // `space`), and nothing requires the substitutes to have the same display width, so both
      // prefixes are measured and the content wraps against the tighter budget — conservative
      // for the first row under a wider follow-on prefix, but no row can exceed `width`.
      val prefix = style.serialize(tiles, textual(t"")).plain.metrics
      val followOnPrefix = style.followOn(tiles, textual(t"")).plain.metrics

      Flow.wrap(content, (width - prefix.max(followOnPrefix)).max(1)).to[List].absolve match
        case Nil => Chain(style.serialize(tiles, content))

        case row :: rest =>
          val continuations = rest.map: row => style.followOn(tiles, row)
          (style.serialize(tiles, row) :: continuations).to[Chain]

  def map[row](line: List[TreeDiagram.Tile] => node => row): Chain[row] = lines.map(line(_)(_))
  def nodes: Chain[node] = lines.map(_(1))
  def tiles: Chain[List[TreeDiagram.Tile]] = lines.map(_(0))
