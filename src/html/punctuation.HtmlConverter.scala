/*
    Punctuation, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package punctuation

import anticipation.*
import fulminate.*
import gossamer.*
import honeycomb.*, html5.*
import rudiments.*
import vacuous.*

open class HtmlConverter(renderers: Renderer*):
  def slug(str: Text): Text =
    str.lower.s.replaceAll("[^a-z0-9]", "-").nn.replaceAll("--*", "-").nn.tt

  lazy val renderersMap: Map[Optional[Text], Renderer] = renderers.indexBy(_.language)

  private val headings = IArray(H1, H2, H3, H4, H5, H6)

  private def heading(level: 1 | 2 | 3 | 4 | 5 | 6, children: Seq[Markdown.Ast.Inline]) =
    headings(level - 1)(id = DomId(slug(text(children))))(children.flatMap(phrasing))

  def blockify(nodes: Seq[Markdown.Ast.Node]): Seq[Markdown.Ast.Block] =
    val acc = nodes.foldLeft((true, List[Markdown.Ast.Block]())):
      case ((fresh, acc), next) => next match
        case node: Markdown.Ast.Block  => (true, node :: acc)

        case node: Markdown.Ast.Inline =>
          if fresh then (false, Markdown.Ast.Block.Paragraph(node) :: acc) else
            val content = acc.head.absolve match
              case Markdown.Ast.Block.Paragraph(nodes*) =>
                Markdown.Ast.Block.Paragraph((nodes :+ node)*) :: acc.tail

            (false, content)

    acc(1).reverse

  def convert(nodes: Seq[Markdown.Ast.Node]): Seq[Html[Flow]] =
    blockify(nodes).foldLeft(List[Html[Flow]]())(_ ++ convertNode(_))

  def convertNode(node: Markdown.Ast.Block): Seq[Html[Flow]] = node match
    case Markdown.Ast.Block.Paragraph(children*)      => Seq(P(children.flatMap(phrasing)*))
    case Markdown.Ast.Block.Heading(level, children*) => Seq(heading(level, children))
    case Markdown.Ast.Block.Blockquote(children*)     => Seq(Blockquote(convert(children)*))
    case Markdown.Ast.Block.ThematicBreak()           => Seq(Hr)
    case Markdown.Ast.Block.Reference(_, _)           => Seq()
    case Markdown.Ast.Block.Table(parts*)             => Seq(Table(parts.flatMap(tableParts)))

    case Markdown.Ast.Block.FencedCode(syntax, meta, value) =>
      renderersMap.get(syntax).optional.lay(Seq(Div.amok(Pre(html5.Code(escape(value)))))): renderer =>
        renderer.render(meta, value)

    case Markdown.Ast.Block.BulletList(num, _, items*) =>
      Seq((if num.absent then Ul else Ol)(items.flatMap(listItem)*))

    case other =>
      Seq()

  def tableParts(node: Markdown.Ast.TablePart): Seq[Node["thead" | "tbody"]] = node match
    case Markdown.Ast.TablePart.Head(rows*) => List(Thead(tableRows(true, rows)))
    case Markdown.Ast.TablePart.Body(rows*) => List(Tbody(tableRows(false, rows)))

  def tableRows(heading: Boolean, rows: Seq[Markdown.Ast.Block.Row]): Seq[Node["tr"]] = rows.map:
    case Markdown.Ast.Block.Row(cells*) => Tr(tableCells(heading, cells))

  def tableCells(heading: Boolean, cells: Seq[Markdown.Ast.Block.Cell]): Seq[Node["th" | "td"]] = cells.map:
    case Markdown.Ast.Block.Cell(content*) => (if heading then Th else Td)(content.flatMap(phrasing))

  def listItem(node: Markdown.Ast.ListItem): Seq[Node["li"]] = node match
    case Markdown.Ast.ListItem(children*) => List(Li(convert(children)*))

  def text(node: Seq[Markdown.Ast.Node]): Text = node.map(textNode(_)).join

  def textNode(node: Markdown.Ast.Node): Text = node match
    case Markdown.Ast.Block.BulletList(_, _, _, _*) => t""
    case Markdown.Ast.Inline.Image(text, _)         => text
    case Markdown.Ast.Inline.Weblink(text, _)       => text
    case Markdown.Ast.Block.Reference(_, _)         => t""
    case Markdown.Ast.Inline.LineBreak              => t""
    case Markdown.Ast.Block.ThematicBreak()         => t""
    case Markdown.Ast.Inline.Emphasis(children*)    => text(children)
    case Markdown.Ast.Inline.Strong(children*)      => text(children)
    case Markdown.Ast.Inline.SourceCode(code)       => code.broken(_.isLetterOrDigit != _.isLetterOrDigit)
    case Markdown.Ast.Block.Paragraph(children*)    => text(children)
    case Markdown.Ast.Block.Heading(_, children*)   => text(children)
    case Markdown.Ast.Block.Blockquote(children*)   => text(children)
    case Markdown.Ast.Block.FencedCode(_, _, code)  => code
    case Markdown.Ast.Inline.Copy(text)             => text
    case Markdown.Ast.Block.Cell(content*)          => text(content)
    case _                                          => t""

  def nonInteractive(node: Markdown.Ast.Inline): Seq[Html[Phrasing]] = node match
    case Markdown.Ast.Inline.Image(altText, location) => List(Img(src = location, alt = altText))
    case Markdown.Ast.Inline.LineBreak                => List(Br)
    case Markdown.Ast.Inline.Emphasis(children*)      => List(Em(children.flatMap(phrasing)))
    case Markdown.Ast.Inline.Strong(children*)        => List(Strong(children.flatMap(phrasing)))
    case Markdown.Ast.Inline.SourceCode(code)         => List(html5.Code(code.broken(_.isLetterOrDigit != _.isLetterOrDigit)))
    case Markdown.Ast.Inline.Copy(str)                => List(escape(str))
    case _                                            => Nil

  def phrasing(node: Markdown.Ast.Inline): Seq[Html[Phrasing]] = node match
    case Markdown.Ast.Inline.Weblink(location, content) =>

      def interactive(node: Html[Phrasing]): Option[Html[NonInteractive]] = node.absolve match
        case node: Node[NonInteractive] => Some(node)
        case text: Text                 => Some(text)
        case int: Int                   => Some(int)

      val children: Seq[Html[NonInteractive]] = nonInteractive(content).flatMap(interactive(_))

      List(A(href = location)(children))

    case other =>
      nonInteractive(other)

  def escape(str: Text): Text = str.sub(t"&", t"&amp;").sub(t"<", t"&lt;").sub(t">", t"&gt;")
