/*

    Litterateur, version 0.4.0. Copyright 2019-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package litterateur

import honeycomb.*, attributes.strings.given
import rudiments.*

import scala.collection.immutable.ListMap

given ToHtml[MdNode.Document] = HtmlConverter.convert(_)

object HtmlConverter:
  def outline(node: MdNode): Seq[Content[Flow]] = convert(Markdown.parse(headOutline(node).join("\n")))

  def slug(str: String): String = str.toLowerCase.replaceAll("[^a-z0-9]", "-").replaceAll("--*", "-")

  private def headOutline(node: MdNode): Seq[String] = node match
    case MdNode.Document(children*) =>
      children.flatMap(headOutline(_))
    case MdNode.Heading(level, children*) =>
      val text = inlineText(children)
      List(s"${" "*(2*level - 1)}- [${text}](#${slug(text)})")
    case MdNode.Text(str) =>
      List(str)
    case _ =>
      Nil
  
  def convert(node: MdNode): Seq[Content[Flow]] = node match
    case MdNode.Document(children*)        => children.flatMap(convert(_))
    case MdNode.Paragraph(children*)       => List(P(children.flatMap(convertInline(_))))
    case MdNode.Heading(level, children*)  =>
      val ident = slug(inlineText(children))
      val childNodes = children.flatMap(convertInline(_))
      level match
        case 1 => List(H1(id = ident)(childNodes))
        case 2 => List(H2(id = ident)(childNodes))
        case 3 => List(H3(id = ident)(childNodes))
        case 4 => List(H4(id = ident)(childNodes))
        case 5 => List(H5(id = ident)(childNodes))
        case 6 => List(H6(id = ident)(childNodes))
    case MdNode.Blockquote(children*)    => List(Blockquote(children.flatMap(convertInline(_))))
    case MdNode.ThematicBreak                => List(Hr)
    case MdNode.CodeBlock(syntax, MdNode.Text(str)) => List(Pre(Code(escape(str))))
    case MdNode.Reference(_, _)              => Nil
    case MdNode.Text(str)                    => List(P(escape(str)))
    case MdNode.BulletList(children*)      => List(Ul(children.flatMap(convertListItem(_))))
    case MdNode.OrderedList(children*)     => List(Ol(children.flatMap(convertListItem(_))))
    case MdNode.Table(parts*)            => List(Table(parts.flatMap(convertTableParts(_))))
    case MdNode.LineBreak                    => List(Br)

  def convertTableParts(node: TablePart) = node match
    case MdNode.TableHead(rows*) => List(Thead(convertTableRows(true, rows)))
    case MdNode.TableBody(rows*) => List(Tbody(convertTableRows(false, rows)))

  def convertTableRows(heading: Boolean, rows: Seq[MdNode.Row]) =
    rows.map { case MdNode.Row(cells*) => Tr(convertTableCells(heading, cells)) }

  def convertTableCells(heading: Boolean, cells: Seq[MdNode.Cell]) =
    cells.map { case MdNode.Cell(content*) => (if heading then Th else Td)(content.flatMap(convertInline(_))) }

  def convertListItem(node: MdNode): Seq[Content["li"]] = node match
    case MdNode.ListItem(children*) => List(Li(children.flatMap(convert(_))))
    case _                          => Nil

  def inlineText(node: Seq[MdNode]): String = node.map {
    case MdNode.Document(_*) | MdNode.BulletList(_*) | MdNode.Indented(_) | MdNode.OrderedList(_*) | MdNode.ListItem(_*) => ""
    case MdNode.Image(txt, _)                                                                => txt
    case MdNode.Link(txt, _)                                                               => txt
    case MdNode.Reference(_, _)                                                              => ""
    case MdNode.LineBreak                                                                    => ""
    case MdNode.ThematicBreak                                                                => ""
    case MdNode.Emphasis(children*)                                                          => inlineText(children)
    case MdNode.StrongEmphasis(children*)                                                    => inlineText(children)
    case MdNode.Code(children*)                                                            => inlineText(children)
    case MdNode.Paragraph(children*)                                                         => inlineText(children)
    case MdNode.Heading(_, children*)                                                        => inlineText(children)
    case MdNode.Blockquote(children*)                                                      => inlineText(children)
    case MdNode.CodeBlock(_, children*)                                                      => inlineText(children)
    case MdNode.Text(str)                                                                    => str
    case MdNode.Cell(content*)                                                               => inlineText(content)
  }.join

  def convertInline(node: MdNode): Seq[Content[Phrasing]] = node match
    case MdNode.Document(_*) | MdNode.BulletList(_*) | MdNode.Indented(_) | MdNode.OrderedList(_*) | MdNode.ListItem(_*) => Nil
    case MdNode.Image(txt, loc)             => List(Img(src = loc, alt = txt))
    case MdNode.Link(txt, loc)            => List(A(href = loc)(txt))
    case MdNode.Reference(_, _)             => Nil
    case MdNode.LineBreak                   => List(Br)
    case MdNode.ThematicBreak               => Nil
    case MdNode.Emphasis(children*)       => List(Em(children.flatMap(convertInline(_))))
    case MdNode.StrongEmphasis(children*) => List(Strong(children.flatMap(convertInline(_))))
    case MdNode.Code(children*)           => List(Code(children.flatMap(convertInline(_))))
    case MdNode.Paragraph(children*)      => children.flatMap(convertInline(_))
    case MdNode.Heading(_, children*)     => children.flatMap(convertInline(_))
    case MdNode.Blockquote(children*)     => children.flatMap(convertInline(_))
    case MdNode.CodeBlock(_, children*)   => children.flatMap(convertInline(_))
    case MdNode.Text(str)                   => List(escape(str))

  def escape(str: String): String = str.replaceAll("&", "&amp;").replaceAll("<", "&lt;").replaceAll(">", "&gt;")