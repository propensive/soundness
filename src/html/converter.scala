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

given ToHtml[Markdown, Flow] = HtmlConverter().convert(_)
given ToHtml[PhrasingContent, Phrasing] = HtmlConverter().phrasing(_)

open class HtmlConverter():
  def outline(node: Markdown): Seq[Content[Flow]] = convert(Markdown.parse[FlowContent](headOutline(node).join("\n")))
  def slug(str: String): String = str.toLowerCase.replaceAll("[^a-z0-9]", "-").replaceAll("--*", "-")

  def headOutline(node: Markdown): Seq[String] = node match
    case Markdown.Root(children*) =>
      children.flatMap(headOutline(_))
    case Markdown.Heading(level, children*) =>
      val string = text(children)
      List(str"${" "*(2*level - 1)}- [$string](#${slug(string)})")
    case Markdown.Text(str) =>
      List(str)
    case _ =>
      Nil

  private val headings = IArray(H1, H2, H3, H4, H5, H6)
  
  private def heading(level: 1 | 2 | 3 | 4 | 5 | 6, children: Seq[PhrasingContent]) =
    headings(level - 1)(id = slug(text(children)))(children.flatMap(phrasing))

  def convert(node: Markdown): Seq[Content[Flow]] = node match
    case Markdown.Root(children*)               => children.flatMap(convert)
    case Markdown.Paragraph(children*)          => List(P(children.flatMap(phrasing)))
    case Markdown.Heading(level, children*)     => List(heading(level, children))
    case Markdown.Blockquote(children*)         => List(Blockquote(children.flatMap(convert)))
    case Markdown.ThematicBreak()               => List(Hr)
    case Markdown.Code(syntax, meta, value)     => List(Pre(Code(escape(value))))
    case Markdown.Reference(_, _)               => Nil
    case Markdown.Text(str)                     => List(P(escape(str)))
    case Markdown.MdList(ordered, _, _, items*) => List((if ordered then Ol else Ul)(items.flatMap(listItem)*))
    case Markdown.Table(parts*)                 => List(Table(parts.flatMap(tableParts)))
    case Markdown.Break()                       => List(Br)
    case _                                      => Nil

  def tableParts(node: TablePart): Seq[Item["thead" | "tbody"]] = node match
    case Markdown.TableHead(rows*) => List(Thead(tableRows(true, rows)))
    case Markdown.TableBody(rows*) => List(Tbody(tableRows(false, rows)))

  def tableRows(heading: Boolean, rows: Seq[Markdown.Row]): Seq[Item["tr"]] =
    rows.map { case Markdown.Row(cells*) => Tr(tableCells(heading, cells)) }

  def tableCells(heading: Boolean, cells: Seq[Markdown.Cell]): Seq[Item["th" | "td"]] =
    cells.map { case Markdown.Cell(content*) => (if heading then Th else Td)(content.flatMap(phrasing)) }

  def listItem(node: ListContent): Seq[Item["li"]] = node match
    case Markdown.ListItem(children*) => List(Li(children.flatMap(convert)))

  def text(node: Seq[Markdown]): String = node.map {
    case Markdown.Root(_*)              => ""
    case Markdown.MdList(_, _, _, _*)   => ""
    case Markdown.ListItem(_*)          => ""
    case Markdown.Image(text, _)        => text
    case Markdown.Link(text, _)         => text
    case Markdown.Reference(_, _)       => ""
    case Markdown.Break()               => ""
    case Markdown.ThematicBreak()       => ""
    case Markdown.Emphasis(children*)   => text(children)
    case Markdown.Strong(children*)     => text(children)
    case Markdown.InlineCode(code)      => code
    case Markdown.Paragraph(children*)  => text(children)
    case Markdown.Heading(_, children*) => text(children)
    case Markdown.Blockquote(children*) => text(children)
    case Markdown.Code(_, _, code)      => code
    case Markdown.Text(text)            => text
    case Markdown.Cell(content*)        => text(content)
    case _                              => ""
  }.join

  def nonInteractive(node: PhrasingContent): Seq[Content[Phrasing]] = node match
    case Markdown.Image(altText, location) => List(Img(src = location, alt = altText))
    case Markdown.Break()                  => List(Br)
    case Markdown.Emphasis(children*)      => List(Em(children.flatMap(phrasing)))
    case Markdown.Strong(children*)        => List(Strong(children.flatMap(phrasing)))
    case Markdown.InlineCode(code)         => List(Code(code))
    case Markdown.Text(str)                => List(escape(str))
    case _                                 => Nil

  def phrasing(node: PhrasingContent): Seq[Content[Phrasing]] = node match
    case Markdown.Link(location, content) =>
      val children = nonInteractive(content).collect { case node: NonInteractive => node }
      List(A(href = location)(children))
    
    case other =>
      nonInteractive(other)

  def escape(str: String): String = str.replaceAll("&", "&amp;").replaceAll("<", "&lt;").replaceAll(">", "&gt;")