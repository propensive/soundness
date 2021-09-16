/*
    Punctuation, version 0.4.0. Copyright 2019-21 Jon Pretty, Propensive OÜ.

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

import honeycomb.*
import rudiments.*

import scala.collection.immutable.ListMap
import scala.annotation.targetName

extension (value: Markdown[Markdown.Ast.Node])
  def html: Seq[Content[Flow]] = HtmlConverter().convert(value.nodes)

extension (value: Markdown.Ast.Inline)
  @targetName("html2")
  def html: Seq[Content[Phrasing]] = HtmlConverter().phrasing(value)

open class HtmlConverter():
  def outline(node: Markdown[Markdown.Ast.Node]): Seq[Content[Flow]] =
    try convert(Markdown.parse(headOutline(node).join("\n")).nodes)
    catch case MalformedMarkdown(_) => Nil
  
  def slug(str: String): String =
    str.lower.replaceAll("[^a-z0-9]", "-").nn.replaceAll("--*", "-").nn

  def headOutline(node: Markdown[Markdown.Ast.Node]): Seq[String] = node match
    case Markdown(children*) =>
      children.flatMap {
        case Markdown.Ast.Block.Heading(level, children*) =>
          val string = text(children)
          List(str"${" "*(2*level - 1)}- [$string](#${slug(string)})")
        case Markdown.Ast.Inline.Text(str) =>
          List(str)
        case _ =>
          Nil
      }

  private val headings = IArray(H1, H2, H3, H4, H5, H6)
  
  private def heading(level: 1 | 2 | 3 | 4 | 5 | 6, children: Seq[Markdown.Ast.Inline]) =
    headings(level - 1)(id = slug(text(children)))(children.flatMap(phrasing))

  def convert(nodes: Seq[Markdown.Ast.Node]): Seq[Content[Flow]] = nodes.flatMap {
    case Markdown.Ast.Block.Paragraph(children*)            => List(P(children.flatMap(phrasing)))
    case Markdown.Ast.Block.Heading(level, children*)       => List(heading(level, children))
    case Markdown.Ast.Block.Blockquote(children*)           => List(Blockquote(convert(children)*))
    case Markdown.Ast.Block.ThematicBreak()                 => List(Hr)
    case Markdown.Ast.Block.FencedCode(syntax, meta, value) => List(Pre(Code(escape(value))))
    case Markdown.Ast.Block.Reference(_, _)                 => Nil
    case Markdown.Ast.Inline.Text(str)                      => List(P(escape(str)))
    
    case Markdown.Ast.Block.BulletList(num, _, _, items*)   => List((if num.isDefined then Ol else Ul)
                                                                   (items.flatMap(listItem)*))

    case Markdown.Ast.Block.Table(parts*)                   => List(Table(parts.flatMap(tableParts)))
    case Markdown.Ast.Inline.Break()                        => List(Br)
    case _                                                  => Nil
  }

  def tableParts(node: Markdown.Ast.TablePart): Seq[Item["thead" | "tbody"]] = node match
    case Markdown.Ast.TablePart.Head(rows*) => List(Thead(tableRows(true, rows)))
    case Markdown.Ast.TablePart.Body(rows*) => List(Tbody(tableRows(false, rows)))

  def tableRows(heading: Boolean, rows: Seq[Markdown.Ast.Block.Row]): Seq[Item["tr"]] =
    rows.map { case Markdown.Ast.Block.Row(cells*) => Tr(tableCells(heading, cells)) }

  def tableCells(heading: Boolean, cells: Seq[Markdown.Ast.Block.Cell]): Seq[Item["th" | "td"]] =
    cells.map { case Markdown.Ast.Block.Cell(content*) =>
      (if heading then Th else Td)(content.flatMap(phrasing))
    }

  def listItem(node: Markdown.Ast.ListItem): Seq[Item["li"]] = node match
    case Markdown.Ast.ListItem(children*) => List(Li(convert(children)*))

  def text(node: Seq[Markdown.Ast.Node]): String = node.map {
    case Markdown.Ast.Block.BulletList(_, _, _, _*) => ""
    case Markdown.Ast.Inline.Image(text, _)         => text
    case Markdown.Ast.Inline.Link(text, _)          => text
    case Markdown.Ast.Block.Reference(_, _)         => ""
    case Markdown.Ast.Inline.Break()                => ""
    case Markdown.Ast.Block.ThematicBreak()         => ""
    case Markdown.Ast.Inline.Emphasis(children*)    => text(children)
    case Markdown.Ast.Inline.Strong(children*)      => text(children)
    case Markdown.Ast.Inline.Code(code)             => code
    case Markdown.Ast.Block.Paragraph(children*)    => text(children)
    case Markdown.Ast.Block.Heading(_, children*)   => text(children)
    case Markdown.Ast.Block.Blockquote(children*)   => text(children)
    case Markdown.Ast.Block.FencedCode(_, _, code)  => code
    case Markdown.Ast.Inline.Text(text)             => text
    case Markdown.Ast.Block.Cell(content*)          => text(content)
    case _                                          => ""
  }.join

  def nonInteractive(node: Markdown.Ast.Inline): Seq[Content[Phrasing]] = node match
    case Markdown.Ast.Inline.Image(altText, location) => List(Img(src = location, alt = altText))
    case Markdown.Ast.Inline.Break()                  => List(Br)
    case Markdown.Ast.Inline.Emphasis(children*)      => List(Em(children.flatMap(phrasing)))
    case Markdown.Ast.Inline.Strong(children*)        => List(Strong(children.flatMap(phrasing)))
    case Markdown.Ast.Inline.Code(code)               => List(Code(code))
    case Markdown.Ast.Inline.Text(str)                => List(escape(str))
    case _                                   => Nil

  def phrasing(node: Markdown.Ast.Inline): Seq[Content[Phrasing]] = node match
    case Markdown.Ast.Inline.Link(location, content) =>
      val children = nonInteractive(content).collect { case node: NonInteractive => node }
      List(A(href = location)(children))
    
    case other =>
      nonInteractive(other)

  def escape(str: String): String =
    str.replaceAll("&", "&amp;").nn.replaceAll("<", "&lt;").nn.replaceAll(">", "&gt;").nn
