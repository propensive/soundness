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

import com.vladsch.flexmark.ast
import com.vladsch.flexmark.parser._
import com.vladsch.flexmark.util.options._
import com.vladsch.flexmark.ext.gfm.tables, tables.TablesExtension

import annotation.tailrec

import scala.collection.JavaConverters._

object Markdown {
  private val options = new MutableDataSet()
  options.set(Parser.INLINE_DELIMITER_DIRECTIONAL_PUNCTUATIONS, java.lang.Boolean.TRUE)
  options.set(Parser.EXTENSIONS, java.util.Arrays.asList(TablesExtension.create()))
  private val parser = Parser.builder(options).build()

  def parse(string: String): Document = {
    val root = parser.parse(string)
    Document(root.getChildIterator.asScala.to[List].map(convert(root, _)): _*)
  }

  def parse(reader: java.io.Reader): Document = {
    val root = parser.parseReader(reader)
    Document(root.getChildIterator.asScala.to[List].map(convert(root, _)): _*)
  }

  @tailrec
  def coalesce(xs: List[MdNode], done: List[MdNode] = Nil): List[MdNode] = xs match {
    case Nil => done.reverse
    case Text(str) :: Text(str2) :: tail => coalesce(Text(str+str2) :: tail, done)
    case head :: tail => coalesce(tail, head :: done)
  }

  @tailrec
  def format(str: String, buf: StringBuilder = new StringBuilder(), i: Int = 0, chr: Char = 0, dash: Boolean = false, space: Boolean = false): String = {
    if(chr != 0) buf.append(chr)
    if(i < str.length) {
      val chr = str(i)
      if(dash && chr != '-') buf.append('-')
      chr match {
        case '"' => format(str, buf, i + 1, if(space) '“' else '”')
        case '\'' => format(str, buf, i + 1, if(space) '‘' else '’')
        case '-' => if(dash) format(str, buf, i + 1, '—') else format(str, buf, i + 1, dash = true)
        case ' ' => format(str, buf, i + 1, ' ', space = true)
        case chr => format(str, buf, i + 1, chr)
      }
    } else buf.toString
  }

  def convert(root: ast.Document, node: ast.Node, noFormat: Boolean = false): MdNode = {
    lazy val children = coalesce(node.getChildren.iterator.asScala.to[List].map(convert(root, _)))
    node match {
      case node: ast.BlockQuote => Blockquote(children: _*)
      case node: ast.BulletList => BulletList(children: _*)
      case node: ast.BulletListItem => ListItem(children: _*)
      case node: ast.Code => Code(children: _*)
      
      case node: ast.CodeBlock =>
        lazy val unmodifiedChildren =
          coalesce(node.getChildren.iterator.asScala.to[List].map(convert(root, _, true)))
        CodeBlock(None, unmodifiedChildren: _*)

      case node: ast.Emphasis => Emphasis(children: _*)
      
      case node: ast.FencedCodeBlock =>
        val syntax = node.getInfo.toString
        lazy val unmodifiedChildren =
          coalesce(node.getChildren.iterator.asScala.to[List].map(convert(root, _, true)))
        CodeBlock(if(syntax == "") None else Some(syntax), unmodifiedChildren: _*)
      
      case node: ast.HardLineBreak => LineBreak
      case node: ast.Heading => Heading(node.getLevel, children: _*)
      case node: ast.Image => Image(node.getText.toString, node.getUrl.toString)
      
      case node: ast.ImageRef =>
        Image(node.getText.toString, node.getReferenceNode(root).getUrl.toString)
      
      case node: ast.IndentedCodeBlock => Indented(node.getContentChars.toString)
      case node: ast.Link => Link(node.getText.toString, node.getUrl.toString)
      
      case node: ast.LinkRef =>
        val ref = Option(node.getReferenceNode(root))
        ref.fold[MdNode](Text(node.getText.toString)) { ref => Link(node.getText.toString, ref.getUrl.toString) }
      
      case node: ast.MailLink => Link(node.getText.toString, s"mailto:${node.getText}")
      case node: ast.OrderedList => OrderedList(children: _*)
      case node: ast.OrderedListItem => ListItem(children: _*)
      case node: ast.Paragraph => Paragraph(children: _*)
      case node: ast.Reference => Reference(node.getReference.toString, node.getUrl.toString)
      case node: ast.SoftLineBreak => Text("\n")
      case node: ast.StrongEmphasis => StrongEmphasis(children: _*)
      
      case node: ast.Text =>
        val content = node.getChars.toString
        Text(if(noFormat) content else format(content))

      case node: ast.ThematicBreak => ThematicBreak
      case node: tables.TableBlock => Table(node.getChildren.iterator.asScala.to[List].flatMap(parseTableParts(root, _)): _*)
    }
  }
  
  def parseTableParts(root: ast.Document, node: ast.Node): List[TablePart] = node match {
    case node: tables.TableHead => List(TableHead(node.getChildren.iterator.asScala.to[List].map(parseTableRows(root, _)): _*))
    case node: tables.TableBody => List(TableBody(node.getChildren.iterator.asScala.to[List].map(parseTableRows(root, _)): _*))
    case _ => Nil
  }
  
  def parseTableRows(root: ast.Document, node: ast.Node): Row = node match {
    case node: tables.TableRow => Row(node.getChildren.iterator.asScala.to[List].map(parseTableCells(root, _)): _*)
  }

  def parseTableCells(root: ast.Document, node: ast.Node): Cell = node match {
    case node: tables.TableCell => Cell(node.getChildren.iterator.asScala.to[List].map(convert(root, _)): _*)
  }
}

sealed trait MdNode
case object LineBreak extends MdNode
case object ThematicBreak extends MdNode
case class Paragraph(children: MdNode*) extends MdNode
case class Heading(level: Int, children: MdNode*) extends MdNode
case class Text(string: String) extends MdNode
case class Code(children: MdNode*) extends MdNode
case class CodeBlock(syntax: Option[String], children: MdNode*) extends MdNode
case class Emphasis(children: MdNode*) extends MdNode
case class StrongEmphasis(children: MdNode*) extends MdNode
case class Document(children: MdNode*) extends MdNode
case class ListItem(children: MdNode*) extends MdNode
case class BulletList(children: MdNode*) extends MdNode
case class OrderedList(children: MdNode*) extends MdNode
case class Blockquote(children: MdNode*) extends MdNode
case class Indented(content: String) extends MdNode
case class Link(text: String, location: String) extends MdNode
case class Image(alt: String, src: String) extends MdNode
case class Reference(id: String, location: String) extends MdNode
case class Table(children: TablePart*) extends MdNode
sealed trait TablePart extends MdNode
case class TableHead(rows: Row*) extends TablePart
case class TableBody(rows: Row*) extends TablePart
case class Row(cells: Cell*) extends MdNode
case class Cell(children: MdNode*) extends MdNode
