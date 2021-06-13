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
import com.vladsch.flexmark.parser.*
import com.vladsch.flexmark.util.options.*
import com.vladsch.flexmark.ext.gfm.tables, tables.TablesExtension

import annotation.tailrec

import scala.collection.JavaConverters.*

object Markdown:
  private val options = MutableDataSet()
  options.set(Parser.INLINE_DELIMITER_DIRECTIONAL_PUNCTUATIONS, java.lang.Boolean.TRUE)
  options.set(Parser.EXTENSIONS, java.util.Arrays.asList(TablesExtension.create()))
  private val parser = Parser.builder(options).build()

  def parse(string: String): MdNode.Document =
    val root = parser.parse(string)
    MdNode.Document(root.getChildIterator.asScala.to(List).map(convert(root, _))*)

  def parse(reader: java.io.Reader): MdNode.Document =
    val root = parser.parseReader(reader)
    MdNode.Document(root.getChildIterator.asScala.to(List).map(convert(root, _))*)

  @tailrec
  def coalesce(xs: List[MdNode], done: List[MdNode] = Nil): List[MdNode] = xs match
    case Nil                                           => done.reverse
    case MdNode.Text(str) :: MdNode.Text(str2) :: tail => coalesce(MdNode.Text(str+str2) :: tail, done)
    case head :: tail                                  => coalesce(tail, head :: done)

  @tailrec
  def format(str: String, buf: StringBuilder = StringBuilder(), i: Int = 0, chr: Char = 0, dash: Boolean = false, space: Boolean = false): String =
    if chr != 0 then buf.append(chr)
    if i < str.length
    then
      val chr = str(i)
      if dash && chr != '-' then buf.append('-')
      chr match
        case '"'  => format(str, buf, i + 1, if space then '“' else '”')
        case '\'' => format(str, buf, i + 1, if space then '‘' else '’')
        case '-'  => if dash then format(str, buf, i + 1, '—') else format(str, buf, i + 1, dash = true)
        case ' '  => format(str, buf, i + 1, ' ', space = true)
        case chr  => format(str, buf, i + 1, chr)
    else buf.toString

  def convert(root: ast.Document, node: ast.Node, noFormat: Boolean = false): MdNode =
    lazy val children = coalesce(node.getChildren.iterator.asScala.to(List).map(convert(root, _)))
    node match
      case node: ast.BlockQuote =>
        MdNode.Blockquote(children*)
      
      case node: ast.BulletList =>
        MdNode.BulletList(children*)
      
      case node: ast.BulletListItem =>
        MdNode.ListItem(children*)
      
      case node: ast.Code =>
        MdNode.Code(children*)
      
      case node: ast.CodeBlock =>
        lazy val unmodifiedChildren =
          coalesce(node.getChildren.iterator.asScala.to(List).map(convert(root, _, true)))
        
        MdNode.CodeBlock(None, unmodifiedChildren*)

      case node: ast.Emphasis =>
        MdNode.Emphasis(children*)
      
      case node: ast.FencedCodeBlock =>
        val syntax = node.getInfo.toString

        lazy val unmodifiedChildren =
          coalesce(node.getChildren.iterator.asScala.to(List).map(convert(root, _, true)))
        
        MdNode.CodeBlock(if syntax == "" then None else Some(syntax), unmodifiedChildren*)
      
      case node: ast.HardLineBreak =>
        MdNode.LineBreak
      
      case node: ast.Heading =>
        MdNode.Heading(node.getLevel, children*)
      
      case node: ast.Image =>
        MdNode.Image(node.getText.toString, node.getUrl.toString)
      
      case node: ast.ImageRef =>
        MdNode.Image(node.getText.toString, node.getReferenceNode(root).getUrl.toString)
      
      case node: ast.IndentedCodeBlock =>
        MdNode.Indented(node.getContentChars.toString)
      
      case node: ast.Link =>
        MdNode.Link(node.getText.toString, node.getUrl.toString)
      
      case node: ast.LinkRef =>
        val ref = Option(node.getReferenceNode(root))
        ref.fold(MdNode.Text(node.getText.toString)) { ref =>
          MdNode.Link(node.getText.toString, ref.getUrl.toString)
        }
      
      case node: ast.MailLink =>
        MdNode.Link(node.getText.toString, s"mailto:${node.getText}")
      
      case node: ast.OrderedList =>
        MdNode.OrderedList(children*)
      
      case node: ast.OrderedListItem =>
        MdNode.ListItem(children*)
      
      case node: ast.Paragraph =>
        MdNode.Paragraph(children*)
      
      case node: ast.Reference =>
        MdNode.Reference(node.getReference.toString, node.getUrl.toString)
      
      case node: ast.SoftLineBreak =>
        MdNode.Text("\n")
      
      case node: ast.StrongEmphasis =>
        MdNode.StrongEmphasis(children*)
      
      case node: ast.Text =>
        val content = node.getChars.toString
        MdNode.Text(if noFormat then content else format(content))

      case node: ast.ThematicBreak =>
        MdNode.ThematicBreak
      
      case node: tables.TableBlock =>
        MdNode.Table(node.getChildren.iterator.asScala.to(List).flatMap(parseTableParts(root, _))*)
  
  def parseTableParts(root: ast.Document, node: ast.Node): List[TablePart] = node match
    case node: tables.TableHead
      => List(MdNode.TableHead(node.getChildren.iterator.asScala.to(List).map(parseTableRows(root, _))*))
    
    case node: tables.TableBody =>
      List(MdNode.TableBody(node.getChildren.iterator.asScala.to(List).map(parseTableRows(root, _))*))
    
    case _ =>
      Nil
  
  def parseTableRows(root: ast.Document, node: ast.Node): MdNode.Row = node match
    case node: tables.TableRow =>
      MdNode.Row(node.getChildren.iterator.asScala.to(List).map(parseTableCells(root, _))*)

  def parseTableCells(root: ast.Document, node: ast.Node): MdNode.Cell = node match
    case node: tables.TableCell =>
      MdNode.Cell(node.getChildren.iterator.asScala.to(List).map(convert(root, _))*)

trait TablePart:
  this: MdNode =>

enum MdNode:
  case LineBreak
  case ThematicBreak
  case Paragraph(children: MdNode*)
  case Heading(level: Int, children: MdNode*)
  case Text(string: String)
  case Code(children: MdNode*)
  case CodeBlock(syntax: Option[String], children: MdNode*)
  case Emphasis(children: MdNode*)
  case StrongEmphasis(children: MdNode*)
  case Document(children: MdNode*)
  case ListItem(children: MdNode*)
  case BulletList(children: MdNode*)
  case OrderedList(children: MdNode*)
  case Blockquote(children: MdNode*)
  case Indented(content: String)
  case Link(text: String, location: String)
  case Image(alt: String, src: String)
  case Reference(id: String, location: String)
  case Table(children: TablePart*)

  case TableHead(rows: MdNode.Row*) extends MdNode, TablePart
  case TableBody(rows: MdNode.Row*) extends MdNode, TablePart
  case Row(cells: MdNode.Cell*) extends MdNode, TablePart
  case Cell(children: MdNode*) extends MdNode, TablePart
