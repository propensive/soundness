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

  def parse(string: String): Markdown.Document =
    val root = parser.parse(string)
    Markdown.Document(root.getChildIterator.asScala.to(List).map(convert(root, _))*)

  def parse(reader: java.io.Reader): Markdown.Document =
    val root = parser.parseReader(reader)
    Markdown.Document(root.getChildIterator.asScala.to(List).map(convert(root, _))*)

  @tailrec
  def coalesce(xs: List[Markdown], done: List[Markdown] = Nil): List[Markdown] = xs match
    case Nil                                           => done.reverse
    case Markdown.Text(str) :: Markdown.Text(str2) :: tail => coalesce(Markdown.Text(str+str2) :: tail, done)
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

  def convert(root: ast.Document, node: ast.Node, noFormat: Boolean = false): Markdown =
    lazy val children = coalesce(node.getChildren.iterator.asScala.to(List).map(convert(root, _)))
    node match
      case node: ast.BlockQuote =>
        Markdown.Blockquote(children*)
      
      case node: ast.BulletList =>
        Markdown.BulletList(children*)
      
      case node: ast.BulletListItem =>
        Markdown.ListItem(children*)
      
      case node: ast.Code =>
        Markdown.Code(children*)
      
      case node: ast.CodeBlock =>
        lazy val unmodifiedChildren =
          coalesce(node.getChildren.iterator.asScala.to(List).map(convert(root, _, true)))
        
        Markdown.CodeBlock(None, unmodifiedChildren*)

      case node: ast.Emphasis =>
        Markdown.Emphasis(children*)
      
      case node: ast.FencedCodeBlock =>
        val syntax = node.getInfo.toString

        lazy val unmodifiedChildren =
          coalesce(node.getChildren.iterator.asScala.to(List).map(convert(root, _, true)))
        
        Markdown.CodeBlock(if syntax == "" then None else Some(syntax), unmodifiedChildren*)
      
      case node: ast.HardLineBreak =>
        Markdown.LineBreak
      
      case node: ast.Heading =>
        Markdown.Heading(node.getLevel, children*)
      
      case node: ast.Image =>
        Markdown.Image(node.getText.toString, node.getUrl.toString)
      
      case node: ast.ImageRef =>
        Markdown.Image(node.getText.toString, node.getReferenceNode(root).getUrl.toString)
      
      case node: ast.IndentedCodeBlock =>
        Markdown.Indented(node.getContentChars.toString)
      
      case node: ast.Link =>
        Markdown.Link(node.getText.toString, node.getUrl.toString)
      
      case node: ast.LinkRef =>
        val ref = Option(node.getReferenceNode(root))
        ref.fold(Markdown.Text(node.getText.toString)) { ref =>
          Markdown.Link(node.getText.toString, ref.getUrl.toString)
        }
      
      case node: ast.MailLink =>
        Markdown.Link(node.getText.toString, s"mailto:${node.getText}")
      
      case node: ast.OrderedList =>
        Markdown.OrderedList(children*)
      
      case node: ast.OrderedListItem =>
        Markdown.ListItem(children*)
      
      case node: ast.Paragraph =>
        Markdown.Paragraph(children*)
      
      case node: ast.Reference =>
        Markdown.Reference(node.getReference.toString, node.getUrl.toString)
      
      case node: ast.SoftLineBreak =>
        Markdown.Text("\n")
      
      case node: ast.StrongEmphasis =>
        Markdown.StrongEmphasis(children*)
      
      case node: ast.Text =>
        val content = node.getChars.toString
        Markdown.Text(if noFormat then content else format(content))

      case node: ast.ThematicBreak =>
        Markdown.ThematicBreak
      
      case node: tables.TableBlock =>
        Markdown.Table(node.getChildren.iterator.asScala.to(List).flatMap(parseTableParts(root, _))*)
  
  def parseTableParts(root: ast.Document, node: ast.Node): List[TablePart] = node match
    case node: tables.TableHead
      => List(Markdown.TableHead(node.getChildren.iterator.asScala.to(List).map(parseTableRows(root, _))*))
    
    case node: tables.TableBody =>
      List(Markdown.TableBody(node.getChildren.iterator.asScala.to(List).map(parseTableRows(root, _))*))
    
    case _ =>
      Nil
  
  def parseTableRows(root: ast.Document, node: ast.Node): Markdown.Row = node match
    case node: tables.TableRow =>
      Markdown.Row(node.getChildren.iterator.asScala.to(List).map(parseTableCells(root, _))*)

  def parseTableCells(root: ast.Document, node: ast.Node): Markdown.Cell = node match
    case node: tables.TableCell =>
      Markdown.Cell(node.getChildren.iterator.asScala.to(List).map(convert(root, _))*)

trait TablePart:
  this: Markdown =>

enum Markdown:
  case LineBreak
  case ThematicBreak
  case Paragraph(children: Markdown*)
  case Heading(level: Int, children: Markdown*)
  case Text(string: String)
  case Code(children: Markdown*)
  case CodeBlock(syntax: Option[String], children: Markdown*)
  case Emphasis(children: Markdown*)
  case StrongEmphasis(children: Markdown*)
  case Document(children: Markdown*)
  case ListItem(children: Markdown*)
  case BulletList(children: Markdown*)
  case OrderedList(children: Markdown*)
  case Blockquote(children: Markdown*)
  case Indented(content: String)
  case Link(text: String, location: String)
  case Image(alt: String, src: String)
  case Reference(id: String, location: String)
  case Table(children: TablePart*)

  case TableHead(rows: Markdown.Row*) extends Markdown, TablePart
  case TableBody(rows: Markdown.Row*) extends Markdown, TablePart
  case Row(cells: Markdown.Cell*) extends Markdown, TablePart
  case Cell(children: Markdown*) extends Markdown, TablePart
