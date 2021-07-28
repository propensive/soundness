/*

    Punctuation, version 0.4.0. Copyright 2019-21 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package punctuation

import rudiments.*

import com.vladsch.flexmark as cvf
import cvf.ast as cvfa, cvf.parser.*, cvf.util.options.*, cvf.ext.gfm.tables, tables.TablesExtension
import annotation.tailrec

import scala.reflect.Typeable
import scala.collection.JavaConverters.*

case class MalformedMarkdown(message: String) extends Exception(str"punctuation: $message")

object Markdown:
  private val options = MutableDataSet()
  options.set(Parser.INLINE_DELIMITER_DIRECTIONAL_PUNCTUATIONS, java.lang.Boolean.TRUE)
  options.set(Parser.EXTENSIONS, java.util.Arrays.asList(TablesExtension.create()))
  private val parser = Parser.builder(options).nn.build().nn

  def parse[T <: Markdown: Typeable](string: String): Root[T] throws MalformedMarkdown =
    val root = parser.parse(string).nn
    val nodes = root.getChildIterator.nn.asScala.to(List).map(convert(root, _))
    
    Root(nodes.collect {
      case child: T => child
      case other    => throw MalformedMarkdown("found an unexpected markdown node type")
    }*)

  def parsePhrase(string: String): Root[PhrasingContent] throws MalformedMarkdown =
    parse[PhrasingContent](string)
  
  def parseDocument(string: String): Root[Markdown] throws MalformedMarkdown =
    parse[Markdown](string)

  @tailrec
  private def coalesce[M >: Text <: Markdown](xs: List[M], done: List[M] = Nil): List[M] = xs match
    case Nil                             => done.reverse
    case Text(str) :: Text(str2) :: tail => coalesce(Text(str+" "+str2) :: tail, done)
    case head :: tail                    => coalesce(tail, head :: done)

  @tailrec
  def format(str: String, buf: StringBuilder = StringBuilder(), i: Int = 0, chr: Char = 0, space: Boolean = false): String =
    if chr != 0 then buf.append(chr)
    if i < str.length then
      str(i) match
        case '"'  => format(str, buf, i + 1, if space then '“' else '”')
        case '\'' => format(str, buf, i + 1, if space then '‘' else '’')
        case ' '  => format(str, buf, i + 1, ' ', space = true)
        case chr  => format(str, buf, i + 1, chr)
    else buf.toString

  private def resolveReference(root: cvfa.Document, node: cvfa.ImageRef | cvfa.LinkRef)
      : String throws MalformedMarkdown =
    Option(node.getReferenceNode(root)).fold {
      throw MalformedMarkdown(str"the image reference could not be resolved")
    } (_.nn.getUrl.toString)

  type PhrasingInput = cvfa.Emphasis | cvfa.StrongEmphasis | cvfa.Code | cvfa.HardLineBreak | cvfa.Image |
      cvfa.ImageRef | cvfa.Link | cvfa.LinkRef | cvfa.MailLink | cvfa.Text

  def phrasingChildren(root: cvfa.Document, node: cvfa.Node)
      : Seq[PhrasingContent] throws MalformedMarkdown =
    coalesce(node.getChildren.nn.iterator.nn.asScala.to(List).collect {
      case node: PhrasingInput => phrasing(root, node)
    })
  
  def flowChildren(root: cvfa.Document, node: cvfa.Node)
      : Seq[FlowContent] throws MalformedMarkdown =
    node.getChildren.nn.iterator.nn.asScala.to(List).collect {
      case node: FlowInput => flow(root, node)
    }
  
  def listItems(root: cvfa.Document, node: cvfa.BulletList | cvfa.OrderedList)
      : Seq[ListItem] throws MalformedMarkdown =
    node.getChildren.nn.iterator.nn.asScala.to(List).collect {
      case node: (cvfa.BulletListItem | cvfa.OrderedListItem) => ListItem(flowChildren(root, node)*)
    }

  def phrasing(root: cvfa.Document, node: PhrasingInput): PhrasingContent throws MalformedMarkdown =
    node match
      case node: cvfa.Emphasis       => Emphasis(phrasingChildren(root, node)*)
      case node: cvfa.StrongEmphasis => Strong(phrasingChildren(root, node)*)
      case node: cvfa.Code           => InlineCode(node.getText.toString)
      case node: cvfa.HardLineBreak  => Break()
      case node: cvfa.Image          => Image(node.getText.toString, node.getUrl.toString)
      case node: cvfa.ImageRef       => Image(node.getText.toString, resolveReference(root, node))
      case node: cvfa.Link           => Link(node.getUrl.toString, phrasingChildren(root, node)*)
      case node: cvfa.LinkRef        => Link(resolveReference(root, node), phrasingChildren(root, node)*)
      case node: cvfa.MailLink       => Link(node.getText.toString, Text(s"mailto:${node.getText}"))
      case node: cvfa.Text           => Text(node.getChars.toString)

  type FlowInput = cvfa.BlockQuote | cvfa.BulletList | cvfa.CodeBlock | cvfa.FencedCodeBlock |
      cvfa.ThematicBreak | cvfa.Paragraph | cvfa.IndentedCodeBlock | cvfa.Heading | cvfa.OrderedList

  def flow(root: cvfa.Document, node: FlowInput): FlowContent throws MalformedMarkdown =
    node match
      case node: cvfa.BlockQuote        => Blockquote(flowChildren(root, node)*)
      case node: cvfa.BulletList        => MdList(ordered = false, start = 1, loose = node.isLoose, listItems(root, node)*)
      case node: cvfa.CodeBlock         => Code(None, None, node.getContentChars.toString)
      case node: cvfa.IndentedCodeBlock => Code(None, None, node.getContentChars.toString)
      case node: cvfa.Paragraph         => Paragraph(phrasingChildren(root, node)*)
      case node: cvfa.OrderedList       => MdList(ordered = true, start = 1, loose = node.isLoose, listItems(root, node)*)
      case node: cvfa.ThematicBreak     => ThematicBreak()
      case node: cvfa.FencedCodeBlock =>
        Code(if node.getInfo.toString == "" then None else Some(node.getInfo.toString), None, node.getContentChars.toString)
      case node: cvfa.Heading => node.getLevel match
        case lvl@(1 | 2 | 3 | 4 | 5 | 6) => Heading(lvl, phrasingChildren(root, node)*)
        case _                           => throw MalformedMarkdown("the heading level is not in the range 1-6")
      
  def convert(root: cvfa.Document, node: cvfa.Node, noFormat: Boolean = false)
      : Markdown throws MalformedMarkdown =
    node match
      case node: cvfa.HardLineBreak => Break()
      case node: cvfa.SoftLineBreak => Text("\n")
      case node: cvfa.Reference     => Reference(node.getReference.toString, node.getUrl.toString)
      case node: cvfa.Text          => Text(if noFormat then node.getChars.toString else format(node.getChars.toString))
      case node: cvfa.ThematicBreak => ThematicBreak()
      case node: tables.TableBlock  => Table(table(root, node)*)
      case node: FlowInput          => flow(root, node)
      case node: PhrasingInput      => phrasing(root, node)
      case node: cvfa.Node          => throw MalformedMarkdown("unexpected Markdown node")
  
  def table(root: cvfa.Document, node: tables.TableBlock)
      : List[TablePart] throws MalformedMarkdown =
    node.getChildren.nn.iterator.nn.asScala.to(List).collect {
      case node: (tables.TableHead | tables.TableBody) =>
        val rows: Seq[Row] = node.getChildren.nn.iterator.nn.asScala.to(List).collect {
          case row: tables.TableRow =>
            Row(node.getChildren.nn.iterator.nn.asScala.to(List).collect {
              case cell: tables.TableCell => tableCell(root, cell)
            }*)
        }

        node match
          case node: tables.TableHead => TableHead(rows*)
          case node: tables.TableBody => TableBody(rows*)
      
    }
    
  def tableCell(root: cvfa.Document, node: tables.TableCell): Cell throws MalformedMarkdown =
    Cell(phrasingChildren(root, node)*)

type PhrasingContent = Break | Emphasis | HtmlNode | Image | InlineCode | Strong | Text | Link
type FlowContent = Blockquote | Code | Heading | HtmlNode | MdList | ThematicBreak | Paragraph
type ListContent = ListItem

enum Markdown:
  case Break()
  case ThematicBreak()
  case Paragraph(children: PhrasingContent*)
  case Heading(level: 1 | 2 | 3 | 4 | 5 | 6, children: PhrasingContent*)
  case Text(string: String)
  case HtmlNode(value: String)
  case InlineCode(value: String)
  case Code(lang: Option[String], meta: Option[String], value: String)
  case Emphasis(children: PhrasingContent*)
  case Strong(children: PhrasingContent*)
  case Root[M <: Markdown](children: M*)
  case ListItem(children: FlowContent*)
  case MdList(ordered: Boolean, start: Int, loose: Boolean, children: ListContent*)
  case Blockquote(children: FlowContent*)
  case Link(location: String, children: PhrasingContent*)
  case Image(alt: String, src: String)
  case Reference(id: String, location: String)
  case Table(children: TablePart*)
  case TableHead(rows: Row*)
  case TableBody(rows: Row*)
  case Row(cells: Cell*)
  case Cell(children: PhrasingContent*)

import Markdown.*

type TablePart = TableHead | TableBody
type Inline = Text | InlineCode | Emphasis | Strong | Link
