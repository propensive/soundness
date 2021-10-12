/*
    Punctuation, version 0.12.0. Copyright 2019-21 Jon Pretty, Propensive OÜ.

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

import rudiments.*
import gossamer.*

import com.vladsch.flexmark as cvf
import cvf.ast as cvfa, cvf.parser.*, cvf.util.options.*, cvf.util.data.*, cvf.ext.tables,
    tables.TablesExtension, cvf.ext.typographic.*, cvf.util.ast as cvfua
import annotation.tailrec

import scala.reflect.Typeable
import scala.collection.JavaConverters.*

import java.util as ju

case class MalformedMarkdown(message: String) extends Exception(str"punctuation: $message")

case class Markdown[+M <: Markdown.Ast.Node](nodes: M*)

import Markdown.Ast.Inline.*
import Markdown.Ast.Block.*
import Markdown.Ast.TablePart
import Markdown.Ast.ListItem

object Markdown:
  object Ast:
  
    type Node = Block | Inline
  
    enum Block:
      case ThematicBreak()
      case Paragraph(children: Inline*)
      case Heading(level: 1 | 2 | 3 | 4 | 5 | 6, children: Inline*)
      case FencedCode(lang: Option[String], meta: Option[String], value: String)
      case BulletList(numbered: Option[Int], loose: Boolean, children: ListItem*)
      case Blockquote(children: Block*)
      case Reference(id: String, location: String)
      case Table(children: TablePart*)
      case Row(cells: Cell*)
      case Cell(children: Inline*)

    case class ListItem(children: Block*)
    
    enum TablePart:
      case Head(rows: Row*)
      case Body(rows: Row*)

    enum Inline:
      case Break()
      case Emphasis(children: Inline*)
      case HtmlNode(value: String)
      case Image(alt: String, src: String)
      case Code(value: String)
      case Strong(children: Inline*)
      case Text(string: String)
      case Link(location: String, children: Inline*)

  private val options = MutableDataSet()
  options.set[ju.Collection[com.vladsch.flexmark.util.misc.Extension]](Parser.EXTENSIONS,
      ju.Arrays.asList(TablesExtension.create()))
  
  private val parser = Parser.builder(options).nn.build().nn

  def parse(string: String): Markdown[Markdown.Ast.Block] throws MalformedMarkdown =
    val root = parser.parse(string).nn
    val nodes = root.getChildIterator.nn.asScala.to(List).map(convert(root, _).getOrElse(throw MalformedMarkdown("could not parse markdown")))
    
    Markdown(nodes.collect { case child: Markdown.Ast.Block => child }*)

  def parseInline(string: String): Markdown[Markdown.Ast.Inline] throws MalformedMarkdown =
    parse(string) match
      case Markdown(Paragraph(xs*)) => Markdown[Markdown.Ast.Inline](xs*)
      case other                    => throw MalformedMarkdown("markdown contains block-level elements")
  
  @tailrec
  private def coalesce[M >: Text <: Markdown.Ast.Inline](xs: List[M], done: List[M] = Nil): List[M] =
    xs match
      case Nil                             => done.reverse
      case Text(str) :: Text(str2) :: tail => coalesce(Text(format(str+" "+str2)) :: tail, done)
      case head :: tail                    => coalesce(tail, head :: done)

  def format(str: String): String =
    str
      .replaceAll("--", "—").nn
      .replaceAll(" \"", " “").nn
      .replaceAll("\"", "”").nn
      .replaceAll(" '", " ‘").nn
      .replaceAll("'", "’").nn

  private def resolveReference(root: cvfua.Document, node: cvfa.ImageRef | cvfa.LinkRef)
      : String throws MalformedMarkdown =
    Option(node.getReferenceNode(root)).fold {
      throw MalformedMarkdown(str"the image reference could not be resolved")
    } (_.nn.getUrl.toString)

  type PhrasingInput = cvfa.Emphasis | cvfa.StrongEmphasis | cvfa.Code | cvfa.HardLineBreak |
      cvfa.Image | cvfa.ImageRef | cvfa.Link | cvfa.LinkRef | cvfa.MailLink | cvfa.Text

  def phraseChildren(root: cvfua.Document, node: cvfua.Node)
      : Seq[Markdown.Ast.Inline] throws MalformedMarkdown =
    coalesce(node.getChildren.nn.iterator.nn.asScala.to(List).collect {
      case node: PhrasingInput => phrasing(root, node)
    })
  
  def flowChildren(root: cvfua.Document, node: cvfua.Node): Seq[Markdown.Ast.Block] throws MalformedMarkdown =
    node.getChildren.nn.iterator.nn.asScala.to(List).collect {
      case node: FlowInput => flow(root, node)
    }
  
  def listItems(root: cvfua.Document, node: cvfa.BulletList | cvfa.OrderedList)
      : Seq[ListItem] throws MalformedMarkdown =
    node.getChildren.nn.iterator.nn.asScala.to(List).collect {
      case node: (cvfa.BulletListItem | cvfa.OrderedListItem) => ListItem(flowChildren(root, node)*)
    }

  def phrasing(root: cvfua.Document, node: PhrasingInput): Markdown.Ast.Inline throws MalformedMarkdown =
    node match
      case node: cvfa.Emphasis       => Emphasis(phraseChildren(root, node)*)
      case node: cvfa.StrongEmphasis => Strong(phraseChildren(root, node)*)
      case node: cvfa.Code           => Code(node.getText.toString)
      case node: cvfa.HardLineBreak  => Break()
      case node: cvfa.Image          => Image(node.getText.toString, node.getUrl.toString)
      case node: cvfa.ImageRef       => Image(node.getText.toString, resolveReference(root, node))
      case node: cvfa.Link           => Link(node.getUrl.toString, phraseChildren(root, node)*)
      
      case node: cvfa.LinkRef        => Link(resolveReference(root, node),
                                            phraseChildren(root, node)*)
      
      case node: cvfa.MailLink       => Link(node.getText.toString, Text(s"mailto:${node.getText}"))
      case node: cvfa.Text           => Text(format(node.getChars.toString))

  type FlowInput = cvfa.BlockQuote | cvfa.BulletList | cvfa.CodeBlock | cvfa.FencedCodeBlock |
      cvfa.ThematicBreak | cvfa.Paragraph | cvfa.IndentedCodeBlock | cvfa.Heading | cvfa.OrderedList

  def flow(root: cvfua.Document, node: FlowInput): Markdown.Ast.Block throws MalformedMarkdown =
    node match
      case node: cvfa.BlockQuote        => Blockquote(flowChildren(root, node)*)
      
      case node: cvfa.BulletList        => BulletList(numbered = None, loose = node.isLoose,
                                               listItems(root, node)*)
      
      case node: cvfa.CodeBlock         => FencedCode(None, None, node.getContentChars.toString)
      case node: cvfa.IndentedCodeBlock => FencedCode(None, None, node.getContentChars.toString)
      case node: cvfa.Paragraph         => Paragraph(phraseChildren(root, node)*)
      
      case node: cvfa.OrderedList       => BulletList(numbered = Some(1), loose = node.isLoose,
                                               listItems(root, node)*)
      
      case node: cvfa.ThematicBreak     => ThematicBreak()
      
      case node: cvfa.FencedCodeBlock   => FencedCode(if node.getInfo.toString == "" then None
                                               else Some(node.getInfo.toString), None,
                                               node.getContentChars.toString)
      
      case node: cvfa.Heading           => node.getLevel match
                                             case lvl@(1 | 2 | 3 | 4 | 5 | 6) =>
                                               Heading(lvl, phraseChildren(root, node)*)
                                             
                                             case _ =>
                                               throw MalformedMarkdown("the heading level is not "+
                                                   "in the range 1-6")
      
  def convert(root: cvfua.Document, node: cvfua.Node, noFormat: Boolean = false)
      : Option[Markdown.Ast.Node] =
    try Some {
      node match
        case node: cvfa.HardLineBreak => Break()
        case node: cvfa.SoftLineBreak => Text("\n")
        case node: cvfa.Reference     => Reference(node.getReference.toString, node.getUrl.toString)
        
        case node: cvfa.Text          => Text(if noFormat then node.getChars.toString else
                                             format(node.getChars.toString))
        
        case node: cvfa.ThematicBreak => ThematicBreak()
        case node: tables.TableBlock  => Table(table(root, node)*)
        case node: FlowInput          => flow(root, node)
        case node: PhrasingInput      => phrasing(root, node)
        case node: cvfua.Node         => throw MalformedMarkdown("unexpected Markdown node")
    }
    catch case MalformedMarkdown(_) => None
  
  def table(root: cvfua.Document, node: tables.TableBlock)
      : List[Markdown.Ast.TablePart] throws MalformedMarkdown =
    node.getChildren.nn.iterator.nn.asScala.to(List).collect {
      case node: (tables.TableHead | tables.TableBody) =>
        val rows: Seq[Row] = node.getChildren.nn.iterator.nn.asScala.to(List).collect {
          case row: tables.TableRow =>
            Row(node.getChildren.nn.iterator.nn.asScala.to(List).collect {
              case cell: tables.TableCell => tableCell(root, cell)
            }*)
        }

        node match
          case node: tables.TableHead => TablePart.Head(rows*)
          case node: tables.TableBody => TablePart.Body(rows*)
      
    }
    
  def tableCell(root: cvfua.Document, node: tables.TableCell): Cell throws MalformedMarkdown =
    Cell(phraseChildren(root, node)*)
