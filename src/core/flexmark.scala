/*
    Punctuation, version 0.12.0. Copyright 2020-22 Jon Pretty, Propensive OÜ.

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
import cvf.ast as cvfa, cvf.parser.*, cvf.util.options.*, cvf.ext.tables,
    tables.TablesExtension, cvf.util.ast as cvfua
import annotation.tailrec

import java.util as ju

case class MarkdownError(detail: Text) extends Error:
  def message: Text = t"the markdown could not be read: $detail"

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
      case FencedCode(lang: Option[Text], meta: Option[Text], value: Text)
      case BulletList(numbered: Option[Int], loose: Boolean, children: ListItem*)
      case Blockquote(children: Block*)
      case Reference(id: Text, location: Text)
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
      case HtmlNode(value: Text)
      case Image(alt: Text, src: Text)
      case Code(value: Text)
      case Strong(children: Inline*)
      case Textual(string: Text)
      case Link(location: Text, children: Inline*)

  private val options = MutableDataSet()
  //options.set[ju.Collection[com.vladsch.flexmark.util.misc.Extension]](Parser.EXTENSIONS,
  //    ju.Arrays.asList(TablesExtension.create()))
  
  private val parser = Parser.builder(options).nn.build().nn

  def parse(string: Text): Markdown[Markdown.Ast.Block] throws MarkdownError =
    val root = parser.parse(string.s).nn
    val nodes = root.getChildIterator.nn.asScala.to(List).map(convert(root, _).getOrElse(
        throw MarkdownError(t"could not parse markdown")))
    
    Markdown(nodes.collect { case child: Markdown.Ast.Block => child }*)

  def parseInline(string: Text): Markdown[Markdown.Ast.Inline] throws MarkdownError =
    parse(string) match
      case Markdown(Paragraph(xs*)) => Markdown[Markdown.Ast.Inline](xs*)
      case other                    => throw MarkdownError(t"markdown contains block-level elements")
  
  @tailrec
  private def coalesce[M >: Textual <: Markdown.Ast.Inline](xs: List[M], done: List[M] = Nil): List[M] =
    xs match
      case Nil                             => done.reverse
      case Textual(str) :: Textual(str2) :: tail => coalesce(Textual(format(t"$str $str2")) :: tail, done)
      case head :: tail                    => coalesce(tail, head :: done)

  def format(str: Text): Text = Text:
    str.s
      .replaceAll("--", "—").nn
      .replaceAll(" \"", " “").nn
      .replaceAll("\"", "”").nn
      .replaceAll(" '", " ‘").nn
      .replaceAll("'", "’").nn

  private def resolveReference(root: cvfua.Document, node: cvfa.ImageRef | cvfa.LinkRef)
      : Text throws MarkdownError =
    Option(node.getReferenceNode(root)).fold {
      throw MarkdownError(t"the image reference could not be resolved")
    } { node => Showable(node.nn.getUrl).show }

  type PhrasingInput = cvfa.Emphasis | cvfa.StrongEmphasis | cvfa.Code | cvfa.HardLineBreak |
      cvfa.Image | cvfa.ImageRef | cvfa.Link | cvfa.LinkRef | cvfa.MailLink | cvfa.Text

  def phraseChildren(root: cvfua.Document, node: cvfua.Node)
      : Seq[Markdown.Ast.Inline] throws MarkdownError =
    coalesce(node.getChildren.nn.iterator.nn.asScala.to(List).collect {
      case node: PhrasingInput => phrasing(root, node)
    })
  
  def flowChildren(root: cvfua.Document, node: cvfua.Node): Seq[Markdown.Ast.Block] throws MarkdownError =
    node.getChildren.nn.iterator.nn.asScala.to(List).collect {
      case node: FlowInput => flow(root, node)
    }
  
  def listItems(root: cvfua.Document, node: cvfa.BulletList | cvfa.OrderedList)
      : Seq[ListItem] throws MarkdownError =
    node.getChildren.nn.iterator.nn.asScala.to(List).collect {
      case node: (cvfa.BulletListItem | cvfa.OrderedListItem) => ListItem(flowChildren(root, node)*)
    }

  def phrasing(root: cvfua.Document, node: PhrasingInput): Markdown.Ast.Inline throws MarkdownError =
    node match
      case node: cvfa.Emphasis       => Emphasis(phraseChildren(root, node)*)
      case node: cvfa.StrongEmphasis => Strong(phraseChildren(root, node)*)
      case node: cvfa.Code           => Code(Showable(node.getText).show)
      case node: cvfa.HardLineBreak  => Break()
      case node: cvfa.Image          => Image(Showable(node.getText).show, Showable(node.getUrl).show)
      case node: cvfa.ImageRef       => Image(Showable(node.getText).show, resolveReference(root, node))
      case node: cvfa.Link           => Link(Showable(node.getUrl).show, phraseChildren(root, node)*)
      
      case node: cvfa.LinkRef        => Link(resolveReference(root, node),
                                            phraseChildren(root, node)*)
      
      case node: cvfa.MailLink       => Link(Showable(node.getText).show, Textual(t"mailto:${Showable(node.getText.nn).show}"))
      case node: cvfa.Text           => Textual(format(Showable(node.getChars).show))

  type FlowInput = cvfa.BlockQuote | cvfa.BulletList | cvfa.CodeBlock | cvfa.FencedCodeBlock |
      cvfa.ThematicBreak | cvfa.Paragraph | cvfa.IndentedCodeBlock | cvfa.Heading | cvfa.OrderedList

  def flow(root: cvfua.Document, node: FlowInput): Markdown.Ast.Block throws MarkdownError =
    node match
      case node: cvfa.BlockQuote        => Blockquote(flowChildren(root, node)*)
      
      case node: cvfa.BulletList        => BulletList(numbered = None, loose = node.isLoose,
                                               listItems(root, node)*)
      
      case node: cvfa.CodeBlock         => FencedCode(None, None, Showable(node.getContentChars).show)
      case node: cvfa.IndentedCodeBlock => FencedCode(None, None, Showable(node.getContentChars).show)
      case node: cvfa.Paragraph         => Paragraph(phraseChildren(root, node)*)
      
      case node: cvfa.OrderedList       => BulletList(numbered = Some(1), loose = node.isLoose,
                                               listItems(root, node)*)
      
      case node: cvfa.ThematicBreak     => ThematicBreak()
      
      case node: cvfa.FencedCodeBlock   => FencedCode(if Showable(node.getInfo).show == t"" then None
                                               else Some(Showable(node.getInfo).show), None,
                                               Showable(node.getContentChars).show)
      
      case node: cvfa.Heading           => node.getLevel match
                                             case lvl@(1 | 2 | 3 | 4 | 5 | 6) =>
                                               Heading(lvl, phraseChildren(root, node)*)
                                             
                                             case _ =>
                                               throw MarkdownError(txt"""the heading level is not
                                                                            in the range 1-6""")
      
  def convert(root: cvfua.Document, node: cvfua.Node, noFormat: Boolean = false)
      : Option[Markdown.Ast.Node] =
    try Some {
      node match
        case node: cvfa.HardLineBreak => Break()
        case node: cvfa.SoftLineBreak => Textual(t"\n")
        case node: cvfa.Reference     => Reference(Showable(node.getReference).show, Showable(node.getUrl).show)
        
        case node: cvfa.Text          => Textual(if noFormat then Showable(node.getChars).show else
                                             format(Showable(node.getChars).show))
        
        case node: cvfa.ThematicBreak => ThematicBreak()
        case node: tables.TableBlock  => Table(table(root, node)*)
        case node: FlowInput          => flow(root, node)
        case node: PhrasingInput      => phrasing(root, node)
        case node: cvfua.Node         => throw MarkdownError(t"unexpected Markdown node")
    }
    catch case e: MarkdownError    => None
  
  def table(root: cvfua.Document, node: tables.TableBlock)
      : List[Markdown.Ast.TablePart] throws MarkdownError =
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
    
  def tableCell(root: cvfua.Document, node: tables.TableCell): Cell throws MarkdownError =
    Cell(phraseChildren(root, node)*)
