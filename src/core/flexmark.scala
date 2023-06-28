/*
    Punctuation, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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
import digression.*
import gossamer.*
import spectacular.*

import com.vladsch.flexmark as cvf
import cvf.ast as cvfa, cvf.parser.*, cvf.util.options.*, cvf.ext.tables,
    cvf.util.ast as cvfua
import annotation.tailrec

import scala.quoted.*

case class MarkdownError(detail: Text) extends Error(err"the markdown could not be read: $detail")

case class Markdown[+MdType <: Markdown.Ast.Node](nodes: MdType*):
  def serialize: Text =
    val buf = StringBuilder()
    
    nodes.foreach:
      case node: Markdown.Ast.Inline => node.serialize(buf)
      case node: Markdown.Ast.Block  => node.serialize(buf)
    
    buf.text

import Markdown.Ast.Inline.*
import Markdown.Ast.Block.*
import Markdown.Ast.TablePart
import Markdown.Ast.ListItem

type InlineMd = Markdown[Markdown.Ast.Inline]
type Md = Markdown[Markdown.Ast.Block]

object Markdown:

  //given (using CanThrow[MarkdownError]): Canonical[InlineMd] = Canonical(parseInline(_), _.serialize)
  given Show[InlineMd] = _.serialize

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

      def serialize(buf: StringBuilder): Unit = buf.add(t"Serialization of block elements is not currently supported!")

    case class ListItem(children: Block*)
    
    enum TablePart:
      case Head(rows: Row*)
      case Body(rows: Row*)

    enum Inline:
      case Break()
      case Emphasis(children: Inline*)
      case HtmlNode(value: Text)
      case Image(alt: Text, src: Text)
      case SourceCode(value: Text)
      case Strong(children: Inline*)
      case Copy(string: Text)
      case Weblink(location: Text, children: Inline*)

      def serialize(buf: StringBuilder): Unit = this match
        case Break() =>
          buf.add('\n')
        
        case Emphasis(children*) =>
          buf.add('_')
          children.foreach(_.serialize(buf))
          buf.add('_')
        
        case HtmlNode(value) =>
          buf.add(value)
        
        case Image(alt, src) =>
          buf.add(t"![")
          buf.add(alt)
          buf.add(t"](")
          buf.add(src)
          buf.add(t")")
        
        case SourceCode(value) =>
          buf.add('`')
          buf.add(value)
          buf.add('`')
        
        case Strong(children*) =>
          buf.add('*')
          children.foreach(_.serialize(buf))
          buf.add('*')
        
        case Copy(text) =>
          buf.add(text)
        
        case Weblink(location, children*) =>
          buf.add('[')
          children.foreach(_.serialize(buf))
          buf.add(t"](")
          buf.add(location)
          buf.add(')')
        
  private val options = MutableDataSet()
  //options.set[ju.Collection[com.vladsch.flexmark.util.misc.Extension]](Parser.EXTENSIONS,
  //    ju.Arrays.asList(TablesExtension.create()))
  
  private val parser = Parser.builder(options).nn.build().nn

  def parse(string: Text): Md throws MarkdownError =
    val root = parser.parse(string.s).nn
    val nodes = root.getChildIterator.nn.asScala.to(List).map(convert(root, _).getOrElse(
        throw MarkdownError(t"could not parse markdown")))
    
    Markdown(nodes.collect { case child: Markdown.Ast.Block => child }*)

  def parseInline(string: Text): InlineMd throws MarkdownError =
    parse(string) match
      case Markdown(Paragraph(xs*)) => Markdown[Markdown.Ast.Inline](xs*)
      case other                    => throw MarkdownError(t"markdown contains block-level elements")
  
  @tailrec
  private def coalesce
      [MdType >: Copy <: Markdown.Ast.Inline]
      (xs: List[MdType], done: List[MdType] = Nil)
      : List[MdType] =
    xs match
      case Nil                             => done.reverse
      case Copy(str) :: Copy(str2) :: tail => coalesce(Copy(t"$str$str2") :: tail, done)
      case Copy(str) :: tail               => coalesce(tail, Copy(format(str)) :: done)
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
    Option(node.getReferenceNode(root)).fold(throw MarkdownError(t"the image reference could not be resolved")):  node =>
      node.nn.getUrl.toString.show


  type PhrasingInput = cvfa.Emphasis | cvfa.StrongEmphasis | cvfa.Code | cvfa.HardLineBreak |
      cvfa.Image | cvfa.ImageRef | cvfa.Link | cvfa.LinkRef | cvfa.MailLink | cvfa.Text | cvfa.SoftLineBreak

  def phraseChildren(root: cvfua.Document, node: cvfua.Node)
      : Seq[Markdown.Ast.Inline] throws MarkdownError = coalesce:
    node.getChildren.nn.iterator.nn.asScala.to(List).collect:
      case node: PhrasingInput => phrasing(root, node)
  
  def flowChildren(root: cvfua.Document, node: cvfua.Node): Seq[Markdown.Ast.Block] throws MarkdownError =
    node.getChildren.nn.iterator.nn.asScala.to(List).collect:
      case node: FlowInput => flow(root, node)
  
  def listItems(root: cvfua.Document, node: cvfa.BulletList | cvfa.OrderedList)
      : Seq[ListItem] throws MarkdownError =
    node.getChildren.nn.iterator.nn.asScala.to(List).collect:
      case node: (cvfa.BulletListItem | cvfa.OrderedListItem) => ListItem(flowChildren(root, node)*)

  def phrasing(root: cvfua.Document, node: PhrasingInput): Markdown.Ast.Inline throws MarkdownError =
    node match
      case node: cvfa.Emphasis       => Emphasis(phraseChildren(root, node)*)
      case node: cvfa.SoftLineBreak  => Copy(t"\n")
      case node: cvfa.StrongEmphasis => Strong(phraseChildren(root, node)*)
      case node: cvfa.Code           => SourceCode(node.getText.toString.show)
      case node: cvfa.HardLineBreak  => Break()
      case node: cvfa.Image          => Image(node.getText.toString.show, node.getUrl.toString.show)
      case node: cvfa.ImageRef       => Image(node.getText.toString.show, resolveReference(root, node))
      case node: cvfa.Link           => Weblink(node.getUrl.toString.show, phraseChildren(root, node)*)
      
      case node: cvfa.LinkRef        => Weblink(resolveReference(root, node),
                                            phraseChildren(root, node)*)
      
      case node: cvfa.MailLink       => val address: Text = t"mailto:"+node.getText.nn.toString.show
                                        Weblink(node.getText.toString.show, Copy(address))
      case node: cvfa.Text           => Copy(format(node.getChars.toString.show))

  type FlowInput = cvfa.BlockQuote | cvfa.BulletList | cvfa.CodeBlock | cvfa.FencedCodeBlock |
      cvfa.ThematicBreak | cvfa.Paragraph | cvfa.IndentedCodeBlock | cvfa.Heading | cvfa.OrderedList

  def flow(root: cvfua.Document, node: FlowInput): Markdown.Ast.Block throws MarkdownError =
    node match
      case node: cvfa.BlockQuote        => Blockquote(flowChildren(root, node)*)
      
      case node: cvfa.BulletList        => BulletList(numbered = None, loose = node.isLoose,
                                               listItems(root, node)*)
      
      case node: cvfa.CodeBlock         => FencedCode(None, None, node.getContentChars.toString.show)
      case node: cvfa.IndentedCodeBlock => FencedCode(None, None, node.getContentChars.toString.show)
      case node: cvfa.Paragraph         => Paragraph(phraseChildren(root, node)*)
      
      case node: cvfa.OrderedList       => BulletList(numbered = Some(1), loose = node.isLoose,
                                               listItems(root, node)*)
      
      case node: cvfa.ThematicBreak     => ThematicBreak()
      
      case node: cvfa.FencedCodeBlock   => FencedCode(if node.getInfo.toString.show == t"" then None
                                               else Some(node.getInfo.toString.show), None,
                                               node.getContentChars.toString.show)
      
      case node: cvfa.Heading           => node.getLevel match
                                             case lvl@(1 | 2 | 3 | 4 | 5 | 6) =>
                                               Heading(lvl, phraseChildren(root, node)*)
                                             
                                             case _ =>
                                               throw MarkdownError(txt"""the heading level is not
                                                                            in the range 1-6""")
      
  def convert(root: cvfua.Document, node: cvfua.Node, noFormat: Boolean = false): Option[Markdown.Ast.Node] =
    try Some:
      node match
        case node: cvfa.HardLineBreak => Break()
        case node: cvfa.SoftLineBreak => Copy(t"\n")
        case node: cvfa.Reference     => Reference(node.getReference.toString.show, node.getUrl.toString.show)
        
        case node: cvfa.Text          => Copy(if noFormat then node.getChars.toString.show else
                                             format(node.getChars.toString.show))
        
        case node: cvfa.ThematicBreak => ThematicBreak()
        case node: tables.TableBlock  => Table(table(root, node)*)
        case node: FlowInput          => flow(root, node)
        case node: PhrasingInput      => phrasing(root, node)
        case node: cvfua.Node         => throw MarkdownError(t"unexpected Markdown node")
    catch case e: MarkdownError    => None
  
  def table(root: cvfua.Document, node: tables.TableBlock)
      : List[Markdown.Ast.TablePart] throws MarkdownError =
    node.getChildren.nn.iterator.nn.asScala.to(List).collect:
      case node: (tables.TableHead | tables.TableBody) =>
        val rows: Seq[Row] = node.getChildren.nn.iterator.nn.asScala.to(List).collect:
          case row: tables.TableRow =>
            Row(node.getChildren.nn.iterator.nn.asScala.to(List).collect {
              case cell: tables.TableCell => tableCell(root, cell)
            }*)

        node match
          case node: tables.TableHead => TablePart.Head(rows*)
          case node: tables.TableBody => TablePart.Body(rows*)
      
  def tableCell(root: cvfua.Document, node: tables.TableCell): Cell throws MarkdownError =
    Cell(phraseChildren(root, node)*)

object Punctuation:
  def md
      (context: Expr[StringContext], parts: Expr[Seq[Any]])(using Quotes)
      : Expr[Markdown[Markdown.Ast.Node]] =
    import quotes.reflect.*

    Md.Interpolator.expansion(context, parts) match
      case (Md.Input.Inline(_), result) => '{$result.asInstanceOf[InlineMd]}
      case (Md.Input.Block(_), result)  => '{$result.asInstanceOf[Md]}