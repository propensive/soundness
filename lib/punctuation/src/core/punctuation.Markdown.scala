                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.27.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package punctuation

import scala.annotation.tailrec

import anticipation.*
import contingency.*
import distillate.*
import gossamer.*
import honeycomb.*
import prepositional.*
import proscenium.*
import rudiments.*
import spectacular.*
import turbulence.*
import vacuous.*

import com.vladsch.flexmark as cvf
import cvf.ast as cvfa, cvf.parser.*, cvf.util.options.*, cvf.ext.tables, cvf.util.ast as cvfua

import Markdown.Ast.Inline.*
import Markdown.Ast.Block.*
import Markdown.Ast.TablePart
import Markdown.Ast.ListItem

case class Markdown[+markdown <: Markdown.Ast.Node](nodes: markdown*):
  def serialize: Text =
    val buf = StringBuilder()

    nodes.each: value =>
      value.absolve match
        case node: Markdown.Ast.Inline => node.serialize(buf)
        case node: Markdown.Ast.Block  => node.serialize(buf)

    buf.text

object Markdown:
  given decoder: Tactic[MarkdownError] => InlineMd is Decodable in Text = parseInline(_)
  given encodable: InlineMd is Encodable in Text = _.serialize
  given showable: InlineMd is Showable = _.serialize

  given renderable: [markdown <: Markdown[Markdown.Ast.Node]] => (translator: Translator)
        => markdown is Renderable into html5.Flow =
    content => translator.translate(content.nodes)

  given renderable2: [markdown <: Markdown.Ast.Inline] => (translator: Translator)
        => markdown is Renderable into html5.Phrasing =
    translator.phrasing(_)

  object Ast:
    type Node = Block | Inline

    enum Block:
      case ThematicBreak()
      case Paragraph(children: Inline*)
      case Heading(level: 1 | 2 | 3 | 4 | 5 | 6, children: Inline*)
      case FencedCode(lang: Optional[Text], meta: Optional[Text], value: Text)
      case BulletList(numbered: Optional[Int], loose: Boolean, children: ListItem*)
      case Blockquote(children: Block*)
      case Reference(id: Text, location: Text)
      case Table(children: TablePart*)
      case Row(cells: Cell*)
      case Cell(children: Inline*)

      def serialize(buf: StringBuilder): Unit =
        buf.add(t"Serialization of block elements is not currently supported!")

    case class ListItem(children: Block*)

    enum TablePart:
      case Head(rows: Row*)
      case Body(rows: Row*)

    enum Inline:
      case LineBreak
      case Emphasis(children: Inline*)
      case HtmlNode(value: Text)
      case Image(alt: Text, src: Text)
      case SourceCode(value: Text)
      case Strong(children: Inline*)
      case Prose(string: Text)
      case Weblink(location: Text, children: Inline*)

      def serialize(buf: StringBuilder): Unit = this match
        case LineBreak =>
          buf.add('\n')

        case Emphasis(children*) =>
          buf.add('_')
          children.each(_.serialize(buf))
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
          children.each(_.serialize(buf))
          buf.add('*')

        case Prose(text) =>
          buf.add(text)

        case Weblink(location, children*) =>
          buf.add('[')
          children.each(_.serialize(buf))
          buf.add(t"](")
          buf.add(location)
          buf.add(')')

  private val options = MutableDataSet()
  //options.set[ju.Collection[com.vladsch.flexmark.util.misc.Extension]](Parser.EXTENSIONS,
  //    ju.Arrays.asList(TablesExtension.create()))

  private val parser = Parser.builder(options).nn.build().nn

  def parse[readable: Readable by Text](value: readable)(using Tactic[MarkdownError])
  :     Markdown[Markdown.Ast.Block] =
    val text = value.stream[Text].read[Text]
    val root = parser.parse(text.s).nn
    val nodes = root.getChildIterator.nn.asScala.to(List).map(convert(root, _))

    Markdown(nodes.collect { case child: Markdown.Ast.Block => child }*)

  def parseInline(text: Text)(using Tactic[MarkdownError]): InlineMd = parse(text) match
    case Markdown(Paragraph(xs*)) =>
      Markdown[Markdown.Ast.Inline](xs*)

    case other =>
      raise(MarkdownError(MarkdownError.Reason.BlockInsideInline))
      Markdown[Markdown.Ast.Inline]()

  @tailrec
  private def coalesce[markdown >: Prose <: Markdown.Ast.Inline]
               (elements: List[markdown], done: List[markdown] = Nil)
  :     List[markdown] =

    elements match
      case Nil                               => done.reverse
      case Prose(str) :: Prose(str2) :: tail => coalesce(Prose(t"$str$str2") :: tail, done)
      case Prose(str) :: tail                => coalesce(tail, Prose(format(str)) :: done)
      case head :: tail                      => coalesce(tail, head :: done)


  def format(str: Text): Text =
    str.s
    . replaceAll("--", "—").nn
    . replaceAll(" \"", " “").nn
    . replaceAll("\"", "”").nn
    . replaceAll(" '", " ‘").nn
    . replaceAll("'", "’").nn
    . tt

  private def resolveReference(root: cvfua.Document, node: cvfa.ImageRef | cvfa.LinkRef)
  :     Text raises MarkdownError =

    Optional(node.getReferenceNode(root)).let(_.nn.getUrl.toString.show).or:
      raise(MarkdownError(MarkdownError.Reason.BrokenImageRef)) yet t"https://example.com/"

  type PhrasingInput =
    cvfa.Emphasis | cvfa.StrongEmphasis | cvfa.Code | cvfa.HardLineBreak | cvfa.Image
    | cvfa.ImageRef | cvfa.Link | cvfa.LinkRef | cvfa.MailLink | cvfa.Text | cvfa.SoftLineBreak

  def phraseChildren(root: cvfua.Document, node: cvfua.Node)
  :     Seq[Markdown.Ast.Inline] raises MarkdownError =
    coalesce:
      node.getChildren.nn.iterator.nn.asScala.to(List).collect:
        case node: PhrasingInput => phrasing(root, node)

  def flowChildren(root: cvfua.Document, node: cvfua.Node)
  :     Seq[Markdown.Ast.Block] raises MarkdownError =
    node.getChildren.nn.iterator.nn.asScala.to(List).collect:
      case node: FlowInput => flow(root, node)

  def listItems(root: cvfua.Document, node: cvfa.BulletList | cvfa.OrderedList)
  :     Seq[ListItem] raises MarkdownError =

    node.getChildren.nn.iterator.nn.asScala.to(List).collect:
      case node: (cvfa.BulletListItem | cvfa.OrderedListItem) => ListItem(flowChildren(root, node)*)

  def phrasing(root: cvfua.Document, node: PhrasingInput)
  :     Markdown.Ast.Inline raises MarkdownError = node match
    case node: cvfa.Emphasis       => Emphasis(phraseChildren(root, node)*)
    case node: cvfa.SoftLineBreak  => Prose(t"\n")
    case node: cvfa.StrongEmphasis => Strong(phraseChildren(root, node)*)
    case node: cvfa.Code           => SourceCode(node.getText.toString.show)
    case node: cvfa.HardLineBreak  => LineBreak
    case node: cvfa.Image          => Image(node.getText.toString.show, node.getUrl.toString.show)
    case node: cvfa.ImageRef       => Image
                                       (node.getText.toString.show, resolveReference(root, node))
    case node: cvfa.Link           => Weblink
                                       (node.getUrl.toString.show, phraseChildren(root, node)*)
    case node: cvfa.LinkRef        => Weblink
                                       (resolveReference(root, node), phraseChildren(root, node)*)
    case node: cvfa.MailLink       => Weblink
                                       (node.getText.toString.show,
                                        Prose(s"mailto:${node.getText.nn}".tt))
    case node: cvfa.Text           => Prose(format(node.getChars.toString.show))

  type FlowInput = cvfa.BlockQuote | cvfa.BulletList | cvfa.CodeBlock | cvfa.FencedCodeBlock |
      cvfa.ThematicBreak | cvfa.Paragraph | cvfa.IndentedCodeBlock | cvfa.Heading | cvfa.OrderedList

  def flow(root: cvfua.Document, node: FlowInput): Markdown.Ast.Block raises MarkdownError =
    node match
      case node: cvfa.BlockQuote        => Blockquote(flowChildren(root, node)*)

      case node: cvfa.BulletList        => BulletList(numbered = Unset, loose = node.isLoose,
                                              listItems(root, node)*)

      case node: cvfa.CodeBlock         => FencedCode
                                            (Unset, Unset, node.getContentChars.toString.show)
      case node: cvfa.IndentedCodeBlock => FencedCode
                                            (Unset, Unset, node.getContentChars.toString.show)
      case node: cvfa.Paragraph         => Paragraph(phraseChildren(root, node)*)
      case node: cvfa.OrderedList       => BulletList
                                            (numbered = 1,
                                             loose    = node.isLoose,
                                             listItems(root, node)*)
      case node: cvfa.ThematicBreak     => ThematicBreak()

      case node: cvfa.FencedCodeBlock =>
        FencedCode
          (if node.getInfo.toString.show == t"" then Unset else node.getInfo.toString.show, Unset,
           node.getContentChars.toString.show)

      case node: cvfa.Heading => node.getLevel match
        case lvl@(1 | 2 | 3 | 4 | 5 | 6) => Heading(lvl, phraseChildren(root, node)*)

        case _ =>
          raise(MarkdownError(MarkdownError.Reason.BadHeadingLevel))
          Heading(6, phraseChildren(root, node)*)

  def convert(root: cvfua.Document, node: cvfua.Node, noFormat: Boolean = false)
  :     Markdown.Ast.Node raises MarkdownError =
    node match
      case node: cvfa.HardLineBreak => LineBreak
      case node: cvfa.SoftLineBreak => Prose(t"\n")
      case node: cvfa.ThematicBreak => ThematicBreak()
      case node: tables.TableBlock  => Table(table(root, node)*)
      case node: FlowInput          => flow(root, node)
      case node: PhrasingInput      => phrasing(root, node)

      case node: cvfa.Reference =>
        Reference(node.getReference.toString.show, node.getUrl.toString.show)

      case node: cvfua.Node =>
        raise(MarkdownError(MarkdownError.Reason.UnexpectedNode)) yet Prose(t"?")

  def table(root: cvfua.Document, node: tables.TableBlock)
  :     List[Markdown.Ast.TablePart] raises MarkdownError =

    node.getChildren.nn.iterator.nn.asScala.to(List).collect:
      case node: (tables.TableHead | tables.TableBody) =>
        val rows: Seq[Row] = node.getChildren.nn.iterator.nn.asScala.to(List).collect:
          case row: tables.TableRow =>
            val cells = node.getChildren.nn.iterator.nn.asScala.to(List).collect:
              case cell: tables.TableCell => tableCell(root, cell)

            Row(cells*)

        node match
          case node: tables.TableHead => TablePart.Head(rows*)
          case node: tables.TableBody => TablePart.Body(rows*)

  def tableCell(root: cvfua.Document, node: tables.TableCell): Cell raises MarkdownError =
    Cell(phraseChildren(root, node)*)
