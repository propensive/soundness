package punctuation

import scala.collection.mutable as scm

import anticipation.*
import contingency.*
import denominative.*
import distillate.*
import gossamer.*
import honeycomb.*
import prepositional.*
import proscenium.*
import rudiments.*
import turbulence.*
import vacuous.*
import zephyrine.*

import doms.whatwg
import attributives.attributiveText

enum Layout extends Markdown.Node:
  case BlockQuote(line: Ordinal, layout: Layout*)

  case OrderedList[tight <: Boolean]
        (line:      Ordinal,
         tight:     tight,
         start:     Int,
         delimiter: Optional['.' | ')'],
         items: List[Layout]*)

  case BulletList(line: Ordinal, items: List[Layout]*)
  case TightBulletList(line: Ordinal, items: List[Prose]*)
  case CodeBlock(line: Ordinal, info: List[Text], content: Text)
  case Paragraph(line: Ordinal, prose: Prose*)
  case Heading(line: Ordinal, level: 1 | 2 | 3 | 4 | 5 | 6, prose: Prose*)
  case ThematicBreak(line: Ordinal)
  case HtmlBlock(line: Ordinal, html: Text)

  def line: Ordinal

  def children: Seq[Prose] = this match
    case Paragraph(_, children*)  => children
    case BlockQuote(_, children*) => children.flatMap(_.children)
    case Heading(_, _, children*) => children
    case _                        => Nil


enum Prose extends Markdown.Node:
  case Textual(text: Text)
  case Softbreak
  case Linebreak
  case Code(code: Text)
  case Emphasis(prose: Prose*)
  case Strong(prose: Prose*)
  case Link(destination: Text, title: Optional[Text], prose: Prose*)
  case Image(destination: Text, title: Optional[Text], prose: Prose*)
  case HtmlInline(html: Text)

  def children: Seq[Prose] = this match
    case Link(_, _, prose*)  => prose
    case Image(_, _, prose*) => prose
    case _                   => Nil

object Markdown:
  trait Node
  case class LinkRef(label: Text, title: Optional[Text], destination: Text)

  given decodable: Markdown of Layout is Aggregable:
    type Operand = Text

    enum Kind:
      case Unknown, Paragraph, Code

    def aggregate(stream: Stream[Text]): Markdown of Layout =
      val cursor = Cursor(stream.iterator)
      var linkRefs: List[Markdown.LinkRef] = Nil
      var layout: List[Layout] = Nil
      val buffer = java.lang.StringBuilder()


      def line(): Unit = cursor.hold:
        var continue: Boolean = true
        var started: Boolean = false
        var begin: Mark = Mark.Initial
        while continue do
          cursor.datum(using Unsafe) match
            case ' '  =>
            case '\n' =>
              continue = false
            case char =>
              if !started then begin = cursor.mark

          continue &&= cursor.next()


      while cursor.next() do line()

      Markdown(linkRefs.reverse, layout.reverse*)


  given renderable: (Markdown of Layout) is Renderable:
    type Form = doms.whatwg.Flow
    def render(markdown: Markdown of Layout): Html of doms.whatwg.Flow =
      import Markdown.*
      import doms.whatwg.*

      def prose(node: Prose): Html of Phrasing = node match
        case Prose.Textual(text)       => text: Html of Phrasing
        case Prose.Emphasis(children*) => Em(children.map(prose(_))*)
        case Prose.Code(code)          => Code(code)
        case Prose.Strong(children*)   => Strong(children.map(prose(_))*)
        case Prose.Softbreak           => t"\n": Html of Phrasing
        case Prose.Linebreak           => Br
        case Prose.HtmlInline(content) => Comment(t"[CDATA[$content]]")

        case Prose.Link(destination, title, content*) =>
          val base = title.lay(A(href = destination)): title =>
            A(href = destination, title = title)

          base(content.map(prose(_))*)

        case Prose.Image(destination, title, content*) =>
          val img: Element of "img" = Img(src = destination)
          title.lay(img)(img.alt = _)
          Img(src = destination).per(title)(_.alt = _)

      def layout(node: Layout): Html of Flow = node match
        case Layout.BlockQuote(line, children*) =>
          Blockquote(children.map(layout(_))*)

        case Layout.Paragraph(line, children*) =>
          P(children.map(prose(_))*)

        case Layout.BulletList(line, items*) =>
          val items2 = items.map: item =>
            Li(item.map(layout(_))*)

          Ul(items2*)

        case Layout.TightBulletList(line, items*) =>
          val items2 = items.map: item =>
            Li(item.flatMap(_.children.map(prose(_)))*)

          Ul(items2*)

        case Layout.OrderedList(line, tight, start, delimiter, items*) =>
          val items2 = items.map: item =>
            if tight then Li(item.flatMap(_.children.map(prose(_)))*)
            else Li(item.map(layout(_))*)

          Ol(items2*)

        case Layout.ThematicBreak(line) =>
          Hr

        case Layout.HtmlBlock(line, content) => content

        case Layout.Heading(line, level, content*) => level match
          case 1 => H1(content.map(prose(_))*)
          case 2 => H2(content.map(prose(_))*)
          case 3 => H3(content.map(prose(_))*)
          case 4 => H4(content.map(prose(_))*)
          case 5 => H5(content.map(prose(_))*)
          case 6 => H6(content.map(prose(_))*)

        case Layout.CodeBlock(line, info, code) =>
          Pre(info.prim.lay(Code(code)) { info => Code(`class` = t"language-$info")(code) })

      Fragment(markdown.children.map(layout(_))*)


  def apply(linkRefs0: List[Markdown.LinkRef], layout: Layout*): Markdown of Layout = new Markdown:
    type Topic = Layout
    val linkRefs: List[Markdown.LinkRef] = linkRefs0
    val children: Seq[Layout] = layout

  @targetName("applyProse")
  def apply(prose: Prose*): Markdown of Prose = new Markdown:
    type Topic = Prose
    val linkRefs: List[Markdown.LinkRef] = Nil
    val children: Seq[Prose] = prose

abstract class Markdown:
  type Topic <: Markdown.Node

  val linkRefs: List[Markdown.LinkRef]
  val children: Seq[Topic]

  override def equals(that: Any): Boolean = that match
    case that: Markdown => that.children == children
    case _              => false

  override def hashCode: Int = children.hashCode
