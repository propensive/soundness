package punctuation

import scala.collection.mutable as scm

import anticipation.*
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

enum Layout extends Markdown.Node:
  case BlockQuote(line: Ordinal, layout: Layout*)
  
  case OrderedList
        (line:      Ordinal,
         tight:     Boolean,
         start:     Int,
         delimiter: Optional['.' | ')'],
         items: List[Layout]*)
         
  case BulletList(line: Ordinal, tight: Boolean, items: List[Layout]*)
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


  given renderable: (Markdown of Layout) is Renderable to html5.Flow = markdown =>
    import Markdown.*
    import html5.*

    def prose(node: Prose): List[Html[Phrasing]] = node match
      case Prose.Textual(text)       => List(text)
      case Prose.Emphasis(children*) => List(Em(children.flatMap(prose(_))))
      case Prose.Code(code)          => List(Code(code))
      case Prose.Strong(children*)   => List(Strong(children.flatMap(prose(_))))
      case Prose.Softbreak           => List(t"\n")
      case Prose.Linebreak           => List(Br)
      case Prose.HtmlInline(content) => List(content)
      
      case Prose.Link(destination, title, content*) =>
        val base = title.lay(A(href = destination)): title =>
          A(href = destination, title = title)
          
        List(base(content.flatMap(prose(_).asInstanceOf[List[Html[Noninteractive]]])))
        
      case Prose.Image(destination, title, content*) =>
        List:
          title.lay(Img(src = destination)): title =>
            Img(src = destination, alt = title)
          
    def layout(node: Layout): List[Html[Flow]] = node match
      case Layout.BlockQuote(line, children*) =>
        List(Blockquote(children.flatMap(layout(_))))
        
      case Layout.Paragraph(line, children*) =>
        List(P(children.flatMap(prose(_))))
        
      case Layout.BulletList(line, tight, items*) =>
        List(Ul(items.flatMap: item =>
          List(if tight then Li(item.flatMap(_.children).flatMap(prose(_))) else Li(item.flatMap(layout(_))))))
          
      case Layout.OrderedList(line, tight, start, delimiter, items*) =>
        List(Ol(items.flatMap: item =>
          List(if tight then Li(item.flatMap(_.children).flatMap(prose(_))) else Li(item.flatMap(layout(_))))))
          
      case Layout.ThematicBreak(line) =>
        List(Hr)
      
      case Layout.HtmlBlock(line, content) => List(content)
      
      case Layout.Heading(line, level, content*) => level match
        case 1 => List(H1(content.map(prose(_))*))
        case 2 => List(H2(content.map(prose(_))*))
        case 3 => List(H3(content.map(prose(_))*))
        case 4 => List(H4(content.map(prose(_))*))
        case 5 => List(H5(content.map(prose(_))*))
        case 6 => List(H6(content.map(prose(_))*))
        
      case Layout.CodeBlock(line, info, code) =>
        List(Pre(info.prim.lay(Code(code)) { info => Code.applyDynamic(s"language-$info")(code) }))
          
    markdown.children.flatMap(layout(_))
        

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
