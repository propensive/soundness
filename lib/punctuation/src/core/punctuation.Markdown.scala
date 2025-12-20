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

import doms.html.whatwg, whatwg.*
import attributives.attributiveText

object Markdown:
  trait Node
  case class LinkRef(label: Text, title: Optional[Text], destination: Text)

  private def url(text: Text): Text =
    val builder = StringBuilder()
    text.urlDecode.chars.each:
      case char if char >= 128          => builder.append(char.toString.urlEncode)
      case char if char.isLetterOrDigit => builder.append(char)
      case ' '                          => builder.append("%20")
      case '\\'                         => builder.append("%5C")

      case char@('-' | '.' | '+' | ',' | '&' | '@' | '#' | '~' | '/' | '*' | '_' | '(' | ')' | '=' | ':' | '?') =>
        builder.append(char)

      case char =>
        builder.append(char.toString.urlEncode)

    builder.toString.tt

  private def text(node: Prose): Text = node match
    case Prose.Textual(text)       => text
    case Prose.Emphasis(children*) => children.map(text(_)).join
    case Prose.Code(code)          => code
    case Prose.Strong(children*)   => children.map(text(_)).join
    case Prose.Softbreak           => "\n"
    case Prose.Linebreak           => "\n"
    case Prose.HtmlInline(content) => ""

    case Prose.Link(destination, title, content*) =>
      content.map(text(_)).join

    case Prose.Image(destination, title, content*) =>
      content.map(text(_)).join

  private def phrasing(node: Prose): Html of Phrasing = node match
    case Prose.Textual(text)       => text
    case Prose.Emphasis(children*) => Em(children.map(phrasing(_))*)
    case Prose.Code(code)          => Code(code)
    case Prose.Strong(children*)   => Strong(children.map(phrasing(_))*)
    case Prose.Softbreak           => "\n"
    case Prose.Linebreak           => Fragment(Br, "\n")
    case Prose.HtmlInline(content) => Comment(s"[CDATA[$content]]")

    case Prose.Link(destination, title, content*) =>
      val destination2 = url(destination)
      title.lay(A(href = destination2)(content.map(phrasing(_))*)): title =>
        A(href = destination2, title = title)(content.map(phrasing(_))*)

    case Prose.Image(destination, title, content*) =>
      val alt: Text = content.map(text(_)).join
      val base = Img(src = destination, alt = alt)

      title.lay(base): title =>
        base.title = title

  given prose: (Markdown of Prose) is Renderable:
    type Form = Phrasing

    def render(markdown: Markdown of Prose): Html of Phrasing =
      Fragment(markdown.children.map(phrasing(_))*)

  given layout: (Markdown of Layout) is Renderable:
    type Form = doms.html.whatwg.Flow

    def render(markdown: Markdown of Layout): Html of whatwg.Flow =
      import Markdown.*
      import doms.html.whatwg.*

      def tightItem(node: Layout): Html of Flow = node match
        case Layout.Paragraph(_, content*) => Fragment(content.map(phrasing(_))*)
        case node                          => Fragment("\n", layout(node))

      @tailrec
      def merge(block: Boolean, nodes: List[Layout], done: List[Html of Flow], tight: Boolean)
      : List[Html of Flow] =
          nodes match
            case Nil =>
              if block then ((TextNode("\n"): Html of Flow) :: done).reverse else done.reverse

            case Layout.Paragraph(_, contents*) :: tail if tight =>
              val content = Fragment(contents.map(phrasing(_))*)
              merge
               (false,
                tail,
                (if block then Fragment("\n", content) else content) :: done,
                tight)

            case head :: tail =>
              merge(true, tail, Fragment("\n", layout(head)) :: done, tight)

      def block(node: Layout): Boolean = node match
        case Layout.Paragraph(_, content*) => false
        case node                          => true

      def layout(node: Layout): Html of Flow = node match
        case Layout.BlockQuote(line, children*) =>
          val fragment = Fragment(children.map { node => Fragment(TextNode("\n"), layout(node)) }*)
          Blockquote(fragment, "\n")

        case Layout.Paragraph(line, children*) =>
          P(children.map(phrasing(_))*)

        case Layout.BulletList(line, tight, items*) =>
          val items2 = items.map: item =>
            if item.isEmpty then Li
            else Li(merge(false, item, Nil, tight)*)

          Ul(items2*)

        case Layout.OrderedList(line, start, tight, delimiter, items*) =>
          val items2 = items.map: item =>
            if item.isEmpty then Li
            else Li(merge(false, item, Nil, tight)*)

          val start2 = start.puncture(1)

          Ol(items2*).per(start2)(_.start = _)


        case Layout.ThematicBreak(line) =>
          Hr

        case Layout.HtmlBlock(line, content) => Comment(s"[CDATA[$content]]")

        case Layout.Heading(line, level, content*) => level match
          case 1 => H1(content.map(phrasing(_))*)
          case 2 => H2(content.map(phrasing(_))*)
          case 3 => H3(content.map(phrasing(_))*)
          case 4 => H4(content.map(phrasing(_))*)
          case 5 => H5(content.map(phrasing(_))*)
          case 6 => H6(content.map(phrasing(_))*)

        case Layout.CodeBlock(line, info, code) =>
          Pre(info.prim.lay(Code(code)) { info => Code(`class` = t"language-$info")(code) })

      Fragment(markdown.children.map { node => Fragment(layout(node), "\n") }*)


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
