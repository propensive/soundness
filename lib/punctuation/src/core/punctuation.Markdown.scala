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
┃    Soundness, version 0.54.0.                                                                    ┃
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
import attributives.textAttributes

object Markdown:
  trait Node
  case class LinkRef(label: Text, title: Optional[Text], destination: Text)

  // FIXME: This implementation needs to be cleaned up
  private def url(text: Text): Text =
    val builder = StringBuilder()
    text.urlDecode.chars.each:
      case char if char >= 128          => builder.append(char.toString.urlEncode)
      case char if char.isLetterOrDigit => builder.append(char)
      case ' '                          => builder.append("%20")
      case '\\'                         => builder.append("%5C")

      case char@('-' | '.' | '+' | ',' | '&' | '@' | '#' | '~' | '/' | '*' | '_' | '(' | ')' | '='
                 | ':' | '?') =>
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

  given layout: (Markdown of Layout) is Renderable = markdown =>
    layout2[EmptyTuple].render(markdown.asInstanceOf[Markdown of Layout across EmptyTuple])

  given layout2: [domains: Formattable] => (Markdown of Layout across domains) is Renderable:
    type Form = doms.html.whatwg.Flow

    def render(markdown: Markdown of Layout across domains): Html of whatwg.Flow =
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
                ( false,
                  tail,
                  (if block then Fragment("\n", content) else content) :: done,
                  tight )

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
            if item.nil then Li
            else Li(merge(false, item, Nil, tight)*)

          Ul(items2*)

        case Layout.OrderedList(line, start, tight, delimiter, items*) =>
          val items2 = items.map: item =>
            if item.nil then Li
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
          domains.format(info, code).or:
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

trait Markdown:
  type Topic <: Markdown.Node
  type Domain <: Label

  val linkRefs: List[Markdown.LinkRef]
  val children: Seq[Topic]

  override def equals(that: Any): Boolean = that match
    case that: Markdown => that.children == children
    case _              => false

  override def hashCode: Int = children.hashCode
