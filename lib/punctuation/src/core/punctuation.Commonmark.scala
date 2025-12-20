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
┃    Soundness, version 0.46.0.                                                                    ┃
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

import ambience.*
import anticipation.*
import contingency.*
import denominative.*
import fulminate.*
import gossamer.*
import hieroglyph.*
import honeycomb.*
import kaleidoscope.*
import prepositional.*
import rudiments.*
import turbulence.*
import vacuous.*

import charDecoders.utf8
import charEncoders.utf8
import textSanitizers.skip
import environments.java
import temporaryDirectories.system

import doms.html.whatwg

object Commonmark:
  import org.commonmark.*, node.*, parser.{IncludeSourceSpans, Parser}

  def parse(text: soundness.Text): Markdown of Layout =
    object visitor extends AbstractVisitor:
      private var root: Optional[Markdown of Layout] = Unset
      private var refs: List[Markdown.LinkRef] = Nil
      private var layouts: List[Layout] = Nil
      private var proses: List[Prose] = Nil
      private var listItemsList: List[List[Layout]] = Nil

      def markdown: Markdown of Layout = root.or(panic(m"Failed to parse Markdown"))

      def layout(node: Node | Null): List[Layout] =
        val old = layouts
        layouts = Nil
        visitChildren(node)
        layouts.reverse.also:
          layouts = old

      def listItems(node: Node | Null): List[List[Layout]] =
        val old = listItemsList
        listItemsList = Nil
        visitChildren(node)
        listItemsList.reverse.also:
          listItemsList = old

      def prose(node: Node | Null): List[Prose] =
        val old = proses
        proses = Nil
        visitChildren(node)
        proses.reverse.also:
          proses = old

      def add(prose: Prose): Unit = proses ::= prose
      def add(layout: Layout): Unit = layouts ::= layout

      protected def line(node: Node | Null) = node.nn.getSourceSpans.nn.get(0).nn.getLineIndex.z

      override def visit(node: Document | Null): Unit =
        visitChildren(node.nn)
        root = Markdown(refs.reverse, layouts.reverse*)

      override def visit(node: Paragraph | Null): Unit =
        add(Layout.Paragraph(line(node), prose(node)*))

      override def visit(node: Text | Null): Unit = add(Prose.Textual(node.nn.getLiteral.nn))
      override def visit(node: Emphasis | Null): Unit = add(Prose.Emphasis(prose(node)*))

      override def visit(node: BlockQuote | Null): Unit =
        add(Layout.BlockQuote(line(node), layout(node)*))

      override def visit(node: Code | Null): Unit = add(Prose.Code(node.nn.getLiteral.nn))
      override def visit(node: HardLineBreak | Null): Unit = add(Prose.Linebreak)

      override def visit(node: BulletList | Null): Unit =
        val ln = line(node)
        add(Layout.BulletList(ln, node.nn.isTight, listItems(node)*))

      override def visit(node: FencedCodeBlock | Null): Unit =
        val info = node.nn.getInfo.nn.tt
        val info2 = if info.trim == t"" then Nil else info.trim.cut(r" +")
        add(Layout.CodeBlock(line(node), info2, node.nn.getLiteral.nn))

      override def visit(node: Heading | Null): Unit =
        val level: 1 | 2 | 3 | 4 | 5 | 6 = node.nn.getLevel match
          case level: (1 | 2 | 3 | 4 | 5 | 6) => level
          case _                              => panic(m"unexpected level number")

        add(Layout.Heading(line(node), level, prose(node)*))

      override def visit(node: HtmlBlock | Null): Unit =
        add(Layout.HtmlBlock(line(node), node.nn.getLiteral.nn))

      override def visit(node: HtmlInline | Null): Unit =
        add(Prose.HtmlInline(node.nn.getLiteral.nn))

      override def visit(node: Image | Null): Unit =
        val title = Optional(node.nn.getTitle).let(_.tt)
        add(Prose.Image(node.nn.getDestination.nn, title, prose(node)*))

      override def visit(node: IndentedCodeBlock | Null): Unit =
        add(Layout.CodeBlock(line(node), Nil, node.nn.getLiteral.nn))

      override def visit(node: Link | Null): Unit = add:
        val title = Optional(node.nn.getTitle).let(_.tt)
        Prose.Link(node.nn.getDestination.nn, title, prose(node)*)

      override def visit(node : LinkReferenceDefinition | Null): Unit =
        val label = node.nn.getLabel.nn
        val title = Optional(node.nn.getTitle).let(_.tt)
        val destination = node.nn.getDestination.nn
        refs ::= Markdown.LinkRef(label, title, destination)

      override def visit(node: ListItem | Null): Unit =
        listItemsList ::= layout(node)

      override def visit(node: OrderedList | Null): Unit = add:
        val delimiter: Optional['.' | ')'] = node.nn.getMarkerDelimiter match
          case "."  => '.'
          case ")"  => ')'
          case null => Unset

        val start: Int = node.nn.getMarkerStartNumber match
          case null     => 1
          case int: Int => int

        Layout.OrderedList(line(node), start, node.nn.isTight, delimiter, listItems(node)*)

      override def visit(node: SoftLineBreak | Null): Unit = add(Prose.Softbreak)
      override def visit(node: StrongEmphasis | Null): Unit = add(Prose.Strong(prose(node)*))
      override def visit(node: ThematicBreak | Null): Unit = add(Layout.ThematicBreak(line(node)))

    Parser.builder().nn
    . includeSourceSpans(IncludeSourceSpans.BLOCKS).nn
    . build().nn
    . parse(text.s).nn
    . accept(visitor)

    visitor.markdown
