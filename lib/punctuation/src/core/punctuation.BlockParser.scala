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

import scala.collection.mutable.ArrayBuffer

import anticipation.*
import denominative.*
import prepositional.*
import vacuous.*
import zephyrine.*
import zephyrine.lineation.linefeedChars

// Pass 1 of the native parser: line-by-line dispatch with a single-leaf
// "open block" pointer. Stage 2 supports leaf blocks only (paragraph,
// ATX/setext heading, thematic break, fenced/indented code block); container
// blocks (blockquote, lists) come in stage 3, at which point this single
// `openLeaf` pointer becomes a stack of open builders.

final class BlockParser:
  private val children: ArrayBuffer[Layout] = ArrayBuffer()
  private val refs: LinkRefs = LinkRefs()
  private var openLeaf: Optional[BlockBuilder] = Unset

  def parse(text: Text): Markdown of Layout =
    val cursor = Cursor(Iterator(text))
    while cursor.more do
      val ln = cursor.line

      val line = cursor.hold:
        val start = cursor.mark
        val foundLf = cursor.seek('\n')
        val end = cursor.mark
        val captured = cursor.grab(start, end)
        if foundLf then cursor.advance()
        captured

      processLine(line, ln)

    closeOpenLeaf()
    Markdown(refs.all, children.toSeq*)

  private def closeOpenLeaf(): Unit = openLeaf match
    case Unset => ()

    case builder: BlockBuilder =>
      children += builder.finish(refs)
      openLeaf = Unset


  private def processLine(line: Text, ln: Ordinal): Unit = openLeaf match
    case fenced: FencedCodeBlockBuilder =>
      if ParserSupport.isFenceCloser(line, fenced.fenceChar, fenced.fenceLen)
      then closeOpenLeaf()
      else fenced.addLine(ParserSupport.stripIndent(line, fenced.indent))

    case indented: IndentedCodeBlockBuilder =>
      if ParserSupport.isBlank(line) then
        indented.addLine(ParserSupport.stripIndent(line, 4))
      else if ParserSupport.indentColumn(line) >= 4 then
        indented.addLine(ParserSupport.stripIndent(line, 4))
      else
        closeOpenLeaf()
        dispatchGeneral(line, ln)

    case _ => dispatchGeneral(line, ln)


  private def dispatchGeneral(line: Text, ln: Ordinal): Unit =
    if ParserSupport.isBlank(line) then
      openLeaf match
        case _: ParagraphBuilder => closeOpenLeaf()
        case _                   => ()
      return

    ParserSupport.fenceOpener(line) match
      case (ch: Char, count: Int, indent: Int, info: Text) =>
        closeOpenLeaf()
        val tokens = ParserSupport.cutInfo(info)
        openLeaf = FencedCodeBlockBuilder(ln, ch, count, indent, tokens)
        return

      case Unset => ()

    openLeaf match
      case para: ParagraphBuilder if !para.isEmpty =>
        ParserSupport.setextUnderline(line) match
          case 1 =>
            children += para.toHeading(1, refs)
            openLeaf = Unset
            return

          case 2 =>
            children += para.toHeading(2, refs)
            openLeaf = Unset
            return

          case Unset => ()

      case _ => ()

    if ParserSupport.isThematicBreak(line) then
      closeOpenLeaf()
      children += Layout.ThematicBreak(ln)
      return

    ParserSupport.atxHeading(line) match
      case (level: (1 | 2 | 3 | 4 | 5 | 6), content: Text) =>
        closeOpenLeaf()
        children += Layout.Heading(ln, level, InlineParser.parse(content, refs)*)
        return

      case Unset => ()

    if openLeaf.absent && ParserSupport.indentColumn(line) >= 4 then
      val indented = IndentedCodeBlockBuilder(ln)
      indented.addLine(ParserSupport.stripIndent(line, 4))
      openLeaf = indented
      return

    val text = ParserSupport.stripTrailingSpaces(line)
    openLeaf match
      case para: ParagraphBuilder => para.addLine(text)

      case _ =>
        closeOpenLeaf()
        val para = ParagraphBuilder(ln)
        para.addLine(text)
        openLeaf = para
