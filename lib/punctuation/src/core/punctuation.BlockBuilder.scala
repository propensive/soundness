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

// Mutable builders mirroring the leaf cases of `Layout`. Each accumulates
// content during the block-parse pass and produces a finalized `Layout` via
// `finish`. Container builders (BlockQuote, lists) come in a later stage.

sealed trait BlockBuilder:
  val line: Ordinal
  def finish(refs: LinkRefs): Layout

final class ParagraphBuilder(val line: Ordinal) extends BlockBuilder:
  private val lines: ArrayBuffer[Text] = ArrayBuffer()

  def addLine(text: Text): Unit = lines += text
  def isEmpty: Boolean = lines.isEmpty

  def joined: Text =
    val builder = new StringBuilder
    var first = true
    for line <- lines do
      if first then first = false else builder.append('\n')
      builder.append(line.s)
    Text(builder.toString)

  def finish(refs: LinkRefs): Layout =
    Layout.Paragraph(line, InlineParser.parse(joined, refs)*)

  // Setext headings rewrite an open paragraph as a heading; the underline
  // line itself is consumed by the dispatcher, not added here.
  def toHeading(level: 1 | 2 | 3 | 4 | 5 | 6, refs: LinkRefs): Layout =
    Layout.Heading(line, level, InlineParser.parse(joined, refs)*)

final class FencedCodeBlockBuilder
  ( val line:      Ordinal,
    val fenceChar: Char,
    val fenceLen:  Int,
    val indent:    Int,
    val info:      List[Text] )
extends BlockBuilder:
  private val content: ArrayBuffer[Text] = ArrayBuffer()

  def addLine(text: Text): Unit = content += text

  def finish(refs: LinkRefs): Layout =
    val builder = new StringBuilder
    for line <- content do
      builder.append(line.s)
      builder.append('\n')
    Layout.CodeBlock(line, info, Text(builder.toString))

final class IndentedCodeBlockBuilder(val line: Ordinal) extends BlockBuilder:
  // Holds raw content lines after stripping the 4-column indent. Trailing
  // blank lines are stripped at finish-time per the CommonMark spec.
  private val content: ArrayBuffer[Text] = ArrayBuffer()

  def addLine(text: Text): Unit = content += text

  def finish(refs: LinkRefs): Layout =
    while content.nonEmpty && ParserSupport.isBlank(content.last) do
      content.dropRightInPlace(1)
    val builder = new StringBuilder
    for line <- content do
      builder.append(line.s)
      builder.append('\n')
    Layout.CodeBlock(line, Nil, Text(builder.toString))
