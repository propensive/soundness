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
import vacuous.*

// Mutable builder hierarchy for the block-parse pass. Containers hold a
// child buffer and "pass through" continuation lines (after stripping their
// own prefix) to the next-deeper open block. Leaves accumulate raw line
// content and finalize to a single `Layout` value.

sealed trait BlockBuilder:
  val line: Ordinal

sealed trait LeafBuilder extends BlockBuilder:
  def finish(refs: LinkRefs): Layout

sealed trait ContainerBuilder extends BlockBuilder:
  val children: ArrayBuffer[Layout] = ArrayBuffer()
  // Try to continue this container on the given line. Returns the remaining
  // (post-prefix) line content if it continues, or `Unset` if it doesn't.
  def tryContinue(line: Text): Optional[Text]
  def finish(refs: LinkRefs): Optional[Layout]
  // Whether a blank line at this point closes the container.
  def closesOnBlank: Boolean = false

// Document is the always-open root container; never closes until end-of-input.
final class DocumentBuilder extends ContainerBuilder:
  val line: Ordinal = denominative.Prim
  def tryContinue(line: Text): Optional[Text] = line
  def finish(refs: LinkRefs): Optional[Layout] = Unset

// `> ` blockquote container.
final class BlockQuoteBuilder(val line: Ordinal) extends ContainerBuilder:
  def tryContinue(line: Text): Optional[Text] =
    val s = line.s
    val n = s.length
    var i = 0
    var indent = 0
    while i < n && s.charAt(i) == ' ' && indent < 4 do { i += 1; indent += 1 }
    if indent >= 4 || i >= n || s.charAt(i) != '>' then return Unset
    i += 1
    if i < n && s.charAt(i) == ' ' then i += 1
    else if i < n && s.charAt(i) == '\t' then i += 1
    if i == 0 then line else Text(s.substring(i, n).nn)

  def finish(refs: LinkRefs): Optional[Layout] =
    Layout.BlockQuote(line, children.toSeq*)

// A list item with a known content-indent column count. The marker has
// already been stripped by the dispatcher when the item was opened.
final class ListItemBuilder(val line: Ordinal, val indent: Int) extends ContainerBuilder:
  // Tracks blank lines for tight/loose detection at the list level.
  var hadBlank: Boolean = false

  def tryContinue(line: Text): Optional[Text] =
    if ParserSupport.isBlank(line) then
      hadBlank = true
      // a blank line continues the item; pass through empty
      return Text("")
    if ParserSupport.indentColumn(line) >= indent then
      ParserSupport.stripIndent(line, indent)
    else
      Unset

  def finish(refs: LinkRefs): Optional[Layout] =
    Layout.Paragraph(line)  // placeholder; lists assemble items, not Paragraph

// Bullet list container holding a sequence of `ListItemBuilder` children
// (each finalised to a `List[Layout]` for the `Layout.BulletList` items*).
final class BulletListBuilder(val line: Ordinal, val marker: Char) extends ContainerBuilder:
  // Each entry is the children of one list item, in document order.
  val items: ArrayBuffer[List[Layout]] = ArrayBuffer()
  // True if any blank line has been observed between items (=> loose list).
  var loose: Boolean = false

  def tryContinue(line: Text): Optional[Text] = line

  def finish(refs: LinkRefs): Optional[Layout] =
    Layout.BulletList(line, !loose, items.toList*)

// Ordered list container; `delimiter` is `.` or `)`; `start` is the first
// item's number.
final class OrderedListBuilder
  ( val line:      Ordinal,
    val start:     Int,
    val delimiter: '.' | ')' )
extends ContainerBuilder:
  val items: ArrayBuffer[List[Layout]] = ArrayBuffer()
  var loose: Boolean = false

  def tryContinue(line: Text): Optional[Text] = line

  def finish(refs: LinkRefs): Optional[Layout] =
    val delim: Optional['.' | ')'] = delimiter
    Layout.OrderedList(line, start, !loose, delim, items.toList*)

final class ParagraphBuilder(val line: Ordinal) extends LeafBuilder:
  private val lines: ArrayBuffer[Text] = ArrayBuffer()

  def addLine(text: Text): Unit =
    // CommonMark §4.8: leading whitespace on each line of a paragraph is
    // stripped before forming the paragraph's raw content.
    val s = text.s
    val n = s.length
    var i = 0
    while i < n && (s.charAt(i) == ' ' || s.charAt(i) == '\t') do i += 1
    val stripped = if i == 0 then text else Text(s.substring(i, n).nn)
    lines += stripped

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
extends LeafBuilder:
  private val content: ArrayBuffer[Text] = ArrayBuffer()

  def addLine(text: Text): Unit = content += text

  def finish(refs: LinkRefs): Layout =
    val builder = new StringBuilder
    for line <- content do
      builder.append(line.s)
      builder.append('\n')
    Layout.CodeBlock(line, info, Text(builder.toString))

final class IndentedCodeBlockBuilder(val line: Ordinal) extends LeafBuilder:
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
