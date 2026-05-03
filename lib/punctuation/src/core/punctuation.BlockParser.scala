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
import fulminate.*
import gossamer.*
import prepositional.*
import vacuous.*
import zephyrine.*
import zephyrine.lineation.linefeedChars

// Pass 1: line-by-line dispatch over a stack of open block builders. The
// algorithm follows CommonMark's "phase 1" (block parsing):
//
//   1. For each line, walk down the open-block stack and ask each container
//      builder to "continue" — for blockquotes that means stripping `> `,
//      for list items it means stripping the required indent.
//   2. Containers that can't continue terminate the walk; they get closed
//      from the failure point downward (with `finish`), unless lazy
//      paragraph continuation rescues them.
//   3. With the residual line content, repeatedly try to open new container
//      blocks (`>`, list markers).
//   4. The leftover content goes into a leaf — heading, code block, or
//      paragraph (start new or continue existing).

final class BlockParser:
  private val refs: LinkRefs = LinkRefs()
  private val docBuilder: DocumentBuilder = DocumentBuilder()
  private val openStack: ArrayBuffer[BlockBuilder] = ArrayBuffer(docBuilder)

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

    closeAll()
    Markdown(refs.all, docBuilder.children.toSeq*)

  // ─── stack management ───────────────────────────────────────────────────

  private def deepest: BlockBuilder = openStack.last

  private def closeOne(): Unit =
    val builder = openStack.remove(openStack.length - 1)
    val parent = openStack.last
    builder match
      case item: ListItemBuilder =>
        parent match
          case bl: BulletListBuilder  => bl.items += item.children.toList
          case ol: OrderedListBuilder => ol.items += item.children.toList
          case _                      => panic(m"ListItem parent must be a List")

      case container: ContainerBuilder =>
        container.finish(refs).let: layout =>
          addToParent(parent, layout)

      case leaf: LeafBuilder =>
        addToParent(parent, leaf.finish(refs))

  private def addToParent(parent: BlockBuilder, layout: Layout): Unit = parent match
    case item: ListItemBuilder       => item.children += layout
    case container: ContainerBuilder => container.children += layout
    case _                           => panic(m"cannot add child to a leaf builder")

  private def closeAll(): Unit = while openStack.length > 1 do closeOne()

  private def closeFromIndex(idx: Int): Unit =
    while openStack.length > idx do closeOne()

  private def closeOpenLeafForNewBlock(): Unit = deepest match
    case _: LeafBuilder => closeOne()
    case _              => ()

  private def closeListChainAtTop(): Unit =
    var done = false
    while !done do
      deepest match
        case _: ListItemBuilder    => closeOne()
        case _: BulletListBuilder  => closeOne()
        case _: OrderedListBuilder => closeOne()
        case _                     => done = true

  // ─── phase 1: walk containers ───────────────────────────────────────────

  // Returns (firstUnmatchedIdx, residualLine). Indices < firstUnmatchedIdx
  // are still-open containers that successfully continued.
  private def walkContainers(line: Text): (Int, Text) =
    var idx = 1
    var remaining = line
    var continueLoop = true
    while idx < openStack.length && continueLoop do
      openStack(idx) match
        case container: ContainerBuilder =>
          val cont = container.tryContinue(remaining)
          if cont.absent then continueLoop = false
          else
            remaining = cont.vouch
            idx += 1

        case _: LeafBuilder => continueLoop = false
    (idx, remaining)

  // ─── phase 3: try to open new container blocks repeatedly ───────────────

  private def tryOpenContainers(line: Text, ln: Ordinal): Text =
    var current = line
    var keepGoing = true
    while keepGoing do
      val (next, opened) = tryOpenOneContainer(current, ln)
      if opened then current = next else keepGoing = false
    current

  private def tryOpenOneContainer(line: Text, ln: Ordinal): (Text, Boolean) =
    // BlockQuote `>`
    val s = line.s
    val n = s.length
    var i = 0
    var indent = 0
    while i < n && s.charAt(i) == ' ' && indent < 4 do { i += 1; indent += 1 }
    if indent < 4 && i < n && s.charAt(i) == '>' then
      i += 1
      if i < n && s.charAt(i) == ' ' then i += 1
      else if i < n && s.charAt(i) == '\t' then i += 1
      closeOpenLeafForNewBlock()
      val bq = BlockQuoteBuilder(ln)
      openStack += bq
      val rest = if i >= n then t"" else Text(s.substring(i, n).nn)
      return (rest, true)

    // Bullet list marker
    ParserSupport.bulletMarker(line) match
      case bm: BulletMarker =>
        return openListItem(ln, Some(bm), None, line)

      case Unset => ()

    // Ordered list marker
    ParserSupport.orderedMarker(line) match
      case om: OrderedMarker =>
        // CommonMark: an ordered list cannot interrupt a paragraph unless it
        // starts at 1
        deepest match
          case _: ParagraphBuilder if om.start != 1 => ()

          case _ =>
            return openListItem(ln, None, Some(om), line)

      case Unset => ()

    (line, false)

  // Open a new ListItem (and a parent List if needed).
  private def openListItem
    ( ln:      Ordinal,
      bullet:  Option[BulletMarker],
      ordered: Option[OrderedMarker],
      line:    Text )
  :   (Text, Boolean) =

    val rest: Text = bullet.map(_.rest).orElse(ordered.map(_.rest)).get

    // CommonMark: a list item with empty content cannot interrupt a paragraph
    deepest match
      case _: ParagraphBuilder if ParserSupport.isBlank(rest) =>
        return (line, false)

      case _ => ()

    // Decide whether to reuse the open List or open a new one
    val reuseList: Boolean = deepest match
      case bl: BulletListBuilder =>
        bullet match
          case Some(bm) => bl.marker == bm.char
          case None     => false

      case ol: OrderedListBuilder =>
        ordered match
          case Some(om) => ol.delimiter == om.delimiter
          case None     => false

      case _ => false

    if !reuseList then
      closeOpenLeafForNewBlock()
      closeListChainAtTop()

      val newList: ContainerBuilder = bullet match
        case Some(bm) => BulletListBuilder(ln, bm.char)

        case None =>
          val om = ordered.get
          OrderedListBuilder(ln, om.start, om.delimiter)

      openStack += newList
    else
      // The open list of matching kind is at the top; an open list-item
      // would have been closed during phase-1 walk if its indent didn't
      // match, but in case it didn't we close any item now.
      closeOpenLeafForNewBlock()
      deepest match
        case _: ListItemBuilder => closeOne()
        case _                  => ()

    val contentIndent = bullet.map(_.contentIndent).getOrElse(ordered.get.contentIndent)
    val item = ListItemBuilder(ln, contentIndent)
    openStack += item

    (rest, true)

  // ─── per-line dispatch ──────────────────────────────────────────────────

  private def processLine(originalLine: Text, ln: Ordinal): Unit =
    val (matchedDepth, residual0) = walkContainers(originalLine)

    // Active fenced/indented code block at the deepest position bypasses
    // opening logic — feed it the residual after matched containers.
    if matchedDepth < openStack.length then
      openStack(matchedDepth) match
        case fenced: FencedCodeBlockBuilder =>
          if ParserSupport.isFenceCloser(residual0, fenced.fenceChar, fenced.fenceLen)
          then closeFromIndex(matchedDepth)
          else fenced.addLine(ParserSupport.stripIndent(residual0, fenced.indent))
          return

        case indented: IndentedCodeBlockBuilder =>
          if ParserSupport.isBlank(residual0) then
            indented.addLine(ParserSupport.stripIndent(residual0, 4))
            return
          else if ParserSupport.indentColumn(residual0) >= 4 then
            indented.addLine(ParserSupport.stripIndent(residual0, 4))
            return
          else
            // close indented code, fall through
            closeFromIndex(matchedDepth)

        case _ => ()

    // Lazy paragraph continuation
    val deepestIsParagraph = deepest.isInstanceOf[ParagraphBuilder]

    val canLazyContinue =
      deepestIsParagraph
      && !ParserSupport.startsNonParagraphBlock(residual0)
      && !ParserSupport.isBlank(residual0)

    if canLazyContinue then
      val para = deepest.asInstanceOf[ParagraphBuilder]
      para.addLine(ParserSupport.stripTrailingSpaces(residual0))
      return

    // Mark blank-line-induced loose flag on enclosing list items before
    // closing failed containers
    if matchedDepth < openStack.length && ParserSupport.isBlank(residual0) then
      var idx = 1
      while idx < openStack.length do
        openStack(idx) match
          case item: ListItemBuilder => item.hadBlank = true
          case _                     => ()
        idx += 1

    if matchedDepth < openStack.length then closeFromIndex(matchedDepth)

    val residual = tryOpenContainers(residual0, ln)
    placeLeaf(residual, ln)

  private def placeLeaf(residual: Text, ln: Ordinal): Unit =
    if ParserSupport.isBlank(residual) then
      deepest match
        case _: ParagraphBuilder => closeOne()
        case _                   => ()
      return

    ParserSupport.fenceOpener(residual) match
      case (ch: Char, count: Int, indent: Int, info: Text) =>
        closeOpenLeafForNewBlock()
        val tokens = ParserSupport.cutInfo(info)
        val fenced = FencedCodeBlockBuilder(ln, ch, count, indent, tokens)
        openStack += fenced
        return

      case Unset => ()

    deepest match
      case para: ParagraphBuilder if !para.isEmpty =>
        ParserSupport.setextUnderline(residual) match
          case 1 =>
            openStack.remove(openStack.length - 1)
            addToParent(deepest, para.toHeading(1, refs))
            return

          case 2 =>
            openStack.remove(openStack.length - 1)
            addToParent(deepest, para.toHeading(2, refs))
            return

          case Unset => ()

      case _ => ()

    if ParserSupport.isThematicBreak(residual) then
      closeOpenLeafForNewBlock()
      addToParent(deepest, Layout.ThematicBreak(ln))
      return

    ParserSupport.atxHeading(residual) match
      case (level: (1 | 2 | 3 | 4 | 5 | 6), content: Text) =>
        closeOpenLeafForNewBlock()
        addToParent(deepest, Layout.Heading(ln, level, InlineParser.parse(content, refs)*))
        return

      case Unset => ()

    val canStartIndentedCode = deepest match
      case _: LeafBuilder => false
      case _              => true

    if canStartIndentedCode && ParserSupport.indentColumn(residual) >= 4 then
      val indented = IndentedCodeBlockBuilder(ln)
      indented.addLine(ParserSupport.stripIndent(residual, 4))
      openStack += indented
      return

    val text = ParserSupport.stripTrailingSpaces(residual)
    deepest match
      case para: ParagraphBuilder => para.addLine(text)

      case _ =>
        closeOpenLeafForNewBlock()
        val para = ParagraphBuilder(ln)
        para.addLine(text)
        openStack += para
