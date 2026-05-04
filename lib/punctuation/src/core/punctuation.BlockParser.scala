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

    // Post-pass: with the link-reference table now fully populated, run the
    // inline parser over each Paragraph/Heading's deferred raw text.
    val resolved = docBuilder.children.iterator.map(resolveInlines).toSeq
    Markdown(refs.all, resolved*)

  private def resolveInlines(layout: Layout): Layout = layout match
    case Layout.Paragraph(ln, Prose.Textual(raw)) =>
      Layout.Paragraph(ln, InlineParser.parse(raw, refs)*)

    case Layout.Heading(ln, level, Prose.Textual(raw)) =>
      Layout.Heading(ln, level, InlineParser.parse(raw, refs)*)

    case Layout.BlockQuote(ln, children*) =>
      Layout.BlockQuote(ln, children.map(resolveInlines)*)

    case Layout.BulletList(ln, tight, items*) =>
      Layout.BulletList(ln, tight, items.map(_.map(resolveInlines))*)

    case Layout.OrderedList(ln, start, tight, delim, items*) =>
      Layout.OrderedList(ln, start, tight, delim, items.map(_.map(resolveInlines))*)

    case other => other

  // ─── stack management ───────────────────────────────────────────────────

  private def deepest: BlockBuilder = openStack.last

  private def closeOne(): Unit =
    val builder = openStack.remove(openStack.length - 1)
    val parent = openStack.last
    builder match
      case item: ListItemBuilder =>
        parent match
          case bl: BulletListBuilder =>
            bl.items += item.children.toList
            if item.hadBlank then bl.pendingBlank = true

          case ol: OrderedListBuilder =>
            ol.items += item.children.toList
            if item.hadBlank then ol.pendingBlank = true

          case _ => panic(m"ListItem parent must be a List")

      case container: ContainerBuilder =>
        container.finish(refs).let: layout =>
          addToParent(parent, layout)
        // Propagate trailing-blank from a closing inner list to the
        // enclosing ListItem so the outer list can detect looseness.
        container match
          case bl: BulletListBuilder if bl.pendingBlank =>
            parent match
              case item: ListItemBuilder => item.hadBlank = true
              case _                     => ()

          case ol: OrderedListBuilder if ol.pendingBlank =>
            parent match
              case item: ListItemBuilder => item.hadBlank = true
              case _                     => ()

          case _ => ()

      case leaf: LeafBuilder =>
        leaf.finish(refs).let: layout =>
          addToParent(parent, layout)

  private def addToParent(parent: BlockBuilder, layout: Layout): Unit = parent match
    case item: ListItemBuilder =>
      // If a blank line came between this child and a previous block in the
      // same item, the enclosing list is loose. Don't reset hadBlank when
      // children was empty: the blank may still be relevant to a future
      // child added later.
      if item.hadBlank && item.children.nonEmpty then
        val itemIdx = openStack.indexOf(item)
        if itemIdx > 0 then openStack(itemIdx - 1) match
          case bl: BulletListBuilder  => bl.loose = true
          case ol: OrderedListBuilder => ol.loose = true
          case _                      => ()

        item.hadBlank = false

      item.children += layout

    case container: ContainerBuilder => container.children += layout
    case _                           => panic(m"cannot add child to a leaf builder")

  private def closeAll(): Unit = while openStack.length > 1 do closeOne()

  private def closeFromIndex(idx: Int): Unit =
    while openStack.length > idx do closeOne()

  private def closeOpenLeafForNewBlock(): Unit = deepest match
    case _: LeafBuilder => closeOne()
    case _              => ()

  // True if `residual` is a list-item marker AND we're inside any list (of
  // either kind). Inside an existing list, list markers always start new
  // items or terminate the list — they never continue an open paragraph.
  private def insideAnyListWithListMarker(residual: Text): Boolean =
    val bm = ParserSupport.bulletMarker(residual)
    val om = ParserSupport.orderedMarker(residual)
    if bm.absent && om.absent then return false
    var idx = openStack.length - 1
    while idx >= 0 do
      openStack(idx) match
        case _: BulletListBuilder | _: OrderedListBuilder | _: ListItemBuilder =>
          return true

        case _ => ()
      idx -= 1
    false

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
      val colAfterMarker = indent + 1
      var leftover = 0
      var startCol = colAfterMarker

      if i < n then s.charAt(i) match
        case ' ' =>
          i += 1; startCol = colAfterMarker + 1

        case '\t' =>
          val advance = 4 - (colAfterMarker & 3)
          leftover = advance - 1
          i += 1
          startCol = colAfterMarker + advance

        case _ => ()

      closeOpenLeafForNewBlock()
      val bq = BlockQuoteBuilder(ln)
      openStack += bq
      val tail = if i >= n then "" else s.substring(i, n).nn
      val rest = Text(ParserSupport.buildResidual(tail, startCol, leftover))
      return (rest, true)

    // Thematic break has higher priority than bullet/ordered list markers
    // (a line of `- - -` is a thematic break, not a list with content `- -`).
    if ParserSupport.isThematicBreak(line) then return (line, false)

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
      // Close any wrong-kind List on top of the stack. If the deepest is a
      // ListItem (we're opening a sublist inside it), keep the LI open.
      deepest match
        case _: BulletListBuilder | _: OrderedListBuilder => closeOne()
        case _                                            => ()

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

      // The previous item closed with a trailing blank line — sibling-item
      // separation means the list is loose.
      deepest match
        case bl: BulletListBuilder if bl.pendingBlank =>
          bl.loose = true; bl.pendingBlank = false

        case ol: OrderedListBuilder if ol.pendingBlank =>
          ol.loose = true; ol.pendingBlank = false

        case _ => ()

    val contentIndent = bullet.map(_.contentIndent).getOrElse(ordered.get.contentIndent)
    val item = ListItemBuilder(ln, contentIndent.n0)
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

    // Setext heading promotion. If walkContainers fully matched (the deepest
    // open block is reachable from the current container context) and the
    // residual is a setext underline, promote the open paragraph to a heading.
    // Must run before lazy continuation, otherwise the underline gets absorbed
    // as paragraph content.
    val paraReachable = matchedDepth >= openStack.length - 1
    if paraReachable then
      deepest match
        case para: ParagraphBuilder if !para.isEmpty =>
          ParserSupport.setextUnderline(residual0) match
            case lvl @ (1 | 2) =>
              val maybeHeading = para.toHeading(lvl, refs)
              openStack.remove(openStack.length - 1)
              maybeHeading.let: heading =>
                addToParent(deepest, heading)
              if maybeHeading.present then return
              // else: paragraph turned out to be all link reference defs;
              // the setext underline has nothing to promote, so it falls
              // through and becomes a regular line.

            case Unset => ()

        case _ => ()

    // Lazy paragraph continuation
    val deepestIsParagraph = deepest.isInstanceOf[ParagraphBuilder]

    val canLazyContinue =
      deepestIsParagraph
      && !ParserSupport.startsNonParagraphBlock(residual0, paragraphOpen = true)
      && !ParserSupport.isBlank(residual0)
      && !insideAnyListWithListMarker(residual0)

    if canLazyContinue then
      val para = deepest.asInstanceOf[ParagraphBuilder]
      para.addLine(residual0)
      return

    if matchedDepth < openStack.length then closeFromIndex(matchedDepth)

    // After failed containers close (Para closes → its content joins parent
    // LI's children), check what the blank means. Find the deepest open
    // ListItem (skipping a trailing leaf if any) and mark hadBlank for
    // loose-list and empty-item handling. Don't close the item here:
    // CommonMark §5.2 only terminates an empty item when the next non-blank
    // content arrives that isn't a sibling marker — that's handled in
    // `ListItemBuilder.tryContinue` when it returns Unset for a non-blank
    // line on an empty item with hadBlank=true.
    if ParserSupport.isBlank(residual0) then
      var idx = openStack.length - 1
      while idx >= 0 && openStack(idx).isInstanceOf[LeafBuilder] do idx -= 1
      if idx >= 0 then openStack(idx) match
        case item: ListItemBuilder => item.hadBlank = true
        case _                     => ()

    val residual = tryOpenContainers(residual0, ln)
    placeLeaf(residual, ln)

  private def placeLeaf(residual: Text, ln: Ordinal): Unit =
    // If a List (not ListItem) is on top of the stack, no new same-kind item
    // opened in Phase 2, so the list closes here before placing the leaf.
    deepest match
      case _: BulletListBuilder | _: OrderedListBuilder => closeOne()
      case _                                            => ()

    if ParserSupport.isBlank(residual) then
      deepest match
        case _: ParagraphBuilder => closeOne()
        case _                   => ()
      return

    ParserSupport.fenceOpener(residual) match
      case (ch: Char, count: Int, indent: Ordinal, info: Text) =>
        closeOpenLeafForNewBlock()
        val tokens = ParserSupport.cutInfo(info).map(InlineSupport.decodeEscapesAndEntities)
        val fenced = FencedCodeBlockBuilder(ln, ch, count, indent.n0, tokens)
        openStack += fenced
        return

      case Unset => ()

    deepest match
      case para: ParagraphBuilder if !para.isEmpty =>
        ParserSupport.setextUnderline(residual) match
          case lvl @ (1 | 2) =>
            val maybeHeading = para.toHeading(lvl, refs)
            openStack.remove(openStack.length - 1)
            maybeHeading.let: heading =>
              addToParent(deepest, heading)
            if maybeHeading.present then return
            // else: paragraph was all link-ref-defs; fall through.

          case Unset => ()

      case _ => ()

    if ParserSupport.isThematicBreak(residual) then
      closeOpenLeafForNewBlock()
      addToParent(deepest, Layout.ThematicBreak(ln))
      return

    ParserSupport.atxHeading(residual) match
      case (level: (1 | 2 | 3 | 4 | 5 | 6), content: Text) =>
        closeOpenLeafForNewBlock()
        addToParent(deepest, Layout.Heading(ln, level, Prose.Textual(content)))
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

    // Trailing whitespace per-line is preserved so the inline parser can
    // detect hard line breaks via the two-trailing-spaces rule.
    deepest match
      case para: ParagraphBuilder => para.addLine(residual)

      case _ =>
        closeOpenLeafForNewBlock()
        val para = ParagraphBuilder(ln)
        para.addLine(residual)
        openStack += para
