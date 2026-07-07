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
┃    Soundness, version 0.63.0.                                                                    ┃
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
package decorum

import scala.collection.mutable

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.util.SourceFile

// One keyword's worth of information for a keyword-sequence chain. The head
// of `Sequence.elements` is K₁ — its (line, col) is the anchor point. For
// every later element, `line`/`col` is the keyword's position in the source
// and `startsLine` records whether it is the first code token on its line
// (used by the placement rule to distinguish "inline with the anchor" from
// "broken to its own line"). `bodyLine`/`bodyCol`/`bodyIndented` describe
// the body that follows the keyword and feed the body-cascade check.
//
// `else if` bridges are represented as a single element whose `label` is
// "else if" (or "else inline if", etc.), `line`/`col` track the leading
// `else`, and `body*` describe what follows the bridge's inner `then` — so
// the body cascade looks past the inline `if cond` predicate.
case class SeqElem
  ( label:        String,
    line:         Int,
    col:          Int,
    startsLine:   Boolean,
    bodyLine:     Int,
    bodyCol:      Int,
    bodyIndented: Boolean )

case class Sequence(elements: List[SeqElem]):
  def anchor: SeqElem = elements.head

object Sequences:
  // Walk the untyped tree and emit one `Sequence` per keyword-sequence
  // construct: `if/then/else`, `while/do`, `for/yield`, `for/do`,
  // `try/catch/finally`. Compared to a token-based search this avoids
  // re-discovering the parser's structure (case-guard `if`s, for-filter
  // `if`s, nested `try`s, etc. all naturally fall out — they are not
  // `If`/`Try` nodes in the relevant position).
  def extract(tree: untpd.Tree, source: SourceFile): List[Sequence] =
    val content   = String(source.content)
    val out       = mutable.ListBuffer[Sequence]()
    // If nodes whose top-level chain has been absorbed by an enclosing
    // `else if` bridge; we still descend into their sub-trees but don't
    // start a fresh top-level chain for them.
    val absorbed  = mutable.Set[Int]()

    def visit(t: untpd.Tree): Unit =
      t match
        case ifT: untpd.If if !absorbed.contains(ifT.span.start) =>
          mkIfSequence(ifT, content, source, absorbed).foreach(out += _)

        case w: untpd.WhileDo =>
          mkWhileSequence(w, content, source).foreach(out += _)

        case fy: untpd.ForYield =>
          mkForSequence(fy, fy.enums, fy.expr, "yield", content, source).foreach(out += _)

        case fd: untpd.ForDo =>
          mkForSequence(fd, fd.enums, fd.body, "do", content, source).foreach(out += _)

        case tr: untpd.ParsedTry =>
          mkTrySequence(tr, content, source).foreach(out += _)

        case _ => ()
      t.productIterator.foreach(descend(_, visit))

    visit(tree)
    out.toList

  private def descend(x: Any, visit: untpd.Tree => Unit): Unit = x match
    case sub: untpd.Tree  => visit(sub)
    case it:  Iterable[?] => it.foreach(descend(_, visit))
    case _                => ()

  // ── If chains ────────────────────────────────────────────────────────────

  private def mkIfSequence
    ( ifT:      untpd.If,
      content:  String,
      source:   SourceFile,
      absorbed: mutable.Set[Int] )
  :   Option[Sequence] =

    val sp = ifT.span
    if !sp.exists then return None
    val condSp  = ifT.cond.span
    val thenpSp = ifT.thenp.span
    if !condSp.exists || !thenpSp.exists then return None

    // K₁: the `if` keyword, possibly preceded by `inline`/`transparent
    // inline`. The span starts at the leftmost modifier (the parser captures
    // it), so the anchor column is `sp.start`'s column; the label is the
    // contiguous keyword run up to and including `if`.
    val ifOffset = findIfWithinSpan(content, sp.start)
    if ifOffset < 0 then return None
    val anchorLabel = labelFrom(content, sp.start, ifOffset + 2)
    val anchor      =
      makeElem
        ( label        = anchorLabel,
          pos          = sp.start,
          bodyStart    = condSp.start,
          content      = content,
          source       = source )

    // K₂: `then`, between cond's end and thenp's start. Some parser paths
    // give thenp a span that opens at `then` itself, so the search is
    // capped at thenp.span.end as a safety bound rather than thenp.start.
    val thenOffset = findKeyword(content, condSp.end, thenpSp.end, "then")
    if thenOffset < 0 then return None
    val thenElem =
      makeElem
        ( label        = "then",
          pos          = thenOffset,
          bodyStart    = bodyStartAfter(content, thenOffset + "then".length, thenpSp),
          content      = content,
          source       = source )

    val elems = mutable.ListBuffer[SeqElem](anchor, thenElem)
    extendElseChain(ifT, elems, content, source, absorbed)
    Some(Sequence(elems.toList))

  // Walk the else-chain of `ifT`. For each `else` we either fold an `else
  // if … then …` bridge into a single element (and keep walking the bridge's
  // inner If) or end the chain at this `else`. After at least one bridge,
  // the next `else` only continues the outer chain when it is broken to
  // the anchor column — an inline `else` past a bridge belongs to the
  // bridge unit, not to the outer chain.
  private def extendElseChain
    ( ifT:      untpd.If,
      elems:    mutable.ListBuffer[SeqElem],
      content:  String,
      source:   SourceFile,
      absorbed: mutable.Set[Int] )
  :   Unit =

    val anchorCol     = elems.head.col
    var current       = ifT
    var afterBridge   = false
    var done          = false
    while !done do
      val elseTree = current.elsep
      if !elseTree.span.exists || elseTree.isEmpty then done = true
      else
        val thenpEnd  = current.thenp.span.end
        val elseStart = elseTree.span.start
        val elseOffset = findKeyword(content, thenpEnd, elseStart + 1, "else")
        if elseOffset < 0 then done = true
        else
          val elseLine = source.offsetToLine(elseOffset) + 1
          val elseCol  = source.column(elseOffset) + 1
          val isBridgeShape =
            elseTree match
              case innerIf: untpd.If =>
                source.offsetToLine(innerIf.span.start) + 1 == elseLine

              case _ => false
          // After a bridge, an inline `else` (col ≠ anchorCol) belongs to
          // the bridge's inner If — not the outer chain. We skip past it
          // and (for inline bridges) continue walking the inner chain in
          // case a later `else` is broken to anchorCol.
          if afterBridge && elseCol != anchorCol then
            if isBridgeShape then
              val innerIf = elseTree.asInstanceOf[untpd.If]
              absorbed += innerIf.span.start
              current = innerIf
              // keep afterBridge = true; don't add to elems
            else
              done = true
          else
            elseTree match
              case innerIf: untpd.If if isBridgeShape =>
                val innerSpan   = innerIf.span
                val innerCondSp = innerIf.cond.span
                val innerThenSp = innerIf.thenp.span
                val innerIfKw   = findIfWithinSpan(content, innerSpan.start)
                if innerIfKw < 0 || !innerCondSp.exists || !innerThenSp.exists then
                  elems +=
                    makeElem
                      ( label        = "else",
                        pos          = elseOffset,
                        bodyStart    = elseStart,
                        content      = content,
                        source       = source )
                  done = true
                else
                  val innerThenOffset =
                    findKeyword(content, innerCondSp.end, innerThenSp.end, "then")
                  if innerThenOffset < 0 then
                    elems +=
                      makeElem
                        ( label        = "else",
                          pos          = elseOffset,
                          bodyStart    = elseStart,
                          content      = content,
                          source       = source )
                    done = true
                  else
                    val bridgeLabel = "else " +
                      labelFrom
                        ( content,
                          innerSpan.start,
                          innerIfKw + 2 )
                    val bridgeBody  =
                      bodyStartAfter
                        ( content,
                          innerThenOffset + "then".length,
                          innerThenSp )
                    elems +=
                      makeElem
                        ( label         = bridgeLabel,
                          pos           = elseOffset,
                          bodyStart     = bridgeBody,
                          content       = content,
                          source        = source,
                          bodyAnchorPos = innerThenOffset )
                    absorbed += innerIf.span.start
                    current     = innerIf
                    afterBridge = true

              case _ =>
                elems +=
                  makeElem
                    ( label        = "else",
                      pos          = elseOffset,
                      bodyStart    = elseStart,
                      content      = content,
                      source       = source )
                done = true

  // ── While / For / Try ────────────────────────────────────────────────────

  private def mkWhileSequence
    ( w:       untpd.WhileDo,
      content: String,
      source:  SourceFile )
  :   Option[Sequence] =

    val sp     = w.span
    val condSp = w.cond.span
    val bodySp = w.body.span
    if !sp.exists || !condSp.exists then return None
    // Old-style `while (cond) body` has no `do` keyword — no K₂, no rule.
    val doEnd    = if bodySp.exists then bodySp.end else sp.end
    val doOffset = findKeyword(content, condSp.end, doEnd, "do")
    if doOffset < 0 then return None
    val anchor =
      makeElem
        ( label        = "while",
          pos          = sp.start,
          bodyStart    = condSp.start,
          content      = content,
          source       = source )
    val doElem =
      makeElem
        ( label        = "do",
          pos          = doOffset,
          bodyStart    = bodyStartAfter(content, doOffset + "do".length, bodySp),
          content      = content,
          source       = source )
    Some(Sequence(List(anchor, doElem)))

  private def mkForSequence
    ( t:       untpd.Tree,
      enums:   List[untpd.Tree],
      body:    untpd.Tree,
      kw:      String, // "yield" or "do"
      content: String,
      source:  SourceFile )
  :   Option[Sequence] =

    val sp     = t.span
    val bodySp = body.span
    if !sp.exists then return None
    // The `for` keyword is at the start of the span. The K₂ keyword
    // (`yield` or `do`) follows the last enumerator.
    val enumEnd =
      enums.lastOption.flatMap{ e => if e.span.exists then Some(e.span.end) else None }
      . getOrElse(sp.start + 3)
    val kwUntil = if bodySp.exists then bodySp.end else sp.end
    val kwOffset = findKeyword(content, enumEnd, kwUntil, kw)
    if kwOffset < 0 then return None
    val firstEnumStart =
      enums.headOption.flatMap{ e => if e.span.exists then Some(e.span.start) else None }
      . getOrElse(sp.start + 4)
    val anchor =
      makeElem
        ( label        = "for",
          pos          = sp.start,
          bodyStart    = firstEnumStart,
          content      = content,
          source       = source )
    val kwElem =
      makeElem
        ( label        = kw,
          pos          = kwOffset,
          bodyStart    = bodyStartAfter(content, kwOffset + kw.length, bodySp),
          content      = content,
          source       = source )
    Some(Sequence(List(anchor, kwElem)))

  private def mkTrySequence
    ( tr:      untpd.ParsedTry,
      content: String,
      source:  SourceFile )
  :   Option[Sequence] =

    val sp     = tr.span
    val exprSp = tr.expr.span
    if !sp.exists || !exprSp.exists then return None
    val anchor =
      makeElem
        ( label        = "try",
          pos          = sp.start,
          bodyStart    = exprSp.start,
          content      = content,
          source       = source )
    val elems = mutable.ListBuffer[SeqElem](anchor)

    val handlerSp = tr.handler.span
    val finSp     = tr.finalizer.span
    val hasCatch  = !tr.handler.isEmpty && handlerSp.exists
    val hasFin    = !tr.finalizer.isEmpty && finSp.exists
    val catchOffset =
      if !hasCatch then -1
      else findKeyword(content, exprSp.end, handlerSp.end, "catch")
    if catchOffset >= 0 then
      elems +=
        makeElem
          ( label        = "catch",
            pos          = catchOffset,
            bodyStart    = handlerSp.start,
            content      = content,
            source       = source )

    if hasFin then
      val searchFrom = if hasCatch then handlerSp.end else exprSp.end
      val finOffset = findKeyword(content, searchFrom, finSp.start + 1, "finally")
      if finOffset >= 0 then
        elems +=
          makeElem
            ( label        = "finally",
              pos          = finOffset,
              bodyStart    = finSp.start,
              content      = content,
              source       = source )

    if elems.length < 2 then None else Some(Sequence(elems.toList))

  // ── helpers ──────────────────────────────────────────────────────────────

  // Build a `SeqElem` from an offset into the source and the body's start
  // offset. `bodyAnchorPos` is the keyword that immediately introduces the
  // body — for a normal element that's the keyword itself (`pos`); for an
  // `else if … then` bridge it's the inner `then`'s offset, so the body
  // cascade looks past the inline `if cond` predicate when deciding whether
  // the body is indented.
  private def makeElem
    ( label:         String,
      pos:           Int,
      bodyStart:     Int,
      content:       String,
      source:        SourceFile,
      bodyAnchorPos: Int = -1 )
  :   SeqElem =

    val bodyAnchor = if bodyAnchorPos < 0 then pos else bodyAnchorPos
    val line       = source.offsetToLine(pos) + 1
    val col        = source.column(pos) + 1
    val startsLine = isFirstCodeOnLine(content, pos)
    val bodyAnchorLine =
      if bodyAnchor >= 0 && bodyAnchor <= content.length
      then source.offsetToLine(bodyAnchor) + 1
      else line
    val bodyLine =
      if bodyStart >= 0 && bodyStart <= content.length
      then source.offsetToLine(bodyStart) + 1
      else line
    val bodyCol =
      if bodyStart >= 0 && bodyStart <= content.length
      then source.column(bodyStart) + 1
      else col
    val bodyIndented = bodyLine > bodyAnchorLine
    SeqElem(label, line, col, startsLine, bodyLine, bodyCol, bodyIndented)

  // True iff there is no non-whitespace, non-comment code on the same
  // source line strictly before `pos`. Used to distinguish a broken keyword
  // (starts its own line) from an inline one.
  private def isFirstCodeOnLine(content: String, pos: Int): Boolean =
    var i = pos - 1
    while i >= 0 && content.charAt(i) != '\n' do
      val c = content.charAt(i)
      if c != ' ' && c != '\t' then return false
      i -= 1
    true

  // Locate the next occurrence of `keyword` at a word boundary in
  // `content[from until until]`, skipping `//`-line and `/* */`-block
  // comments. Returns the byte offset, or -1 if not found.
  private def findKeyword(content: String, from: Int, until: Int, keyword: String): Int =
    val limit = until.min(content.length)
    var i     = from.max(0)
    while i + keyword.length <= limit do
      val c = content.charAt(i)
      if c == '/' && i + 1 < limit && content.charAt(i + 1) == '/' then
        while i < limit && content.charAt(i) != '\n' do i += 1
      else if c == '/' && i + 1 < limit && content.charAt(i + 1) == '*' then
        i += 2
        var closed = false
        while !closed && i + 1 < limit do
          if content.charAt(i) == '*' && content.charAt(i + 1) == '/' then
            i += 2
            closed = true
          else
            i += 1
      else if content.regionMatches(i, keyword, 0, keyword.length) then
        val ok = isWordBoundary(content, i, keyword.length)
        if ok then return i
        i += 1
      else
        i += 1
    -1

  private def isWordBoundary(content: String, i: Int, len: Int): Boolean =
    val before = if i > 0 then content.charAt(i - 1) else ' '
    val after  = if i + len < content.length then content.charAt(i + len) else ' '
    !isWordChar(before) && !isWordChar(after)

  private def isWordChar(c: Char): Boolean =
    c.isLetterOrDigit || c == '_' || c == '$'

  // Find the `if` keyword within an `If` node's span — needed because the
  // span may start at a leading `inline`/`transparent inline` modifier.
  private def findIfWithinSpan(content: String, start: Int): Int =
    findKeyword(content, start, content.length, "if")

  // Build a label from a contiguous keyword run by extracting the source
  // between `start` and `end` and collapsing whitespace.
  private def labelFrom(content: String, start: Int, end: Int): String =
    val safeEnd = end.min(content.length)
    val raw     = content.substring(start.max(0), safeEnd)
    raw.nn.split("\\s+").nn.iterator.collect { case s: String if s.nonEmpty => s }.mkString(" ")

  // The "body start" for a keyword is the first non-whitespace character
  // after the keyword. Fall back to the tree's body span if scanning past
  // whitespace runs off the end of the construct.
  private def bodyStartAfter
    ( content: String, from: Int, bodySp: dotty.tools.dotc.util.Spans.Span )
  :   Int =

    val end = content.length
    var i   = from
    while i < end && (content.charAt(i) == ' ' || content.charAt(i) == '\t') do
      i += 1
    if i < end && content.charAt(i) == '\n' then i += 1
    while i < end && (content.charAt(i) == ' ' || content.charAt(i) == '\t') do
      i += 1
    if i < end then i
    else if bodySp.exists then bodySp.start
    else from
