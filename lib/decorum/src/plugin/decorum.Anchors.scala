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
┃    Soundness, version 0.64.0.                                                                    ┃
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
import dotty.tools.dotc.util.Spans.Span

// A tree-derived model of the indented scopes ("anchor frames") open at each
// source line. Built once per file from the untyped tree; the line-by-line
// scope tracking in the checker (rules 473.8/.1/.9) is due to be ported onto
// this model. For now the model has no consumers: it is built and tested,
// but does not affect the checker's behaviour.
object Anchors:

  // The construct whose trailing token opens the scope. `ChainCall` replaces
  // `ColonBlock`/`LambdaBody` when the opener line begins with `.` (a chain
  // method application, whose body indent is governed by the R31 deep-step
  // allowance rather than a fixed anchor + 2).
  enum FrameKind:
    case ColonBlock, MatchBody, LambdaBody, DefnRhs, Signature, QuoteSplice, ChainCall

  // One open indented scope. `anchorLine`/`anchorCol` locate the leftmost
  // non-whitespace column of the anchor token's line — the leftward
  // extension of §"Indented scopes and their anchor" in
  // doc/standards/syntax.md — and `bodyCol` is anchorCol + 2. `openLine` is
  // the line whose end opens the scope; `endLine` is the last line covered
  // by the scope's body.
  case class Frame
    ( kind:       FrameKind,
      anchorLine: Int,
      anchorCol:  Int,
      bodyCol:    Int,
      openLine:   Int,
      endLine:    Int )

  // All frames of a file, sorted outermost-first (by opener line, then by
  // decreasing extent). `stackAt` gives the frames whose body region covers
  // the given line, outermost first — so the innermost frame is last.
  class AnchorModel(val frames: List[Frame], val lineCount: Int):
    def stackAt(line: Int): List[Frame] =
      frames.filter: frame => line > frame.openLine && line <= frame.endLine

    def innermostAt(line: Int): Option[Frame] = stackAt(line).lastOption

  def build(tree: untpd.Tree, source: SourceFile, text: String): AnchorModel =
    val out     = mutable.ListBuffer[Frame]()
    val lineMap = LineMap(text)

    walk(tree): t =>
      t match
        case m: untpd.Match       => matchFrame(m, text, lineMap).foreach(out += _)
        case c: untpd.CaseDef     => caseFrame(c, text, lineMap).foreach(out += _)
        case f: untpd.Function    => functionFrame(f, text, lineMap).foreach(out += _)
        case d: untpd.ValOrDefDef => defnFrame(d, text, lineMap).foreach(out += _)
        case tm: untpd.Template   => templateFrame(tm, text, lineMap).foreach(out += _)
        case a: untpd.Apply       => applyFrame(a, text, lineMap).foreach(out += _)
        case q: untpd.Quote       => spanFrame(q.span, text, lineMap).foreach(out += _)
        case s: untpd.Splice      => spanFrame(s.span, text, lineMap).foreach(out += _)
        case _                    => ()

    val frames =
      out.toList.distinct.sortBy: frame => (frame.openLine, -frame.endLine)

    AnchorModel(frames, text.count(_ == '\n') + 1)

  // A `match` whose first case sits on a later line than the `match` keyword
  // opens a scope over the case region.
  private def matchFrame(m: untpd.Match, content: String, lineMap: LineMap): Option[Frame] =
    val selSp = m.selector.span

    if !m.span.exists || m.span.isZeroExtent || !selSp.exists then None
    else
      m.cases match
        case first :: _ if first.span.exists =>
          val matchOffset = findKeyword(content, selSp.end, first.span.start, "match")

          if matchOffset < 0 then None
          else
            val openLine = lineMap.lineOf(matchOffset)

            if lineMap.lineOf(first.span.start) <= openLine then None
            else
              val endLine = lineMap.endLineOf(m.span)
              Some(mkFrame(FrameKind.MatchBody, openLine, openLine, endLine, content, lineMap))

        case _ =>
          None

  // A `case` whose body starts on a later line than its `=>` opens a scope
  // over the body lines.
  private def caseFrame(c: untpd.CaseDef, content: String, lineMap: LineMap): Option[Frame] =
    val bodySp = c.body.span

    if !bodySp.exists || bodySp.isZeroExtent || !c.pat.span.exists then None
    else
      val from =
        if !c.guard.isEmpty && c.guard.span.exists then c.guard.span.end else c.pat.span.end

      arrowFrame(from, bodySp, content, lineMap)

  // A function literal whose body starts on a later line than its `=>` opens
  // a scope over the body lines.
  private def functionFrame(f: untpd.Function, content: String, lineMap: LineMap): Option[Frame] =
    val bodySp = f.body.span

    if !bodySp.exists || bodySp.isZeroExtent then None
    else
      val from =
        f.args.lastOption match
          case Some(arg) if arg.span.exists => arg.span.end
          case _                            => if f.span.exists then f.span.start else -1

      if from < 0 then None else arrowFrame(from, bodySp, content, lineMap)

  // Shared tail of `caseFrame`/`functionFrame`: locate the `=>` between the
  // parameters (or pattern) and the body, and frame the body region if it
  // starts on a later line. The parser sometimes gives the body a span that
  // opens at the `=>` itself (a `case` body wrapped in a `Block`), so the
  // search is bounded just past the body's start and the true body start is
  // the first non-whitespace character after the arrow. A chain opener line
  // (`. map: x =>`) yields a `ChainCall` frame instead of `LambdaBody`.
  private def arrowFrame
    ( from:    Int,
      bodySp:  Span,
      content: String,
      lineMap: LineMap )
  :   Option[Frame] =

    val arrowOffset = findKeyword(content, from, bodySp.start + 2, "=>")

    if arrowOffset < 0 then None
    else
      val openLine  = lineMap.lineOf(arrowOffset)
      val bodyStart = nextNonWs(content, arrowOffset + 2, bodySp.end)

      if bodyStart < 0 || lineMap.lineOf(bodyStart) <= openLine then None
      else
        val kind = chainAware(FrameKind.LambdaBody, content, lineMap, openLine)
        Some(mkFrame(kind, openLine, openLine, lineMap.endLineOf(bodySp), content, lineMap))

  // A definition whose right-hand side starts on a later line than its `=`
  // opens a scope over the RHS lines. The anchor is the definition keyword's
  // line (leftward-extended past any leading modifiers), not the `=` line.
  private def defnFrame(d: untpd.ValOrDefDef, content: String, lineMap: LineMap): Option[Frame] =
    if !d.span.exists then None
    else
      d.unforcedRhs match
        case rhs: untpd.Tree @unchecked if !rhs.isEmpty =>
          val sp = rhs.span

          if !sp.exists || sp.isZeroExtent then None
          else
            val eq = prevNonWs(content, sp.start)

            if eq < 0 || content.charAt(eq) != '=' then None
            else
              val openLine = lineMap.lineOf(eq)

              if lineMap.lineOf(sp.start) <= openLine then None
              else
                val point      = d.span.point.max(0).min(content.length - 1)
                val anchorLine = lineMap.lineOf(point)
                val endLine    = lineMap.endLineOf(sp)
                Some(mkFrame(FrameKind.DefnRhs, anchorLine, openLine, endLine, content, lineMap))

        case _ =>
          None

  // A template body (`class Foo:`, `object Bar:`, `enum Quux:`) whose first
  // statement sits on a later line than the introducing `:` opens a scope
  // over the statements.
  private def templateFrame(t: untpd.Template, content: String, lineMap: LineMap): Option[Frame] =
    t.unforcedBody match
      case stats: List[untpd.Tree @unchecked] =>
        val spanned =
          stats.filter: s => s.span.exists && !s.span.isZeroExtent

        spanned match
          case first :: _ =>
            val endLine = lineMap.endLineOf(spanned.last.span)
            colonFrame(first.span.start, endLine, content, lineMap)

          case Nil =>
            None

      case _ =>
        None

  // A colon-block application (`recv:` with an indented body): the last
  // argument starts on a later line, and the last code character before it
  // is a line-final `:` after the receiver.
  private def applyFrame(a: untpd.Apply, content: String, lineMap: LineMap): Option[Frame] =
    a.args.lastOption match
      case Some(arg) if arg.span.exists && !arg.span.isZeroExtent && a.fun.span.exists =>
        val colon = prevNonWs(content, arg.span.start)

        if colon < a.fun.span.end then None
        else colonFrame(arg.span.start, lineMap.endLineOf(arg.span), content, lineMap)

      case _ =>
        None

  // Shared tail of `templateFrame`/`applyFrame`: the body region beginning
  // at `bodyStart` is framed if the last code character before it is a
  // line-final `:` on an earlier line. A chain opener line (`. within:`)
  // yields a `ChainCall` frame instead of `ColonBlock`.
  private def colonFrame
    ( bodyStart: Int,
      endLine:   Int,
      content:   String,
      lineMap:   LineMap )
  :   Option[Frame] =

    val colon = prevNonWs(content, bodyStart)

    if colon < 0 || content.charAt(colon) != ':' || !lineFinal(content, colon) then None
    else
      val openLine = lineMap.lineOf(colon)

      if lineMap.lineOf(bodyStart) <= openLine then None
      else
        val kind = chainAware(FrameKind.ColonBlock, content, lineMap, openLine)
        Some(mkFrame(kind, openLine, openLine, endLine, content, lineMap))

  // A quote or splice spanning multiple lines. The body-column rules for
  // quote/splice interiors differ from ordinary scopes (473.2–473.6); the
  // frame records the region and the opener line's leftward-extended anchor.
  private def spanFrame(span: Span, content: String, lineMap: LineMap): Option[Frame] =
    if !span.exists || span.isZeroExtent then None
    else
      val openLine = lineMap.lineOf(span.start)
      val endLine  = lineMap.endLineOf(span)

      if endLine <= openLine then None
      else Some(mkFrame(FrameKind.QuoteSplice, openLine, openLine, endLine, content, lineMap))

  private def mkFrame
    ( kind:       FrameKind,
      anchorLine: Int,
      openLine:   Int,
      endLine:    Int,
      content:    String,
      lineMap:    LineMap )
  :   Frame =

    val anchorCol = leadingColumn(content, lineMap, anchorLine)
    Frame(kind, anchorLine, anchorCol, anchorCol + 2, openLine, endLine)

  private def chainAware
    ( default: FrameKind,
      content: String,
      lineMap: LineMap,
      line:    Int )
  :   FrameKind =

    if lineStartsWithDot(content, lineMap, line) then FrameKind.ChainCall else default

  // Line bookkeeping computed from the raw text, counting only `\n` as a
  // line terminator. The compiler's `SourceFile` additionally treats form
  // feeds and carriage returns as line breaks, which would desynchronise
  // frame line numbers from the tokenizer's (`Context.lines`) whenever such
  // a character appears in a source file.
  private final class LineMap(content: String):
    private val starts: Array[Int] =
      val buffer = mutable.ArrayBuffer[Int](0)
      var i = 0

      while i < content.length do
        if content.charAt(i) == '\n' then buffer += i + 1
        i += 1

      buffer.toArray

    // The 1-based line containing `offset` (clamped to the text).
    def lineOf(offset: Int): Int =
      val target = offset.max(0).min(content.length)
      var low  = 0
      var high = starts.length - 1

      while low < high do
        val mid = (low + high + 1)/2
        if starts(mid) <= target then low = mid else high = mid - 1

      low + 1

    def endLineOf(span: Span): Int = lineOf((span.end - 1).max(span.start))

    // The offset of the first character of the given 1-based line.
    def startOf(line: Int): Int = starts((line - 1).max(0).min(starts.length - 1))

  // The 1-based column of the leftmost non-whitespace character on a line
  // (mirrors `Definitions.leadingColumn`).
  private def leadingColumn(content: String, lineMap: LineMap, line: Int): Int =
    val start = lineMap.startOf(line)
    var i     = start
    while i < content.length && (content.charAt(i) == ' ' || content.charAt(i) == '\t') do i += 1
    i - start + 1

  // True if the first non-whitespace character of the line is `.` — the
  // line is a chain method-application continuation.
  private def lineStartsWithDot(content: String, lineMap: LineMap, line: Int): Boolean =
    var i = lineMap.startOf(line)
    while i < content.length && (content.charAt(i) == ' ' || content.charAt(i) == '\t') do i += 1
    i < content.length && content.charAt(i) == '.'

  // True if only whitespace follows `offset` on its line.
  private def lineFinal(content: String, offset: Int): Boolean =
    var i    = offset + 1
    var only = true

    while only && i < content.length && content.charAt(i) != '\n' do
      if content.charAt(i) != ' ' && content.charAt(i) != '\t' then only = false
      i += 1

    only

  // The offset of the first non-whitespace character in [from, until), or -1.
  private def nextNonWs(content: String, from: Int, until: Int): Int =
    val limit = until.min(content.length)
    var i     = from.max(0)
    while i < limit && content.charAt(i).isWhitespace do i += 1
    if i < limit then i else -1

  // The offset of the last non-whitespace character before `from`, or -1.
  private def prevNonWs(content: String, from: Int): Int =
    var i = (from - 1).min(content.length - 1)
    while i >= 0 && content.charAt(i).isWhitespace do i -= 1
    i

  // Bounded keyword search between two offsets, skipping comments (mirrors
  // `Sequences.findKeyword`). Returns the byte offset, or -1 if not found.
  private def findKeyword(content: String, from: Int, until: Int, keyword: String): Int =
    val limit = until.min(content.length)
    var i     = from.max(0)
    var found = -1

    while found < 0 && i + keyword.length <= limit do
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
        if isWordBoundary(content, i, keyword.length) then found = i else i += 1
      else
        i += 1

    found

  private def isWordBoundary(content: String, i: Int, len: Int): Boolean =
    val before = if i > 0 then content.charAt(i - 1) else ' '
    val after  = if i + len < content.length then content.charAt(i + len) else ' '
    !isWordChar(before) && !isWordChar(after)

  private def isWordChar(c: Char): Boolean =
    c.isLetterOrDigit || c == '_' || c == '$'

  // Generic untyped-tree pre-order traversal driven off `productIterator`,
  // mirroring the helper in `Definitions` and `Annotations`.
  private def walk(t: untpd.Tree)(visit: untpd.Tree => Unit): Unit =
    visit(t)
    t.productIterator.foreach(descend(_, visit))

  private def descend(x: Any, visit: untpd.Tree => Unit): Unit = x match
    case sub: untpd.Tree  => walk(sub)(visit)
    case it:  Iterable[?] => it.foreach(descend(_, visit))
    case _                => ()
