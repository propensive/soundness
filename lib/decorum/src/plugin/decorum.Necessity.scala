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

// SN-247 — the "necessity" extractor (Principle P3, Density): the most
// compact compliant layout that fits is the only correct one, so a construct
// spread over several source lines whose one-line rendering would fit within
// 100 columns is a violation — a break must always mean "didn't fit".
//
// The extractor is deliberately conservative. Only three high-confidence
// families of construct are considered: keyword sequences (`if`/`then`/
// `else`, `while`/`do`, `for`/`yield`, `for`/`do`, `try`/`finally`),
// top-of-chain symbolic infix chains (mirroring the chain walk in
// `Operators`), and lambdas whose body starts on a later line. And a site is
// *bailed* — never flagged — whenever its one-line rendering cannot be
// proved to be a simple whitespace-collapsing join: comments, multi-line
// strings, colon-block lines, `case` keywords, `end`-led lines, blank
// interior lines, multi-line multi-statement blocks, form feeds and any
// scanner ambiguity all disqualify the site. The collapsed width joins every
// break with exactly one space, even where the author might legally write a
// tighter form — overestimating the width is the safe direction, since it
// can only suppress a finding, never invent one.
object Necessity:
  // One flaggable construct: `line`/`col` locate its first character (the
  // emission point for SN-247); `width` is the column count of the one-line
  // rendering, including the code already to the left and right of the
  // construct on its first and last lines; `start`/`end` delimit the
  // measured region (used for nested-site deduplication); `rendering` is the
  // one-line text that replaces `[start, end)` — exactly the string the
  // width was measured from, so a mechanical fix writes what was checked.
  case class Site
    ( kind: String, line: Int, col: Int, width: Int, start: Int, end: Int, rendering: String )

  def extract(tree: untpd.Tree, source: SourceFile, text: String): List[Site] =
    val out = mutable.ListBuffer[Site]()

    def offer(node: untpd.Tree, kind: String): Unit =
      siteFor(node, kind, source, text).foreach(out += _)

    // `continued` is true when `t` was reached as the continuation of a
    // same-kind chain — the direct operand of a reportable `InfixOp`
    // (mirroring the chain walk in `Operators`), or the `else` branch of an
    // enclosing `If` (an `else if` link). Such a node's layout is coupled
    // to its whole chain — an `else if` cannot be one-lined on its own
    // without breaking the body-cascade rule (833.2) — so only the top of
    // each chain is offered as a site.
    def visit(t: untpd.Tree, continued: Boolean): Unit = t match
      case op: untpd.InfixOp if reportableOp(op) =>
        if !continued then offer(op, "operator chain")
        chainOperand(op.left)
        chainOperand(op.right)

      case node: untpd.If =>
        if !continued then offer(node, "if-expression")
        descendFresh(node.cond)
        descendFresh(node.thenp)

        node.elsep match
          case sub: untpd.If => visit(sub, continued = true)
          case other         => descendFresh(other)

      case _ =>
        t match
          case node: untpd.WhileDo   => offer(node, "while-loop")
          case node: untpd.ForYield  => offer(node, "for-comprehension")
          case node: untpd.ForDo     => offer(node, "for-comprehension")
          case node: untpd.ParsedTry => offer(node, "try-expression")
          case node: untpd.Try       => offer(node, "try-expression")
          case node: untpd.Function  => offer(node, "lambda")
          case _                     => ()

        t.productIterator.foreach(descendFresh)

    def chainOperand(t: untpd.Tree): Unit = t match
      case op: untpd.InfixOp if reportableOp(op) => visit(op, continued = true)
      case _                                     => visit(t, continued = false)

    def descendFresh(x: Any): Unit = x match
      case sub: untpd.Tree  => visit(sub, continued = false)
      case it:  Iterable[?] => it.foreach(descendFresh)
      case _                => ()

    visit(tree, continued = false)

    // Nested-site deduplication: when an outer flagged construct strictly
    // contains an inner one, one-lining the outer subsumes the inner — keep
    // only the outermost report.
    val sites = out.toList

    sites.filter: site =>
      !sites.exists: outer =>
        (outer ne site) && outer.start <= site.start && site.end <= outer.end &&
          (outer.start < site.start || site.end < outer.end)

  // A chain node is reportable when it has full position information and its
  // operator name is wholly symbolic — the same notion `Operators` uses.
  private def reportableOp(node: untpd.InfixOp): Boolean =
    node.span.exists && node.op.span.exists && node.left.span.exists &&
      node.right.span.exists && isSymbolicOperator(node.op.name.toString)

  private def isSymbolicOperator(name: String): Boolean =
    name.nonEmpty && name.forall: c =>
      c match
        case '+' | '-' | '*' | '/' | '%' | '&' | '|' | '^' | '~' => true
        case '<' | '>' | '=' | '!' | '?' | ':' | '@' | '#'       => true
        case _                                                   => false

  // Measure one candidate construct, applying every bail-out. Returns a
  // `Site` only when the construct spans multiple lines, is structurally
  // one-lineable, and its collapsed rendering fits within 100 columns.
  private def siteFor
    ( node: untpd.Tree, kind: String, source: SourceFile, text: String )
  :   Option[Site] =

    val sp = node.span

    if !sp.exists || sp.start < 0 || sp.end > text.length || sp.end <= sp.start then None
    else if !text.substring(sp.start, sp.end).nn.contains('\n') then None
    else if structuralBail(node, text) then None
    else
      // For a lambda, the measured region widens to its immediate wrapper:
      // the opener (`{`, `(` or `:`) that may sit on the line above, and —
      // for brace/paren wrappers — the matching closer that may sit on the
      // line below. Both would have to join the one-line rendering, so
      // excluding them would understate the width.
      val region = node match
        case f: untpd.Function => lambdaRegion(f, text)
        case _                 => Some((sp.start, sp.end, false, 0))

      region.flatMap: (start, end, colonException, pad) =>
        val lineStart = startOfLine(text, start)
        var suffixEnd = end

        while suffixEnd < text.length && text.charAt(suffixEnd) != '\n' do suffixEnd += 1

        var suffixTrim = suffixEnd

        while suffixTrim > end && isBlankChar(text.charAt(suffixTrim - 1)) do suffixTrim -= 1

        // A form feed is a line break to dotty's SourceFile but not to this
        // extractor's `\n`-only line arithmetic — bail rather than disagree
        // (the same precedent as Anchors' `\n`-only line map).
        if text.substring(lineStart, suffixEnd).nn.contains('\f') then None
        else
          collapsedText(text, start, end, colonException).flatMap: collapsed =>
            val width = (start - lineStart) + collapsed.length + (suffixTrim - end) + pad

            if width <= 100 then
              val line = source.offsetToLine(sp.start) + 1
              val col  = source.column(sp.start) + 1

              // A paren-wrapped lambda whose closer joined the region must
              // be *rewritten* brace-wrapped on one line (312.1) — the same
              // two-column allowance `pad` charged to the width.
              val rendering =
                if pad == 2 && end > sp.end
                then "{ "+collapsed.substring(1, collapsed.length - 1).nn.trim+" }"
                else collapsed

              Some(Site(kind, line, col, width, start, end, rendering))
            else
              None

  private def startOfLine(text: String, offset: Int): Int =
    var i = offset
    while i > 0 && text.charAt(i - 1) != '\n' do i -= 1
    i

  private def isBlankChar(c: Char): Boolean = c == ' ' || c == '\t' || c == '\r'

  // Per-kind structural bail-outs, plus the blocking-subtree scan shared by
  // every kind.
  private def structuralBail(node: untpd.Tree, text: String): Boolean =
    val kindBail = node match
      case f: untpd.Function =>
        // Only term lambdas with explicit named parameters qualify: an
        // empty or non-`ValDef` parameter list cannot be told apart from a
        // function *type*, and the flag condition is specifically "the body
        // starts on a later line".
        val bodySp = f.body.span

        f.args.isEmpty || !f.args.forall(_.isInstanceOf[untpd.ValDef]) || !bodySp.exists ||
          bodySp.start < f.span.start || bodySp.start > text.length ||
          !text.substring(f.span.start, bodySp.start).nn.contains('\n')

      case node: untpd.ForYield => forEnumsBail(node.enums, text)
      case node: untpd.ForDo    => forEnumsBail(node.enums, text)
      case _                    => false

    kindBail || containsBlockingSubtree(node, text)

  // A `for` whose generators themselves span several lines cannot be joined
  // by whitespace collapsing alone — adjacent generators would need `;`
  // separators. Only fors whose whole enumerator section sits on one line
  // (with the break before `yield`/`do` or inside the body) qualify.
  private def forEnumsBail(enums: List[untpd.Tree], text: String): Boolean =
    enums.isEmpty || !enums.forall(_.span.exists) ||
      enums.head.span.start > enums.last.span.end || enums.last.span.end > text.length ||
      text.substring(enums.head.span.start, enums.last.span.end).nn.contains('\n')

  // Any multi-line block with more than a bare expression would need `;`
  // separators after the join (with or without braces), and any `match`
  // brings a vertical case run — both disqualify the whole construct. A
  // block without a span cannot be proved harmless, so it also bails.
  private def containsBlockingSubtree(node: untpd.Tree, text: String): Boolean =
    var blocked = false

    def walk(x: Any): Unit =
      if !blocked then x match
        case b: untpd.Block =>
          if b.stats.nonEmpty then
            val sp = b.span

            if !sp.exists || sp.start < 0 || sp.end > text.length || sp.end < sp.start then
              blocked = true
            else if text.substring(sp.start, sp.end).nn.contains('\n') then
              blocked = true

          if !blocked then b.productIterator.foreach(walk)

        case _: untpd.Match => blocked = true

        case t: untpd.Tree => t.productIterator.foreach(walk)

        case it: Iterable[?] => it.foreach(walk)

        case _ => ()

    walk(node)
    blocked

  // The measured region for a lambda: widened backward to its immediate
  // wrapper character and — for `{`/`(` — forward to the matching closer
  // when that closer is the next semantic character after the body. The
  // returned boolean is true when the wrapper is a colon opening the
  // region: the scanner's line-ends-with-`:` bail is then waived for the
  // first break, since `f: x => body` is exactly the sanctioned one-line
  // colon-arg form. A colon-arg must also be the last thing on its joined
  // line, so a colon-wrapped lambda with trailing code after its body
  // cannot be one-lined and yields `None`. The final component is a width
  // pad: a paren-wrapped named lambda would have to become brace-wrapped
  // (`{ λ }`) on one line per 312.1, which is two columns wider than the
  // collapsed paren form.
  private def lambdaRegion(f: untpd.Function, text: String): Option[(Int, Int, Boolean, Int)] =
    val sp = f.span
    var i  = sp.start - 1

    while i >= 0 && isWhitespace(text.charAt(i)) do i -= 1

    val opener = if i >= 0 then text.charAt(i) else ' '

    opener match
      case ':' =>
        if trailingCode(text, sp.end) then None else Some((i, sp.end, true, 0))

      case '{' | '(' =>
        val closer = if opener == '{' then '}' else ')'
        val pad    = if opener == '(' then 2 else 0
        var j      = sp.end

        while j < text.length && isWhitespace(text.charAt(j)) do j += 1

        if j < text.length && text.charAt(j) == closer then Some((i, j + 1, false, pad))
        else Some((i, sp.end, false, pad))

      case _ =>
        Some((sp.start, sp.end, false, 0))

  private def isWhitespace(c: Char): Boolean =
    c == ' ' || c == '\t' || c == '\n' || c == '\r'

  // True when a non-whitespace character follows `offset` on its line.
  private def trailingCode(text: String, offset: Int): Boolean =
    var i = offset

    while i < text.length && text.charAt(i) != '\n' do
      if !isBlankChar(text.charAt(i)) then return true
      i += 1

    false

  // The heart of the rule: the region's one-line rendering, where every
  // whitespace run outside string literals collapses to a single space.
  // Returns `None` — bail — on comments, multi-line strings, a line whose
  // last semantic character is `:` (colon-block syntax), the `case`
  // keyword, a line led by `end` (an end marker), a blank interior line, a
  // form feed, an unterminated literal, or any interpolation the scanner
  // cannot prove flat.
  private def collapsedText
    ( text: String, start: Int, end: Int, colonException: Boolean )
  :   Option[String] =

    var i           = start
    val buf         = new StringBuilder
    var pendingWs   = false
    var lastCh      = ' '
    var atLineStart = false
    var colonWaiver = colonException

    // Scan a string literal beginning at `from` (the opening quote; any
    // interpolator prefix has already been consumed). Returns the offset
    // just past the closing quote, or -1 to bail, having appended every
    // consumed character to `buf`. Interpolation blocks `${…}` are
    // traversed only when provably flat: any nested quote, comment
    // character, or line break inside one bails.
    def scanString(from: Int, interpolated: Boolean): Int =
      val triple =
        from + 2 < end && text.charAt(from) == '"' && text.charAt(from + 1) == '"' &&
          text.charAt(from + 2) == '"'

      var j = from + (if triple then 3 else 1)

      while j < end do
        val c = text.charAt(j)

        if c == '\n' || c == '\f' then return -1
        else if triple && c == '"' then
          var run = 0
          while j + run < end && text.charAt(j + run) == '"' do run += 1
          j += run

          if run >= 3 then
            buf.append(text.substring(from, j).nn)
            return j
        else if !triple && c == '"' then
          j += 1
          buf.append(text.substring(from, j).nn)
          return j
        else if !triple && c == '\\' && j + 1 < end then
          j += 2
        else if interpolated && c == '$' && j + 1 < end && text.charAt(j + 1) == '$' then
          j += 2
        else if interpolated && c == '$' && j + 1 < end && text.charAt(j + 1) == '{' then
          j += 2
          var depth = 1

          while j < end && depth > 0 do
            text.charAt(j) match
              case '\n' | '\f' | '"' | '\'' | '/' => return -1
              case '{'                            => depth += 1
              case '}'                            => depth -= 1
              case _                              => ()

            j += 1

          if depth > 0 then return -1
        else
          j += 1

      -1

    while i < end do
      val c = text.charAt(i)

      if c == '\f' then return None
      else if isWhitespace(c) then
        var newlines = 0

        while i < end && isWhitespace(text.charAt(i)) do
          if text.charAt(i) == '\n' then newlines += 1
          i += 1

        if newlines >= 2 then return None
        if newlines >= 1 && lastCh == ':' && !colonWaiver then return None
        if newlines >= 1 then atLineStart = true

        pendingWs = true
        colonWaiver = false
      else
        if pendingWs && buf.nonEmpty then buf.append(' ')
        pendingWs = false

        if c == '/' && i + 1 < end && (text.charAt(i + 1) == '/' || text.charAt(i + 1) == '*')
        then return None
        else if c == '"' then
          val next = scanString(i, interpolated = false)
          if next < 0 then return None
          i = next
          lastCh = '"'
        else if isIdentStart(c) then
          val identStart = i
          while i < end && isIdentPart(text.charAt(i)) do i += 1
          val ident = text.substring(identStart, i).nn

          if ident == "case" then return None
          if atLineStart && ident == "end" then return None

          buf.append(ident)
          lastCh = ident.charAt(ident.length - 1)

          if i < end && text.charAt(i) == '"' then
            val next = scanString(i, interpolated = true)
            if next < 0 then return None
            i = next
            lastCh = '"'
        else if c == '\'' then
          val litLen = charLiteralLength(text, i, end)

          if litLen > 0 then
            buf.append(text.substring(i, i + litLen).nn)
            i += litLen
          else
            buf.append(c)
            i += 1

          lastCh = '\''
        else if c == '`' then
          val idStart = i
          i += 1

          while i < end && text.charAt(i) != '`' && text.charAt(i) != '\n' do i += 1

          if i >= end || text.charAt(i) != '`' then return None
          i += 1
          buf.append(text.substring(idStart, i).nn)
          lastCh = '`'
        else
          buf.append(c)
          lastCh = c
          i += 1

        atLineStart = false

    Some(buf.toString)

  private def isIdentStart(c: Char): Boolean = c.isLetter || c == '_'
  private def isIdentPart(c: Char): Boolean  = c.isLetterOrDigit || c == '_'

  // The length of a character literal at `from`, or 0 when the `'`
  // introduces quote syntax instead. The recognized forms mirror the
  // Tokenizer's: `'X'`, `'\X'` and `'\uXXXX'`.
  private def charLiteralLength(text: String, from: Int, end: Int): Int =
    val simple = from + 2 < end && text.charAt(from + 1) != '\\' && text.charAt(from + 2) == '\''

    val escaped =
      from + 3 < end && text.charAt(from + 1) == '\\' && text.charAt(from + 3) == '\''

    val unicode =
      from + 7 < end && text.charAt(from + 1) == '\\' && text.charAt(from + 2) == 'u' &&
        text.charAt(from + 7) == '\''

    if simple then 3 else if escaped then 4 else if unicode then 8 else 0
