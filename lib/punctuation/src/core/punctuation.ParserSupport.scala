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

import anticipation.*
import denominative.*
import gossamer.*
import vacuous.*

case class BulletMarker(char: Char, contentIndent: Ordinal, rest: Text)

case class OrderedMarker
  ( start:         Int,
    delimiter:     '.' | ')',
    contentIndent: Ordinal,
    rest:          Text )

object ParserSupport:

  // CommonMark treats a tab as advancing to the next 4-column boundary.
  // Block-parser indent counting (`indentColumn`) returns those expanded
  // columns; `stripIndent` removes a column-quantum prefix correctly.

  // ─── tiny scanning kit ────────────────────────────────────────────────────

  inline def isSpaceTab(inline c: Char): Boolean = c == ' ' || c == '\t'

  // Walk forward from `from` while `pred` holds, returning the new index
  // (first non-matching position, or end-of-string).
  private inline def walk(s: String, from: Int)(inline pred: Char => Boolean): Int =
    var j = from
    while j < s.length && pred(s.charAt(j)) do j += 1
    j

  // As `walk` but consume at most `limit` chars.
  private inline def walkUpTo
    ( s: String, from: Int, limit: Int )
    ( inline pred: Char => Boolean )
  :   Int =

    var j = from
    while j < s.length && (j - from) < limit && pred(s.charAt(j)) do j += 1
    j

  // Walk backward from `to` (exclusive) while `pred` holds for the previous
  // char, stopping at `floor`. Returns the new boundary.
  private inline def walkBack
    ( s: String, floor: Int, to: Int )
    ( inline pred: Char => Boolean )
  :   Int =

    var j = to
    while j > floor && pred(s.charAt(j - 1)) do j -= 1
    j

  // Skip up to 3 leading spaces (CommonMark indent gate). Returns the index
  // of the first non-space char, or -1 if 4+ spaces were found (which
  // disqualifies the line for most leaf/container openers).
  private inline def leadingIndent(s: String): Int =
    val i = walkUpTo(s, 0, 4)(_ == ' ')
    if i >= 4 then -1 else i

  // ─── line predicates and trims ────────────────────────────────────────────

  def isBlank(line: Text): Boolean =
    val s = line.s
    walk(s, 0)(isSpaceTab) == s.length

  // Number of leading-indent columns, expanding tabs to 4-column stops.
  def indentColumn(line: Text): Int =
    val s = line.s
    val n = s.length
    var i = 0
    var col = 0
    while i < n do
      s.charAt(i) match
        case ' '  => col += 1; i += 1
        case '\t' => col += 4 - (col & 3); i += 1
        case _    => return col
    col

  // Strip up to `n` columns of indent, expanding tabs as needed. Returns the
  // remainder of the line. If the line has fewer than n columns of leading
  // whitespace, the entire whitespace prefix is stripped and the rest returned.
  def stripIndent(line: Text, n: Int): Text =
    if n <= 0 then return line
    val s = line.s
    val len = s.length
    var i = 0
    var col = 0
    while i < len && col < n do
      s.charAt(i) match
        case ' ' =>
          col += 1; i += 1

        case '\t' =>
          val advance = 4 - (col & 3)
          if col + advance <= n then { col += advance; i += 1 }
          else
            // Partial tab: replace with leftover spaces
            val leftover = (col + advance) - n
            val builder = new StringBuilder
            var k = 0
            while k < leftover do { builder.append(' '); k += 1 }
            builder.append(s.substring(i + 1, len))
            return Text(builder.toString)

        case _ =>
          return Text(s.substring(i, len).nn)

    if i == 0 then line else if i >= len then t"" else Text(s.substring(i, len).nn)

  // After stripping a container's marker and follow-space (which may have
  // partially consumed a tab character), build a self-contained residual:
  // prepend `leftoverSpaces` then pre-expand any further leading tabs in
  // `rest` to spaces using absolute column positions starting at `startCol`.
  def buildResidual(rest: String, startCol: Int, leftoverSpaces: Int): String =
    val sb = new StringBuilder
    var k = 0
    while k < leftoverSpaces do { sb.append(' '); k += 1 }
    var col = startCol
    var i = 0
    val n = rest.length
    while i < n && isSpaceTab(rest.charAt(i)) do
      if rest.charAt(i) == ' ' then { sb.append(' '); col += 1 }
      else
        val adv = 4 - (col & 3)
        var k2 = 0
        while k2 < adv do { sb.append(' '); col += 1; k2 += 1 }
      i += 1
    if i < n then sb.append(rest.substring(i, n))
    sb.toString

  // Cut info string into whitespace-separated tokens (CommonMark spec).
  def cutInfo(info: Text): List[Text] =
    val s = info.s
    val n = s.length
    if n == 0 then return Nil
    val result = scala.collection.mutable.ListBuffer[Text]()
    var i = 0
    while i < n do
      i = walk(s, i)(isSpaceTab)
      val start = i
      i = walk(s, i)(c => !isSpaceTab(c))
      if i > start then result += Text(s.substring(start, i).nn)
    result.toList

  // ─── leaf classifiers ─────────────────────────────────────────────────────

  // ATX heading: ^ {0,3}#{1,6}( |\t|$).*?(#+)? *$
  def atxHeading(line: Text): Optional[(1 | 2 | 3 | 4 | 5 | 6, Text)] =
    val s = line.s
    val n = s.length
    val indent = leadingIndent(s)
    if indent < 0 then return Unset

    val hashEnd = walkUpTo(s, indent, 7)(_ == '#')
    val hashes = hashEnd - indent
    if hashes < 1 || hashes > 6 then return Unset
    if hashEnd < n && !isSpaceTab(s.charAt(hashEnd)) then return Unset

    val contentStart = walk(s, hashEnd)(isSpaceTab)
    var contentEnd = walkBack(s, contentStart, n)(isSpaceTab)

    // Strip optional trailing #s if preceded by space (or whole content)
    val hashesEnd = walkBack(s, contentStart, contentEnd)(_ == '#')

    val hashesPrecededBySpace =
      hashesEnd == contentStart || isSpaceTab(s.charAt(hashesEnd - 1))

    if hashesEnd < contentEnd && hashesPrecededBySpace then
      contentEnd = walkBack(s, contentStart, hashesEnd)(isSpaceTab)

    val content =
      if contentEnd > contentStart then Text(s.substring(contentStart, contentEnd).nn) else t""

    val level: 1 | 2 | 3 | 4 | 5 | 6 = (hashes: @unchecked) match
      case 1 => 1; case 2 => 2; case 3 => 3; case 4 => 4; case 5 => 5; case 6 => 6

    (level, content)

  // Setext underline: ^ {0,3}(=+|-+) *$
  def setextUnderline(line: Text): Optional[1 | 2] =
    val s = line.s
    val n = s.length
    val indent = leadingIndent(s)
    if indent < 0 || indent >= n then return Unset

    val ch = s.charAt(indent)
    if ch != '=' && ch != '-' then return Unset
    val end = walk(s, indent)(_ == ch)
    if end == indent then return Unset
    if walk(s, end)(isSpaceTab) < n then return Unset
    if ch == '=' then 1 else 2

  // Thematic break: ≥3 of `-`/`*`/`_` (all the same char), spaces/tabs
  // allowed in between, no other chars.
  def isThematicBreak(line: Text): Boolean =
    val s = line.s
    val n = s.length
    val indent = leadingIndent(s)
    if indent < 0 || indent >= n then return false

    val ch = s.charAt(indent)
    if ch != '-' && ch != '*' && ch != '_' then return false

    var count = 0
    var i = indent
    while i < n do
      val c = s.charAt(i)
      if c == ch then count += 1
      else if !isSpaceTab(c) then return false
      i += 1
    count >= 3

  // Fence opener: ^ {0,3}(`{3,}|~{3,})(.*)$. Backtick fences cannot contain
  // backticks in the info string.
  def fenceOpener(line: Text): Optional[(Char, Int, Ordinal, Text)] =
    val s = line.s
    val n = s.length
    val indent = leadingIndent(s)
    if indent < 0 || indent >= n then return Unset

    val ch = s.charAt(indent)
    if ch != '`' && ch != '~' then return Unset
    val end = walk(s, indent)(_ == ch)
    val count = end - indent
    if count < 3 then return Unset

    val info = Text(s.substring(end, n).nn).trim
    if ch == '`' && info.s.indexOf('`') >= 0 then return Unset
    (ch, count, indent.z, info)

  // Fence closer: ^ {0,3}(`{minCount,}|~{minCount,}) *$
  def isFenceCloser(line: Text, fenceChar: Char, minCount: Int): Boolean =
    val s = line.s
    val n = s.length
    val indent = leadingIndent(s)
    if indent < 0 then return false

    val end = walk(s, indent)(_ == fenceChar)
    if end - indent < minCount then return false
    walk(s, end)(_ == ' ') == n

  // ─── list-item markers ────────────────────────────────────────────────────

  // Walk post-marker whitespace tracking (j, postCol). Stops once visual
  // col extent reaches 5 (the 5+ rule threshold for the spec follow-space).
  private def postMarkerWhitespace(s: String, markerEnd: Int, markerColEnd: Int): (Int, Int) =
    var j = markerEnd
    var postCol = 0
    while j < s.length && isSpaceTab(s.charAt(j)) && postCol < 5 do
      if s.charAt(j) == ' ' then postCol += 1
      else postCol += 4 - ((markerColEnd + postCol) & 3)
      j += 1
    (j, postCol)

  // After parsing a marker, compute the (contentIndent, residual) pair from
  // the post-marker whitespace state. Encapsulates the "1–4 spaces follow"
  // vs "5+ spaces use 1-col follow" rule plus partial-tab handling.
  private def markerResidual
    ( s:            String,
      n:            Int,
      markerEnd:    Int,
      j:            Int,
      postCol:      Int,
      markerColEnd: Int )
  :   (Ordinal, Text) =

    if markerEnd >= n || isBlank(Text(s.substring(markerEnd, n).nn)) then
      ((markerColEnd + 1).z, t"")
    else if postCol >= 5 then
      // 5+ rule: consume only the first post-marker char as follow-space;
      // remaining post-marker whitespace becomes residual leading content.
      val firstCh = s.charAt(markerEnd)
      val firstAdvance = if firstCh == ' ' then 1 else 4 - (markerColEnd & 3)
      val nextCol = markerColEnd + firstAdvance
      val tail = if markerEnd + 1 >= n then "" else s.substring(markerEnd + 1, n).nn
      ((markerColEnd + 1).z, Text(buildResidual(tail, nextCol, firstAdvance - 1)))
    else
      // 1–4 cols of follow consume all post-marker whitespace.
      val tail = if j >= n then "" else s.substring(j, n).nn
      ((markerColEnd + postCol).z, Text(buildResidual(tail, markerColEnd + postCol, 0)))

  // Bullet list marker: ^ {0,3}([-*+])( +|\t|$)(.*)$. `contentIndent` is the
  // visual column at which the item's content begins (used by `ListItemBuilder`
  // to know how many columns to strip from continuation lines).
  def bulletMarker(line: Text): Optional[BulletMarker] =
    val s = line.s
    val n = s.length
    val indent = leadingIndent(s)
    if indent < 0 || indent >= n then return Unset

    val ch = s.charAt(indent)
    if ch != '-' && ch != '+' && ch != '*' then return Unset
    val markerEnd = indent + 1
    if markerEnd < n && !isSpaceTab(s.charAt(markerEnd)) then return Unset

    val markerColEnd = indent + 1
    val (j, postCol) = postMarkerWhitespace(s, markerEnd, markerColEnd)
    val (contentIndent, rest) = markerResidual(s, n, markerEnd, j, postCol, markerColEnd)
    BulletMarker(ch, contentIndent, rest)

  // Ordered list marker: ^ {0,3}(\d{1,9})([.)])( +|\t|$)(.*)$
  def orderedMarker(line: Text): Optional[OrderedMarker] =
    val s = line.s
    val n = s.length
    val indent = leadingIndent(s)
    if indent < 0 || indent >= n then return Unset

    val digitEnd = walkUpTo(s, indent, 9)(c => c >= '0' && c <= '9')
    val digitCount = digitEnd - indent
    if digitCount < 1 || digitEnd >= n then return Unset

    val delimChar = s.charAt(digitEnd)
    if delimChar != '.' && delimChar != ')' then return Unset
    val markerEnd = digitEnd + 1
    if markerEnd < n && !isSpaceTab(s.charAt(markerEnd)) then return Unset

    val start =
      try Integer.parseInt(s.substring(indent, digitEnd))
      catch case _: NumberFormatException => return Unset

    val markerColEnd = indent + digitCount + 1
    val (j, postCol) = postMarkerWhitespace(s, markerEnd, markerColEnd)
    val (contentIndent, rest) = markerResidual(s, n, markerEnd, j, postCol, markerColEnd)
    val delim: '.' | ')' = if delimChar == '.' then '.' else ')'
    OrderedMarker(start, delim, contentIndent, rest)

  // ─── lazy-continuation gate ───────────────────────────────────────────────

  // Whether the line, ignoring leading 0–3 spaces, starts a new block (other
  // than a paragraph). Used to decide if a line can lazy-continue an open
  // paragraph. If `paragraphOpen` is true, the CommonMark "cannot interrupt
  // a paragraph" restrictions apply (empty bullet list, ordered list not
  // starting at 1).
  def startsNonParagraphBlock(line: Text, paragraphOpen: Boolean): Boolean =
    if isBlank(line) then return true
    if isThematicBreak(line) then return true
    if atxHeading(line).present then return true
    if fenceOpener(line).present then return true
    if startsBlockQuote(line) then return true

    bulletMarker(line) match
      case bm: BulletMarker if !paragraphOpen || !isBlank(bm.rest) => return true
      case _                                                       => ()

    orderedMarker(line) match
      case om: OrderedMarker if !paragraphOpen || (om.start == 1 && !isBlank(om.rest)) =>
        return true

      case _ => ()

    false

  // True if the line starts a blockquote (`>` after 0–3 leading spaces).
  def startsBlockQuote(line: Text): Boolean =
    val s = line.s
    val indent = leadingIndent(s)
    indent >= 0 && indent < s.length && s.charAt(indent) == '>'
