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
import gossamer.*
import vacuous.*

case class BulletMarker(char: Char, markerIndent: Int, contentIndent: Int, rest: Text)

case class OrderedMarker
  ( start:         Int,
    delimiter:     '.' | ')',
    markerIndent:  Int,
    contentIndent: Int,
    rest:          Text )

object ParserSupport:

  // CommonMark treats every horizontal-tab as advancing to the next 4-column
  // boundary. For block-parser indent counting that's what `indentColumn`
  // returns; for content stripping after a known indent we use `stripIndent`,
  // which removes whole-tab-equivalent prefix correctly.

  def isBlank(line: Text): Boolean =
    val s = line.s
    var i = 0
    val n = s.length
    while i < n && (s.charAt(i) == ' ' || s.charAt(i) == '\t') do i += 1
    i == n

  // Number of leading-indent columns, expanding tabs to 4-column stops.
  // Stops at the first non-space, non-tab character.
  def indentColumn(line: Text): Int =
    val s = line.s
    var i = 0
    var col = 0
    val n = s.length
    while i < n do
      s.charAt(i) match
        case ' ' =>
          col += 1; i += 1

        case '\t' =>
          col += 4 - (col & 3); i += 1

        case _ => return col

    col

  // Index (in chars) of the first non-space, non-tab char, or s.length.
  def firstNonWhitespace(line: Text): Int =
    val s = line.s
    var i = 0
    val n = s.length
    while i < n && (s.charAt(i) == ' ' || s.charAt(i) == '\t') do i += 1
    i

  // Strip trailing whitespace (spaces and tabs only — newlines aren't expected).
  def stripTrailingSpaces(line: Text): Text =
    val s = line.s
    var i = s.length
    while i > 0 && (s.charAt(i - 1) == ' ' || s.charAt(i - 1) == '\t') do i -= 1
    if i == s.length then line else Text(s.substring(0, i).nn)

  // ATX heading: ^ {0,3}#{1,6}( |\t|$).*?(#+)? *$
  // Returns Some((level, content)) where content is the trimmed heading text.
  // Optional trailing #s are stripped per the spec when surrounded by space.
  def atxHeading(line: Text): Optional[(1 | 2 | 3 | 4 | 5 | 6, Text)] =
    val s = line.s
    val n = s.length
    var i = 0
    // up to 3 leading spaces
    var indent = 0
    while i < n && s.charAt(i) == ' ' && indent < 4 do { i += 1; indent += 1 }
    if indent >= 4 then return Unset
    // 1-6 hashes
    var hashes = 0
    while i < n && s.charAt(i) == '#' && hashes < 7 do { i += 1; hashes += 1 }
    if hashes < 1 || hashes > 6 then return Unset
    // must be followed by space, tab, or end-of-line
    if i < n && s.charAt(i) != ' ' && s.charAt(i) != '\t' then return Unset
    // skip spaces/tabs after hashes
    while i < n && (s.charAt(i) == ' ' || s.charAt(i) == '\t') do i += 1
    // strip trailing whitespace
    var end = n
    while end > i && (s.charAt(end - 1) == ' ' || s.charAt(end - 1) == '\t') do end -= 1
    // strip optional trailing #s if preceded by space (or whole line)
    val origEnd = end
    var hashEnd = end
    while hashEnd > i && s.charAt(hashEnd - 1) == '#' do hashEnd -= 1
    val hashesPrecededBySpace =
      hashEnd == i || s.charAt(hashEnd - 1) == ' ' || s.charAt(hashEnd - 1) == '\t'

    if hashEnd < origEnd && hashesPrecededBySpace then
      end = hashEnd
      while end > i && (s.charAt(end - 1) == ' ' || s.charAt(end - 1) == '\t') do end -= 1
    val content = if end > i then Text(s.substring(i, end).nn) else t""
    val level: 1 | 2 | 3 | 4 | 5 | 6 = (hashes: @unchecked) match
      case 1 => 1; case 2 => 2; case 3 => 3; case 4 => 4; case 5 => 5; case 6 => 6
    (level, content)

  // Setext underline: ^ {0,3}(=+|-+) *$
  // Returns Some(1) for `=`, Some(2) for `-`.
  def setextUnderline(line: Text): Optional[1 | 2] =
    val s = line.s
    val n = s.length
    var i = 0
    var indent = 0
    while i < n && s.charAt(i) == ' ' && indent < 4 do { i += 1; indent += 1 }
    if indent >= 4 || i >= n then return Unset
    val ch = s.charAt(i)
    if ch != '=' && ch != '-' then return Unset
    var count = 0
    while i < n && s.charAt(i) == ch do { i += 1; count += 1 }
    if count < 1 then return Unset
    while i < n && (s.charAt(i) == ' ' || s.charAt(i) == '\t') do i += 1
    if i < n then return Unset
    if ch == '=' then 1 else 2

  // Thematic break: ^ {0,3}([-*_])(\1| |\t)*\1\1.*$ where total non-space chars >= 3 of same
  def isThematicBreak(line: Text): Boolean =
    val s = line.s
    val n = s.length
    var i = 0
    var indent = 0
    while i < n && s.charAt(i) == ' ' && indent < 4 do { i += 1; indent += 1 }
    if indent >= 4 || i >= n then return false
    val ch = s.charAt(i)
    if ch != '-' && ch != '*' && ch != '_' then return false
    var count = 0
    while i < n do
      val c = s.charAt(i)
      if c == ch then count += 1
      else if c != ' ' && c != '\t' then return false
      i += 1
    count >= 3

  // Fence opener: ^ {0,3}(`{3,}|~{3,})(.*)$
  // Backtick fences cannot contain backticks in info string.
  // Returns Some((char, count, indent, info)).
  def fenceOpener(line: Text): Optional[(Char, Int, Int, Text)] =
    val s = line.s
    val n = s.length
    var i = 0
    var indent = 0
    while i < n && s.charAt(i) == ' ' && indent < 4 do { i += 1; indent += 1 }
    if indent >= 4 || i >= n then return Unset
    val ch = s.charAt(i)
    if ch != '`' && ch != '~' then return Unset
    val fenceStart = i
    while i < n && s.charAt(i) == ch do i += 1
    val count = i - fenceStart
    if count < 3 then return Unset
    val infoStart = i
    val info = Text(s.substring(infoStart, n).nn).trim
    // Backtick fences cannot contain backticks in info string
    if ch == '`' && info.s.indexOf('`') >= 0 then return Unset
    (ch, count, indent, info)

  // Fence closer: ^ {0,3}(`{minCount,}|~{minCount,}) *$
  def isFenceCloser(line: Text, fenceChar: Char, minCount: Int): Boolean =
    val s = line.s
    val n = s.length
    var i = 0
    var indent = 0
    while i < n && s.charAt(i) == ' ' && indent < 4 do { i += 1; indent += 1 }
    if indent >= 4 then return false
    val start = i
    while i < n && s.charAt(i) == fenceChar do i += 1
    val count = i - start
    if count < minCount then return false
    while i < n && s.charAt(i) == ' ' do i += 1
    i == n

  // Strip up to `n` columns of indent, expanding tabs as needed. Returns the
  // remainder of the line. If the line has fewer than n columns of leading
  // whitespace, the entire whitespace prefix is stripped and the rest returned.
  def stripIndent(line: Text, n: Int): Text =
    if n <= 0 then return line
    val s = line.s
    var i = 0
    var col = 0
    val len = s.length
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
            builder.append(s, i + 1, len)
            return Text(builder.toString)

        case _ =>
          return Text(s.substring(i, len).nn)
    if i == 0 then line else if i >= len then t"" else Text(s.substring(i, len).nn)

  // Cut info string into whitespace-separated tokens (CommonMark spec).
  def cutInfo(info: Text): List[Text] =
    val s = info.s
    val n = s.length
    if n == 0 then return Nil
    val result = scala.collection.mutable.ListBuffer[Text]()
    var i = 0
    while i < n do
      while i < n && (s.charAt(i) == ' ' || s.charAt(i) == '\t') do i += 1
      val start = i
      while i < n && s.charAt(i) != ' ' && s.charAt(i) != '\t' do i += 1
      if i > start then result += Text(s.substring(start, i).nn)
    result.toList

  // Bullet list marker: ^ {0,3}([-*+])( +|\t|$)(.*)$
  // Returns (marker char, marker indent, content indent, remainder).
  // `contentIndent` is the column at which the item's content begins (used
  // by `ListItemBuilder` to know how many columns to strip from continuation
  // lines).
  def bulletMarker(line: Text): Optional[BulletMarker] =
    val s = line.s
    val n = s.length
    var i = 0
    var indent = 0
    while i < n && s.charAt(i) == ' ' && indent < 4 do { i += 1; indent += 1 }
    if indent >= 4 || i >= n then return Unset
    val ch = s.charAt(i)
    if ch != '-' && ch != '+' && ch != '*' then return Unset
    val markerEnd = i + 1
    // Must be followed by space, tab, or end-of-line
    if markerEnd < n && s.charAt(markerEnd) != ' ' && s.charAt(markerEnd) != '\t' then return Unset
    // Count post-marker whitespace columns (1..4); if 5+, treat as 1 (content
    // becomes indented code) — caller handles that by not stripping past 1.
    var j = markerEnd
    var postCol = 0
    while j < n && (s.charAt(j) == ' ' || s.charAt(j) == '\t') && postCol < 5 do
      if s.charAt(j) == ' ' then postCol += 1
      else postCol += 4 - ((indent + 1 + postCol) & 3)
      j += 1

    val followSpace =
      if markerEnd >= n then 1
      else if isBlank(Text(s.substring(markerEnd, n).nn)) then 1
      else if postCol >= 5 then 1
      else postCol

    val contentStart =
      markerEnd + (if markerEnd >= n then 0 else followSpace.min(n - markerEnd))

    val contentIndent = indent + 1 + followSpace
    val rest = if contentStart >= n then t"" else Text(s.substring(contentStart, n).nn)
    BulletMarker(ch, indent, contentIndent, rest)

  // Ordered list marker: ^ {0,3}(\d{1,9})([.)])( +|\t|$)(.*)$
  def orderedMarker(line: Text): Optional[OrderedMarker] =
    val s = line.s
    val n = s.length
    var i = 0
    var indent = 0
    while i < n && s.charAt(i) == ' ' && indent < 4 do { i += 1; indent += 1 }
    if indent >= 4 || i >= n then return Unset
    val digitStart = i
    while i < n && s.charAt(i) >= '0' && s.charAt(i) <= '9' && (i - digitStart) < 9 do i += 1
    val digitCount = i - digitStart
    if digitCount < 1 then return Unset
    if i >= n then return Unset
    val delimChar = s.charAt(i)
    if delimChar != '.' && delimChar != ')' then return Unset
    val markerEnd = i + 1
    if markerEnd < n && s.charAt(markerEnd) != ' ' && s.charAt(markerEnd) != '\t' then
      return Unset
    val start =
      try Integer.parseInt(s.substring(digitStart, digitStart + digitCount))
      catch case _: NumberFormatException => return Unset

    var j = markerEnd
    var postCol = 0
    while j < n && (s.charAt(j) == ' ' || s.charAt(j) == '\t') && postCol < 5 do
      if s.charAt(j) == ' ' then postCol += 1
      else postCol += 4 - ((indent + digitCount + 1 + postCol) & 3)
      j += 1

    val followSpace =
      if markerEnd >= n then 1
      else if isBlank(Text(s.substring(markerEnd, n).nn)) then 1
      else if postCol >= 5 then 1
      else postCol

    val contentStart =
      markerEnd + (if markerEnd >= n then 0 else followSpace.min(n - markerEnd))

    val contentIndent = indent + digitCount + 1 + followSpace
    val rest = if contentStart >= n then t"" else Text(s.substring(contentStart, n).nn)
    val delim: '.' | ')' = if delimChar == '.' then '.' else ')'
    OrderedMarker(start, delim, indent, contentIndent, rest)

  // Whether the line, ignoring leading 0..3 spaces, starts a new block
  // (other than a paragraph). Used for lazy-paragraph-continuation: if a line
  // doesn't start a new block, it can continue an open paragraph regardless
  // of failed container continuation.
  def startsNonParagraphBlock(line: Text): Boolean =
    if isBlank(line) then return true
    if isThematicBreak(line) then return true
    if atxHeading(line).present then return true
    if fenceOpener(line).present then return true
    if bulletMarker(line).present then return true
    if orderedMarker(line).present then return true
    false
