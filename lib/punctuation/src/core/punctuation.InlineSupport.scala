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

import java.util.regex as jur

import anticipation.*
import gossamer.*
import vacuous.*

case class EntityMatch(decoded: String, end: Int)
case class CodeSpanMatch(content: Text, end: Int)
case class AutolinkMatch(link: Prose, end: Int)

object InlineSupport:

  // CommonMark "ASCII punctuation": !"#$%&'()*+,-./:;<=>?@[\]^_`{|}~
  def isAsciiPunctuation(c: Char): Boolean = c match
    case '!' | '"' | '#' | '$' | '%' | '&' | '\'' | '(' | ')' | '*' | '+' | ',' => true
    case '-' | '.' | '/' | ':' | ';' | '<' | '=' | '>' | '?' | '@' | '[' | '\\' => true
    case ']' | '^' | '_' | '`' | '{' | '|' | '}' | '~'                          => true
    case _                                                                      => false

  inline def isDecDigit(c: Char): Boolean = c >= '0' && c <= '9'

  inline def isHexDigit(c: Char): Boolean =
    (c >= '0' && c <= '9') || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f')

  inline def isAsciiAlpha(c: Char): Boolean =
    (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')

  inline def isAsciiAlnum(c: Char): Boolean = isAsciiAlpha(c) || isDecDigit(c)

  // Try to parse an HTML entity at position `start` (which points at `&`).
  // Returns the decoded string and index past the closing `;`, or Unset.
  def parseEntity(s: String, start: Int, end: Int): Optional[EntityMatch] =
    var i = start + 1
    if i >= end then return Unset

    if s.charAt(i) == '#' then
      i += 1
      if i >= end then return Unset
      val isHex = s.charAt(i) == 'x' || s.charAt(i) == 'X'
      if isHex then i += 1
      val digitStart = i
      val maxDigits = if isHex then 6 else 7

      def hasDigit: Boolean =
        if isHex then isHexDigit(s.charAt(i)) else isDecDigit(s.charAt(i))

      while i < end && (i - digitStart) < maxDigits && hasDigit do i += 1

      if i == digitStart then return Unset
      if i >= end || s.charAt(i) != ';' then return Unset
      val numStr = s.substring(digitStart, i).nn

      val codePoint =
        try Integer.parseInt(numStr, if isHex then 16 else 10)
        catch case _: NumberFormatException => return Unset

      val ch =
        if codePoint == 0 || codePoint > 0x10FFFF then "�"
        else String.valueOf(Character.toChars(codePoint).nn).nn

      EntityMatch(ch, i + 1)
    else
      val nameStart = i
      while i < end && isAsciiAlnum(s.charAt(i)) do i += 1
      if i == nameStart then return Unset
      if i >= end || s.charAt(i) != ';' then return Unset
      val name = s.substring(nameStart, i).nn
      val decoded = HtmlEntities.lookup(name)
      if decoded == null then Unset else EntityMatch(decoded, i + 1)

  // Try to parse a code span starting at the run of backticks at position
  // `start`. Returns the content text and index past the closing run.
  def parseCodeSpan(s: String, start: Int, end: Int): Optional[CodeSpanMatch] =
    var i = start
    while i < end && s.charAt(i) == '`' do i += 1
    val openerLen = i - start
    val contentStart = i

    while i < end do
      while i < end && s.charAt(i) != '`' do i += 1
      if i >= end then return Unset
      val closerStart = i
      while i < end && s.charAt(i) == '`' do i += 1
      val closerLen = i - closerStart
      if closerLen == openerLen then
        val raw = s.substring(contentStart, closerStart).nn
        // Replace newlines with spaces (CommonMark §6.1)
        val noNewlines = raw.replace('\n', ' ').nn

        // Strip one leading + one trailing space if both ends have space and
        // content is not all spaces
        val needsTrim =
          noNewlines.length >= 2
          && noNewlines.charAt(0) == ' '
          && noNewlines.charAt(noNewlines.length - 1) == ' '
          && existsNonSpace(noNewlines)

        val stripped =
          if needsTrim then noNewlines.substring(1, noNewlines.length - 1).nn
          else noNewlines

        return CodeSpanMatch(Text(stripped), i)
    Unset

  private def existsNonSpace(s: String): Boolean =
    val n = s.length
    var i = 0
    while i < n do
      if s.charAt(i) != ' ' then return true
      i += 1
    false

  // Email autolink pattern from the CommonMark spec (§6.4)
  private val EmailRegex: jur.Pattern =
    val local = "[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+"
    val labelChars = "[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?"
    jur.Pattern.compile(s"^${local}@${labelChars}(?:\\.${labelChars})*$$").nn

  // Try to parse an autolink starting at `<` at position `start`.
  // Returns a `Prose.Link` and index past `>`, or Unset.
  def parseAutolink(s: String, start: Int, end: Int): Optional[AutolinkMatch] =
    var i = start + 1
    var keep = true
    while keep && i < end do
      val c = s.charAt(i)
      if c == '>' || c <= 0x20 || c == '<' then keep = false
      else i += 1

    if i >= end || s.charAt(i) != '>' then return Unset
    val content = s.substring(start + 1, i).nn

    if isUriAutolink(content) then
      val text = Text(content)
      val link = Prose.Link(text, Unset, Prose.Textual(text))
      AutolinkMatch(link, i + 1)
    else if EmailRegex.matcher(content).nn.matches then
      val text = Text(content)
      val mailto = Text("mailto:" + content)
      val link = Prose.Link(mailto, Unset, Prose.Textual(text))
      AutolinkMatch(link, i + 1)
    else
      Unset

  // Scheme: [A-Za-z][A-Za-z0-9+.-]{1,31} followed by `:` then a body of
  // characters that are neither whitespace, controls, `<`, nor `>` (already
  // ensured by the caller).
  private def isUriAutolink(content: String): Boolean =
    val n = content.length
    if n < 3 then return false
    if !isAsciiAlpha(content.charAt(0)) then return false
    var i = 1
    while i < n && i <= 32 do
      val c = content.charAt(i)
      val isSchemeChar = isAsciiAlnum(c) || c == '+' || c == '.' || c == '-'
      if c == ':' then
        if i < 2 then return false
        return validateUriBody(content, i + 1, n)
      if !isSchemeChar then return false
      i += 1
    false

  private def validateUriBody(content: String, start: Int, end: Int): Boolean =
    var i = start
    while i < end do
      val c = content.charAt(i)
      if c <= 0x20 || c == '<' || c == '>' then return false
      i += 1
    true

  // Parse a single-line link reference definition: `[label]: dest "title"`.
  // Multi-line link refs are not yet supported. Returns the definition and
  // the consumed-character count, or Unset.
  def parseLinkRefDef(line: Text): Optional[Markdown.LinkRef] =
    val s = line.s
    val n = s.length
    var i = 0
    var indent = 0
    while i < n && s.charAt(i) == ' ' && indent < 4 do { i += 1; indent += 1 }
    if indent >= 4 then return Unset
    if i >= n || s.charAt(i) != '[' then return Unset
    i += 1

    val labelStart = i
    var labelDone = false
    while i < n && !labelDone do
      val c = s.charAt(i)
      if c == ']' then labelDone = true
      else if c == '[' then return Unset
      else if c == '\\' && i + 1 < n then i += 2
      else i += 1

    if !labelDone then return Unset
    if i == labelStart then return Unset
    val label = Text(s.substring(labelStart, i).nn)
    i += 1

    if i >= n || s.charAt(i) != ':' then return Unset
    i += 1

    while i < n && (s.charAt(i) == ' ' || s.charAt(i) == '\t') do i += 1
    if i >= n then return Unset

    val destResult = parseLinkDestination(s, i, n)
    if destResult.absent then return Unset
    val (destination, afterDest) = (destResult.vouch.dest, destResult.vouch.end)
    i = afterDest

    val beforeWs = i
    while i < n && (s.charAt(i) == ' ' || s.charAt(i) == '\t') do i += 1

    var title: Optional[Text] = Unset
    if i > beforeWs && i < n then
      parseLinkTitle(s, i, n) match
        case Unset =>
          // No title: backtrack to before the whitespace to allow trailing-only
          i = beforeWs

        case t: TitleMatch =>
          title = Text(t.title)
          i = t.end

    while i < n && (s.charAt(i) == ' ' || s.charAt(i) == '\t') do i += 1
    if i < n then return Unset

    Markdown.LinkRef(label, title, destination)

  case class DestMatch(dest: Text, end: Int)
  case class TitleMatch(title: String, end: Int)

  // Link destination: either `<...>` (allows escapes; no unescaped < or >),
  // or a sequence of non-whitespace characters with balanced parentheses.
  def parseLinkDestination(s: String, start: Int, end: Int): Optional[DestMatch] =
    if start >= end then return Unset
    if s.charAt(start) == '<' then
      var i = start + 1
      val buf = new StringBuilder
      while i < end do
        val c = s.charAt(i)
        if c == '>' then return DestMatch(Text(buf.toString), i + 1)
        else if c == '<' || c == '\n' then return Unset
        else if c == '\\' && i + 1 < end && isAsciiPunctuation(s.charAt(i + 1)) then
          buf.append(s.charAt(i + 1))
          i += 2
        else
          buf.append(c)
          i += 1
      Unset
    else
      var i = start
      var depth = 0
      val buf = new StringBuilder
      while i < end do
        val c = s.charAt(i)
        if c <= 0x20 then
          if i == start then return Unset
          return DestMatch(Text(buf.toString), i)
        else if c == '\\' && i + 1 < end && isAsciiPunctuation(s.charAt(i + 1)) then
          buf.append(s.charAt(i + 1))
          i += 2
        else if c == '(' then
          depth += 1; buf.append(c); i += 1
        else if c == ')' then
          if depth == 0 then return DestMatch(Text(buf.toString), i)
          depth -= 1; buf.append(c); i += 1
        else
          buf.append(c)
          i += 1

      if depth != 0 then return Unset
      if i == start then Unset else DestMatch(Text(buf.toString), i)

  // Link title: `"..."`, `'...'`, or `(...)` with backslash-escapes.
  def parseLinkTitle(s: String, start: Int, end: Int): Optional[TitleMatch] =
    if start >= end then return Unset
    val opener = s.charAt(start)
    val closer = opener match
      case '"'  => '"'
      case '\'' => '\''
      case '('  => ')'
      case _    => return Unset

    var i = start + 1
    val buf = new StringBuilder
    while i < end do
      val c = s.charAt(i)
      if c == closer then return TitleMatch(buf.toString, i + 1)
      else if c == opener && opener == '(' then return Unset  // unbalanced inner (
      else if c == '\\' && i + 1 < end && isAsciiPunctuation(s.charAt(i + 1)) then
        buf.append(s.charAt(i + 1))
        i += 2
      else
        buf.append(c)
        i += 1
    Unset

  case class HtmlInlineMatch(html: Text, end: Int)

  // Try to parse a raw-HTML inline starting at `<` (position `start`).
  // Recognises open tags, close tags, HTML comments, processing
  // instructions, declarations, and CDATA sections, per CommonMark §6.5.
  def parseRawHtml(s: String, start: Int, end: Int): Optional[HtmlInlineMatch] =
    if start >= end || s.charAt(start) != '<' then return Unset
    if start + 1 >= end then return Unset

    val c = s.charAt(start + 1)

    val matchedEnd: Int =
      if c == '!' then parseHtmlBangForm(s, start, end)
      else if c == '?' then parseHtmlProcessingInstruction(s, start, end)
      else if c == '/' then parseHtmlClosingTag(s, start, end)
      else if isAsciiAlpha(c) then parseHtmlOpenTag(s, start, end)
      else -1

    if matchedEnd < 0 then Unset
    else HtmlInlineMatch(Text(s.substring(start, matchedEnd).nn), matchedEnd)

  private def parseHtmlBangForm(s: String, start: Int, end: Int): Int =
    // <!-- ... -->  /  <![CDATA[ ... ]]>  /  <! ... >  (declaration)
    if start + 4 < end && s.charAt(start + 2) == '-' && s.charAt(start + 3) == '-' then
      // HTML comment
      var i = start + 4
      while i + 2 < end do
        if s.charAt(i) == '-' && s.charAt(i + 1) == '-' && s.charAt(i + 2) == '>' then
          return i + 3
        i += 1
      -1
    else if start + 9 < end && s.regionMatches(start + 2, "[CDATA[", 0, 7) then
      var i = start + 9
      while i + 2 < end do
        if s.charAt(i) == ']' && s.charAt(i + 1) == ']' && s.charAt(i + 2) == '>' then
          return i + 3
        i += 1
      -1
    else if start + 2 < end && isAsciiUpper(s.charAt(start + 2)) then
      var i = start + 2
      while i < end do
        if s.charAt(i) == '>' then return i + 1
        i += 1
      -1
    else
      -1

  private def parseHtmlProcessingInstruction(s: String, start: Int, end: Int): Int =
    var i = start + 2
    while i + 1 < end do
      if s.charAt(i) == '?' && s.charAt(i + 1) == '>' then return i + 2
      i += 1
    -1

  private def parseHtmlClosingTag(s: String, start: Int, end: Int): Int =
    // </ tag-name space* >
    var i = start + 2
    if i >= end || !isAsciiAlpha(s.charAt(i)) then return -1
    i += 1
    while i < end && (isAsciiAlnum(s.charAt(i)) || s.charAt(i) == '-') do i += 1
    while i < end && (s.charAt(i) == ' ' || s.charAt(i) == '\t' || s.charAt(i) == '\n') do
      i += 1
    if i < end && s.charAt(i) == '>' then i + 1 else -1

  private def parseHtmlOpenTag(s: String, start: Int, end: Int): Int =
    // < tag-name (attr)* space* /? >
    var i = start + 1
    if i >= end || !isAsciiAlpha(s.charAt(i)) then return -1
    i += 1
    while i < end && (isAsciiAlnum(s.charAt(i)) || s.charAt(i) == '-') do i += 1

    var done = false
    var failed = false
    while !done && !failed do
      val before = i
      while i < end && (s.charAt(i) == ' ' || s.charAt(i) == '\t' || s.charAt(i) == '\n') do
        i += 1

      if i >= end then failed = true
      else if s.charAt(i) == '>' then done = true
      else if s.charAt(i) == '/' then
        if i + 1 < end && s.charAt(i + 1) == '>' then { i += 2; return i }
        else failed = true
      else
        // attribute requires preceding whitespace
        if i == before then failed = true
        else
          val attrEnd = parseHtmlAttribute(s, i, end)
          if attrEnd < 0 then failed = true
          else i = attrEnd

    if failed then -1
    else if done then i + 1
    else -1

  private inline def isAttrNameChar(c: Char): Boolean =
    isAsciiAlnum(c) || c == '_' || c == '.' || c == ':' || c == '-'

  private inline def isUnquotedValueChar(c: Char): Boolean =
    c != ' ' && c != '\t' && c != '\n'
    && c != '"' && c != '\''
    && c != '=' && c != '<' && c != '>' && c != '`'

  private def parseHtmlAttribute(s: String, start: Int, end: Int): Int =
    var i = start
    val c = s.charAt(i)
    if !(isAsciiAlpha(c) || c == '_' || c == ':') then return -1
    i += 1
    while i < end && isAttrNameChar(s.charAt(i)) do i += 1

    // Optional value
    val nameEnd = i
    while i < end && (s.charAt(i) == ' ' || s.charAt(i) == '\t' || s.charAt(i) == '\n') do
      i += 1

    if i < end && s.charAt(i) == '=' then
      i += 1
      while i < end && (s.charAt(i) == ' ' || s.charAt(i) == '\t' || s.charAt(i) == '\n') do
        i += 1
      if i >= end then return -1
      val q = s.charAt(i)
      if q == '"' || q == '\'' then
        i += 1
        while i < end && s.charAt(i) != q do i += 1
        if i >= end then return -1
        i + 1
      else
        // unquoted value: no whitespace, ", ', =, <, >, `
        val vstart = i
        while i < end && isUnquotedValueChar(s.charAt(i)) do i += 1
        if i == vstart then -1 else i
    else
      nameEnd

  private def isAsciiUpper(c: Char): Boolean = c >= 'A' && c <= 'Z'

  case class InlineLinkBody(dest: Text, title: Optional[Text], end: Int)

  // Parse the body of an inline link `(dest "title")` starting at the `(`.
  // Returns the destination, optional title, and the index past the closing
  // `)`, or Unset if the body doesn't parse.
  def parseInlineLinkBody(s: String, start: Int, end: Int): Optional[InlineLinkBody] =
    if start >= end || s.charAt(start) != '(' then return Unset
    var i = start + 1
    i = skipLinkWhitespace(s, i, end)

    val (dest, afterDest): (Text, Int) =
      if i < end && s.charAt(i) != ')' then
        parseLinkDestination(s, i, end) match
          case Unset        => return Unset
          case d: DestMatch => (d.dest, d.end)
      else
        (t"", i)

    i = afterDest
    val beforeWs = i
    i = skipLinkWhitespace(s, i, end)

    var title: Optional[Text] = Unset
    if i > beforeWs && i < end then
      parseLinkTitle(s, i, end) match
        case t: TitleMatch =>
          title = Text(t.title)
          i = t.end

        case Unset =>
          // No title; rewind to before the inter-token whitespace
          i = beforeWs

    i = skipLinkWhitespace(s, i, end)
    if i >= end || s.charAt(i) != ')' then return Unset
    InlineLinkBody(dest, title, i + 1)

  case class RefLabelMatch(label: Text, end: Int)

  // Parse a reference link label `[label]` starting at the `[`. Returns the
  // raw label text (or empty for `[]`) and the index past the closing `]`,
  // or Unset if no valid label parses.
  def parseRefLabel(s: String, start: Int, end: Int): Optional[RefLabelMatch] =
    if start >= end || s.charAt(start) != '[' then return Unset
    var i = start + 1
    val labelStart = i
    var done = false

    while i < end && !done do
      val c = s.charAt(i)
      if c == ']' then done = true
      else if c == '[' then return Unset
      else if c == '\\' && i + 1 < end then i += 2
      else i += 1

    if !done then return Unset
    val label = if i == labelStart then t"" else Text(s.substring(labelStart, i).nn)
    RefLabelMatch(label, i + 1)

  // Skip whitespace allowed in link bodies — spaces, tabs, and up to one
  // newline (per CommonMark §6.6).
  private def skipLinkWhitespace(s: String, start: Int, end: Int): Int =
    var i = start
    var newlines = 0
    var done = false
    while i < end && !done do
      val c = s.charAt(i)
      if c == ' ' || c == '\t' then i += 1
      else if c == '\n' && newlines < 1 then
        newlines += 1
        i += 1
      else
        done = true
    i
