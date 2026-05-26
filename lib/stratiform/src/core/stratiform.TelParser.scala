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
package stratiform

import scala.language.unsafeNulls

import anticipation.*
import contingency.*
import fulminate.*
import vacuous.*

import TelError.Reason

// Phase 1 presentation-only parser. Single-pass recursive descent over the
// byte input, producing the §17 presentation AST. Schema-driven type
// assignment and full error-recovery semantics arrive in phase 3.

object TelParser:

  def parse(input: Data): Tel.Document raises TelError = TelParser(input).parseDocument()

  private final class Line
     ( val content:       String,
       val leadingSpaces: Int,
       val blank:         Boolean,
       val start:         Int,
       val end:           Int )

private final class TelParser(input: Data):
  import TelParser.Line

  private val source: String =
    val arr = input.asInstanceOf[IArray[Byte]]
    val bytes = new Array[Byte](arr.length)
    var i = 0
    while i < arr.length do { bytes(i) = arr(i); i += 1 }
    String(bytes, "UTF-8")

  private val lineEndings: Tel.LineEndings =
    val firstLf = source.indexOf('\n')
    if firstLf > 0 && source.charAt(firstLf - 1) == '\r' then Tel.LineEndings.Crlf
    else Tel.LineEndings.Lf

  // Pre-split lines retaining leading-space count and blank flag. CR before
  // LF is stripped in CRLF mode. The final stretch of content after the
  // last LF is included only if it has content; an empty stretch after a
  // trailing LF still counts as a blank line for §17 trailing-blank-line
  // bookkeeping.
  private val lines: IArray[Line] =
    val builder = scala.collection.mutable.ArrayBuffer.empty[Line]
    var lineStart = 0
    var index = 0
    while index < source.length do
      if source.charAt(index) == '\n' then
        val contentEnd =
          if index > 0 && source.charAt(index - 1) == '\r' then index - 1
          else index

        builder += makeLine(source.substring(lineStart, contentEnd), lineStart, contentEnd)
        lineStart = index + 1

      index += 1

    if lineStart < source.length then
      builder += makeLine(source.substring(lineStart), lineStart, source.length)
    else if source.length > 0 && source.charAt(source.length - 1) == '\n' then
      builder += makeLine("", source.length, source.length)

    IArray.from(builder)

  private def makeLine(content: String, start: Int, end: Int): Line =
    var spaces = 0
    while spaces < content.length && content.charAt(spaces) == ' ' do spaces += 1
    val blank = content.forall(_ == ' ')
    Line(content, spaces, blank, start, end)

  private var cursor: Int = 0
  private var margin: Int = 0
  private var sigil: Char = '#'

  def parseDocument(): Tel.Document raises TelError =
    checkBom()
    val directive = parseInterpreterDirective()
    val pragma = parsePragma()

    if directive.absent && pragma.absent then determineMargin()
    else margin = 0

    val children = parseChildren(parentIndent = -1)

    Tel.Document(directive, pragma, lineEndings, children)

  private def checkBom(): Unit raises TelError =
    if source.length >= 1 && source.charAt(0) == '﻿' then
      abort(TelError(Reason.BomPresent))

  private def parseInterpreterDirective(): Optional[Text] =
    if cursor >= lines.length then Unset
    else if lines(cursor).content.startsWith("#!") then
      val payload = lines(cursor).content.substring(2)
      cursor += 1
      Text(payload)
    else Unset

  private def parsePragma(): Optional[Tel.Pragma] raises TelError =
    val firstNonBlank = peekNextNonBlankLine()
    firstNonBlank.let: idx =>
      val line = lines(idx)
      val content = line.content.substring(line.leadingSpaces)
      if content.startsWith("tel ") || content == "tel" then
        cursor = idx + 1
        Optional(parsePragmaContent(content))
      else Unset
    .or(Unset)

  private def parsePragmaContent(content: String): Tel.Pragma raises TelError =
    val parts = splitPhrases(content)
    if parts.head != "tel" then abort(TelError(Reason.PragmaNotFirst))
    val version =
      if parts.length >= 2 then parseVersion(parts(1))
      else (1, 0)

    val schema: Optional[Text] =
      if parts.length >= 3 then Text(parts(2)): Optional[Text] else Unset

    val pragmaSigil: Optional[Char] =
      if parts.length >= 4 && parts(3).length == 1 then
        val c = parts(3).charAt(0)
        sigil = c
        c: Optional[Char]
      else Unset

    Tel.Pragma(version, schema, pragmaSigil)

  private def parseVersion(s: String): (Int, Int) raises TelError =
    val dot = s.indexOf('.')
    if dot <= 0 || dot == s.length - 1 then abort(TelError(Reason.BadVersion))
    try
      val major = s.substring(0, dot).toInt
      val minor = s.substring(dot + 1).toInt
      if major < 0 || minor < 0 then abort(TelError(Reason.BadVersion))
      (major, minor)
    catch case _: NumberFormatException => abort(TelError(Reason.BadVersion))

  // Inline atoms are separated by single soft spaces until the first hard
  // space; from that point onward only hard spaces separate them. The
  // current pragma parsing path is a simplified subset (no remark, no hard
  // spaces in atom positions per the spec).
  private def splitPhrases(content: String): List[String] =
    val parts = scala.collection.mutable.ListBuffer.empty[String]
    val builder = StringBuilder()
    var i = 0
    var hardSpaceMode = false
    while i < content.length do
      val ch = content.charAt(i)
      if ch == ' ' then
        // Count consecutive spaces
        var j = i
        while j < content.length && content.charAt(j) == ' ' do j += 1
        val runLength = j - i
        if !hardSpaceMode && runLength == 1 then
          if builder.nonEmpty then { parts += builder.toString; builder.clear() }
          i += 1
        else
          // hard space; switches into hard-space mode and ends the phrase
          if !hardSpaceMode then hardSpaceMode = true
          if builder.nonEmpty then { parts += builder.toString; builder.clear() }
          i = j
      else
        builder.append(ch)
        i += 1

    if builder.nonEmpty then parts += builder.toString

    parts.toList

  private def determineMargin(): Unit =
    peekNextNonBlankLine().let: idx =>
      margin = lines(idx).leadingSpaces

  private def peekNextNonBlankLine(): Optional[Int] =
    var i = cursor
    while i < lines.length && lines(i).blank do i += 1
    if i < lines.length then i else Unset

  // Returns the indent in levels (units of two spaces beyond the margin).
  // Raises E106 if leading spaces fall short of the margin, E107 if the
  // post-margin offset is odd. Phase-1 recovery is to bail on first error;
  // a future phase will adopt §19.5's accumulating recovery.
  private def indentOf(line: Line): Int raises TelError =
    val relative = line.leadingSpaces - margin
    if relative < 0 then abort(TelError(Reason.LessThanMargin))
    else if relative % 2 != 0 then abort(TelError(Reason.OddIndentation))
    else relative / 2

  private def checkTrailingSpaces(line: Line): Unit raises TelError =
    if line.content.length > 0 && line.content.charAt(line.content.length - 1) == ' '
      && !line.blank
    then abort(TelError(Reason.TrailingSpaces))

  // Recursively parses the child blocks of a node at parentIndent. The
  // children themselves are at parentIndent + 1, except at top-level where
  // parentIndent = -1 means children are at indent 0.
  private def parseChildren(parentIndent: Int): IArray[Tel.Block] raises TelError =
    val expected = parentIndent + 1
    val builder = scala.collection.mutable.ArrayBuffer.empty[Tel.Block]

    while
      val nextIdx = peekNextNonBlankLine()
      nextIdx.let(idx => indentOf(lines(idx)) == expected).or(false)
    do
      cursor = peekNextNonBlankLine().vouch
      builder += parseBlock(expected)

    IArray.from(builder)

  // Parses a single block: optional leading comments at the given indent,
  // followed by a contiguous run of compounds at the same indent, followed
  // by zero or more trailing blank lines. Blanks between the comment-group
  // and the compound-group are "interior" (not counted as trailing).
  private def parseBlock(indent: Int): Tel.Block raises TelError =
    val comments = scala.collection.mutable.ArrayBuffer.empty[Tel.Comment]

    while atCommentLine(indent) do
      comments += parseCommentLine(lines(cursor))
      cursor += 1

    // If comments are followed by blanks then a compound at the same indent,
    // consume the blanks as interior whitespace (do not count them anywhere).
    if comments.nonEmpty && cursor < lines.length && lines(cursor).blank then
      var probe = cursor
      while probe < lines.length && lines(probe).blank do probe += 1
      if probe < lines.length && indentOf(lines(probe)) == indent
        && !isCommentLine(lines(probe), indent)
      then cursor = probe

    // A tabulation line at this indent introduces a tabulated block: rows
    // follow as ordinary compound lines (the column-alignment rules of §16
    // are not enforced in phase 1; the row's keyword + inline atoms are
    // captured verbatim per the phrase-separation rule).
    val tabulation: Optional[Tel.Tabulation] =
      if cursor < lines.length && isTabulationLineAt(lines(cursor), indent) then
        val line = lines(cursor)
        cursor += 1
        Optional(parseTabulationLine(line))
      else Unset

    val compounds = scala.collection.mutable.ArrayBuffer.empty[Tel.Compound]

    while
      cursor < lines.length
      && !lines(cursor).blank
      && indentOf(lines(cursor)) == indent
      && !isCommentLine(lines(cursor), indent)
      && !isTabulationLineAt(lines(cursor), indent)
    do
      val line = lines(cursor)
      cursor += 1
      val parsed = parseCompoundLine(line, indent)
      val extraAtom =
        if tabulation.absent then parseSourceOrLiteralAtom(line.leadingSpaces)
        else Unset

      val children =
        if extraAtom.absent && tabulation.absent then parseChildren(indent)
        else IArray.empty[Tel.Block]

      val withAtom = extraAtom.lay(parsed): atom =>
        parsed.copy(atoms = parsed.atoms :+ atom)

      compounds += withAtom.copy(children = children)

    val trailingBlankLines = consumeTrailingBlanksFor(indent)

    Tel.Block(IArray.from(comments), tabulation, IArray.from(compounds), trailingBlankLines)

  private def isTabulationLineAt(line: Line, indent: Int): Boolean raises TelError =
    if line.blank || indentOf(line) != indent then false
    else
      val start = line.leadingSpaces
      start < line.content.length
      && line.content.charAt(start) == sigil
      && hasTabulationMarker(line, start)

  // Extracts marker offsets and headings from a tabulation line. The first
  // marker is at the first non-space position; subsequent markers are
  // sigil characters immediately preceded by a hard space. Headings are
  // the text between a marker and the next hard-space-marker boundary
  // (or end of line for the final column), with the introducer space
  // consumed per §16.1.
  private def parseTabulationLine(line: Line): Tel.Tabulation =
    val content = line.content
    val markers = scala.collection.mutable.ArrayBuffer.empty[Int]

    val first = line.leadingSpaces
    markers += first
    var i = first + 1
    while i < content.length do
      if content.charAt(i) == ' ' then
        var j = i
        while j < content.length && content.charAt(j) == ' ' do j += 1
        val runLen = j - i
        if runLen >= 2 && j < content.length && content.charAt(j) == sigil then
          markers += j
          i = j + 1
        else i = j
      else i += 1

    val headings = scala.collection.mutable.ArrayBuffer.empty[Text]
    var m = 0
    while m < markers.length do
      val markerPos = markers(m)
      val nextLimit = if m + 1 < markers.length then markers(m + 1) else content.length
      headings += extractHeading(content, markerPos + 1, nextLimit)
      m += 1

    Tel.Tabulation(IArray.from(markers), IArray.from(headings))

  // After a marker (which sits at the sigil's index, so `start` is index+1),
  // an optional soft space introduces the heading text; the heading ends at
  // the next hard space or at `limit` (the position of the next marker, or
  // end of line for the final column).
  private def extractHeading(content: String, start: Int, limit: Int): Text =
    if start >= limit then Text("")
    else if content.charAt(start) != ' ' then Text("")  // malformed; E120 territory
    else if start + 1 < limit && content.charAt(start + 1) == ' ' then Text("")
    else
      var i = start + 1
      var stop = limit
      while i < limit - 1 do
        if content.charAt(i) == ' ' && content.charAt(i + 1) == ' ' then
          stop = i
          i = limit
        else i += 1

      Text(content.substring(start + 1, stop))

  // Counts blank lines that "belong" to a block at the given indent: the
  // run of blanks immediately following the block, *stopping* when the next
  // non-blank line is at a different indent. Blanks preceding a shallower
  // line belong to the enclosing block; blanks preceding a deeper line
  // would indicate over-indentation (handled elsewhere).
  private def consumeTrailingBlanksFor(indent: Int): Int raises TelError =
    var count = 0
    while
      cursor < lines.length && lines(cursor).blank
      && nextNonBlankMatches(indent)
    do
      count += 1
      cursor += 1

    count

  private def nextNonBlankMatches(indent: Int): Boolean raises TelError =
    var probe = cursor
    while probe < lines.length && lines(probe).blank do probe += 1
    if probe >= lines.length then true
    else indentOf(lines(probe)) == indent

  // A line is a comment if, after its leading indentation, its first
  // non-space character is the sigil and that sigil is either at end of
  // line or followed by exactly one soft space; the `#foo` form (sigil
  // concatenated with content) is NOT a comment. Tabulation lines are
  // distinguished by a second sigil preceded by a hard space (deferred for
  // phase 1; for now any sigil-prefixed line that does not have a tabulation
  // marker is a comment).
  private def atCommentLine(indent: Int): Boolean raises TelError =
    cursor < lines.length && isCommentLine(lines(cursor), indent)

  private def isCommentLine(line: Line, indent: Int): Boolean raises TelError =
    if line.blank || indentOf(line) != indent then false
    else
      val start = line.leadingSpaces
      if start >= line.content.length || line.content.charAt(start) != sigil then false
      else
        val next = start + 1
        if next >= line.content.length then true  // bare sigil
        else if line.content.charAt(next) == ' ' then !hasTabulationMarker(line, start)
        else false  // `#foo` — not a comment

  private def hasTabulationMarker(line: Line, fromIdx: Int): Boolean =
    var i = fromIdx + 1
    val content = line.content
    while i < content.length - 1 do
      if content.charAt(i) == ' ' && content.charAt(i + 1) == ' ' then
        var j = i
        while j < content.length && content.charAt(j) == ' ' do j += 1
        if j < content.length && content.charAt(j) == sigil then return true
        i = j
      else i += 1

    false

  // After a compound line, optionally consume a source atom (§14, indent
  // = compound + 2 levels, 4 spaces deeper) or literal atom (§15, indent
  // = compound + 3 levels, 6 spaces deeper). Returns the atom if one was
  // consumed; otherwise Unset and parsing falls through to parseChildren.
  private def parseSourceOrLiteralAtom(compoundLeadingSpaces: Int)
  : Optional[Tel.Atom] raises TelError =
    if cursor >= lines.length || lines(cursor).blank then Unset
    else
      val line = lines(cursor)
      val sourceIndent = compoundLeadingSpaces + 4
      val literalIndent = compoundLeadingSpaces + 6
      if line.leadingSpaces == literalIndent then Optional(parseLiteralAtom(literalIndent))
      else if line.leadingSpaces == sourceIndent then Optional(parseSourceAtom(sourceIndent))
      else Unset

  // Source atom: captures lines whose leading-spaces ≥ sourceIndent (or
  // are blank, when followed by more source content or EOF), strips
  // exactly sourceIndent spaces from each non-blank line, trims trailing
  // spaces, and joins with `\n` per captured line (each line, including
  // the last, contributes one terminating `\n`).
  private def parseSourceAtom(sourceIndent: Int): Tel.Atom.Source =
    val captured = scala.collection.mutable.ListBuffer.empty[String]

    var done = false
    while !done && cursor < lines.length do
      val line = lines(cursor)
      if line.blank then
        var probe = cursor
        while probe < lines.length && lines(probe).blank do probe += 1
        val keep =
          probe >= lines.length
          || lines(probe).leadingSpaces >= sourceIndent

        if keep then
          while cursor < probe do
            captured += ""
            cursor += 1
        else done = true
      else if line.leadingSpaces >= sourceIndent then
        val rest = line.content.substring(sourceIndent)
        var endIdx = rest.length
        while endIdx > 0 && rest.charAt(endIdx - 1) == ' ' do endIdx -= 1
        captured += rest.substring(0, endIdx)
        cursor += 1
      else done = true

    val sb = StringBuilder()
    captured.foreach: c =>
      sb.append(c)
      sb.append('\n')

    Tel.Atom.Source(Text(sb.toString))

  // Literal atom: the opening line's content (after literalIndent spaces)
  // is the delimiter. Payload is everything between the LF terminating the
  // opening line and the next bare `\n<delim>\n` match in the *raw* byte
  // stream. The closing delimiter line is flush left (column 0).
  private def parseLiteralAtom(literalIndent: Int): Tel.Atom.Literal raises TelError =
    val openingLine = lines(cursor)
    val delimiter = openingLine.content.substring(literalIndent)
    cursor += 1
    val payloadStart = openingLine.end + (if lineEndings == Tel.LineEndings.Crlf then 2 else 1)
    val emptyPattern = delimiter + "\n"

    // An empty payload (§15: "An empty literal payload (a LF immediately
    // followed by the delimiter and a LF) is permitted") shares the
    // opening line's terminating LF with the closing pattern's leading
    // LF; the search-from-payloadStart path can't match because there's
    // no leading LF to find at payloadStart. Handle that case explicitly.
    val (rawPayload, afterClose) =
      if source.startsWith(emptyPattern, payloadStart) then
        ("", payloadStart + emptyPattern.length)
      else
        val pattern = "\n" + delimiter + "\n"
        val closeIdx = source.indexOf(pattern, payloadStart)
        if closeIdx < 0 then abort(TelError(Reason.UnclosedLiteral))
        (source.substring(payloadStart, closeIdx), closeIdx + pattern.length)

    // Strip \r\n → \n inside the literal payload. The TEL spec (§15) states
    // payload bytes are preserved literally, including any CR. The Rust
    // reference implementation, however, normalises CRLF to LF inside the
    // payload; the upstream `pos/literal-atom-cr-in-payload.check` fixture
    // pins this normalisation. Recorded in doc/spec-notes.md.
    val payload = rawPayload.replace("\r\n", "\n")

    // Advance cursor past the closing delimiter line: the line whose start
    // is exactly afterClose.
    while cursor < lines.length && lines(cursor).start < afterClose do cursor += 1

    Tel.Atom.Literal(Text(delimiter), Text(payload))

  private def parseCommentLine(line: Line): Tel.Comment =
    val start = line.leadingSpaces + 1
    val payload =
      if start >= line.content.length then ""
      else if line.content.charAt(start) == ' ' then line.content.substring(start + 1)
      else line.content.substring(start)

    Tel.Comment(Text(payload))

  // Splits the post-indent content of an ordinary line into a keyword (the
  // first phrase), subsequent inline atoms with their preceding-space
  // counts, and an optional remark per §10.3 / §11.2. Source and literal
  // atoms are added in a separate pass.
  private def parseCompoundLine(line: Line, indent: Int): Tel.Compound raises TelError =
    checkTrailingSpaces(line)
    val content = line.content.substring(line.leadingSpaces)

    // First phrase = keyword (precedingSpaces = 0, never a remark).
    var i = 0
    val keywordBuilder = StringBuilder()
    while i < content.length && content.charAt(i) != ' ' do
      keywordBuilder.append(content.charAt(i))
      i += 1

    val keyword = Text(keywordBuilder.toString)

    val atoms = scala.collection.mutable.ListBuffer.empty[Tel.Atom]
    val builder = StringBuilder()
    var precedingSpaces = 0
    var hardSpaceMode = false
    var atomOpen = false
    var remark: Optional[Text] = Unset

    inline def commit(): Unit =
      if atomOpen then
        atoms += Tel.Atom.Inline(Text(builder.toString), precedingSpaces)
        builder.clear()
        atomOpen = false

    while i < content.length && remark.absent do
      val ch = content.charAt(i)
      if ch == ' ' then
        var j = i
        while j < content.length && content.charAt(j) == ' ' do j += 1
        val run = j - i

        if hardSpaceMode then
          if run >= 2 then
            commit()
            precedingSpaces = run
            i = j
          else
            builder.append(' ')
            atomOpen = true
            i = j
        else
          if run == 1 then
            commit()
            precedingSpaces = 1
            i = j
          else
            commit()
            precedingSpaces = run
            hardSpaceMode = true
            i = j
      else if ch == sigil && !atomOpen then
        val afterSigil = i + 1
        val softSpaceAfter =
          afterSigil < content.length
          && content.charAt(afterSigil) == ' '
          && (afterSigil + 1 >= content.length || content.charAt(afterSigil + 1) != ' ')

        if softSpaceAfter then
          remark = Text(content.substring(afterSigil + 1))
          i = content.length
        else
          builder.append(ch)
          atomOpen = true
          i += 1
      else
        builder.append(ch)
        atomOpen = true
        i += 1

    commit()

    Tel.Compound(keyword, IArray.from(atoms), remark, IArray.empty)
