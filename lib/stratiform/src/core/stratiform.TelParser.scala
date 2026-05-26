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
import gossamer.*
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
       val blank:         Boolean )

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
    var start = 0
    var index = 0
    while index < source.length do
      if source.charAt(index) == '\n' then
        val end =
          if index > 0 && source.charAt(index - 1) == '\r' then index - 1
          else index

        builder += makeLine(source.substring(start, end))
        start = index + 1

      index += 1

    if start < source.length then builder += makeLine(source.substring(start))
    else if source.length > 0 && source.charAt(source.length - 1) == '\n' then
      builder += makeLine("")

    IArray.from(builder)

  private def makeLine(content: String): Line =
    var spaces = 0
    while spaces < content.length && content.charAt(spaces) == ' ' do spaces += 1
    val blank = content.forall(_ == ' ')
    Line(content, spaces, blank)

  private var cursor: Int = 0
  private var margin: Int = 0
  private var sigil: Char = '#'

  def parseDocument(): Tel.Document raises TelError =
    val directive = parseInterpreterDirective()
    val pragma = parsePragma()

    if directive.absent && pragma.absent then determineMargin()
    else margin = 0

    val children = parseChildren(parentIndent = -1)

    Tel.Document(directive, pragma, lineEndings, children)

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

  // Returns a non-negative indent for a line, or -1 if leading spaces are
  // less than the margin (which is an E106 condition; deferred for phase 1).
  private def indentOf(line: Line): Int =
    val relative = line.leadingSpaces - margin
    if relative < 0 then -1
    else relative / 2

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

  // Parses a single block: a contiguous run of compounds at the given
  // indent with no blank lines between them, followed by zero or more
  // trailing blank lines.
  private def parseBlock(indent: Int): Tel.Block raises TelError =
    val compounds = scala.collection.mutable.ArrayBuffer.empty[Tel.Compound]

    while
      cursor < lines.length
      && !lines(cursor).blank
      && indentOf(lines(cursor)) == indent
    do
      val line = lines(cursor)
      cursor += 1
      val parsed = parseCompoundLine(line, indent)
      val children = parseChildren(indent)
      compounds += parsed.copy(children = children)

    val trailingBlankLines = consumeTrailingBlanks()

    Tel.Block(IArray.empty, Unset, IArray.from(compounds), trailingBlankLines)

  private def consumeTrailingBlanks(): Int =
    var count = 0
    while cursor < lines.length && lines(cursor).blank do
      count += 1
      cursor += 1

    count

  private def parseCompoundLine(line: Line, indent: Int): Tel.Compound raises TelError =
    val content = line.content.substring(line.leadingSpaces)
    val (atoms, remark) = parseInlineAtomsAndRemark(content)
    val keyword =
      if atoms.isEmpty then Text("")
      else atoms.head match
        case Tel.Atom.Inline(text, _) => text
        case _                        => Text("")

    val rest = atoms.tail.map:
      case Tel.Atom.Inline(text, ps) => Tel.Atom.Inline(text, ps)
      case other                     => other

    Tel.Compound(keyword, IArray.from(rest), remark, IArray.empty)

  // Splits the post-indent content of an ordinary line into the keyword
  // (first inline atom with precedingSpaces = 0), subsequent inline atoms
  // with their preceding-space counts, and an optional remark. Source and
  // literal atoms are not handled here.
  private def parseInlineAtomsAndRemark(content: String): (List[Tel.Atom], Optional[Text]) =
    val atoms = scala.collection.mutable.ListBuffer.empty[Tel.Atom]
    val builder = StringBuilder()
    var precedingSpaces = 0
    var i = 0
    var hardSpaceMode = false
    var remark: Optional[Text] = Unset

    inline def commit(): Unit =
      if builder.nonEmpty then
        atoms += Tel.Atom.Inline(Text(builder.toString), precedingSpaces)
        builder.clear()

    while i < content.length && remark.absent do
      val ch = content.charAt(i)
      if ch == ' ' then
        var j = i
        while j < content.length && content.charAt(j) == ' ' do j += 1
        val run = j - i
        if !hardSpaceMode && run == 1 then
          // soft space; phrase terminator
          commit()
          precedingSpaces = 1
          i = j
        else
          // hard space; switches into hard-space mode
          commit()
          precedingSpaces = run
          hardSpaceMode = true
          i = j

      else if ch == sigil && builder.isEmpty && atoms.nonEmpty
        && precedingSpaces == 1 && !hardSpaceMode
        && (i + 1 < content.length && content.charAt(i + 1) == ' ' || i + 1 == content.length)
      then
        // Remark: sigil at phrase boundary followed by exactly one soft space
        if i + 1 < content.length && content.charAt(i + 1) == ' ' then
          remark = Text(content.substring(i + 2))
        else
          remark = Text("")
        i = content.length

      else
        builder.append(ch)
        i += 1

    commit()

    // The "keyword" is the first phrase with precedingSpaces=0; later
    // inline atoms record the actual run length. The caller reassigns
    // precedingSpaces=1 for the first atom in keyword-only contexts.
    val first = atoms.headOption
    val rest = atoms.drop(1).toList
    val fixedFirst = first match
      case Some(Tel.Atom.Inline(text, _)) => List(Tel.Atom.Inline(text, 0))
      case _                              => Nil

    (fixedFirst ::: rest, remark)
