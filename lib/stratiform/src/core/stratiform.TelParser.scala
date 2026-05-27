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

  def parse(input: Data): Tel.Document raises TelError =
    TelParser(input, Unset).parseDocument()

  // Schema-aware parse: when an odd-indented line is encountered the
  // parser uses §19.5's schema-aware E107 recovery to disambiguate
  // shallower vs deeper. Without a schema the original shallower-wins
  // rule still applies.
  def parse(input: Data, schema: Tels): Tel.Document raises TelError =
    TelParser(input, schema: Optional[Tels]).parseDocument()

  private final class Line
     ( val content:       String,
       val leadingSpaces: Int,
       val blank:         Boolean,
       val start:         Int,
       val end:           Int )

private final class TelParser(input: Data, schema: Optional[Tels]):
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

  // Detect line-ending violations per §17 / E121. A CR that is not part
  // of a CR LF pair is a bare CR and raises E121. In CRLF mode, a bare
  // LF (one not preceded by CR) outside of literal-atom payloads also
  // raises E121. We approximate "outside literal payload" structurally
  // here by walking the byte stream; the literal-atom payload exception
  // is handled correctly because the structural line break terminating
  // a literal-atom opening line is itself a valid CRLF in CRLF-mode
  // documents.
  private def checkLineEndings(): Unit raises TelError =
    var i = 0
    val crlfMode = lineEndings == Tel.LineEndings.Crlf
    while i < source.length do
      val c = source.charAt(i)
      if c == '\r' && (i + 1 >= source.length || source.charAt(i + 1) != '\n')
      then errorAtOffset(Reason.BadLineEnding, i)
      else if crlfMode && c == '\n' && (i == 0 || source.charAt(i - 1) != '\r')
      then errorAtOffset(Reason.BadLineEnding, i)
      i += 1

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
  // The highest line index consumed by the prologue (interpreter
  // directive or pragma). The E109 check treats anything at or before
  // this line as equivalent to "start of file" — comments immediately
  // after the prologue are valid.
  private var prologueEndLine: Int = -1

  // Ancestor stack of Struct types known for each open compound, used
  // by the schema-aware E107 recovery rule of §19.5. The element at
  // index `i` is the schema struct corresponding to the compound at
  // depth `i+1` (so the document root at depth 0 is implicit and
  // refers to `schema.document`). `Unset` entries mark compounds
  // whose schema position couldn't be resolved (e.g. an unknown
  // keyword in a permissive layer), in which case both shallower and
  // deeper recovery treat that ancestor as "schema-blind".
  private val ancestors =
    scala.collection.mutable.ArrayBuffer.empty[Optional[Tels.Struct]]

  // Build a `TelError.Position` for a given line index (0-based internally;
  // converted to the 1-indexed line numbers callers expect). Column
  // defaults to `1` meaning "first character of the line"; pass an
  // explicit `column` for mid-line errors. Returns `(1, 1)` for an
  // out-of-range index so a buggy caller can't crash the parser
  // mid-error.
  private inline def positionAt(lineIdx: Int, column: Int = 1): TelError.Position =
    if lineIdx < 0 || lineIdx >= lines.length then TelError.Position(1, 1)
    else TelError.Position(lineIdx + 1, column)

  // Compute a position from an absolute byte offset into the original
  // `source`. Used by `checkBom` / `checkLineEndings`, which walk the
  // raw bytes before any cursor advance, so they don't have a line
  // index to hand to `positionAt`. Linear scan is fine — these helpers
  // run at most a handful of times per parse.
  private def positionAtOffset(byteOffset: Int): TelError.Position =
    var lineIdx = 0
    while lineIdx < lines.length && lines(lineIdx).end <= byteOffset do lineIdx += 1
    if lineIdx >= lines.length then
      val n = lines.length.max(1)
      TelError.Position(n, 1)
    else
      val line = lines(lineIdx)
      TelError.Position(lineIdx + 1, byteOffset - line.start + 1)

  // Abort with a positional `TelError`. The convenience helpers
  // `errorAt(reason, lineIdx)` and `errorAtOffset(reason, offset)`
  // construct the position before delegating here.
  private def errorAt(reason: Reason, lineIdx: Int, column: Int = 1)
                     (using Tactic[TelError])
  :   Nothing =

    abort(TelError(reason, positionAt(lineIdx, column)))

  private def errorAtOffset(reason: Reason, byteOffset: Int)
                           (using Tactic[TelError])
  :   Nothing =

    abort(TelError(reason, positionAtOffset(byteOffset)))

  // Reverse-lookup of a `Line` instance's index in the pre-split
  // `lines` array. The comparison uses object identity (`ne`) so it
  // stays O(1) per slot — only the error path hits this, never the
  // hot parse path, so the linear scan is fine. Returns `-1` if the
  // line isn't part of this parser's `lines` (e.g. a synthetic
  // line constructed mid-parse), which `positionAt` then maps to the
  // sentinel position `(1, 1)`.
  private def lineIndexOf(line: Line): Int =
    var i = 0
    while i < lines.length && (lines(i) ne line) do i += 1
    if i < lines.length then i else -1

  private def errorAtLine(reason: Reason, line: Line, column: Int = 1)
                         (using Tactic[TelError])
  :   Nothing =

    errorAt(reason, lineIndexOf(line), column)

  // Push a child compound's resolved struct onto the ancestor stack.
  // The compound is at depth `parentDepth + 1`; `parentDepth` is the
  // length of the current ancestor stack (0 = document root). If we
  // can resolve the compound's keyword to a Struct type inside the
  // parent's schema struct, push it; otherwise push Unset.
  private def pushAncestor(keyword: Text): Unit raises TelError =
    schema.let: s =>
      val parent: Optional[Tels.Struct] =
        if ancestors.isEmpty then s.document else ancestors(ancestors.length - 1)

      val resolved: Optional[Tels.Struct] = parent.let: p =>
        resolveKeywordStruct(p, keyword, s)

      ancestors += resolved
    .or:
      // Without a schema we still keep the depth alignment so pop
      // works symmetrically, but the entries are always Unset.
      ancestors += Unset

  private def popAncestor(): Unit =
    if ancestors.nonEmpty then ancestors.remove(ancestors.length - 1)

  // Resolve `keyword` against `parent`'s direct Field / SelectRef
  // members. Returns the Struct type if the keyword names a child
  // whose resolved type is a Struct, or Unset otherwise (Scalar /
  // Flag / unknown keyword / Reference to a non-struct).
  private def resolveKeywordStruct(parent: Tels.Struct, keyword: Text, schema: Tels)
  :     Optional[Tels.Struct] raises TelError =
    val matched: Optional[Tels.Type] =
      var found: Optional[Tels.Type] = Unset
      var i = 0
      while i < parent.members.length && found.absent do
        parent.members(i) match
          case f: Tels.Field =>
            if f.keyword == keyword then found = f.fieldType

          case s: Tels.SelectRef =>
            // Expand the SelectRef's variants.
            schema.selects.find(_.name == s.reference).foreach: selectDef =>
              selectDef.variants.find(_.keyword == keyword).foreach: variant =>
                found = variant.variantType

          case _: Tels.Exclude => ()

        i += 1

      found

    matched.let: t =>
      resolveTypeToStruct(t, schema)
    .or(Unset)

  // Resolve a Type to a Struct (following one level of Reference) or
  // Unset if it doesn't resolve to a Struct.
  private def resolveTypeToStruct(t: Tels.Type, schema: Tels)
  :     Optional[Tels.Struct] =
    t match
      case s: Tels.Struct =>
        s

      case Tels.Reference(name) =>
        schema.records.find(_.name == name) match
          case Some(rec) => Tels.Struct(rec.members, rec.validators)
          case None      => Unset

      case _ =>
        Unset

  // Does `parent` contain `keyword` in its direct Field/SelectRef
  // keyword set?
  private def keywordAdmissible(parent: Tels.Struct, keyword: Text, schema: Tels): Boolean =
    var i = 0
    while i < parent.members.length do
      parent.members(i) match
        case f: Tels.Field =>
          if f.keyword == keyword then return true

        case s: Tels.SelectRef =>
          schema.selects.find(_.name == s.reference).foreach: selectDef =>
            if selectDef.variants.exists(_.keyword == keyword) then return true

        case _: Tels.Exclude => ()

      i += 1

    false

  // Extract the keyword phrase from a non-blank, non-comment,
  // non-tabulation line — used by the schema-aware E107 recovery to
  // probe the schema. Falls back to the empty string if the line has
  // no leading keyword.
  private def extractKeyword(line: Line): Text =
    val start = line.leadingSpaces
    var end = start
    while end < line.content.length
      && line.content.charAt(end) != ' '
      && line.content.charAt(end) != sigil
    do end += 1

    Text(line.content.substring(start, end))

  def parseDocument(): Tel.Document raises TelError =
    checkBom()
    checkLineEndings()
    val directive = parseInterpreterDirective()
    val pragma = parsePragma()

    if directive.absent && pragma.absent then determineMargin()
    else margin = 0

    val children = parseChildren(parentIndent = -1)

    Tel.Document(directive, pragma, lineEndings, children)

  private def checkBom(): Unit raises TelError =
    if source.length >= 1 && source.charAt(0) == '﻿' then
      errorAtOffset(Reason.BomPresent, 0)

  private def parseInterpreterDirective(): Optional[Text] =
    if cursor >= lines.length then Unset
    else if lines(cursor).content.startsWith("#!") then
      val payload = lines(cursor).content.substring(2)
      prologueEndLine = cursor
      cursor += 1
      Text(payload)
    else Unset

  private def parsePragma(): Optional[Tel.Pragma] raises TelError =
    val firstNonBlank = peekNextNonBlankLine()
    val result = firstNonBlank.let: idx =>
      val line = lines(idx)
      val content = line.content.substring(line.leadingSpaces)
      if content.startsWith("tel ") || content == "tel" then
        // §8: the pragma must be entirely within the first 4096 bytes of
        // the document. A pragma-shaped line that begins at or after
        // byte 4096 — or extends past it — is E103.
        if line.start >= 4096 || line.end > 4096
        then errorAt(Reason.PragmaTooLong, idx)
        prologueEndLine = idx
        cursor = idx + 1
        Optional(parsePragmaContent(content, idx))
      else Unset
    .or(Unset)

    // Detect a pragma-shaped line that occurs after the first non-blank
    // line — that's E102 (PragmaNotFirst) per §8 of the TEL spec.
    if result.absent then
      var j = firstNonBlank.or(0) + 1
      while j < lines.length do
        val ln = lines(j)
        if !ln.blank && ln.leadingSpaces == 0 then
          val c = ln.content
          if c == "tel" || c.startsWith("tel ") then errorAt(Reason.PragmaNotFirst, j)

        j += 1

    result

  private def parsePragmaContent(content: String, lineIdx: Int): Tel.Pragma raises TelError =
    val parts = splitPhrases(content)
    if parts.head != "tel" then errorAt(Reason.PragmaNotFirst, lineIdx)
    val version =
      if parts.length >= 2 then parseVersion(parts(1), lineIdx)
      else (1, 0)

    // §8: the pragma carries exactly tel + version + optional schema +
    // optional sigil. Anything beyond that — including a remark
    // introducer — is E123. Check up front so a stray remark doesn't
    // first hit the schema-identifier validator.
    if parts.length > 4 then errorAt(Reason.ExtraPragmaContent, lineIdx)

    val schema: Optional[Text] =
      if parts.length >= 3 then
        // §8.2: the schema atom must be either a URL (containing "://")
        // or a BASE-256 schema signature. Plain identifier-style strings
        // are E122. The BASE-256 signature is either a 64-character hex
        // hash or a string of non-ASCII BASE-256 encoded bytes; a string
        // of only ASCII characters that lacks "://" fails both forms.
        val s = parts(2)
        val isUrl = s.indexOf("://") >= 0
        val isHexHash = s.length == 64 && s.forall: c =>
          (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
        val isBase256 = s.exists(_.toInt > 127)
        if !isUrl && !isHexHash && !isBase256
        then errorAt(Reason.BadSchemaIdentifier, lineIdx)
        Text(s): Optional[Text]
      else Unset

    val pragmaSigil: Optional[Char] =
      if parts.length >= 4 && parts(3).length == 1 then
        val c = parts(3).charAt(0)
        // Per §8.3 the sigil must be a symbolic character — not a digit,
        // not a letter, not whitespace, not the keyword-class character.
        if c.isLetterOrDigit then errorAt(Reason.BadSigil, lineIdx)
        sigil = c
        c: Optional[Char]
      else Unset

    Tel.Pragma(version, schema, pragmaSigil)

  private def parseVersion(s: String, lineIdx: Int): (Int, Int) raises TelError =
    val dot = s.indexOf('.')
    if dot <= 0 || dot == s.length - 1 then errorAt(Reason.BadVersion, lineIdx)
    try
      val major = s.substring(0, dot).toInt
      val minor = s.substring(dot + 1).toInt
      if major < 0 || minor < 0 then errorAt(Reason.BadVersion, lineIdx)
      (major, minor)
    catch case _: NumberFormatException => errorAt(Reason.BadVersion, lineIdx)

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

  // Returns the indent in levels (units of two spaces beyond the
  // margin). Raises E106 if leading spaces fall short of the margin.
  // Odd post-margin offsets go through E107 recovery: schema-aware
  // when a schema is in scope (§19.5), schema-independent shallower-
  // wins otherwise.
  private def indentOf(line: Line): Int raises TelError =
    val relative = line.leadingSpaces - margin
    if relative < 0 then errorAtLine(Reason.LessThanMargin, line)
    else if relative % 2 == 0 then relative / 2
    else recoverOddIndent(line, relative)

  // §19.5 E107 recovery. When no schema is in scope, return the
  // shallower depth (the original v1.0 rule). When a schema is in
  // scope, compute admissibility at both candidate depths via the
  // current ancestor stack, then:
  //   - both invalid or both valid → shallower (tie-break favours
  //     the shallower interpretation; type assignment can later
  //     report E306 against the chosen parent).
  //   - only one valid → that one.
  // Edge cases tracked in §19.5:
  //   1. shallower over-dedents past the ancestor stack → shallower
  //      invalid; deeper's parent doesn't exist either, so we fall
  //      back to shallower (graceful degradation).
  //   2. deeper's parent doesn't exist (no open compound at depth
  //      `shallower`) → deeper invalid.
  //   3. deeper's parent resolved to a non-Struct (Scalar/Flag) → it
  //      wasn't pushed onto the ancestor stack at all, so deeperParent
  //      is Unset → deeper invalid.
  private def recoverOddIndent(line: Line, relative: Int): Int raises TelError =
    schema.let: s =>
      val shallower = relative / 2
      val deeper    = shallower + 1
      val keyword   = extractKeyword(line)

      val shallowerParent: Optional[Tels.Struct] =
        if shallower == 0 then s.document
        else if shallower - 1 < ancestors.length then ancestors(shallower - 1)
        else Unset

      val deeperParent: Optional[Tels.Struct] =
        if shallower < ancestors.length then ancestors(shallower)
        else Unset

      val shallowerValid =
        shallowerParent.let(p => keywordAdmissible(p, keyword, s)).or(false)

      val deeperValid =
        deeperParent.let(p => keywordAdmissible(p, keyword, s)).or(false)

      if !shallowerValid && deeperValid then deeper else shallower
    .or:
      errorAtLine(Reason.OddIndentation, line)

  private def checkTrailingSpaces(line: Line): Unit raises TelError =
    if line.content.length > 0 && line.content.charAt(line.content.length - 1) == ' '
      && !line.blank
    then errorAtLine(Reason.TrailingSpaces, line, line.content.length)

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

    // After consuming children at `expected`, a remaining non-blank line
    // more indented than `expected` is an error. The kind of error
    // depends on the last block's shape:
    //   - tabulation present, no rows: E116, a tabulated row at the
    //     wrong indent
    //   - tabulation present, rows present: E112, a child of a
    //     tabulated row (rows cannot themselves have children)
    //   - no tabulation, no compounds (comment-only): E112, the
    //     orphaned line is a child of a non-compound
    //   - otherwise: E111, the line over-indents past the last
    //     compound's child level
    val rest = peekNextNonBlankLine()
    rest.let: idx =>
      val di = indentOf(lines(idx))
      if di > expected then builder.lastOption match
        case Some(last) if last.tabulation.present && last.compounds.isEmpty =>
          errorAt(Reason.RowWrongIndent, idx)

        case Some(last) if last.tabulation.present =>
          errorAt(Reason.ChildOfNonCompound, idx)

        case Some(last) if last.compounds.isEmpty =>
          errorAt(Reason.ChildOfNonCompound, idx)

        case _ => errorAt(Reason.OverIndentation, idx)

    IArray.from(builder)

  // Parses a single block: optional leading comments at the given indent,
  // followed by a contiguous run of compounds at the same indent, followed
  // by zero or more trailing blank lines. Blanks between the comment-group
  // and the compound-group are "interior" (not counted as trailing).
  private def parseBlock(indent: Int): Tel.Block raises TelError =
    val comments = scala.collection.mutable.ArrayBuffer.empty[Tel.Comment]

    while atCommentLine(indent) do
      // §9 E109: a comment must be preceded by start of file, the
      // prologue (directive / pragma), a blank line, another comment,
      // or a line at lesser indent. A non-blank non-comment line at
      // the same or greater indent immediately before violates the
      // rule.
      if cursor > 0 && cursor - 1 > prologueEndLine then
        val prev = lines(cursor - 1)
        if !prev.blank && !isCommentLine(prev, indent) && prev.leadingSpaces >= margin + indent * 2
        then errorAt(Reason.CommentNotPreceded, cursor)

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
      tabulation.let(validateTabulatedRow(line, _))
      val extraAtom =
        if tabulation.absent then parseSourceOrLiteralAtom(line.leadingSpaces)
        else Unset

      val children =
        if extraAtom.absent && tabulation.absent then
          pushAncestor(parsed.keyword)
          try parseChildren(indent) finally popAncestor()
        else IArray.empty[Tel.Block]

      val withAtom = extraAtom.lay(parsed): atom =>
        parsed.copy(atoms = parsed.atoms :+ atom)

      compounds += withAtom.copy(children = children)

    val trailingBlankLines = consumeTrailingBlanksFor(indent)

    Tel.Block(IArray.from(comments), tabulation, IArray.from(compounds), trailingBlankLines)

  // §16.2 column-rule validation for tabulated rows. Walks the row left-
  // to-right. Each contiguous run of ≥2 spaces is a hard-space and MUST
  // end exactly at one of the column marker positions M_k (k ≥ 1)
  // declared on the tabulation line (**E117**). Each non-final column
  // value's width MUST NOT exceed M_{k+1} − M_k − 2 (**E119**). A 2+
  // space run that isn't a column separator is implicitly **E118**
  // (consecutive spaces within a value), but the spec lets the parser
  // report either E117 or E118 in that case — we report E117, matching
  // the reference parser's behaviour. Remarks (hard-space + sigil +
  // soft-space introducer) are exempt and end the validation walk.
  private def validateTabulatedRow(line: Line, tabulation: Tel.Tabulation)
  :     Unit raises TelError =
    val content = line.content
    val markers = tabulation.markerOffsets
    val n = content.length

    var i = line.leadingSpaces
    var columnIdx = 0
    var phraseStart = i

    while i < n do
      if content.charAt(i) == ' ' then
        var j = i
        while j < n && content.charAt(j) == ' ' do j += 1
        val runLen = j - i

        if runLen >= 2 then
          val isRemark =
            j < n
            && content.charAt(j) == sigil
            && (j + 1 >= n || (content.charAt(j + 1) == ' '
                               && (j + 2 >= n || content.charAt(j + 2) != ' ')))

          if isRemark then i = n
          else
            if columnIdx >= 1 && columnIdx < markers.length - 1 then
              val phraseWidth = i - phraseStart
              val colMax = markers(columnIdx + 1) - markers(columnIdx) - 2
              if phraseWidth > colMax then
                errorAtLine(Reason.ColumnValueTooWide, line, phraseStart + 1)

            var foundIdx = -1
            var k = 1
            while k < markers.length && foundIdx < 0 do
              if markers(k) == j then foundIdx = k
              k += 1

            if foundIdx < 0 then errorAtLine(Reason.HardSpaceWrongPosition, line, j + 1)

            columnIdx = foundIdx
            phraseStart = j
            i = j
        else i = j
      else i += 1

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
  private def parseTabulationLine(line: Line): Tel.Tabulation raises TelError =
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
      headings += extractHeading(line, markerPos + 1, nextLimit)
      m += 1

    Tel.Tabulation(IArray.from(markers), IArray.from(headings))

  // After a marker (which sits at the sigil's index, so `start` is index+1),
  // an optional soft space introduces the heading text; the heading ends at
  // the next hard space or at `limit` (the position of the next marker, or
  // end of line for the final column). §16 / E120 forbids: non-space
  // immediately after the marker, more than one space before heading
  // content, or a sigil character inside the heading text.
  private def extractHeading(line: Line, start: Int, limit: Int): Text raises TelError =
    val content = line.content
    if start >= limit then t""
    else if content.charAt(start) != ' ' then
      errorAtLine(Reason.BadTabulationHeading, line, start + 1)
    else if start + 1 < limit && content.charAt(start + 1) == ' ' then
      // Two consecutive spaces immediately after the marker: either an
      // empty heading (introducer + single terminator space = exactly
      // two spaces between markers) or malformed (a real heading
      // starting after extra leading spaces, which is E120).
      if limit - start <= 2 then t""
      else errorAtLine(Reason.BadTabulationHeading, line, start + 1)
    else
      var i = start + 1
      var stop = limit
      while i < limit - 1 do
        if content.charAt(i) == ' ' && content.charAt(i + 1) == ' ' then
          stop = i
          i = limit
        else
          if content.charAt(i) == sigil then
            errorAtLine(Reason.BadTabulationHeading, line, i + 1)
          i += 1

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
      val first =
        if line.leadingSpaces == literalIndent then Optional(parseLiteralAtom(literalIndent))
        else if line.leadingSpaces == sourceIndent then Optional(parseSourceAtom(sourceIndent))
        else Unset

      // §10.4 / §11.1: a compound may carry at most one source or literal
      // atom. The error code names the *second* atom's kind: E113 if a
      // source follows an existing source/literal, E114 if a literal
      // follows an existing source/literal.
      if first.present && cursor < lines.length && !lines(cursor).blank then
        val nextLine = lines(cursor)
        if nextLine.leadingSpaces == literalIndent
        then errorAt(Reason.DuplicateLiteral, cursor)
        else if nextLine.leadingSpaces == sourceIndent
        then errorAt(Reason.DuplicateSource, cursor)

      first

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
        if closeIdx < 0 then errorAtLine(Reason.UnclosedLiteral, openingLine)
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
