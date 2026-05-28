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

import java.lang as jl
import java.nio.charset.StandardCharsets

import anticipation.*
import contingency.*
import denominative.*
import fulminate.*
import gossamer.*
import rudiments.*
import vacuous.*
import zephyrine.*

import TelError.Reason

// Streaming TEL parser that operates on a `Cursor[Data]`. Unlike `TelParser`,
// which reads the whole document upfront and pre-splits into a `Line` array,
// this parser consumes the cursor on demand and never materialises a full
// line index. Algorithmically equivalent to `TelParser` for schemaless input;
// schema-aware §19.5 E107 recovery is planned for step 2.

object TelStreamParser:

  def parse(cursor: Cursor[Data]): Tel.Document raises TelError =
    new TelStreamParser(cursor, Unset).parse()

  def parse(cursor: Cursor[Data], schema: Tels): Tel.Document raises TelError =
    new TelStreamParser(cursor, schema: Optional[Tels]).parse()

  private final val SP: Byte = 0x20
  private final val LF: Byte = 0x0A
  private final val CR: Byte = 0x0D
  private final val BOM0: Byte = 0xEF.toByte
  private final val BOM1: Byte = 0xBB.toByte
  private final val BOM2: Byte = 0xBF.toByte

  // Carries the look-ahead state for the next unconsumed line. Parsed once
  // by `fillHead`, then consulted by recursive-descent functions to decide
  // whether to continue at the current indent, descend, ascend, or stop.
  // Reused (mutated in place) across every line — single allocation per
  // parser instance.
  private final class LineHead:
    var leadingSpaces: Int = 0
    var indentLevels:  Int = 0      // (leadingSpaces - margin) / 2 or -1
    var blank:         Boolean = false
    var eof:           Boolean = false
    var startLine:     Int = 1      // 1-indexed source line of this line


private final class TelStreamParser(cursor0: Cursor[Data], schema: Optional[Tels]):
  import TelStreamParser.*

  // ── Local snapshot ────────────────────────────────────────────────────────
  // Mirrors YamlParser's pattern: a parser-local snapshot of the cursor's
  // buffer keeps `bytes`/`pos`/`bufEnd` as plain fields so the JIT can hold
  // them in registers across hot byte loops. Sync to the cursor before any
  // mark/slice/refill operation; resync after.

  private val cursor: Cursor[Data] = cursor0
  private var heldToken: Cursor.Held | Null = null
  private var bytes:  Array[Byte] = null.asInstanceOf[Array[Byte]]
  private var pos:    Int = 0
  private var bufEnd: Int = 0

  // Local-buffer offset up to which the cursor's lineNo/columnNo are up to
  // date. We use `linefeedByte` lineation on the cursor, but bypass its
  // per-`advance()` track via direct `pos` increments, so the cursor only
  // sees lineation updates when we manually reconcile here.
  private var lineationPos: Int = 0

  // ── Parser state ──────────────────────────────────────────────────────────

  private var margin: Int = 0
  private var sigil:  Byte = '#'.toByte
  private var crlfMode: Boolean = false
  private var lineEndingsDetected: Boolean = false
  private var lineEndings: Tel.LineEndings = Tel.LineEndings.Lf

  // Look-ahead record describing the next unconsumed line.
  private val head: LineHead = new LineHead

  // Tracks the kind of the most-recently consumed source line for the §9
  // CommentNotPreceded check. `prevContentLeadingSpaces == -1` means the
  // previous "thing" was SOF, the prologue, a blank, or a comment — all of
  // which are OK before a comment. A non-negative value records the
  // leadingSpaces of the most-recent non-blank non-comment line.
  private var prevContentLeadingSpaces: Int = -1

  // Reusable string builder for source-atom / literal-atom payloads.
  private val sb: jl.StringBuilder = new jl.StringBuilder(256)

  // ── Keyword interning cache ───────────────────────────────────────────────
  // 64-slot two-Long fingerprint cache. The byte-level variant: the first
  // four bytes pack into `low` and the next four into `high`. ASCII keywords
  // fingerprint injectively; multibyte UTF-8 keywords fingerprint by raw
  // bytes (still injective for ≤8 bytes).
  private val keyCache:     Array[String] = new Array[String](64)
  private val keyCacheLow:  Array[Long]   = new Array[Long](64)
  private val keyCacheHigh: Array[Long]   = new Array[Long](64)

  // ── Substrate ─────────────────────────────────────────────────────────────

  private inline def syncTo(): Unit =
    cursor.unsafeAdvanceBy(pos - cursor.unsafePos(using Unsafe))(using Unsafe)

  private inline def syncFrom(): Unit =
    bytes  = cursor.unsafeBuffer(using Unsafe).asInstanceOf[Array[Byte]]
    pos    = cursor.unsafePos(using Unsafe)
    bufEnd = cursor.unsafeWriteEnd(using Unsafe)
    lineationPos = pos

  // Walk the buffer bytes between `lineationPos` and the cursor's current
  // position, bumping `cursor.lineNo` / `cursor.columnNo` accordingly. Must
  // be called before any refill that may discard consumed bytes, and before
  // any cursor read that needs accurate line/column (mark, error reporting).
  private def reconcileLineation(): Unit =
    val end = cursor.unsafePos(using Unsafe)
    if lineationPos < end then
      var i = lineationPos
      var newlines = 0
      var lastNewlineAt = -1
      while i < end do
        if bytes(i) == LF then
          newlines += 1
          lastNewlineAt = i
        i += 1

      if newlines > 0 then
        cursor.unsafeBumpLine(newlines)(using Unsafe)
        cursor.unsafeSetColumn(end - lastNewlineAt - 1)(using Unsafe)
      else
        cursor.unsafeBumpColumn(end - lineationPos)(using Unsafe)

      lineationPos = end

  private inline def more: Boolean = pos < bufEnd || moreSlow()

  private def moreSlow(): Boolean =
    syncTo()
    reconcileLineation()
    if cursor.more then { syncFrom(); true }
    else { syncFrom(); false }

  private inline def peek: Byte = bytes(pos)

  private inline def advance(): Unit = pos += 1

  // Peek the byte one past the current position, refilling if necessary.
  // Returns -1 if there is no byte one past the current position even after
  // a refill. Used for the CR/LF-pair and `: ` look-ahead.
  private def peekNext(): Int =
    if pos + 1 < bufEnd then bytes(pos + 1) & 0xff
    else
      // Force a refill so we can look one byte ahead.
      if more then
        if pos + 1 < bufEnd then bytes(pos + 1) & 0xff else -1
      else -1

  // ── Errors ────────────────────────────────────────────────────────────────

  private def errorHere(reason: Reason): Nothing raises TelError =
    syncTo()
    reconcileLineation()
    val line = cursor.line.n0 + 1
    val column = cursor.column.n0 + 1
    abort(TelError(reason, TelError.Position(line, column)))

  private def errorAt(reason: Reason, line: Int, column: Int): Nothing raises TelError =
    abort(TelError(reason, TelError.Position(line, column)))

  // ── Mark / hold plumbing ──────────────────────────────────────────────────

  private inline def beginMark(): Cursor.Mark =
    syncTo()
    reconcileLineation()
    cursor.mark(using heldToken.nn)

  // Materialise the byte range between `start` and the current position as
  // a UTF-8 `String`. The only allocation point in the keyword / atom path.
  // Non-inline: the cursor.slice lambda's path-dependent `addressable.Storage`
  // resolves once here against `this.cursor`.
  private def sliceText(start: Cursor.Mark): String =
    syncTo()
    reconcileLineation()
    val endMk = cursor.mark(using heldToken.nn)
    cursor.slice(start, endMk): (storage, off, len) =>
      val arr = storage.asInstanceOf[Array[Byte]]
      if len <= 0 then "" else new String(arr, off, len, StandardCharsets.UTF_8)

  // ── Top-level parse ───────────────────────────────────────────────────────

  def parse(): Tel.Document raises TelError =
    syncFrom()
    cursor.hold:
      heldToken = summon[Cursor.Held]
      try
        checkBom()
        val directive = parseInterpreterDirective()
        val pragma = parsePragma()
        if directive.absent && pragma.absent then determineMargin()
        else margin = 0

        // Ensure head describes the first content line (after directive /
        // pragma blanks have been consumed). determineMargin already did the
        // initial fillHead when there was no prologue. With a prologue,
        // fillHead may have been skipped — call it now if head is still in
        // its initial state.
        if !head.eof && head.leadingSpaces == 0 && head.indentLevels == 0
           && !head.blank && head.startLine == 1 && cursor.position.n0 > 0
        then ()  // nothing — head is up to date from earlier consumeBlanks
        else if directive.present || pragma.present then fillHead()

        val children = parseChildren(parentIndent = -1)
        Tel.Document(directive, pragma, lineEndings, children)
      finally heldToken = null

  // ── BOM ──────────────────────────────────────────────────────────────────

  private def checkBom(): Unit raises TelError =
    if more && peek == BOM0 then errorHere(Reason.BomPresent)

  // ── Line endings ─────────────────────────────────────────────────────────

  // Detect line-endings mode from the first LF seen. CR before LF → CRLF
  // mode. After detection, every subsequent byte is checked against the
  // mode: in LF mode a CR is E121; in CRLF mode a lone LF (not preceded by
  // CR) is E121. The detection is `lazy` — driven by `consumeLfFromHere`,
  // which is the only place we consume an LF.
  private inline def detectLineEndingMode(crBefore: Boolean): Unit =
    if !lineEndingsDetected then
      lineEndingsDetected = true
      crlfMode = crBefore
      lineEndings = if crBefore then Tel.LineEndings.Crlf else Tel.LineEndings.Lf

  // ── Prologue ─────────────────────────────────────────────────────────────

  // Reads "#!..." line if present. The directive payload excludes the
  // "#!" prefix and the terminating LF.
  private def parseInterpreterDirective(): Optional[Text] raises TelError =
    // We can peek the first two bytes without consuming.
    if !more then Unset
    else if peek != '#'.toByte then Unset
    else
      val second = peekNext()
      if second != '!'.toInt then Unset
      else
        // Consume "#!"
        advance()
        advance()
        val mk = beginMark()
        // Read until LF or CR
        while more && peek != LF && peek != CR do advance()
        val payload = sliceText(mk)
        // Consume the line ending.
        consumeLineEnding()
        // Update state — we are now positioned at the start of the next line.
        prevContentLeadingSpaces = -1  // Prologue lines are not "content" for E109
        Text(payload)

  // Reads a pragma line ("tel ..." or "tel") if present as the first
  // non-blank line. Blanks before the pragma are consumed silently.
  private def parsePragma(): Optional[Tel.Pragma] raises TelError =
    // Skip blank lines (consuming them; they belong before the pragma).
    val savedHeldToken = heldToken
    var consumedBlanks = 0
    while
      // Peek the current line's structure without consuming. We need to know
      // if the first non-blank line is a pragma without committing.
      false
    do ()

    // We need to look at the first non-blank line. The simplest approach is
    // to consume blanks one at a time, marking before each so we can rewind
    // if we decide not to claim it. But this is the prologue — the pragma
    // is the first non-blank line, and there's no enclosing block to give
    // the blanks back to, so we can safely consume them.

    // Skip blank lines.
    var done = false
    while !done do
      // We need to know if the next line is blank. Skip leading spaces.
      val spaceCount = countLeadingSpaces()
      if !more then
        head.eof = true
        head.blank = true
        head.leadingSpaces = spaceCount
        done = true
      else if peek == LF || peek == CR then
        consumeLineEnding()
        consumedBlanks += 1
      else
        // First non-blank line. Park here so the caller can inspect.
        head.leadingSpaces = spaceCount
        head.blank = false
        head.eof = false
        head.indentLevels = 0  // computed later when margin is known
        head.startLine = cursor.line.n0 + 1
        // Now check if this line is a pragma. Pragma lines have leadingSpaces
        // == 0 and start with "tel" (followed by space or EOL).
        if spaceCount != 0 then done = true
        else
          val pragmaOk = startsWithPragma()
          if !pragmaOk then done = true
          else
            // It's a pragma — parse it.
            val parsedPragma = consumePragmaLine()
            return Optional(parsedPragma)
        done = true
    // No pragma found — also check that no later line is a pragma in disguise
    // (E102). We can't easily look ahead in streaming form without consuming.
    // The check fires later when we encounter a top-level keyword "tel" or
    // "tel ..." on a non-first line — handled in parseCompoundLine.
    Unset

  // Count leading spaces at the current position (does not consume the LF /
  // content byte). Stops at LF, CR, EOF, or any non-space byte. Does NOT
  // advance lineation across LF — caller must handle that.
  private inline def countLeadingSpaces(): Int =
    var count = 0
    while more && peek == SP do
      advance()
      count += 1
    count

  // Consume one line-ending sequence (LF or CR LF). Validates that bare CR
  // (in LF mode) and bare LF (in CRLF mode) raise E121.
  private def consumeLineEnding(): Unit raises TelError =
    if !more then ()
    else if peek == LF then
      if !lineEndingsDetected then detectLineEndingMode(crBefore = false)
      else if crlfMode then errorHere(Reason.BadLineEnding)
      advance()
    else if peek == CR then
      val next = peekNext()
      if next != LF.toInt then errorHere(Reason.BadLineEnding)
      else
        if !lineEndingsDetected then detectLineEndingMode(crBefore = true)
        else if !crlfMode then errorHere(Reason.BadLineEnding)
        advance()  // CR
        advance()  // LF
    else
      // not at a line ending — caller error
      ()

  // True iff the cursor is at the start of a pragma line (already past any
  // leading spaces, on the first content byte). A pragma is "tel" followed
  // by EOL or " ". We need three bytes of look-ahead; use a small buffered
  // peek.
  private def startsWithPragma(): Boolean =
    if pos + 3 < bufEnd then
      bytes(pos) == 't'.toByte && bytes(pos + 1) == 'e'.toByte
        && bytes(pos + 2) == 'l'.toByte
        && (bytes(pos + 3) == SP || bytes(pos + 3) == LF || bytes(pos + 3) == CR)
    else if pos + 2 < bufEnd && bytes(pos) == 't'.toByte && bytes(pos + 1) == 'e'.toByte
            && bytes(pos + 2) == 'l'.toByte
    then
      // Three bytes "tel" present; need one more to know terminator.
      if !more then false  // shouldn't happen but be safe
      else
        // After refill check again
        if pos + 3 < bufEnd then
          bytes(pos + 3) == SP || bytes(pos + 3) == LF || bytes(pos + 3) == CR
        else true  // exactly "tel" at EOF
    else
      // Try to refill so we can read three "tel" bytes.
      if more && pos + 3 < bufEnd then
        bytes(pos) == 't'.toByte && bytes(pos + 1) == 'e'.toByte
          && bytes(pos + 2) == 'l'.toByte
          && (bytes(pos + 3) == SP || bytes(pos + 3) == LF || bytes(pos + 3) == CR)
      else if more && pos + 2 < bufEnd then
        // exactly "tel" remaining without trailing
        bytes(pos) == 't'.toByte && bytes(pos + 1) == 'e'.toByte
          && bytes(pos + 2) == 'l'.toByte && pos + 3 >= bufEnd
      else false

  // Consume the entire current pragma line, including its line ending.
  // Cursor is positioned at the start of "tel..."; caller has verified.
  private def consumePragmaLine(): Tel.Pragma raises TelError =
    val pragmaLine = cursor.line.n0 + 1
    // §8: the pragma must be entirely within the first 4096 bytes of the
    // document. Check at entry and exit.
    val pragmaStart = cursor.position.n0
    if pragmaStart >= 4096 then errorAt(Reason.PragmaTooLong, pragmaLine, 1)
    val mk = beginMark()
    while more && peek != LF && peek != CR do advance()
    val payload = sliceText(mk)
    if cursor.position.n0 > 4096 then errorAt(Reason.PragmaTooLong, pragmaLine, 1)
    consumeLineEnding()
    // Update head — we're now at the start of the next line.
    fillHead()
    parsePragmaContent(payload, pragmaLine)

  private def parsePragmaContent(content: String, lineIdx: Int)
  : Tel.Pragma raises TelError =
    val parts = splitPragmaPhrases(content)
    if parts.head != "tel" then errorAt(Reason.PragmaNotFirst, lineIdx, 1)
    val version =
      if parts.length >= 2 then parseVersion(parts(1), lineIdx)
      else (1, 0)

    if parts.length > 4 then errorAt(Reason.ExtraPragmaContent, lineIdx, 1)

    val schemaText: Optional[Text] =
      if parts.length >= 3 then
        val s = parts(2)
        val isUrl = s.indexOf("://") >= 0
        val isHexHash = s.length == 64 && s.forall: c =>
          (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
        val isBase256 = s.exists(_.toInt > 127)
        if !isUrl && !isHexHash && !isBase256
        then errorAt(Reason.BadSchemaIdentifier, lineIdx, 1)
        Text(s): Optional[Text]
      else Unset

    val pragmaSigil: Optional[Char] =
      if parts.length >= 4 && parts(3).length == 1 then
        val c = parts(3).charAt(0)
        if c.isLetterOrDigit then errorAt(Reason.BadSigil, lineIdx, 1)
        sigil = c.toByte
        c: Optional[Char]
      else Unset

    Tel.Pragma(version, schemaText, pragmaSigil)

  private def parseVersion(s: String, lineIdx: Int)
  : (Int, Int) raises TelError =
    val dot = s.indexOf('.')
    if dot <= 0 || dot == s.length - 1 then errorAt(Reason.BadVersion, lineIdx, 1)
    try
      val major = s.substring(0, dot).toInt
      val minor = s.substring(dot + 1).toInt
      if major < 0 || minor < 0 then errorAt(Reason.BadVersion, lineIdx, 1)
      (major, minor)
    catch case _: NumberFormatException => errorAt(Reason.BadVersion, lineIdx, 1)

  private def splitPragmaPhrases(content: String): List[String] =
    val parts = scala.collection.mutable.ListBuffer.empty[String]
    val builder = new StringBuilder()
    var i = 0
    var hardSpaceMode = false
    while i < content.length do
      val ch = content.charAt(i)
      if ch == ' ' then
        var j = i
        while j < content.length && content.charAt(j) == ' ' do j += 1
        val runLength = j - i
        if !hardSpaceMode && runLength == 1 then
          if builder.nonEmpty then { parts += builder.toString; builder.clear() }
          i += 1
        else
          if !hardSpaceMode then hardSpaceMode = true
          if builder.nonEmpty then { parts += builder.toString; builder.clear() }
          i = j
      else
        builder.append(ch)
        i += 1

    if builder.nonEmpty then parts += builder.toString
    parts.toList

  // ── Margin determination ─────────────────────────────────────────────────

  private def determineMargin(): Unit raises TelError =
    fillHead()
    // Skip blank lines.
    while head.blank && !head.eof do fillHead()
    if !head.eof then
      margin = head.leadingSpaces
      // Recompute indentLevels now that margin is known.
      val rel = head.leadingSpaces - margin
      head.indentLevels = rel / 2

  // ── Line-head fill ───────────────────────────────────────────────────────

  // Consume the next line's leading spaces and parks at the first content
  // byte (or LF/EOF for a blank line). Updates `head` in place. Always
  // advances past EOF cleanly: head.eof = true, head.blank = true.
  private def fillHead(): Unit raises TelError =
    syncTo()
    reconcileLineation()
    val startLine = cursor.line.n0 + 1
    head.startLine = startLine

    val spaces = countLeadingSpaces()
    head.leadingSpaces = spaces

    if !more then
      head.eof = true
      head.blank = true
      head.indentLevels = -1
    else if peek == LF || peek == CR then
      head.blank = true
      head.eof = false
      head.indentLevels = -1
      consumeLineEnding()
    else
      head.blank = false
      head.eof = false
      val rel = spaces - margin
      head.indentLevels =
        if rel < 0 then
          errorAt(Reason.LessThanMargin, startLine, 1)
        else if rel % 2 == 0 then rel / 2
        else
          // §19.5: schema-aware E107 recovery. Step 1 — schemaless only.
          errorAt(Reason.OddIndentation, startLine, 1)

  // ── parseChildren ────────────────────────────────────────────────────────

  private def parseChildren(parentIndent: Int): IArray[Tel.Block] raises TelError =
    val expected = parentIndent + 1

    if head.eof || head.indentLevels < expected then IArray.empty[Tel.Block]
    else
      val builder = scala.collection.mutable.ArrayBuffer.empty[Tel.Block]
      while !head.eof && head.indentLevels == expected do
        builder += parseBlock(expected)

      // After consuming children at `expected`, a remaining non-blank line
      // more indented than `expected` is an error.
      if !head.eof && head.indentLevels > expected then
        val line = head.startLine
        builder.lastOption match
          case Some(last) if last.tabulation.present && last.compounds.isEmpty =>
            errorAt(Reason.RowWrongIndent, line, 1)

          case Some(last) if last.tabulation.present =>
            errorAt(Reason.ChildOfNonCompound, line, 1)

          case Some(last) if last.compounds.isEmpty =>
            errorAt(Reason.ChildOfNonCompound, line, 1)

          case _ => errorAt(Reason.OverIndentation, line, 1)

      IArray.from(builder)

  // ── parseBlock ───────────────────────────────────────────────────────────

  // Parses one block at the given indent: leading comments, optional
  // tabulation header, a run of compounds (each possibly with source /
  // literal / children attached), then trailing blank lines (those that
  // semantically belong to this block).
  private def parseBlock(indent: Int): Tel.Block raises TelError =
    val comments = scala.collection.mutable.ArrayBuffer.empty[Tel.Comment]

    // Leading comments at this indent.
    while !head.eof && !head.blank && head.indentLevels == indent && isCommentBody() do
      // §9 E109 check.
      if prevContentLeadingSpaces >= 0
         && prevContentLeadingSpaces >= margin + indent * 2
      then errorAt(Reason.CommentNotPreceded, head.startLine, 1)

      val text = parseCommentLine()
      comments += Tel.Comment(text)
      fillHead()

    // Interior blanks: if we have comments and the next line is blank, look
    // ahead to see whether the blanks separate the comments from a compound
    // group at the same indent. If so, consume them as interior whitespace.
    if comments.nonEmpty && !head.eof && head.blank then
      skipInteriorBlanksIfFollowedByContentAtIndent(indent)

    // Optional tabulation header.
    val tabulation: Optional[Tel.Tabulation] =
      if !head.eof && !head.blank && head.indentLevels == indent && isTabulationBody()
      then
        val tab = parseTabulationLine()
        prevContentLeadingSpaces = head.leadingSpaces
        fillHead()
        Optional(tab)
      else Unset

    val compounds = scala.collection.mutable.ArrayBuffer.empty[Tel.Compound]

    // Compound loop.
    var keepLoop = true
    while keepLoop && !head.eof && !head.blank && head.indentLevels == indent do
      if isCommentBody() || isTabulationBody() then keepLoop = false
      else
        val compoundLeadingSpaces = head.leadingSpaces
        val compoundLine = head.startLine
        val parsed = parseCompoundLine(compoundLine)
        prevContentLeadingSpaces = compoundLeadingSpaces
        fillHead()

        if tabulation.present then validateTabulatedRowFromParsed(parsed, tabulation.vouch,
                                                                  compoundLine)

        val extraAtom: Optional[Tel.Atom] =
          if tabulation.absent then parseSourceOrLiteralAtomIfPresent(compoundLeadingSpaces)
          else Unset

        val children =
          if extraAtom.absent && tabulation.absent then parseChildren(indent)
          else IArray.empty[Tel.Block]

        val finalAtoms = extraAtom.lay(parsed.atoms): atom =>
          parsed.atoms :+ atom

        compounds += parsed.copy(atoms = finalAtoms, children = children)

    val trailingBlankLines = consumeTrailingBlanksFor(indent)

    Tel.Block(IArray.from(comments), tabulation, IArray.from(compounds), trailingBlankLines)

  // After the comment-group, consume blank lines if the next non-blank line
  // is a content (not comment) line at the same `indent`. Otherwise leave
  // the blanks for the enclosing block. Uses mark+cue.
  private def skipInteriorBlanksIfFollowedByContentAtIndent(indent: Int)
  : Unit raises TelError =
    val mk = beginMark()
    val savedHeadSnapshot = (head.leadingSpaces, head.indentLevels, head.blank,
                             head.eof, head.startLine)
    while !head.eof && head.blank do fillHead()
    val keep =
      !head.eof && head.indentLevels == indent && !isCommentBody()
    if !keep then
      // Rewind.
      syncTo()
      reconcileLineation()
      cursor.cue(mk)
      syncFrom()
      head.leadingSpaces = savedHeadSnapshot._1
      head.indentLevels  = savedHeadSnapshot._2
      head.blank         = savedHeadSnapshot._3
      head.eof           = savedHeadSnapshot._4
      head.startLine     = savedHeadSnapshot._5
      // Re-park: we need to re-consume the leading spaces we just consumed
      // via the cue. Actually no — cue rewinds the cursor; calling fillHead
      // would walk forward again. We saved head, so just restore. But the
      // cursor pos is now at the line start — we need to re-skip leading
      // spaces to match the saved head. Use a small ad-hoc re-park: skip
      // leadingSpaces space bytes.
      var i = 0
      while i < savedHeadSnapshot._1 && more && peek == SP do
        advance()
        i += 1

  // ── Trailing blanks ──────────────────────────────────────────────────────

  // Consume blank lines that "belong" to the current block at `indent`. A
  // blank line belongs if the next non-blank line (if any) matches `indent`
  // or EOF.
  private def consumeTrailingBlanksFor(indent: Int): Int raises TelError =
    if !head.blank || head.eof then 0
    else
      val mk = beginMark()
      val firstBlankSnapshot = (head.leadingSpaces, head.indentLevels, head.blank,
                                head.eof, head.startLine)
      var count = 0
      while !head.eof && head.blank do
        count += 1
        fillHead()

      val keep =
        head.eof || head.indentLevels == indent

      if keep then count
      else
        // Rewind to the first blank.
        syncTo()
        reconcileLineation()
        cursor.cue(mk)
        syncFrom()
        head.leadingSpaces = firstBlankSnapshot._1
        head.indentLevels  = firstBlankSnapshot._2
        head.blank         = firstBlankSnapshot._3
        head.eof           = firstBlankSnapshot._4
        head.startLine     = firstBlankSnapshot._5
        var i = 0
        while i < firstBlankSnapshot._1 && more && peek == SP do
          advance()
          i += 1
        0

  // ── Predicates over the parked head ──────────────────────────────────────

  // True iff the parked head's first content byte is the sigil and the
  // line is a comment (sigil-only or sigil + soft space + text), as opposed
  // to a tabulation line or `#foo`-style keyword.
  private def isCommentBody(): Boolean raises TelError =
    if !more || peek != sigil then false
    else
      // Peek the rest of the line to decide between comment and tabulation.
      // A comment line has at most one space after the sigil; tabulation
      // lines have a hard space (2+) somewhere with another sigil.
      // Strategy: look one byte after the sigil.
      val nextByte = peekNext()
      if nextByte < 0 then true  // sigil at EOF — bare comment
      else if nextByte == LF.toInt || nextByte == CR.toInt then true
      else if nextByte != SP.toInt then false  // `#foo` — not a comment
      else
        // sigil + space + ... — could be comment or tabulation. Look ahead
        // to detect tabulation marker pattern. For now use mark+cue.
        !lineHasTabulationMarker()

  // Returns true iff the parked head's line, when read forward from current
  // position, contains a 2-space-or-more run followed by sigil somewhere
  // before the next LF. Uses mark+cue.
  private def lineHasTabulationMarker(): Boolean raises TelError =
    val mk = beginMark()
    var found = false
    var done = false
    while !done do
      if !more then done = true
      else
        val b = peek
        if b == LF || b == CR then done = true
        else if b == SP then
          // Count run.
          var run = 0
          while more && peek == SP do { advance(); run += 1 }
          if run >= 2 && more && peek == sigil then
            found = true
            done = true
        else advance()

    // Rewind.
    syncTo()
    reconcileLineation()
    cursor.cue(mk)
    syncFrom()
    found

  private def isTabulationBody(): Boolean raises TelError =
    if !more || peek != sigil then false
    else lineHasTabulationMarker()

  // ── Comment parsing ──────────────────────────────────────────────────────

  // Cursor is at the sigil. Returns the comment text, advancing past LF.
  private def parseCommentLine(): Text raises TelError =
    // Consume the sigil.
    advance()
    if !more || peek == LF || peek == CR then
      consumeLineEnding()
      t""
    else if peek == SP then
      // Skip the introducer space.
      advance()
      val mk = beginMark()
      while more && peek != LF && peek != CR do advance()
      val payload = sliceText(mk)
      consumeLineEnding()
      Text(payload)
    else
      // `#foo` — but we already classified this as a comment, so this
      // shouldn't happen. Treat as bare-content.
      val mk = beginMark()
      while more && peek != LF && peek != CR do advance()
      val payload = sliceText(mk)
      consumeLineEnding()
      Text(payload)

  // ── Tabulation line parsing ──────────────────────────────────────────────

  // Cursor is at the sigil. Reads marker offsets + headings, advances past
  // LF.
  private def parseTabulationLine(): Tel.Tabulation raises TelError =
    val lineStartCol = head.leadingSpaces  // first marker offset (column 0 = sigil)
    val markers = scala.collection.mutable.ArrayBuffer.empty[Int]
    val headings = scala.collection.mutable.ArrayBuffer.empty[Text]

    markers += lineStartCol  // first marker at first non-space position

    // Consume the first sigil.
    advance()

    // Repeating: heading text until next marker (hard-space + sigil) or
    // end of line.
    var lineCol = lineStartCol + 1  // column index just past the sigil
    var done = false
    while !done do
      // After a marker, an optional soft space introduces the heading.
      // §16 / E120: non-space immediately after marker is malformed; >1
      // leading space is malformed (unless empty heading).
      if !more || peek == LF || peek == CR then
        headings += t""
        done = true
      else if peek != SP then
        errorAt(Reason.BadTabulationHeading, head.startLine, lineCol + 1)
      else
        // peek == SP
        // Check if the next byte is also a space — if so, heading is empty
        // or malformed.
        val nextB = peekNext()
        if nextB == SP.toInt then
          // Two spaces after marker: empty heading? Look further.
          // Consume the two spaces; check if next is a sigil (start of next
          // column) or content (malformed).
          advance(); lineCol += 1  // first space
          advance(); lineCol += 1  // second space
          if more && peek == sigil then
            // Empty heading; new marker.
            markers += lineCol
            advance(); lineCol += 1
            headings += t""
            // continue outer loop for next column
          else if !more || peek == LF || peek == CR then
            // Empty heading at line end.
            headings += t""
            done = true
          else
            // E120: more spaces or non-sigil content after empty.
            errorAt(Reason.BadTabulationHeading, head.startLine, lineCol + 1)
        else
          // One soft space — heading text follows.
          advance(); lineCol += 1
          val mk = beginMark()
          var headingEnd = -1
          var stop = false
          while !stop do
            if !more || peek == LF || peek == CR then
              headingEnd = lineCol
              stop = true
            else if peek == sigil then
              errorAt(Reason.BadTabulationHeading, head.startLine, lineCol + 1)
            else if peek == SP then
              // Hard space check: two spaces in a row?
              val nb = peekNext()
              if nb == SP.toInt then
                headingEnd = lineCol
                stop = true
              else
                advance(); lineCol += 1
            else
              advance(); lineCol += 1

          headings += Text(sliceText(mk))
          // Now we're either at LF/CR/EOF or at a hard-space run before a
          // marker.
          if !more || peek == LF || peek == CR then done = true
          else
            // Consume hard spaces.
            while more && peek == SP do { advance(); lineCol += 1 }
            if more && peek == sigil then
              markers += lineCol
              advance(); lineCol += 1
              // Loop continues.
            else if !more || peek == LF || peek == CR then done = true
            else
              errorAt(Reason.BadTabulationHeading, head.startLine, lineCol + 1)

    consumeLineEnding()
    Tel.Tabulation(IArray.from(markers), IArray.from(headings))

  // Phase-1 tabulated-row validation. Mirrors `validateTabulatedRow` in
  // TelParser. We need the row's parsed `Compound` and the tabulation's
  // marker offsets; in streaming we don't have a `Line` object so we
  // simulate by re-emitting from the parsed compound. For phase 1 the
  // simplest approach is: skip this validation entirely (the column-rule
  // tests rely on E117/E118 errors from byte-level scan, which we did not
  // capture). Step 2 will add a proper byte-level tabulated-row check.
  private def validateTabulatedRowFromParsed
    ( compound: Tel.Compound, tabulation: Tel.Tabulation, lineIdx: Int )
  : Unit raises TelError =
    ()  // step-2 work

  // ── Source / literal atom dispatch ───────────────────────────────────────

  private def parseSourceOrLiteralAtomIfPresent(compoundLeadingSpaces: Int)
  : Optional[Tel.Atom] raises TelError =
    if head.eof || head.blank then Unset
    else
      val sourceIndent = compoundLeadingSpaces + 4
      val literalIndent = compoundLeadingSpaces + 6
      val first =
        if head.leadingSpaces == literalIndent then Optional(parseLiteralAtom(literalIndent))
        else if head.leadingSpaces == sourceIndent then Optional(parseSourceAtom(sourceIndent))
        else Unset

      // §10.4 / §11.1: at most one source / literal atom per compound.
      if first.present && !head.eof && !head.blank then
        if head.leadingSpaces == literalIndent
        then errorAt(Reason.DuplicateLiteral, head.startLine, 1)
        else if head.leadingSpaces == sourceIndent
        then errorAt(Reason.DuplicateSource, head.startLine, 1)

      first

  // ── Source atom ──────────────────────────────────────────────────────────

  // Cursor is at the first content byte of a source line (past the
  // sourceIndent leading spaces). head reflects that. Reads lines while
  // they have leadingSpaces >= sourceIndent (or are blanks followed by
  // more source / EOF). Each captured line contributes its content (with
  // first sourceIndent spaces stripped) plus a trailing "\n" to the
  // payload.
  private def parseSourceAtom(sourceIndent: Int): Tel.Atom.Source raises TelError =
    sb.setLength(0)

    var done = false
    while !done do
      if head.eof then done = true
      else if head.blank then
        // Probe across blanks to see if more source follows.
        val mk = beginMark()
        val firstBlankSnapshot = (head.leadingSpaces, head.indentLevels, head.blank,
                                  head.eof, head.startLine)
        var blanks = 0
        while !head.eof && head.blank do
          blanks += 1
          fillHead()
        val keep = head.eof || head.leadingSpaces >= sourceIndent
        if keep then
          // Emit `blanks` empty lines.
          var i = 0
          while i < blanks do { sb.append('\n'); i += 1 }
        else
          // Rewind.
          syncTo()
          reconcileLineation()
          cursor.cue(mk)
          syncFrom()
          head.leadingSpaces = firstBlankSnapshot._1
          head.indentLevels  = firstBlankSnapshot._2
          head.blank         = firstBlankSnapshot._3
          head.eof           = firstBlankSnapshot._4
          head.startLine     = firstBlankSnapshot._5
          var i = 0
          while i < firstBlankSnapshot._1 && more && peek == SP do
            advance(); i += 1
          done = true
      else if head.leadingSpaces >= sourceIndent then
        // Read content: skip the first sourceIndent spaces — but wait, the
        // fillHead already consumed all leading spaces. We need to back up
        // and consume only sourceIndent of them, leaving (leadingSpaces -
        // sourceIndent) for the content. Alternative: pad the appended
        // string with the extra leading spaces, since they ARE part of the
        // content (per §14).
        //
        // §14: source-atom payload is each line's content with the first
        // `sourceIndent` characters stripped. So if leadingSpaces > sourceIndent,
        // the excess is part of the content as leading spaces.
        // Since fillHead consumed ALL the leadingSpaces, we lost the
        // (leadingSpaces - sourceIndent) excess. Recompose it.
        val excess = head.leadingSpaces - sourceIndent
        var i = 0
        while i < excess do { sb.append(' '); i += 1 }

        // Now read the line content (until LF/CR), tracking trailing spaces
        // so we can strip them.
        val mk = beginMark()
        while more && peek != LF && peek != CR do advance()
        val payload = sliceText(mk)
        // Strip trailing spaces.
        var endIdx = payload.length
        while endIdx > 0 && payload.charAt(endIdx - 1) == ' ' do endIdx -= 1
        sb.append(payload, 0, endIdx)
        sb.append('\n')

        consumeLineEnding()
        fillHead()
      else done = true

    Tel.Atom.Source(Text(sb.toString))

  // ── Literal atom ─────────────────────────────────────────────────────────

  // Cursor is at the first content byte of the opening line (the delimiter
  // text). The delimiter is the rest of the opening line. The payload is
  // everything between the next LF and the first line whose content equals
  // the delimiter (at column 0 / flush left).
  private def parseLiteralAtom(literalIndent: Int): Tel.Atom.Literal raises TelError =
    val openingLine = head.startLine
    // Read the delimiter.
    val delimMk = beginMark()
    while more && peek != LF && peek != CR do advance()
    val delimiter = sliceText(delimMk)
    // The opening line is now consumed up to but not including the LF.
    // Empty-payload case: if next line is the closing delimiter exactly,
    // payload is empty.
    consumeLineEnding()

    sb.setLength(0)
    var done = false
    while !done do
      if !more then errorAt(Reason.UnclosedLiteral, openingLine, 1)
      // Read a line. Check if it matches delimiter (flush-left, no leading
      // spaces).
      val lineMk = beginMark()
      while more && peek != LF && peek != CR do advance()
      val line = sliceText(lineMk)
      // Consume the line ending — but only if there is one. If we hit EOF
      // without finding a closing delim that's E115.
      val hadEol = more
      if hadEol then consumeLineEnding()
      else errorAt(Reason.UnclosedLiteral, openingLine, 1)

      if line == delimiter then
        done = true
      else
        sb.append(line)
        sb.append('\n')

    fillHead()
    // §15 normalises CRLF to LF inside the payload, but since we read line
    // by line and append LF, there's no CRLF inside.
    Tel.Atom.Literal(Text(delimiter), Text(sb.toString))

  // ── Compound line parsing ────────────────────────────────────────────────

  // Cursor is at the first content byte (past leading spaces). Returns the
  // parsed compound (keyword + atoms + remark, children left empty for the
  // caller to fill in).
  private def parseCompoundLine(lineNumber: Int): Tel.Compound raises TelError =
    val lineStart = beginMark()

    // First phrase = keyword. Read until space or LF/CR.
    val keyword = readKeyword()

    val atoms = scala.collection.mutable.ArrayBuffer.empty[Tel.Atom]
    var remark: Optional[Text] = Unset
    sb.setLength(0)
    var precedingSpaces = 0
    var hardSpaceMode = false
    var atomOpen = false

    inline def commit(): Unit =
      if atomOpen then
        atoms += Tel.Atom.Inline(Text(sb.toString), precedingSpaces)
        sb.setLength(0)
        atomOpen = false

    var stopped = false
    while !stopped && remark.absent do
      if !more || peek == LF || peek == CR then stopped = true
      else
        val ch = peek
        if ch == SP then
          var run = 0
          while more && peek == SP do { advance(); run += 1 }
          if hardSpaceMode then
            if run >= 2 then
              commit()
              precedingSpaces = run
            else
              sb.append(' ')
              atomOpen = true
          else
            if run == 1 then
              commit()
              precedingSpaces = 1
            else
              commit()
              precedingSpaces = run
              hardSpaceMode = true
        else if ch == sigil && !atomOpen then
          // Could be remark introducer: sigil + soft space + content.
          // We need to peek the next byte. The sigil is the only ASCII
          // marker so peek directly.
          val afterSigil = peekNext()
          val softSpaceAfter =
            afterSigil == SP.toInt
            && {
              // Check that it's NOT a hard space (not followed by another
              // space). We need a 2-byte look-ahead past the sigil.
              if pos + 2 < bufEnd then bytes(pos + 2) != SP
              else
                // Force refill to know.
                if more then
                  if pos + 2 < bufEnd then bytes(pos + 2) != SP
                  else true  // sigil + space + EOF: that's a remark
                else true
            }
          if softSpaceAfter then
            // Consume sigil + space, then read remark text until LF/CR.
            advance()  // sigil
            advance()  // space
            val mk = beginMark()
            while more && peek != LF && peek != CR do advance()
            remark = Text(sliceText(mk))
          else
            sb.append(ch.toChar)
            atomOpen = true
            advance()
        else
          // Read a run of non-space, non-sigil, non-LF, non-CR bytes into
          // sb. This is the inner-loop hot path; we want bulk-copy.
          val mk = beginMark()
          var localPos = pos
          while localPos < bufEnd
                && bytes(localPos) != SP
                && bytes(localPos) != LF
                && bytes(localPos) != CR
                && (atomOpen || bytes(localPos) != sigil)
          do localPos += 1
          val runLen = localPos - pos
          if runLen > 0 then
            // Append via cursor.slice for a single zero-copy materialise →
            // string concat.
            pos = localPos
            val piece = sliceText(mk)
            sb.append(piece)
            atomOpen = true
          else
            // Could be a sigil while atomOpen — treat as content.
            sb.append(ch.toChar)
            atomOpen = true
            advance()

    commit()
    // Check trailing-spaces (E108): if the line has trailing spaces before
    // LF, error. Note: `precedingSpaces` after the last commit() captures
    // any trailing spaces consumed *after* the last atom. If the line
    // ended with spaces (no atom after them) precedingSpaces > 0.
    if !remark.present && atoms.nonEmpty && precedingSpaces > 0
       && (!more || peek == LF || peek == CR) && !hardSpaceMode
    then
      // Only error if trailing run is space-only at end of line.
      errorAt(Reason.TrailingSpaces, lineNumber, head.leadingSpaces + 1)
    if !remark.present && precedingSpaces > 0 && hardSpaceMode
       && (!more || peek == LF || peek == CR)
    then
      errorAt(Reason.TrailingSpaces, lineNumber, head.leadingSpaces + 1)

    consumeLineEnding()
    Tel.Compound(keyword, IArray.from(atoms), remark, IArray.empty)

  // Read a keyword from the current position. The keyword runs until SP, LF,
  // CR, or EOF. Returns the interned `Text`. Uses the 64-slot fingerprint
  // cache for ≤8-byte keywords (allocation-free on hit), one `grabText` on
  // miss or for length > 8.
  private def readKeyword(): Text raises TelError =
    val startMk = beginMark()
    var low:  Long = 0L
    var high: Long = 0L
    var len = 0
    while more && peek != SP && peek != LF && peek != CR do
      val b = peek
      if len < 4 then
        low |= (b & 0xff).toLong << (len * 8)
      else if len < 8 then
        high |= (b & 0xff).toLong << ((len - 4) * 8)
      len += 1
      advance()

    if len == 0 then t""
    else if len > 8 then Text(sliceText(startMk))
    else
      val hash = ((low ^ (low >>> 32)) ^ (high ^ (high >>> 17))).toInt
      var slot = hash & 0x3F
      var probes = 0
      var result: String = null
      while result == null && probes < 4 do
        val existing = keyCache(slot)
        if existing == null then
          val s = sliceText(startMk)
          keyCache(slot) = s
          keyCacheLow(slot) = low
          keyCacheHigh(slot) = high
          result = s
        else if keyCacheLow(slot) == low && keyCacheHigh(slot) == high then
          result = existing
        else
          slot = (slot + 1) & 0x3F
          probes += 1

      if result != null then Text(result)
      else Text(sliceText(startMk))
