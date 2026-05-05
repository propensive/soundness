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
package ypsiloid

import scala.annotation.*
import scala.collection.mutable.ArrayBuffer

import anticipation.*
import contingency.*
import denominative.*
import gossamer.*
import rudiments.*
import vacuous.*
import zephyrine.*

import YamlAst.Byte.*
import YamlError.Reason

object YamlParser:
  private val pool: ThreadLocal[YamlParser] =
    ThreadLocal.withInitial(() => new YamlParser).nn

  def parse(input: Text)(using Tactic[YamlError]): YamlAst =
    val parser = pool.get.nn
    parser.resetText(input)
    parser.parse()

  def parse(input: Data)(using Tactic[YamlError]): YamlAst =
    val parser = pool.get.nn
    parser.resetData(input)
    parser.parse()

  def parseAll(input: Text)(using Tactic[YamlError]): List[YamlAst] =
    val parser = pool.get.nn
    parser.resetText(input)
    parser.parseAll()

  def parseAll(input: Data)(using Tactic[YamlError]): List[YamlAst] =
    val parser = pool.get.nn
    parser.resetData(input)
    parser.parseAll()

private[ypsiloid] final class YamlParser:
  import Lineation.untrackedData

  // Parser-local snapshot of the cursor's buffer, mirroring Jacinta's
  // pattern: keep `bytes`/`pos`/`bufEnd` as plain fields so the JIT can
  // hold them in registers across hot byte loops. Sync to the cursor
  // before mark/slice/refill operations and refresh after.
  private var cursor:    Cursor[Data]      = null.asInstanceOf[Cursor[Data]]
  private var heldToken: Cursor.Held | Null = null
  private var bytes:     Array[Byte]       = null.asInstanceOf[Array[Byte]]
  private var pos:       Int               = 0
  private var bufEnd:    Int               = 0

  // Anchor table — names map to fully-parsed YamlAst values.
  private val anchors = scala.collection.mutable.Map.empty[String, YamlAst]

  // Resizable char buffer shared across string-building calls (for
  // quoted-string unescape and UTF-8 decoded plain scalars). Mirrors
  // Jacinta's `chars`/`stringCursor` to avoid per-string allocation.
  private var arraySize: Int        = 64
  private var chars:     Array[Char] = new Array(arraySize)
  private var stringCursor: Int     = 0

  // Pool of buffer instances for nested sequences/mappings so we can
  // collect items without allocating an `ArrayBuffer` per recursion.
  private var bufferId: Int = -1
  private val bufferPool: ArrayBuffer[ArrayBuffer[Any]] = ArrayBuffer.empty

  // Out-parameters for `consumeNodePrefixes` and `readPlainScalarText` —
  // overwritten on each call and consumed immediately by the caller, so
  // no Tuple2/Tuple3 allocation per node.
  private var prefixAnchor:   Text    = t""
  private var prefixTag:      Text    = t""
  private var prefixHeadByte: Int     = -1
  private var sawMappingColon: Boolean = false

  // Indent of the innermost enclosing block collection (block sequence
  // or block mapping). Used by parseBlockScalar to compute the absolute
  // content indent for an explicit indentation indicator: per spec
  // 8.1.1.1, the indicator is added to the parent node's indent. Set
  // and restored on entry/exit of parseBlockSequence and
  // parseBlockMappingFromFirstKey. -1 means the parent is the document.
  private var blockParentIndent: Int = -1

  def resetText(input: Text): Unit =
    val data: Data = input.s.getBytes("UTF-8").nn.immutable(using Unsafe)
    cursor = Cursor[Data](data)
    syncFrom()
    stringCursor = 0
    bufferId = -1
    heldToken = null
    blockParentIndent = -1
    sawMappingColon = false
    prefixAnchor = t""
    prefixTag = t""
    prefixHeadByte = -1
    anchors.clear()
    anchors.clear()

  def resetData(input: Data): Unit =
    cursor = Cursor[Data](input)
    syncFrom()
    stringCursor = 0
    bufferId = -1
    heldToken = null
    blockParentIndent = -1
    sawMappingColon = false
    prefixAnchor = t""
    prefixTag = t""
    prefixHeadByte = -1
    anchors.clear()

  // ── Substrate ────────────────────────────────────────────────────────────

  private inline def syncTo(): Unit =
    cursor.unsafeAdvanceBy(pos - cursor.unsafePos(using Unsafe))(using Unsafe)

  private inline def syncFrom(): Unit =
    bytes  = cursor.unsafeBuffer(using Unsafe).asInstanceOf[Array[Byte]]
    pos    = cursor.unsafePos(using Unsafe)
    bufEnd = cursor.unsafeWriteEnd(using Unsafe)

  private inline def more: Boolean = pos < bufEnd || moreSlow()

  private def moreSlow(): Boolean =
    syncTo()
    if cursor.more then { syncFrom(); true } else false

  private inline def peek: Byte = bytes(pos)

  private inline def advance(): Unit = pos += 1

  // ── Errors ──────────────────────────────────────────────────────────────

  private def fail(message: Text)(using Tactic[YamlError]): Nothing =
    syncTo()
    val line = cursor.line.n0
    val column = cursor.column.n0
    abort(YamlError(Reason.ParseFailure(message, line, column)))

  // ── Position / mark plumbing ────────────────────────────────────────────

  private inline def begin(): Cursor.Mark =
    syncTo()
    cursor.mark(using heldToken.nn)

  private inline def slice(start: Cursor.Mark): String =
    syncTo()
    val end = cursor.mark(using heldToken.nn)
    cursor.slice(start, end): (storage, off, len) =>
      val arr = storage.asInstanceOf[Array[Byte]]
      new String(arr, off, len, java.nio.charset.StandardCharsets.UTF_8)

  private inline def holding[result](inline action: => result): result =
    syncTo()
    cursor.hold:
      heldToken = summon[Cursor.Held]
      try action finally heldToken = null

  // ── String buffer (per-instance, reused) ────────────────────────────────

  private inline def resetString(): Unit = stringCursor = 0

  // Single-char fast path: one bounds check, doubling on growth. Matches
  // Jacinta's `appendChar` shape so the JIT can keep `stringCursor` and
  // the buffer reference in registers across a hot loop.
  private inline def appendChar(char: Char): Unit =
    if stringCursor == arraySize then
      arraySize *= 2
      val newArr = new Array[Char](arraySize)
      System.arraycopy(chars, 0, newArr, 0, stringCursor)
      chars = newArr
    chars(stringCursor) = char
    stringCursor += 1

  // Multi-char append (used for variable-length escapes / surrogate pairs).
  private inline def ensureSpace(n: Int): Unit =
    while stringCursor + n > arraySize do arraySize *= 2
    if chars.length < arraySize then
      val newArr = new Array[Char](arraySize)
      System.arraycopy(chars, 0, newArr, 0, stringCursor)
      chars = newArr

  private inline def getStringText(): Text = String(chars, 0, stringCursor).tt

  // ── Buffer pool for sequences/mappings ──────────────────────────────────

  private inline def acquireBuffer(): ArrayBuffer[Any] =
    bufferId += 1
    if bufferPool.length <= bufferId then
      val b = ArrayBuffer.empty[Any]
      bufferPool += b
      b
    else
      val b = bufferPool(bufferId)
      b.clear()
      b

  private inline def releaseBuffer(): Unit = bufferId -= 1

  // ── Top-level parse ─────────────────────────────────────────────────────

  def parse()(using Tactic[YamlError]): YamlAst = holding:
    skipBom()
    skipBlankAndCommentLines()
    val explicitStart = consumeOptionalDocumentStart()
    if !explicitStart || (more && peek == Newline) then
      if more && peek == Newline then advance()
      skipBlankAndCommentLines()
    if !more || atDocumentBoundary then YamlAst.Null
    else
      val indent = consumeLeadingSpaces()
      if !more || atDocumentBoundary then YamlAst.Null
      else
        val node = parseNode(indent)
        skipBlankAndCommentLines()
        consumeOptionalDocumentEnd()
        node

  def parseAll()(using Tactic[YamlError]): List[YamlAst] = holding:
    val docs = scala.collection.mutable.ArrayBuffer[YamlAst]()
    skipBom()

    var continue = true
    while continue do
      skipBlankAndCommentLines()
      val explicitStart = consumeOptionalDocumentStart()
      // After a `---` marker we may be on the same line as the body;
      // otherwise the body is on a fresh line whose leading whitespace
      // determines the indent.
      if !explicitStart || (more && peek == Newline) then
        if more && peek == Newline then advance()
        skipBlankAndCommentLines()

      if !more then
        if explicitStart then docs.append(YamlAst.Null)
        continue = false
      else if atDocumentBoundary then
        if explicitStart then docs.append(YamlAst.Null)
        consumeOptionalDocumentEnd()
      else
        // The first content line of the document determines the indent
        // passed to parseNode.
        val indent = consumeLeadingSpaces()
        if !more || atDocumentBoundary then
          if explicitStart then docs.append(YamlAst.Null)
          consumeOptionalDocumentEnd()
        else
          val node = parseNode(indent)
          docs.append(node)
          skipBlankAndCommentLines()
          consumeOptionalDocumentEnd()

    docs.toList

  // Consume `---` if at the current position. Returns true if consumed.
  // Per spec the marker requires either a following line-boundary
  // character or end-of-input — `----` is a plain scalar starting with
  // four dashes, not a marker. After consumption we eat any inline
  // whitespace immediately following but leave the rest of the line
  // (including any same-line node body) to the caller.
  private def consumeOptionalDocumentStart(): Boolean =
    if isDocumentMarker(Minus) then
      pos += 3
      while more && (peek == Space || peek == Tab) do advance()
      true
    else false

  // Consume `...` if at the current position; eats trailing whitespace
  // and any same-line comment.
  private def consumeOptionalDocumentEnd(): Boolean =
    if isDocumentMarker(Period) then
      pos += 3
      skipUntilNewline()
      if more then advance()
      true
    else false

  // True if the cursor is positioned at a document marker (three of
  // the same byte followed by a line-boundary or end-of-input). Does
  // not consume.
  private inline def isDocumentMarker(b: Byte): Boolean =
    pos + 2 < bufEnd && bytes(pos) == b && bytes(pos+1) == b && bytes(pos+2) == b
    && (pos + 3 >= bufEnd || {
          val nb = bytes(pos+3)
          nb == Newline || nb == Space || nb == Tab || nb == Return
        })

  private inline def atDocumentBoundary: Boolean =
    isDocumentMarker(Minus) || isDocumentMarker(Period)

  // Skip an optional UTF-8 BOM (EF BB BF) at the start of input.
  private def skipBom(): Unit =
    syncTo()
    cursor.hold:
      val mk = cursor.mark
      val isBom =
        cursor.more && cursor.datum(using Unsafe) == -17.toByte
        && { cursor.next(); cursor.more && cursor.datum(using Unsafe) == -69.toByte }
        && { cursor.next(); cursor.more && cursor.datum(using Unsafe) == -65.toByte }

      if isBom then cursor.next() else cursor.cue(mk)
    syncFrom()

  // ── Whitespace / comment / directive skipping ───────────────────────────

  // Skip horizontal whitespace (spaces, tabs) but not newlines.
  private inline def skipSpaces(): Unit =
    while more && (peek == Space || peek == Tab) do advance()

  // Skip horizontal whitespace, newlines, and `# comment` lines. Also
  // consumes any `%YAML`/`%TAG` directive lines (we don't honour them
  // semantically yet, just step over them).
  private def skipWhitespaceAndCommentsAndDirectives(): Unit =
    var continue = true
    while continue && more do
      val c = peek
      if c == Space || c == Tab || c == Newline || c == Return then advance()
      else if c == Hash then
        while more && peek != Newline do advance()
      else if c == '%'.toByte then
        while more && peek != Newline do advance()
      else continue = false

  // Try to consume three of `b` in a row. Returns true on success.
  private def tryConsume3(b: Byte): Boolean =
    if pos + 2 < bufEnd && bytes(pos) == b && bytes(pos + 1) == b && bytes(pos + 2) == b then
      pos += 3
      true
    else false

  private inline def skipUntilNewline(): Unit =
    while more && peek != Newline do advance()

  // ── Node parsing ────────────────────────────────────────────────────────

  // Parse a single YAML node at the given context indent. Caller has
  // ensured we're positioned at the first byte of the node (after any
  // leading whitespace).
  private def parseNode(indent: Int)(using Tactic[YamlError]): YamlAst =
    skipSpaces()
    if !more then YamlAst.Null
    else parseNodeHere(indent)

  private def parseNodeHere(indent: Int)(using Tactic[YamlError]): YamlAst =
    consumeNodePrefixes()
    val anchorName = prefixAnchor
    val tagText    = prefixTag
    val headByte   = prefixHeadByte

    // Bare anchor/tag followed by newline → value is on the next indented
    // line(s). consumeNodePrefixes stops at the newline so we can detect
    // and descend into it here.
    val hasPrefix = !anchorName.nil || !tagText.nil
    val value: YamlAst =
      if hasPrefix && (headByte == Newline || headByte == -1) then
        if more && peek == Newline then advance()
        skipBlankAndCommentLines()
        val childIndent = consumeLeadingSpaces()
        // The value of a bare anchor/tag is the next node whose indent
        // is strictly greater than the *parent collection*. Same indent
        // as the prefix itself is fine when the parent is the document
        // (top-level `&a\n- x` makes the sequence the anchor's value).
        if !more || childIndent <= blockParentIndent then YamlAst.Null
        else parseNodeHere(childIndent)
      else if headByte == -1 then YamlAst.Null
      else if headByte == Star then
        // An anchor on an alias node is illegal per spec — aliases
        // refer to existing anchored nodes, they don't anchor anything
        // themselves.
        if !anchorName.nil then fail(t"anchor on alias node")
        advance()
        parseAlias()
      else
        (headByte: @switch) match
          case Quote        =>
            advance()
            val s = parseDoubleQuoted()
            maybeBlockMappingFromQuotedKey(s, indent)
          case Apostrophe   =>
            advance()
            val s = parseSingleQuoted()
            maybeBlockMappingFromQuotedKey(s, indent)
          case OpenBracket  => advance(); parseFlowSequence()
          case OpenBrace    => advance(); parseFlowMapping()
          case Pipe         => parseBlockScalar(literal = true, indent)
          case Greater      => parseBlockScalar(literal = false, indent)
          case Minus        => parseMinus(indent)
          case Question     => parseQuestion(indent)
          case CloseBracket | CloseBrace | Comma | 0x40 | 0x60 =>
            fail(t"reserved indicator at start of node")
          case _            => parsePlainOrBlockMapping(indent)

    val tagged = if tagText.nil then value else applyTag(tagText, value)
    if anchorName.nil then tagged
    else
      anchors.update(anchorName.s, tagged)
      tagged

  // Consume any `&anchor`, `!tag`, or both at the current position. Stops
  // at the first non-prefix byte (without crossing newlines, so the caller
  // can detect a bare-prefix-with-block). Writes results to `prefixAnchor`,
  // `prefixTag`, `prefixHeadByte` to avoid per-call Tuple3 allocation.
  private def consumeNodePrefixes()(using Tactic[YamlError]): Unit =
    var anchorName = t""
    var tagText = t""
    var done = false
    while !done do
      skipSpaces()
      if !more then done = true
      else peek match
        case Amp =>
          if !anchorName.nil then fail(t"duplicate anchor on a single node")
          advance()
          anchorName = readWord()
          skipSpaces()

        case Bang =>
          if !tagText.nil then fail(t"duplicate tag on a single node")
          val mk = begin()
          advance()
          if more && peek == Bang then advance()
          // Tag chars exclude flow-indicator bytes (the spec's URI
          // production explicitly forbids them inside tag handles
          // and shorthand tags).
          while more && !isWhitespaceOrEnd(peek)
                    && peek != Comma && peek != OpenBracket
                    && peek != CloseBracket && peek != OpenBrace
                    && peek != CloseBrace do
            advance()
          tagText = slice(mk).tt
          skipSpaces()

        case _ => done = true

    prefixAnchor = anchorName
    prefixTag = tagText
    prefixHeadByte = if !more then -1 else peek & 0xFF

  private inline def isWhitespaceOrEnd(b: Byte): Boolean =
    b == Space || b == Tab || b == Newline || b == Return

  // Read an identifier-like word (anchor or alias name).
  private def readWord(): Text =
    val mk = begin()
    while more && !isWhitespaceOrEnd(peek)
              && peek != OpenBracket && peek != CloseBracket
              && peek != OpenBrace && peek != CloseBrace
              && peek != Comma do
      advance()
    slice(mk).tt

  private def parseAlias()(using Tactic[YamlError]): YamlAst =
    val name = readWord()
    anchors.get(name.s) match
      case Some(value) => value
      case None        =>
        raise(YamlError(Reason.UnknownAlias(name))) yet YamlAst.Null

  // After a newline within a block context, advance past blank/comment
  // lines and through the leading indent of the next content line.
  private def skipBlanksAndIndent(): Unit =
    var done = false
    while !done && more do
      // skip leading spaces
      while more && peek == Space do advance()
      if !more then done = true
      else peek match
        case Newline => advance()
        case Hash    =>
          while more && peek != Newline do advance()
          if more then advance()
        case _ => done = true

  // ── Plain scalars / block mappings detection ────────────────────────────

  // Parse from the current position. Either a plain scalar (yielding a
  // primitive) or, if a top-level `:` follows, a block mapping where this
  // scalar is the first key.
  // After parsing a quoted scalar at node head, decide what follows:
  //  - `:` with a whitespace/EOL terminator → first key of a block
  //    mapping;
  //  - newline or EOF → the scalar is the value;
  //  - `# comment` after at least one space → trailing comment, OK;
  //  - flow-collection terminator (`,`, `]`, `}`) → caller is a flow
  //    context that will consume the terminator;
  // anything else is an error per spec (plain text after a quoted
  // scalar is not a valid construct in block context).
  private def maybeBlockMappingFromQuotedKey
                ( scalar: YamlAst, indent: Int )
                ( using Tactic[YamlError] )
  :   YamlAst =
    val hadSpaceOrTab = more && (peek == Space || peek == Tab)
    skipSpaces()
    if !more then scalar
    else peek match
      case Newline => scalar
      case Hash if hadSpaceOrTab =>
        while more && peek != Newline do advance()
        scalar
      case Colon =>
        val nextByte = if pos + 1 < bufEnd then bytes(pos + 1) else -1
        if nextByte == Space || nextByte == Tab || nextByte == Newline
                || nextByte == Return || nextByte == -1 then
          parseBlockMappingFromFirstKey(scalar, indent)
        else fail(t"trailing content after quoted scalar")
      case Comma | CloseBracket | CloseBrace => scalar
      case _ => fail(t"trailing content after quoted scalar")

  private def parsePlainOrBlockMapping(indent: Int)(using Tactic[YamlError])
  :   YamlAst =
    val textValue = readPlainScalarText(indent)
    if sawMappingColon then
      // We saw `key:` — caller's indent is the mapping's indent.
      parseBlockMappingFromFirstKey(resolvePlainScalar(textValue), indent)
    else
      resolvePlainScalar(textValue)

  // Read a plain scalar at the current position. Sets `sawMappingColon`
  // to true if a `: ` (or `:` at line-end) at the same line level was
  // seen so the caller knows this is the key of a block mapping.
  private def readPlainScalarText(indent: Int)
                              (using Tactic[YamlError])
  :   Text =

    resetString()

    var colon = false
    var done = false

    while !done do
      readPlainScalarLine() match
        case PlainOutcome.Mapping =>
          colon = true
          done = true

        case PlainOutcome.EndOfLine =>
          // Try to fold continuation
          if !attemptPlainContinuation(indent) then done = true

        case PlainOutcome.Stop =>
          done = true

    sawMappingColon = colon
    trimEndWhitespace(getStringText())

  private enum PlainOutcome:
    case Mapping
    case EndOfLine
    case Stop

  // Read one line of a plain scalar (until newline or terminator), pushing
  // characters into `chars`. Returns Mapping if a `:` (followed by space
  // or newline) was found at the top level on this line.
  private def readPlainScalarLine()(using Tactic[YamlError]): PlainOutcome =
    var lineStart = stringCursor

    while more do
      // Fast-prefix ASCII run: bulk-copy printable bytes (>= 0x20) that
      // are neither potential terminators (#, :) nor UTF-8 lead bytes
      // (which are negative as signed Bytes, so excluded by >= 0x20).
      val runStart = pos
      while pos < bufEnd && {
        val b = bytes(pos)
        b >= 0x20 && b != Hash && b != Colon
      } do pos += 1
      val runLen = pos - runStart
      if runLen > 0 then
        ensureSpace(runLen)
        var k = 0
        while k < runLen do
          chars(stringCursor + k) = (bytes(runStart + k) & 0xFF).toChar
          k += 1
        stringCursor += runLen

      if !more then return PlainOutcome.Stop

      val b = peek
      if b == Newline then return PlainOutcome.EndOfLine
      if b == Hash && stringCursor > lineStart && chars(stringCursor - 1) == ' ' then
        // ` # comment` ends the plain scalar. Per spec, a comment also
        // terminates any multi-line continuation: subsequent lines do
        // not fold into the scalar even if they would otherwise be
        // indented enough.
        while more && peek != Newline do advance()
        var i = stringCursor - 1
        while i >= lineStart && chars(i) == ' ' do
          stringCursor -= 1
          i -= 1
        return PlainOutcome.Stop

      if b == Colon then
        // Mapping-key colon iff the byte after the colon is whitespace
        // or end-of-input. The whole input is loaded at parser reset
        // (`Cursor[Data]` over a single `Data` buffer), so a direct
        // bounds check on the snapshot suffices — no refill needed.
        val nextByte = if pos + 1 < bufEnd then bytes(pos + 1) else -1
        if nextByte == Space || nextByte == Tab || nextByte == Newline
                || nextByte == Return || nextByte == -1 then
          return PlainOutcome.Mapping
        // else: `:foo` is part of the scalar; fall through.

      // Slow-path single-byte handling (control chars, UTF-8 lead bytes,
      // or `:` not followed by whitespace).
      appendByteAsChar(b)
      advance()

    PlainOutcome.Stop

  // Attempt to fold a continuation line into a multi-line plain scalar.
  // Returns true if a continuation was consumed. Per spec the next
  // content line must be indented strictly more than the *parent
  // collection*; for top-level plain scalars the parent is the document
  // (indent -1), so any indent (including 0) is accepted.
  private def attemptPlainContinuation(scalarIndent: Int): Boolean =
    if !more || peek != Newline then return false
    val savedPos = pos
    val savedString = stringCursor
    val parent = blockParentIndent

    advance()
    var newlineCount = 1

    @tailrec def findContent(): Boolean =
      val lineStart = pos
      var spaces = 0
      while more && peek == Space do
        spaces += 1
        advance()
      // Tabs after the leading spaces are part of the line's leading
      // whitespace but don't count toward indent (spec forbids tabs in
      // indentation). They're stripped from the folded content.
      while more && peek == Tab do advance()

      if !more then
        pos = savedPos; stringCursor = savedString; false
      else if peek == Newline then
        newlineCount += 1
        advance()
        findContent()
      else if peek == Hash then
        // A comment line terminates plain-scalar continuation: the
        // scalar cannot fold across the comment into a later line.
        pos = savedPos
        stringCursor = savedString
        false
      else if spaces > parent && !atDocumentBoundary then
        // Trim trailing whitespace from the previous line we emitted
        // before adding the fold separator: in plain scalars, both
        // leading and trailing per-line whitespace is stripped before
        // folding.
        while stringCursor > 0
                && (chars(stringCursor - 1) == ' '
                    || chars(stringCursor - 1) == '\t') do
          stringCursor -= 1
        if newlineCount == 1 then appendChar(' ')
        else
          var k = 1
          while k < newlineCount do
            appendChar('\n')
            k += 1
        true
      else
        pos = lineStart
        stringCursor = savedString
        // Restore newline (we consumed it but didn't fold)
        pos = savedPos
        false

    findContent()

  private inline def appendByteAsChar(b: Byte): Unit =
    if (b & 0x80) == 0 then appendChar((b & 0xFF).toChar)
    else
      // Multi-byte UTF-8: decode in place.
      decodeUtf8AndAppend(b)

  // Decode a UTF-8 sequence whose lead byte is at the current cursor
  // position and append the resulting char(s).
  private def decodeUtf8AndAppend(lead: Byte): Unit =
    val u = lead & 0xFF
    if (u & 0xE0) == 0xC0 then
      val b2 = if pos + 1 < bufEnd then bytes(pos + 1) & 0x3F else 0
      val cp = ((u & 0x1F) << 6) | b2
      // advance lead handled by caller
      pos += 1
      appendChar(cp.toChar)
    else if (u & 0xF0) == 0xE0 then
      val b2 = if pos + 1 < bufEnd then bytes(pos + 1) & 0x3F else 0
      val b3 = if pos + 2 < bufEnd then bytes(pos + 2) & 0x3F else 0
      val cp = ((u & 0x0F) << 12) | (b2 << 6) | b3
      pos += 2
      appendChar(cp.toChar)
    else if (u & 0xF8) == 0xF0 then
      val b2 = if pos + 1 < bufEnd then bytes(pos + 1) & 0x3F else 0
      val b3 = if pos + 2 < bufEnd then bytes(pos + 2) & 0x3F else 0
      val b4 = if pos + 3 < bufEnd then bytes(pos + 3) & 0x3F else 0
      val cp = ((u & 0x07) << 18) | (b2 << 12) | (b3 << 6) | b4
      pos += 3
      // Encode as surrogate pair if non-BMP
      if cp >= 0x10000 then
        val adjusted = cp - 0x10000
        appendChar((0xD800 | (adjusted >>> 10)).toChar)
        appendChar((0xDC00 | (adjusted & 0x3FF)).toChar)
      else
        appendChar(cp.toChar)
    else
      // Stray continuation byte or invalid — replace with placeholder.
      appendChar('?')

  // Trim trailing space/tab from the produced text.
  private def trimEndWhitespace(text: Text): Text =
    val s = text.s
    var n = s.length
    while n > 0 && (s.charAt(n - 1) == ' ' || s.charAt(n - 1) == '\t') do n -= 1
    if n == s.length then text else s.substring(0, n).nn.tt

  // Resolve a plain-scalar string into a YamlAst primitive.
  private def resolvePlainScalar(text: Text): YamlAst =
    val s = text.s
    if s.isEmpty then YamlAst.Null
    else s match
      case "null" | "Null" | "NULL" | "~"   => YamlAst.Null
      case "true" | "True" | "TRUE"         => YamlAst.Bool(true)
      case "false" | "False" | "FALSE"      => YamlAst.Bool(false)
      case ".inf" | ".Inf" | ".INF"
        | "+.inf" | "+.Inf" | "+.INF"       => YamlAst.Decimal(Double.PositiveInfinity)
      case "-.inf" | "-.Inf" | "-.INF"      => YamlAst.Decimal(Double.NegativeInfinity)
      case ".nan" | ".NaN" | ".NAN"         => YamlAst.Decimal(Double.NaN)

      case _ =>
        val asInt = parsePlainIntegerOrNull(s)
        if asInt != null then asInt
        else
          val asDec = parsePlainDecimalOrNull(s)
          if asDec != null then asDec else YamlAst.Str(text)

  // Parse a plain string into a Long without throwing on rejection.
  // Returns null when the string does not represent a YAML 1.2 integer
  // we want to recognise (avoids `Option`/exception overhead on the
  // common reject path during plain-scalar resolution).
  private def parsePlainIntegerOrNull(s: String): YamlAst | Null =
    val len = s.length
    if len == 0 then return null
    var i = 0
    var negative = false
    s.charAt(0) match
      case '-' => negative = true; i = 1
      case '+' => i = 1
      case _   => ()

    if i >= len then return null

    // Hex / octal prefixes (after optional sign).
    if i + 1 < len && s.charAt(i) == '0' then
      val p = s.charAt(i + 1)
      if p == 'x' || p == 'X' then
        return parseRadix(s, i + 2, 16, negative)
      if p == 'o' || p == 'O' then
        return parseRadix(s, i + 2, 8, negative)

    // Decimal: pure digits only (no leading zeroes except "0").
    val first = s.charAt(i)
    if first < '0' || first > '9' then return null
    if first == '0' && i + 1 < len then return null  // "01" is not a YAML int

    // Up to 18 digits: cannot overflow Long; manual loop. 19 digits:
    // boundary case, defer to JDK parser. >19 digits: definitely not
    // a Long.
    val digitCount = len - i
    if digitCount > 19 then return null
    if digitCount == 19 then
      try
        val v = java.lang.Long.parseLong(s)
        return YamlAst.Integer(v)
      catch case _: NumberFormatException => return null

    var acc: Long = 0L
    while i < len do
      val c = s.charAt(i)
      if c < '0' || c > '9' then return null
      acc = acc*10L + (c - '0')
      i += 1

    YamlAst.Integer(if negative then -acc else acc)

  private def parseRadix(s: String, start: Int, radix: Int, negative: Boolean)
  :   YamlAst | Null =
    val len = s.length
    if start >= len then return null
    var acc: Long = 0L
    var i = start
    while i < len do
      val c = s.charAt(i)
      val d =
        if c >= '0' && c <= '9' then c - '0'
        else if c >= 'a' && c <= 'f' then c - 'a' + 10
        else if c >= 'A' && c <= 'F' then c - 'A' + 10
        else -1
      if d < 0 || d >= radix then return null
      acc = acc*radix + d
      i += 1
    YamlAst.Integer(if negative then -acc else acc)

  // Pre-filter to avoid `Double.parseDouble` throwing on non-numeric
  // strings: must contain at least one digit, only `[-+0-9.eE]`, ≤1
  // `.`, ≤1 `e`/`E`, and the `e`/`E` (if present) must be followed by
  // an optional sign and at least one digit.
  private def parsePlainDecimalOrNull(s: String): YamlAst | Null =
    val len = s.length
    if len == 0 then return null

    var i = 0
    s.charAt(0) match
      case '-' | '+' => i = 1
      case _         => ()

    var hasDigit = false
    var dotSeen = false
    var expSeen = false
    var expHasDigit = false

    while i < len do
      val c = s.charAt(i)
      if c >= '0' && c <= '9' then
        if expSeen then expHasDigit = true else hasDigit = true
      else if c == '.' then
        if dotSeen || expSeen then return null
        dotSeen = true
      else if c == 'e' || c == 'E' then
        if expSeen || !hasDigit then return null
        expSeen = true
        if i + 1 < len then
          val nx = s.charAt(i + 1)
          if nx == '+' || nx == '-' then i += 1
      else return null
      i += 1

    if !hasDigit then return null
    if expSeen && !expHasDigit then return null

    try YamlAst.Decimal(java.lang.Double.parseDouble(s))
    catch case _: NumberFormatException => null

  // ── Quoted strings ──────────────────────────────────────────────────────

  private def parseDoubleQuoted()(using Tactic[YamlError]): YamlAst =
    resetString()
    var done = false
    while !done do
      // Fast-prefix ASCII run: copy non-special printable bytes in
      // bulk. Quote/Backslash/Newline/UTF-8-lead-bytes exit.
      val runStart = pos
      while pos < bufEnd && {
        val b = bytes(pos)
        b >= 0x20 && b != Quote && b != Backslash
      } do pos += 1
      val runLen = pos - runStart
      if runLen > 0 then
        ensureSpace(runLen)
        var k = 0
        while k < runLen do
          chars(stringCursor + k) = (bytes(runStart + k) & 0xFF).toChar
          k += 1
        stringCursor += runLen

      if !more then fail(t"unterminated double-quoted string")
      val b = peek
      if b == Quote then
        advance()
        done = true
      else if b == Backslash then
        advance()
        if !more then fail(t"unterminated escape")
        consumeDoubleQuotedEscape()
      else if b == Newline then
        consumeMultilineFold()
      else
        appendByteAsChar(b)
        advance()
    YamlAst.Str(getStringText())

  private def consumeDoubleQuotedEscape()(using Tactic[YamlError]): Unit =
    val b = peek
    advance()
    (b: @switch) match
      case Backslash  => appendChar('\\')
      case Quote      => appendChar('"')
      case Slash      => appendChar('/')
      case Space      => appendChar(' ')
      case Num0       => appendChar(0x00.toChar)
      case LowerA     => appendChar(0x07.toChar)
      case LowerB     => appendChar('\b')
      case LowerE     => appendChar(0x1b.toChar)
      case LowerF     => appendChar('\f')
      case LowerN     => appendChar('\n')
      case LowerR     => appendChar('\r')
      case LowerT     => appendChar('\t')
      case LowerV     => appendChar(0x0b.toChar)
      case Tab        => appendChar('\t')
      case Newline    =>
        // \<newline> = explicit line-break suppression. The newline
        // itself is consumed (advance was done above) and any leading
        // whitespace on the continuation line is stripped, so a literal
        // space on the next line must be reintroduced via `\<space>`.
        while more && (peek == Space || peek == Tab) do advance()

      case LowerX =>
        val n = readHex(2)
        appendChar(n.toChar)

      case LowerU =>
        val n = readHex(4)
        appendChar(n.toChar)

      case 0x55 /* 'U' */ =>
        val n = readHex(8)
        if n >= 0x10000 then
          val a = n - 0x10000
          appendChar((0xD800 | (a >>> 10)).toChar)
          appendChar((0xDC00 | (a & 0x3FF)).toChar)
        else appendChar(n.toChar)

      case _ =>
        fail(t"invalid escape sequence")

  private def readHex(count: Int)(using Tactic[YamlError]): Int =
    var acc = 0
    var i = 0
    while i < count do
      if !more then fail(t"truncated hex escape")
      val b = peek
      val digit =
        if b >= Num0 && b <= Num9 then b - Num0
        else if b >= LowerA && b <= LowerF then b - LowerA + 10
        else if b >= 0x41 && b <= UpperF then b - 0x41 + 10
        else fail(t"invalid hex digit")
      acc = (acc << 4) | digit
      advance()
      i += 1
    acc

  private def parseSingleQuoted()(using Tactic[YamlError]): YamlAst =
    resetString()
    var done = false
    while !done do
      // Fast-prefix ASCII run: copy non-special printable bytes in
      // bulk. Apostrophe/Newline/UTF-8-lead-bytes exit.
      val runStart = pos
      while pos < bufEnd && {
        val b = bytes(pos)
        b >= 0x20 && b != Apostrophe
      } do pos += 1
      val runLen = pos - runStart
      if runLen > 0 then
        ensureSpace(runLen)
        var k = 0
        while k < runLen do
          chars(stringCursor + k) = (bytes(runStart + k) & 0xFF).toChar
          k += 1
        stringCursor += runLen

      if !more then fail(t"unterminated single-quoted string")
      val b = peek
      if b == Apostrophe then
        advance()
        if more && peek == Apostrophe then
          appendChar('\'')
          advance()
        else
          done = true
      else if b == Newline then
        consumeMultilineFold()
      else
        appendByteAsChar(b)
        advance()
    YamlAst.Str(getStringText())

  // Inside a quoted string: `\n` followed by zero or more whitespace.
  // Apply YAML line folding: 1 newline → space, N newlines → (N-1)
  // literal newlines. Trailing whitespace on the current line and
  // leading whitespace on the continuation line are both stripped
  // (per spec 6.5).
  private def consumeMultilineFold(): Unit =
    while stringCursor > 0
            && (chars(stringCursor - 1) == ' ' || chars(stringCursor - 1) == '\t') do
      stringCursor -= 1
    var newlineCount = 0
    while more && (peek == Newline || peek == Space || peek == Tab || peek == Return) do
      if peek == Newline then newlineCount += 1
      advance()
    if newlineCount == 1 then appendChar(' ')
    else
      var k = 1
      while k < newlineCount do
        appendChar('\n')
        k += 1

  // ── Flow types ──────────────────────────────────────────────────────────

  private def parseFlowSequence()(using Tactic[YamlError]): YamlAst =
    val buf = acquireBuffer()
    var done = false
    while !done do
      skipFlowWhitespace()
      if !more then fail(t"unterminated flow sequence")
      if peek == CloseBracket then
        advance()
        done = true
      else if peek == Comma then
        fail(t"empty flow-sequence entry")
      else
        val first = parseFlowNode()
        skipFlowWhitespace()
        // A `:` here promotes the entry to a single-pair mapping per
        // spec 7.5: `[foo: bar]` is shorthand for `[{foo: bar}]`.
        val entry =
          if more && peek == Colon then
            advance()
            skipFlowWhitespace()
            val value =
              if !more || peek == Comma || peek == CloseBracket
              then YamlAst.Null
              else parseFlowNode()
            val pairBuf = acquireBuffer()
            pairBuf += first
            pairBuf += value
            val pair = sealMapping(pairBuf)
            releaseBuffer()
            pair
          else first
        buf += entry
        skipFlowWhitespace()
        if !more then fail(t"unterminated flow sequence")
        peek match
          case Comma        => advance()
          case CloseBracket => advance(); done = true
          case _            => fail(t"expected ',' or ']' in flow sequence")
    val result = sealSequence(buf)
    releaseBuffer()
    result

  private def parseFlowMapping()(using Tactic[YamlError]): YamlAst =
    val buf = acquireBuffer()
    var done = false
    while !done do
      skipFlowWhitespace()
      if !more then fail(t"unterminated flow mapping")
      if peek == CloseBrace then
        advance()
        done = true
      else if peek == Comma then
        fail(t"empty flow-mapping entry")
      else
        val key = parseFlowNode()
        skipFlowWhitespace()
        val value =
          if more && peek == Colon then
            advance()
            skipFlowWhitespace()
            if !more || peek == Comma || peek == CloseBrace then YamlAst.Null
            else parseFlowNode()
          else YamlAst.Null
        buf += key
        buf += value
        skipFlowWhitespace()
        if !more then fail(t"unterminated flow mapping")
        peek match
          case Comma     => advance()
          case CloseBrace => advance(); done = true
          case _         => fail(t"expected ',' or '}' in flow mapping")
    val result = sealMapping(buf)
    releaseBuffer()
    result

  // Materialise the buffer as a sequence node: copy directly into a single
  // `Array[Any]`, padding with `arrayPad` if the count is even so the
  // result has odd length (the parity that distinguishes sequences from
  // mappings).
  private def sealSequence(buf: ArrayBuffer[Any]): YamlAst =
    val n = buf.length
    if (n & 1) == 1 then
      val arr = new Array[Any](n)
      buf.copyToArray(arr)
      arr.asInstanceOf[IArray[Any]].asInstanceOf[YamlAst]
    else
      val arr = new Array[Any](n + 1)
      buf.copyToArray(arr)
      arr(n) = YamlAst.arrayPad
      arr.asInstanceOf[IArray[Any]].asInstanceOf[YamlAst]

  // The buffer was filled with alternating key/value items, so the count
  // is already even; copy directly into a flat `Array[Any]`.
  private def sealMapping(buf: ArrayBuffer[Any]): YamlAst =
    val n = buf.length
    val arr = new Array[Any](n)
    buf.copyToArray(arr)
    arr.asInstanceOf[IArray[Any]].asInstanceOf[YamlAst]

  // Within a flow context, whitespace and newlines are insignificant
  // separators; comments still apply.
  private def skipFlowWhitespace(): Unit =
    var continue = true
    while continue && more do
      val c = peek
      if c == Space || c == Tab || c == Newline || c == Return then advance()
      else if c == Hash then
        while more && peek != Newline do advance()
      else continue = false

  // A node within a flow context — same dispatch as parseNodeHere but
  // with flow-specific scalar termination.
  private def parseFlowNode()(using Tactic[YamlError]): YamlAst =
    skipFlowWhitespace()
    if !more then YamlAst.Null
    else
      consumeNodePrefixes()
      val anchorName = prefixAnchor
      val tagText    = prefixTag
      val headByte   = prefixHeadByte
      val value =
        if headByte == -1 then YamlAst.Null
        else if headByte == Star then
          advance()
          parseAlias()
        else
          (headByte: @switch) match
            case Quote        => advance(); parseDoubleQuoted()
            case Apostrophe   => advance(); parseSingleQuoted()
            case OpenBracket  => advance(); parseFlowSequence()
            case OpenBrace    => advance(); parseFlowMapping()
            case _            => parseFlowPlainScalar()

      val tagged = if tagText.nil then value else applyTag(tagText, value)
      if anchorName.nil then tagged
      else
        anchors.update(anchorName.s, tagged)
        tagged

  // Plain scalar within a flow context: terminates on `,`, `]`, `}`,
  // `:`+space, newline, or hash-comment.
  private def parseFlowPlainScalar()(using Tactic[YamlError]): YamlAst =
    resetString()
    var done = false
    while !done && more do
      // Fast-prefix ASCII run: bulk-copy bytes that are neither flow
      // terminators nor UTF-8 lead bytes.
      val runStart = pos
      while pos < bufEnd && {
        val b = bytes(pos)
        b >= 0x20 && b != Comma && b != CloseBracket && b != CloseBrace
                  && b != Colon && b != Hash
      } do pos += 1
      val runLen = pos - runStart
      if runLen > 0 then
        ensureSpace(runLen)
        var k = 0
        while k < runLen do
          chars(stringCursor + k) = (bytes(runStart + k) & 0xFF).toChar
          k += 1
        stringCursor += runLen

      if !more then done = true
      else
        val b = peek
        if b == Comma || b == CloseBracket || b == CloseBrace then done = true
        else if b == Newline then consumeMultilineFold()
        else if b == Colon then
          val nextByte = if pos + 1 < bufEnd then bytes(pos + 1) else -1
          if nextByte == Space || nextByte == Tab || nextByte == Newline
                  || nextByte == Comma || nextByte == CloseBracket
                  || nextByte == CloseBrace || nextByte == -1 then done = true
          else
            appendChar(':')
            advance()
        else if b == Hash && stringCursor > 0 && chars(stringCursor - 1) == ' ' then
          while more && peek != Newline do advance()
          done = true
        else
          appendByteAsChar(b)
          advance()
    val text = trimEndWhitespace(getStringText())
    resolvePlainScalar(text)

  // ── Block sequences / mappings ─────────────────────────────────────────

  // Called when a `-` is at the head of the current node. Either a plain
  // scalar starting with `-` (e.g. negative number) or a block sequence
  // marker. The disambiguation is the byte after `-`: if space/tab/
  // newline/EOF, it's a sequence marker.
  private def parseMinus(indent: Int)(using Tactic[YamlError]): YamlAst =
    val nextByte = if pos + 1 < bufEnd then bytes(pos + 1) else -1
    if nextByte == Space || nextByte == Tab || nextByte == Newline
            || nextByte == Return || nextByte == -1 then
      parseBlockSequence(indent)
    else
      parsePlainOrBlockMapping(indent)

  // Caller has seen a `?` at node-head: we treat this as a `complex key`
  // request only when the byte after `?` is whitespace or end-of-input.
  // `?foo` (no separator) is a plain scalar starting with `?`.
  private def parseQuestion(indent: Int)(using Tactic[YamlError]): YamlAst =
    val nextByte = if pos + 1 < bufEnd then bytes(pos + 1) else -1
    if nextByte == Space || nextByte == Tab || nextByte == Newline
            || nextByte == Return || nextByte == -1 then
      advance()
      skipSpaces()
      parseNodeHere(indent)
    else
      parsePlainOrBlockMapping(indent)

  private def parseBlockSequence(indent: Int)(using Tactic[YamlError]): YamlAst =
    val buf = acquireBuffer()
    val savedParentIndent = blockParentIndent
    blockParentIndent = indent
    var done = false
    while !done do
      // We're at a `-` at column == indent.
      advance()  // consume `-`
      // Either space-separated content or newline (item value on next
      // indented line)
      val next = if more then peek else -1
      val item: YamlAst =
        if next == Space || next == Tab then
          advance() // single space after `-`
          while more && (peek == Space || peek == Tab) do advance()
          if more && peek == Newline then
            advance()
            skipBlankAndCommentLines()
            val childIndent = consumeLeadingSpaces()
            if childIndent <= indent then YamlAst.Null
            else parseNodeHere(childIndent)
          else parseNodeHere(indent + 2)
        else if next == Newline || next == -1 then
          // Just `-` with newline → child on next line
          if more then advance() // consume newline
          skipBlankAndCommentLines()
          val childIndent = consumeLeadingSpaces()
          if childIndent <= indent then YamlAst.Null
          else parseNodeHere(childIndent)
        else
          // shouldn't happen
          parseNodeHere(indent + 2)

      buf += item
      // After parsing the item, skip blank lines and detect next item
      skipBlankAndCommentLines()
      val lineStart = pos
      val nextIndent = consumeLeadingSpaces()
      if more && peek == Tab then fail(t"tab character used in indentation")
      if nextIndent != indent then
        pos = lineStart
        done = true
      else if !more || peek != Minus then
        pos = lineStart
        done = true
      else if atDocumentBoundary then
        pos = lineStart
        done = true
      else
        // Need to verify it's `-` followed by whitespace (sequence
        // marker), not a plain scalar starting with `-`.
        val byteAfterDash = if pos + 1 < bufEnd then bytes(pos + 1) else -1
        if byteAfterDash != Space && byteAfterDash != Tab
                && byteAfterDash != Newline && byteAfterDash != -1 then
          pos = lineStart
          done = true

    val result = sealSequence(buf)
    releaseBuffer()
    blockParentIndent = savedParentIndent
    result

  // From the start of the current line, count leading spaces. Returns the
  // count and leaves the cursor positioned at the first non-space byte.
  private def consumeLeadingSpaces(): Int =
    var n = 0
    while more && peek == Space do
      n += 1
      advance()
    n

  // Skip blank lines (only whitespace), comment-only lines, and
  // directive lines (those starting with `%`). Leaves the cursor at
  // the start of the first non-skip line (before any leading
  // whitespace), so the caller can measure that line's indent via
  // consumeLeadingSpaces.
  private def skipBlankAndCommentLines(): Unit =
    var continue = true
    while continue && more do
      val savedPos = pos
      // Scan to end of line
      while more && (peek == Space || peek == Tab) do advance()
      if !more then continue = false
      else peek match
        case Newline =>
          advance()
        case Hash    =>
          while more && peek != Newline do advance()
          if more then advance()
        case 0x25 /* '%' */ =>
          while more && peek != Newline do advance()
          if more then advance()
        case _ =>
          pos = savedPos
          continue = false

  // Called when we've already read the first key (plain or quoted) and
  // are positioned at the `:` following it. Parse the rest of the
  // block mapping at `indent`. Subsequent keys may also be quoted.
  private def parseBlockMappingFromFirstKey
                ( firstKey: YamlAst, indent: Int )
                ( using Tactic[YamlError] )
  :   YamlAst =
    val buf = acquireBuffer()
    val savedParentIndent = blockParentIndent
    blockParentIndent = indent

    if !more || peek != Colon then fail(t"expected ':' after mapping key")
    advance()
    val firstValue = parseMappingValue(indent)
    buf += firstKey
    buf += firstValue

    var done = false
    while !done do
      skipBlankAndCommentLines()
      val lineStart = pos
      val nextIndent = consumeLeadingSpaces()
      if more && peek == Tab then fail(t"tab character used in indentation")
      if nextIndent != indent then
        pos = lineStart
        done = true
      else if !more then done = true
      else if atDocumentBoundary then
        pos = lineStart
        done = true
      else
        // The next key may be plain, double-quoted, or single-quoted.
        val keyAst: YamlAst = peek match
          case Quote =>
            advance()
            parseDoubleQuoted()
          case Apostrophe =>
            advance()
            parseSingleQuoted()
          case _ =>
            val keyText = readPlainScalarText(indent)
            if !sawMappingColon then
              fail(t"plain scalar at mapping indent without ':'")
            resolvePlainScalar(keyText)
        // For quoted keys, sawMappingColon wasn't set during reading —
        // skip whitespace and require an explicit `:` here. For plain
        // keys, sawMappingColon was true so the parser is already at
        // the colon.
        skipSpaces()
        if !more || peek != Colon then fail(t"expected ':' after mapping key")
        advance()
        val value = parseMappingValue(indent)
        buf += keyAst
        buf += value

    val result = sealMapping(buf)
    releaseBuffer()
    blockParentIndent = savedParentIndent
    result

  // Parse the value side of a `key: VALUE` entry. Either inline (after
  // the colon, on the same line) or a block on the next indented lines.
  private def parseMappingValue(parentIndent: Int)(using Tactic[YamlError])
  :   YamlAst =
    skipSpaces()
    if !more then YamlAst.Null
    else if peek == Newline then
      advance()
      skipBlankAndCommentLines()
      val lineStart = pos
      val childIndent = consumeLeadingSpaces()
      pickValueOrNull(parentIndent, childIndent, lineStart)
    else if peek == Hash then
      while more && peek != Newline do advance()
      if more then advance()
      skipBlankAndCommentLines()
      val lineStart = pos
      val childIndent = consumeLeadingSpaces()
      pickValueOrNull(parentIndent, childIndent, lineStart)
    else
      // Inline value on same line as `key:`. Pass `parentIndent` directly
      // so block scalars require content indent strictly greater than the
      // surrounding block's indent.
      parseNodeHere(parentIndent)

  // Decide whether a next-line node belongs to the current mapping
  // value. Less-indented => Null (no value, the line is a sibling key
  // or out-of-scope). Same-indent block sequence => the sequence is
  // the value (spec 8.2.2). More-indented => a regular nested node.
  // Rewinds to lineStart on Null so the outer scope re-reads the line.
  private inline def pickValueOrNull
                       ( parentIndent: Int, childIndent: Int, lineStart: Int )
                       ( using Tactic[YamlError] )
  :   YamlAst =
    if more && peek == Tab then fail(t"tab character used in indentation")
    if !more then YamlAst.Null
    else if childIndent < parentIndent then
      pos = lineStart
      YamlAst.Null
    else if childIndent == parentIndent then
      if peek == Minus && {
        val nb = if pos + 1 < bufEnd then bytes(pos + 1) else -1
        nb == Space || nb == Tab || nb == Newline || nb == Return || nb == -1
      } then parseNodeHere(childIndent)
      else
        pos = lineStart
        YamlAst.Null
    else parseNodeHere(childIndent)

  // ── Block scalars (|/>) ─────────────────────────────────────────────────

  private def parseBlockScalar(literal: Boolean, parentIndentParam: Int)
                            ( using Tactic[YamlError] )
  :   YamlAst =
    advance() // consume `|` or `>`

    // Header: optional indentation indicator (1-9) and chomping
    // indicator (+/-), in either order, followed by optional whitespace
    // and an optional comment.
    var explicitIndent: Int = -1
    var chomp: Char = 'c' // 'c' = clip (default), '-' = strip, '+' = keep
    while more && peek != Newline && peek != Hash do
      val b = peek
      if b >= Num0 && b <= Num9 then
        if b == Num0 then fail(t"block-scalar indentation indicator must be 1-9")
        if explicitIndent >= 0 then fail(t"duplicate block-scalar indentation indicator")
        explicitIndent = b - Num0
      else if b == Plus then
        if chomp != 'c' then fail(t"duplicate block-scalar chomping indicator")
        chomp = '+'
      else if b == Minus then
        if chomp != 'c' then fail(t"duplicate block-scalar chomping indicator")
        chomp = '-'
      else if b == Space || b == Tab then ()
      else fail(t"invalid block-scalar header")
      advance()
    if more && peek == Hash then
      while more && peek != Newline do advance()
    if more then advance() // consume newline

    resetString()
    val parent = blockParentIndent
    var baseIndent: Int =
      if explicitIndent >= 0 then parent + explicitIndent else -1

    // For folded mode: track the previous emitted-line classification
    // to choose the join string when starting the next content line.
    //   0 = nothing yet, 1 = blank, 2 = regular, 3 = more-indented.
    var lastLineType: Int = 0

    var done = false
    while !done && more do
      val lineStart = pos
      var spaces = 0
      while more && peek == Space do
        spaces += 1
        advance()

      if !more then
        done = true
      else if peek == Newline then
        // Whitespace-only line. Classification depends on whether we've
        // already established baseIndent.
        if baseIndent >= 0 && spaces > baseIndent then
          // More-indented all-spaces line — its "content" is the
          // (spaces - baseIndent) extra spaces.
          if literal then
            var k = 0
            while k < spaces - baseIndent do { appendChar(' '); k += 1 }
            appendChar('\n')
          else
            // Folded: emit join from prior line, then the spaces.
            val sep: Char =
              if lastLineType == 0 || lastLineType == 1 then 0.toChar
              else '\n'
            if sep != 0.toChar then appendChar(sep)
            var k = 0
            while k < spaces - baseIndent do { appendChar(' '); k += 1 }
            // No trailing terminator; the next line decides the join.
          lastLineType = 3
        else
          // Plain blank line.
          appendChar('\n')
          lastLineType = 1
        advance()
      else
        // Non-whitespace content line.
        if baseIndent < 0 then
          // First content line establishes baseIndent; the block has
          // a body only if its indent strictly exceeds the parent's.
          if spaces > parent then baseIndent = spaces
          else
            pos = lineStart
            done = true
        if !done then
          if spaces < baseIndent then
            pos = lineStart
            done = true
          else
            val moreIndented = spaces > baseIndent
            if literal then
              // Trailing-terminator scheme.
              var k = 0
              while k < spaces - baseIndent do { appendChar(' '); k += 1 }
              readBlockScalarLineContent()
              appendChar('\n')
              if more && peek == Newline then advance()
              lastLineType = if moreIndented then 3 else 2
            else
              // Folded: prefix-separator scheme.
              val curType = if moreIndented then 3 else 2
              val sep: Char = lastLineType match
                case 0                           => 0.toChar
                case 1                           => 0.toChar
                case 2 if curType == 2           => ' '
                case _                           => '\n'
              if sep != 0.toChar then appendChar(sep)
              var k = 0
              while k < spaces - baseIndent do { appendChar(' '); k += 1 }
              readBlockScalarLineContent()
              if more && peek == Newline then advance()
              lastLineType = curType

    // For folded mode the per-line scheme leaves no trailing newline;
    // add one so chomping rules see a uniform "final break".
    if !literal && stringCursor > 0 && chars(stringCursor - 1) != '\n' then
      appendChar('\n')

    // Chomp.
    chomp match
      case '-' =>
        while stringCursor > 0 && chars(stringCursor - 1) == '\n' do
          stringCursor -= 1
      case 'c' =>
        // Clip: keep at most one trailing newline. If the buffer is
        // entirely newlines (no real content), reduce to empty.
        var lastContent = stringCursor
        while lastContent > 0 && chars(lastContent - 1) == '\n' do
          lastContent -= 1
        stringCursor = if lastContent == 0 then 0 else lastContent + 1
      case '+' =>
        () // keep all trailing newlines
      case _  => ()

    YamlAst.Str(getStringText())

  // Read the non-newline content of the current line into the chars
  // buffer. Bulk-copies printable ASCII via the fast prefix loop and
  // falls back to per-byte handling for tabs and UTF-8 lead bytes.
  private def readBlockScalarLineContent(): Unit =
    var done = false
    while !done do
      val runStart = pos
      while pos < bufEnd && bytes(pos) >= 0x20 do pos += 1
      val runLen = pos - runStart
      if runLen > 0 then
        ensureSpace(runLen)
        var k = 0
        while k < runLen do
          chars(stringCursor + k) = (bytes(runStart + k) & 0xFF).toChar
          k += 1
        stringCursor += runLen
      if !more || peek == Newline then done = true
      else
        appendByteAsChar(peek)
        advance()

  // ── Tags ────────────────────────────────────────────────────────────────

  private def applyTag(tag: Text, value: YamlAst)(using Tactic[YamlError])
  :   YamlAst = tag.s match
    case "!" | "!!" =>
      // Non-specific tags. `!` forces the string type for plain
      // scalars (preventing implicit type resolution into int/bool/etc).
      if value.asInstanceOf[AnyRef] == null then YamlAst.Str(t"")
      else value.asInstanceOf[Matchable] match
        case _: String  => value
        case n: Long    => YamlAst.Str(n.toString.tt)
        case d: Double  => YamlAst.Str(d.toString.tt)
        case b: Boolean => YamlAst.Str(b.toString.tt)
        case _          => value

    case "!!str" =>
      // A bare `!!str` with no scalar content is the empty string,
      // not the literal text "null".
      if value.asInstanceOf[AnyRef] == null then YamlAst.Str(t"")
      else value.asInstanceOf[Matchable] match
        case _: String  => value
        case n: Long    => YamlAst.Str(n.toString.tt)
        case d: Double  => YamlAst.Str(d.toString.tt)
        case b: Boolean => YamlAst.Str(b.toString.tt)
        case _          => value

    case "!!int" =>
      value.asInstanceOf[Matchable] match
        case _: Long   => value
        case d: Double => YamlAst.Integer(d.toLong)
        case s: String =>
          val r = parsePlainIntegerOrNull(s)
          if r != null then r else value
        case _         => value

    case "!!float" =>
      value.asInstanceOf[Matchable] match
        case _: Double => value
        case n: Long   => YamlAst.Decimal(n.toDouble)
        case s: String =>
          val r = parsePlainDecimalOrNull(s)
          if r != null then r else value
        case _         => value

    case "!!bool" =>
      value.asInstanceOf[Matchable] match
        case _: Boolean                  => value
        case "true" | "True" | "TRUE"    => YamlAst.Bool(true)
        case "false" | "False" | "FALSE" => YamlAst.Bool(false)
        case _                           => value

    case "!!null" => YamlAst.Null
    case _        => value
