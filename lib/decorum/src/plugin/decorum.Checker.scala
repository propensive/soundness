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

object Checker:

  // Test-friendly entry point: parses `rawText` standalone via `Parsing.parse`
  // before delegating to the tree-aware overload. The plugin should call the
  // overload below directly with the compilation unit's existing untyped
  // tree to avoid re-parsing.
  def check
    ( file:             String,
      expectedModule:   Option[String],
      rawText:          String,
      siblingTypes:     List[String] = Nil,
      siblingExtensions: List[String] = Nil,
      unexported:       Set[String] = Set.empty )
  :   LazyList[Violation] =
    val (tree, source) = Parsing.parse(file, rawText)
    check(file, expectedModule, rawText, tree, source, siblingTypes, siblingExtensions, unexported)

  def check
    ( file:             String,
      expectedModule:   Option[String],
      rawText:          String,
      untpdTree:        untpd.Tree,
      source:           SourceFile,
      siblingTypes:     List[String],
      siblingExtensions: List[String],
      unexported:       Set[String] )
  :   LazyList[Violation] =

    val ctx =
      Context
        ( file, expectedModule, rawText, untpdTree, source, siblingTypes, siblingExtensions,
          unexported )

    val out   = mutable.ListBuffer[Violation]()
    val state = State(file, expectedModule)
    state.annotationEndLines = ctx.annotationEndLines
    scanRawTabs(file, rawText, out)
    Rules.all.foreach: rule =>
      out ++= rule.check(ctx)
    var idx = 0

    while idx < ctx.lines.length do
      val lineNum = idx + 1
      checkLine(state, lineNum, ctx.lines(idx), out)
      idx += 1

    if state.prevWasAnnotation then
      out +=
        Violation
          ( file, state.prevLineNum, 1, "551.1", "annotation is not followed by a declaration" )

    state.pendingR11.foreach(out += _)
    state.pendingR11 = Nil
    LazyList.from(out)

  def expectedModule(filePath: String): Option[String] =
    val parts = filePath.split("/lib/").nn
    if parts.length < 2 then None
    else
      val moduleDir = parts(1).nn.split("/").nn(0).nn
      val segments = filePath.split("/").nn
      val fileName = segments(segments.length - 1).nn

      val base =
        if fileName.endsWith(".scala")
        then fileName.substring(0, fileName.length - ".scala".length).nn
        else fileName
      // Cross-module export files (e.g. `soundness_serpentine_core.scala`,
      // `anticipation_serpentine_core.scala`) declare a different package
      // — the prefix before `_<module>_<suffix>`. Detect this pattern and
      // return that prefix as the expected package.

      val prefix = s"_${moduleDir}_"
      val idx    = base.indexOf(prefix)
      if idx > 0 then Some(base.substring(0, idx).nn) else Some(moduleDir)

  private class State(val file: String, val expectedModule: Option[String]):
    var consecutiveBlanks:    Int                        = 0
    var annotationEndLines:   Set[Int]                   = Set.empty
    var prevWasAnnotation:    Boolean                    = false
    var prevLineNum:          Int                        = 0
    // The net `(`-depth carried across lines, clamped at zero at each line
    // end. R3 (926) suspends inside open `(...)` blocks: continuation rows
    // inside parameter lists align under names from the opener line and
    // may need an odd number of leading spaces.
    var openParens:           Int                        = 0
    // Columns of every multi-space-after-comma site on the previous
    // line. R37 (529.2) uses this to decide whether the current line
    // genuinely continues a multi-row alignment (one of *its* extra-
    // space comma columns must match a column from the previous row),
    // rather than blindly suppressing whenever both lines happen to
    // have extra padding somewhere.
    var prevAlignmentCols:    Set[Int]                   = Set.empty
    var pendingR11:           List[Violation]            = Nil

  private def checkLine
    ( s:       State,
      lineNum: Int,
      line:    Line,
      out:     mutable.ListBuffer[Violation] )
  :   Unit =

    val leadingWs   = line.leadingWs
    val rest        = line.rest
    val firstReal   = line.firstReal
    val isBlank     = line.isBlank
    val leadingCols = line.leadingCols

    inline def emit(col: Int, rule: String, message: String): Unit =
      out += Violation(s.file, lineNum, col, rule, message)

    val isStringContent = firstReal.exists(_.kind == Sort.Strs)
    // The interior (and closing) lines of a multi-line triple-quoted string are
    // tokenised as a single `Strs` token with no leading `Space` (so
    // `leadingWs` is empty). Their text is string content — for `sh`/raw
    // strings the whitespace is significant, for `m`/`j`/`x`/`y`/`tel` it is
    // prose — so R2 (line length) and R4 (trailing whitespace) must not fire on
    // them. The R560 layout rule governs the structure of the listed
    // interpolators instead.
    val isStringContinuation = isStringContent && leadingWs.isEmpty
    // Skip R3 inside open `(...)` blocks: continuation rows inside parameter
    // lists align under names from the opener line and may need an odd
    // number of leading spaces (e.g. under `inline commensurable` at col 18).
    if !isStringContent && s.openParens == 0 then
      checkR3IndentWidth(isBlank, leadingCols, emit)
    if !isStringContinuation then checkR4TrailingWs(line.lexemes, emit)

    if isBlank then
      s.consecutiveBlanks += 1
      if s.consecutiveBlanks > 2 then
        emit(1, "783", "more than two consecutive blank lines")
    else
      s.consecutiveBlanks = 0

    checkTokens(lineNum, line, emit)
    checkCommas(s, lineNum, line, isBlank, out)
    // R-444 hard spaces, R-163 chain continuations, R32 (140) given-arrow
    // alignment and R-677 return-type separation are enforced by registry
    // rules (`ContinuationRules.HardSpace`, `ContinuationRules
    // .ChainContinuation`, `AnchorRules.GivenArrowAlign` and
    // `ProximityRules.ReturnTypeBlank`).
    // R28 chunk separation (315) is enforced tree-based by
    // `checkChunkBlanks`; the old line-by-line sibling-padding check has
    // been removed as redundant.
    // R30/R12 bracket interiors (811/402), R36 using-clause alignment (946)
    // and R33.4 heavy-bracket anchoring (833.4) are enforced by registry
    // rules (`BalanceRules.FormalBlockSpacing`, `BalanceRules
    // .CompactBracketSpacing`, `TabulationRules.UsingAlignment` and
    // `AnchorRules.HeavyBracketAnchor`).
    updateOpenParens(s, rest)

    if isBlank then
      if s.prevWasAnnotation then
        emit
          ( 1, "551.2",
            "blank line is not permitted between an annotation and the declaration it annotates" )
        s.prevWasAnnotation = false
    else
      s.prevWasAnnotation = s.annotationEndLines.contains(lineNum)
      s.prevLineNum = lineNum

  private def scanRawTabs
    ( file: String, rawText: String, out: mutable.ListBuffer[Violation] )
  :   Unit =

    var line = 1
    var col  = 1
    var i    = 0
    while i < rawText.length do
      val ch = rawText.charAt(i)
      if ch == '\t' then
        out +=
          Violation
            ( file, line, col, "135", "tab character is not permitted; use spaces" )
      if ch == '\n' then
        line += 1
        col = 1
      else
        col += 1
      i += 1

  private def checkR3IndentWidth
    ( isBlank: Boolean, leadingCols: Int, emit: (Int, String, String) => Unit )
  :   Unit =

    if !isBlank && leadingCols%2 != 0 then
      emit(1, "926", s"indent width $leadingCols is not a multiple of 2")

  private def checkR4TrailingWs
    ( line: IndexedSeq[Lexeme], emit: (Int, String, String) => Unit )
  :   Unit =

    line.lastOption match
      case Some(token) if token.kind == Sort.Space && token.text.length > 0 =>
        val hasNonWs = line.exists{ t => t.kind != Sort.Space && t.kind != Sort.Comment }
        if hasNonWs then
          val col = line.iterator.map(_.text.length).sum - token.text.length + 1
          emit(col, "015", "line has trailing whitespace")

      case _ =>
        ()

  private def checkTokens
    ( lineNum: Int,
      line:    Line,
      emit:    (Int, String, String) => Unit )
  :   Unit =

    val arr  = line.arr
    val cols = line.cols

    checkComments(lineNum, arr, cols, emit)
    checkOperatorSpacing(arr, cols, emit)
    checkAssignmentSpacing(arr, cols, emit)
    checkSymbolicMethodNames(arr, cols, emit)

  private def checkCommas
    ( s:       State,
      lineNum: Int,
      line:    Line,
      isBlank: Boolean,
      out:     mutable.ListBuffer[Violation] )
  :   Unit =

    val arr  = line.arr
    val cols = line.cols

    val deferred       = mutable.ListBuffer[Violation]()
    val alignmentCols  = mutable.Set[Int]()

    var i = 0
    while i < arr.length do
      if arr(i).text == "," && arr(i).kind == Sort.Code then
        if i > 0 && arr(i - 1).kind == Sort.Space && arr(i - 1).text.length > 0 then
          out +=
            Violation
              ( s.file, lineNum, cols(i), "529.1",
                "no space is permitted before a comma" )

        if i + 1 < arr.length then
          val next = arr(i + 1)
          if next.kind != Sort.Space then
            out +=
              Violation
                ( s.file, lineNum, cols(i + 1), "529.2",
                  "exactly one space is required after a comma" )
          else if next.text != " " then
            // Extra spaces after comma — possibly part of a multi-row
            // alignment column. Record the column where the *next* token
            // begins (i.e. the would-be-aligned column) and defer until
            // we can confirm the previous line shared that column.
            val nextTokCol = cols(i + 1) + next.text.length
            alignmentCols += nextTokCol
            if !next.text.startsWith("\n") then
              deferred +=
                Violation
                  ( s.file, lineNum, cols(i + 1), "529.2",
                    "exactly one space is required after a comma" )
      i += 1

    // The current line "continues" an alignment iff one of its
    // multi-space comma sites lands at a column the previous line also
    // had. Otherwise the extra spaces are unjustified — fire.
    val continuesAlignment =
      alignmentCols.nonEmpty && alignmentCols.exists(s.prevAlignmentCols.contains)

    if continuesAlignment then s.pendingR11 = Nil
    else
      s.pendingR11.foreach(out += _)
      s.pendingR11 = Nil

    if continuesAlignment then
      // Both directions confirmed — drop current deferred too.
      ()
    else if alignmentCols.nonEmpty then
      // Current line has extra-space commas but doesn't continue a known
      // alignment run; defer in case the *next* line shares a column.
      s.pendingR11 = deferred.toList
    else
      deferred.foreach(out += _)

    if !isBlank then s.prevAlignmentCols = alignmentCols.toSet
    else s.prevAlignmentCols = Set.empty

  private def checkComments
    ( lineNum: Int,
      arr:     Array[Lexeme],
      cols:    Array[Int],
      emit:    (Int, String, String) => Unit )
  :   Unit =

    val inLicense = lineNum >= 1 && lineNum <= 32
    var i = 0
    while i < arr.length do
      if arr(i).kind == Sort.Comment then
        val text = arr(i).text
        if text.startsWith("/**") then
          emit
            ( cols(i), "162.2",
              "`/** ... */` block comments are not permitted; use `doc/` markdown instead" )
        else if text.startsWith("/*") && !inLicense then
          emit
            ( cols(i), "162.1",
              "`/* ... */` block comments are reserved for the license header (lines 1-32)" )
      i += 1

  private val CheckedOps: Set[String] =
    Set
      ( "+", "-", "*", "/", "%", "&", "|", "^", "<", ">", "<<", ">>", ">>>",
        "&&", "||", "==", "!=", "<=", ">=", "=>", "->", "<-", "<:", ">:",
        "&~", "?=>" )

  private def operatorPrecedence(op: String): Int =
    if op.isEmpty then 0
    else op.head match
      case c if c.isLetter        => 1
      case '|'                    => 2
      case '^'                    => 3
      case '&'                    => 4
      case '=' | '!'              => 5
      case '<' | '>'              => 6
      case ':'                    => 7
      case '+' | '-'              => 8
      case '*' | '/' | '%'        => 9
      case _                      => 10

  private case class OpHit
    ( text:       String,
      idx:        Int,
      col:        Int,
      leftSpace:  Boolean,
      rightSpace: Boolean )

  private[decorum] val NonOperandWords: Set[String] =
    Set
      ( "case", "if", "then", "else", "do", "while", "for", "yield", "return",
        "match", "with", "extends", "derives", "given", "using", "new", "throw",
        "try", "catch", "finally", "import", "package", "def", "val", "var",
        "lazy", "object", "class", "trait", "enum", "type", "private", "protected",
        "public", "final", "sealed", "abstract", "implicit", "override", "inline",
        "transparent", "infix", "open", "opaque", "erased", "tracked",
        "is", "of", "in", "by", "to", "under", "on", "raises", "until" )

  // Control-flow keywords that separate sub-expressions: encountering one
  // closes the current operator frame and opens a fresh one at the same
  // nesting depth, just like `,` does. `case` is excluded when used as a
  // modifier (`case class`, `case object`).
  private val BoundaryWords: Set[String] =
    Set
      ( "if", "then", "else", "match", "case", "do", "while", "for", "yield",
        "return", "throw", "try", "catch", "finally" )

  private def caseIsModifier(arr: Array[Lexeme], i: Int): Boolean =
    var j = i + 1
    while j < arr.length && (arr(j).kind == Sort.Space || arr(j).kind == Sort.Comment) do
      j += 1
    j < arr.length && arr(j).kind == Sort.Code
      && (arr(j).text == "class" || arr(j).text == "object")

  private def isBinaryContext(arr: Array[Lexeme], i: Int): Boolean =
    val left =
      var j = i - 1
      while j >= 0 && (arr(j).kind == Sort.Space || arr(j).kind == Sort.Comment) do j -= 1
      if j < 0 then false
      else if arr(j).kind == Sort.Strs then true
      else if arr(j).kind == Sort.Code then
        val t = arr(j).text
        if t == ")" || t == "]" then true
        else if t.isEmpty then false
        else
          val c = t.head
          (c.isLetterOrDigit || c == '_' || c == '`') && !NonOperandWords.contains(t)
      else
        false

    val right =
      var j = i + 1
      while j < arr.length && (arr(j).kind == Sort.Space || arr(j).kind == Sort.Comment) do j += 1
      if j >= arr.length then false
      else if arr(j).kind == Sort.Strs then true
      else if arr(j).kind == Sort.Code then
        val t = arr(j).text
        // The next token must look like an operand (not a closing bracket or
        // separator). This excludes postfix usages like `xs*`, `tuple*`, etc.
        if t == ")" || t == "]" || t == "}" || t == "," || t == ";" then false
        else if t.isEmpty then false
        else
          val c = t.head
          (c.isLetterOrDigit || c == '_' || c == '`' || c == '"' || c == '\'')
            && !NonOperandWords.contains(t)
      else
        false

    left && right

  private def checkOperatorSpacing
    ( arr:  Array[Lexeme],
      cols: Array[Int],
      emit: (Int, String, String) => Unit )
  :   Unit =

    val firstSemantic = arr.indexWhere{ t => t.kind != Sort.Space && t.kind != Sort.Comment }
    val lastSemantic  = arr.lastIndexWhere{ t => t.kind != Sort.Space && t.kind != Sort.Comment }
    val frames = mutable.Stack[mutable.ArrayBuffer[OpHit]]()
    frames.push(mutable.ArrayBuffer.empty)

    var i = 0
    while i < arr.length do
      val tok = arr(i)
      if tok.kind == Sort.Code then
        val text = tok.text
        if text == "(" || text == "[" then
          frames.push(mutable.ArrayBuffer.empty)
        else if text == ")" || text == "]" then
          if frames.size > 1 then checkOpFrame(frames.pop(), emit)
        else if text == "," then
          // A comma separates independent expressions: close the current
          // operator frame and open a fresh one at the same nesting level.
          checkOpFrame(frames.pop(), emit)
          frames.push(mutable.ArrayBuffer.empty)
        else if BoundaryWords.contains(text)
          && !(text == "case" && caseIsModifier(arr, i))
        then
          // Control-flow keywords cut the expression into sub-expressions:
          // `if` predicates, `then`/`else` clauses, `match` scrutinee and
          // `case` patterns/bodies, etc. should each be checked independently.
          checkOpFrame(frames.pop(), emit)
          frames.push(mutable.ArrayBuffer.empty)
        else if CheckedOps.contains(text) then
          val isAtStart  = i == firstSemantic
          val isAtEnd    = i == lastSemantic
          val isBinary   = isAtStart || isBinaryContext(arr, i)
          val leftSpace  = i > 0 && arr(i - 1).kind == Sort.Space
          val rightSpace = i + 1 < arr.length && arr(i + 1).kind == Sort.Space
          if isBinary then
            // Symmetry: if both edges have a token of the same kind around, the
            // spaces should be matched. Skip at line edges.
            if !isAtStart && !isAtEnd && leftSpace != rightSpace then
              emit
                ( cols(i), "376",
                  s"`$text` has asymmetric spacing — use 0 or 1 space on both sides" )
            // Multi-char operators must have one space around (zero is reserved
            // for single-character operators).
            if text.length > 1 && !isAtStart && !isAtEnd
              && (!leftSpace || !rightSpace)
            then
              emit
                ( cols(i), "376",
                  s"multi-character `$text` requires one space on each side" )
            frames.top += OpHit(text, i, cols(i), leftSpace, rightSpace)
            // `=>` and `?=>` separate a pattern/parameter list from the body
            // it produces, so they too cut the expression. Flush *after* the
            // OpHit append so the arrow itself participates in the left-side
            // frame's classification.
            if text == "=>" || text == "?=>" then
              checkOpFrame(frames.pop(), emit)
              frames.push(mutable.ArrayBuffer.empty)
      i += 1

    while frames.nonEmpty do checkOpFrame(frames.pop(), emit)

  // Assignment and mutation operators (`=`, `+=`, `-=`, etc.) require
  // *at least* one space before and *exactly* one space after when the
  // right-hand side appears on the same line. The "at least one
  // before" lets multi-line parameter blocks align their `=`s
  // vertically:
  //
  //     ( mode:  UnixMode          = UnixMode(),
  //       user:  UnixUser          = UnixUser(0),
  //       group: UnixGroup         = UnixGroup(0) )
  //
  // The tokenizer keeps each operator as one `Code` token, so we match
  // by literal text. The check skips when the operator is the last
  // semantic token on its line (multi-line RHS — `def f =\n  body`).
  private val AssignOps: Set[String] =
    Set("=", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "<<=", ">>=", ">>>=")

  private def checkAssignmentSpacing
    ( arr: Array[Lexeme], cols: Array[Int], emit: (Int, String, String) => Unit )
  :   Unit =

    var lastSemantic = arr.length - 1
    while lastSemantic >= 0
          && (arr(lastSemantic).kind == Sort.Space
              || arr(lastSemantic).kind == Sort.Comment)
    do lastSemantic -= 1

    var i = 0
    while i < arr.length do
      val t = arr(i)
      if t.kind == Sort.Code && AssignOps.contains(t.text) && i != lastSemantic then
        // `name_=` setter method-name suffix: skip. `=` follows an
        // identifier ending in `_` with no intervening space.
        val isSetterSuffix =
          t.text == "=" && i > 0 && arr(i - 1).kind == Sort.Code
            && arr(i - 1).text.endsWith("_")
        if !isSetterSuffix then
          val leftHasSpace =
            i > 0 && arr(i - 1).kind == Sort.Space && arr(i - 1).text.length >= 1
          val rightExactSpace =
            i + 1 < arr.length && arr(i + 1).kind == Sort.Space && arr(i + 1).text == " "
          if !leftHasSpace then
            emit
              ( cols(i), "376.1",
                s"`${t.text}` requires at least one space before it when "
                  +"the right-hand side is on the same line" )
          else if !rightExactSpace then
            emit
              ( cols(i), "376.1",
                s"`${t.text}` requires exactly one space after it when "
                  +"the right-hand side is on the same line" )
      i += 1

  private def checkOpFrame
    ( ops: mutable.ArrayBuffer[OpHit], emit: (Int, String, String) => Unit )
  :   Unit =

    if ops.isEmpty then ()
    else
      // Same-precedence consistency: every operator at a given precedence
      // must use the same spacing within this frame.
      ops.groupBy{ op => operatorPrecedence(op.text) }.foreach: (_, group) =>
        val mixed = group.exists{ op => op.leftSpace || op.rightSpace }
          && group.exists{ op => !(op.leftSpace || op.rightSpace) }
        if mixed then
          group.foreach: op =>
            emit
              ( op.col, "376",
                s"`${op.text}` has inconsistent spacing with same-precedence operators" )

      // Cross-precedence ordering: every spaced operator must have *strictly*
      // lower precedence than every unspaced operator.  Equivalently: the
      // highest-precedence spaced operator must be lower than the
      // lowest-precedence unspaced one.
      var maxSpacedPrec   = Int.MinValue
      var minUnspacedPrec = Int.MaxValue
      ops.foreach: op =>
        val prec   = operatorPrecedence(op.text)
        val spaced = op.leftSpace || op.rightSpace
        if spaced && prec > maxSpacedPrec then maxSpacedPrec = prec
        if !spaced && prec < minUnspacedPrec then minUnspacedPrec = prec

      if maxSpacedPrec > minUnspacedPrec then
        ops.foreach: op =>
          val prec   = operatorPrecedence(op.text)
          val spaced = op.leftSpace || op.rightSpace
          if spaced && prec > minUnspacedPrec then
            emit
              ( op.col, "376",
                s"`${op.text}` cannot have more spacing than lower-precedence "
                  +"operators in the same expression" )

  private[decorum] def isSymbolicOperator(text: String): Boolean =
    text.nonEmpty && text.forall: c =>
      c match
        case '+' | '-' | '*' | '/' | '%' | '&' | '|' | '^' | '~' => true
        case '<' | '>' | '=' | '!' | '?' | ':' | '@' | '#'       => true
        case _                                                   => false

  private def checkSymbolicMethodNames
    ( arr:  Array[Lexeme],
      cols: Array[Int],
      emit: (Int, String, String) => Unit )
  :   Unit =

    var i = 0
    while i < arr.length do
      if arr(i).kind == Sort.Code && arr(i).text == "def" then
        var j = i + 1
        while j < arr.length && (arr(j).kind == Sort.Space || arr(j).kind == Sort.Comment) do
          j += 1
        if j < arr.length && arr(j).kind == Sort.Code && isSymbolicOperator(arr(j).text) then
          val opIdx = j
          val nextIdx = opIdx + 1
          if nextIdx < arr.length && arr(nextIdx).kind == Sort.Code
            && (arr(nextIdx).text == "(" || arr(nextIdx).text == "[")
          then
            emit
              ( cols(nextIdx), "013",
                s"a single space is required between `${arr(opIdx).text}` and "
                  +s"`${arr(nextIdx).text}`" )
      i += 1

  private[decorum] val ModifierWords: Set[String] =
    Set
      ( "private", "protected", "public", "final", "sealed", "abstract",
        "implicit", "lazy", "override", "case", "inline", "transparent",
        "infix", "open", "opaque", "erased", "tracked", "given", "into" )

  private[decorum] val DeclKeywords: Set[String] =
    Set("def", "val", "var", "type", "class", "trait", "object", "enum", "given")

  // Maintain the net `(`-depth carried across lines (clamped at zero at
  // each line end, as the old using-alignment scan did). R3 (926) is the
  // walk's remaining consumer: it suspends inside open `(...)` blocks.
  private def updateOpenParens(s: State, rest: IndexedSeq[Lexeme]): Unit =
    var depth = s.openParens
    var i     = 0

    while i < rest.length do
      val t = rest(i)

      if t.kind == Sort.Code then
        if t.text == "(" then depth += 1
        else if t.text == ")" then depth -= 1

      i += 1

    s.openParens = depth max 0


  private[decorum] def hasBlankLineBetween
    ( endLine: Int, startLine: Int, content: String, source: SourceFile ): Boolean =
    // Walk each line index `l` strictly between `endLine` and `startLine`
    // (1-indexed) — those are the lines that lie *between* the two
    // statements. If any is blank (whitespace only), there's a separator.
    var l = endLine + 1
    while l < startLine do
      val from = source.lineToOffset(l - 1)
      val to   =
        if l < content.split('\n').length + 1
        then source.lineToOffset(l).min(content.length)
        else content.length
      var i = from
      var blank = true
      while blank && i < to do
        val c = content.charAt(i)
        if c != ' ' && c != '\t' && c != '\n' && c != '\r' then blank = false
        i += 1
      if blank then return true
      l += 1
    false

