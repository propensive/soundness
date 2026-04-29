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
package decorum

import scala.collection.mutable

object Checker:
  private val MaxColumns: Int = 100
  private val PackageLine: Int = 33

  def check(file: String, expectedModule: Option[String], rawText: String): LazyList[Violation] =
    val out   = mutable.ListBuffer[Violation]()
    val state = State(file, expectedModule)
    val lines = Tokenizer.tokenize(rawText)
    scanRawTabs(file, rawText, out)
    checkFileNaming(file, out)
    var idx = 0

    while idx < lines.length do
      val lineNum = idx + 1
      checkLine(state, lineNum, lines(idx), out)
      idx += 1

    if state.prevWasAnnotation then
      out +=
        Violation(file, state.prevLineNum, 1, "15.1", "annotation is not followed by a declaration")

    state.pendingR11.foreach(out += _)
    state.pendingR11 = Nil
    finalizeCaseRun(state, out)
    checkCompanionOrdering(file, state, out)
    checkSequences(file, lines, out)
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

  private case class CaseEntry(lineNum: Int, arrowCol: Int)
  private class CaseRun(val indent: Int, val cases: mutable.ListBuffer[CaseEntry])

  private case class DeclShape(line: Int, indent: Int, kwSeq: String, padding: Int)

  private class State(val file: String, val expectedModule: Option[String]):
    var phase:                Phase                      = Phase.License
    var consecutiveBlanks:    Int                        = 0
    var prevImportGroup:      Option[Int]                = None
    var prevImportName:       Option[String]             = None
    var prevLineWasBlank:     Boolean                    = false
    var prevWasAnnotation:    Boolean                    = false
    var prevLineNum:          Int                        = 0
    var prevWasReturnType:    Boolean                    = false
    var prevReturnTypeLine:   Int                        = 0
    var prevCodeLineIndent:   Int                        = -1
    var prevLineEndedMatch:   Boolean                    = false
    var prevCodeLineLastTok:  String                     = ""
    var inMultilineImport:    Boolean                    = false
    var caseRun:              Option[CaseRun]            = None
    var blanksSinceDecl:      Int                        = 0
    var prevDeclByIndent:     mutable.Map[Int, DeclShape] = mutable.Map.empty
    var openParens:           Int                        = 0
    var usingNameColumn:      Option[Int]                = None
    var prevLineHadAlignment: Boolean                    = false
    var pendingR11:           List[Violation]            = Nil
    val typeDecls:            mutable.Map[String, Int]   = mutable.Map.empty
    val objectDecls:          mutable.Map[String, Int]   = mutable.Map.empty
    // Cross-line tracking for R30: each unclosed `(`/`[` records whether it
    // looked like a formal-block opener and the indent of the line it
    // opened on. Closers on later lines pop to decide whether the bracket
    // was formal and what indent a multi-clause continuation must match.
    val bracketFormality:     mutable.Stack[(Boolean, Int)] = mutable.Stack.empty
    // The indent of the opener line of the most recently closed formal
    // block, or -1 if the previous code line did not end a formal block.
    // Lets multi-clause def signatures (`( a )\n( using b )`) recognise the
    // second clause as a continuation: leadingCols of the new line must
    // equal this value.
    var prevFormalOpenerIndent: Int                       = -1
    // The opener-line indent of the formal block whose closer ends the
    // current line (set during checkBracketInteriors, transferred to
    // `prevFormalOpenerIndent` at end of line).
    var currentFormalOpenerIndent: Int                    = -1
    // True if the previous code line started a declaration (with a keyword
    // like `def`, `val`, `given`, `class`, `extension`, etc.) or continued
    // one (a `(`/`[` continuation block at the same indent as a previous
    // signature line). Used to recognise multi-clause def signatures whose
    // first param block is on a single line, so the second clause `(
    // using ... )` on the next line is a continuation.
    var prevLineStartedDecl: Boolean                      = false
    // True if the previous code line ended with `' {` or `$ {` — a quote
    // or splice context that takes its body indented +4 (instead of the
    // usual +2) on the following line. R31 widens its allowance accordingly.
    var prevLineOpensQuoteSplice: Boolean                 = false
    // True if the previous code line was a chain method-application opener
    // — i.e. it began with `.` and ended with `:` or `=>`. The body of such
    // a chain takes indent +4 from the opener line (R31 exception).
    var prevLineOpensChain:       Boolean                 = false
    // Stack of columns of opening `{` for currently-unclosed quote/splice
    // contexts (`' {` or `$ {`). The corresponding closing `}` must appear
    // at the same column.
    val quoteSpliceBraces:        mutable.Stack[Int]      = mutable.Stack.empty
    // Indent of the first line of the current `given` declaration whose
    // signature spans multiple lines, or -1 if we are not inside one. Set
    // when a line begins with `given` (after any modifiers) and reset when
    // the body opener (`=`) is reached. Used by R32 to require that any
    // `=>` continuation line align vertically with the leading modifier or
    // `given` keyword on the first signature line.
    var givenSignatureIndent:     Int                     = -1

  private def checkLine
    ( s:       State,
      lineNum: Int,
      line:    IndexedSeq[Token],
      out:     mutable.ListBuffer[Violation] )
  :   Unit =

    val leadingWs   = line.takeWhile(t => t.kind == Kind.Space)
    val rest        = line.drop(leadingWs.length)
    val visibleLen  = line.iterator.map(_.text.length).sum
    val firstReal   = rest.headOption
    val isBlank     = firstReal.isEmpty
    val leadingCols = leadingWs.iterator.map(_.text.length).sum

    inline def emit(col: Int, rule: String, message: String): Unit =
      out += Violation(s.file, lineNum, col, rule, message)

    val isStringContent = firstReal.exists(_.kind == Kind.Strs)
    checkR2LineLength(visibleLen, emit)
    // Skip R3 inside open `(...)` blocks: continuation rows inside parameter
    // lists align under names from the opener line and may need an odd
    // number of leading spaces (e.g. under `inline commensurable` at col 18).
    if !isStringContent && s.openParens == 0 then
      checkR3IndentWidth(isBlank, leadingCols, emit)
    checkR4TrailingWs(line, emit)
    checkR31ContinuationIndent(s, leadingCols, isBlank, firstReal, emit)

    if isBlank then
      s.consecutiveBlanks += 1
      if s.consecutiveBlanks > 2 then
        emit(1, "5", "more than two consecutive blank lines")
    else s.consecutiveBlanks = 0

    s.phase match
      case Phase.License      => checkLicense(s, lineNum, line, emit)
      case Phase.Package      => checkPackage(s, lineNum, rest, emit)
      case Phase.AfterPackage => checkAfterPackage(s, isBlank, emit)
      case Phase.Imports      => checkImports(s, isBlank, firstReal, rest, emit)
      case Phase.Body         => ()

    checkTokens(s, lineNum, line, s.prevCodeLineLastTok, emit)
    checkCommas(s, lineNum, line, isBlank, out)
    checkHardSpace(rest, leadingCols, emit)
    checkChainContinuation(s, lineNum, leadingCols, isBlank, firstReal, emit)
    checkR32GivenArrowAlign(s, leadingCols, isBlank, rest, emit)
    checkReturnTypeBlank(s, lineNum, isBlank, rest, emit)
    checkMatchCases(s, lineNum, leadingCols, isBlank, rest, line, out)
    checkSiblingPadding(s, lineNum, leadingCols, isBlank, rest, out)
    checkUsingAlignment(s, lineNum, leadingCols, rest, emit)
    recordDeclarations(s, lineNum, rest)
    if !isBlank then
      s.prevLineEndedMatch = lineEndsWithMatch(rest)
      val sem = rest.filter(t => t.kind != Kind.Space && t.kind != Kind.Comment)
      sem.lastOption.foreach: t => s.prevCodeLineLastTok = t.text
      s.prevFormalOpenerIndent = s.currentFormalOpenerIndent
      // Track whether the previous line is part of a (still incomplete)
      // declaration signature. A declaration is "complete" once we see a
      // top-level `=` (assignment body) on the line. This lets multi-clause
      // signatures `def name(args)\n( using ... )` recognise the second
      // clause as a continuation, while preventing `val foo = expr\n
      // (tuple)` from being misread as one.
      val hasTopLevelEq =
        var depth = 0
        var found = false
        sem.foreach: t =>
          if t.text == "(" || t.text == "[" || t.text == "{" then depth += 1
          else if t.text == ")" || t.text == "]" || t.text == "}" then depth -= 1
          else if depth == 0 && t.text == "=" then found = true
        found

      val startsWithDecl =
        sem.headOption.exists(t => DeclKeywords.contains(t.text) || ModifierWords.contains(t.text))

      val isContinuationOfDecl =
        s.prevLineStartedDecl
          && sem.headOption.exists(t => t.text == "(" || t.text == "[")
      s.prevLineStartedDecl = !hasTopLevelEq && (startsWithDecl || isContinuationOfDecl)

      // R31 exceptions: detect quote/splice opener and chain opener on this
      // line so the next line gets a +4 (rather than +2) allowance. Also
      // track quote/splice brace columns for the closing-`}`-alignment check.
      s.prevLineOpensQuoteSplice = lineEndsWithQuoteSpliceBrace(sem)
      s.prevLineOpensChain       = lineIsChainOpener(sem)
      trackQuoteSpliceBraces(s, line, leadingCols, lineNum, out)

      // R32 anchor: a line that begins a `given` declaration (after any
      // modifiers) records its leading-cols as the anchor for arrow
      // continuation. The anchor clears when the body opener (`=`) appears
      // at top level or when an unrelated declaration begins.
      val (_, kwIdx) = skipModifiers(sem, 0)
      val startsGiven =
        kwIdx < sem.length && sem(kwIdx).kind == Kind.Code && sem(kwIdx).text == "given"
      if startsGiven then s.givenSignatureIndent = leadingCols
      else if s.givenSignatureIndent >= 0 && hasTopLevelEq then s.givenSignatureIndent = -1
      else if s.givenSignatureIndent >= 0 && !sem.headOption.exists(_.text == "=>") then
        // Any line that's neither an `=>` continuation nor part of the
        // initial signature ends the given-signature region.
        if !startsWithDecl && !isContinuationOfDecl then s.givenSignatureIndent = -1

    if isBlank then
      if s.prevWasAnnotation then
        emit
          ( 1, "15.2",
            "blank line is not permitted between an annotation and the declaration it annotates" )
        s.prevWasAnnotation = false
    else
      s.prevWasAnnotation = lineStartsAnnotation(firstReal)
      s.prevLineNum = lineNum
      // Comment-only and annotation-only lines belong to the next declaration:
      // they must not update `prevCodeLineIndent`, otherwise sibling-scope
      // detection mis-classifies the next declaration as a same-scope sibling
      // of whatever appeared before the comment.
      val isCommentOnly = firstReal.exists(_.kind == Kind.Comment)
      val isAnnotationOnly = lineStartsAnnotation(firstReal)
      // Continuation lines inside a multi-line triple-quoted string are
      // tokenised as a single Strs token with no leading Space token, so
      // their leadingCols is always 0. Don't let that corrupt the indent
      // state used by chain-continuation / bracket / sibling / R31 checks.
      // A normal code line that *starts* with a string interpolation (e.g.
      // `sh"…".exec()`) does have a leading Space token and is not affected.
      val isStringContinuation =
        firstReal.exists(_.kind == Kind.Strs) && leadingWs.isEmpty
      if !isCommentOnly && !isAnnotationOnly && !isStringContinuation then
        s.prevCodeLineIndent = leadingCols

    s.prevLineWasBlank = isBlank

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
            ( file, line, col, "1", "tab character is not permitted; use spaces" )
      if ch == '\n' then
        line += 1
        col = 1
      else col += 1
      i += 1

  private def checkR2LineLength(visibleLen: Int, emit: (Int, String, String) => Unit): Unit =
    if visibleLen > MaxColumns then
      emit
        ( MaxColumns + 1, "2",
          s"line exceeds 100 columns (is $visibleLen columns)" )

  private def checkR3IndentWidth
    ( isBlank: Boolean, leadingCols: Int, emit: (Int, String, String) => Unit )
  :   Unit =

    if !isBlank && leadingCols%2 != 0 then
      emit(1, "3", s"indent width $leadingCols is not a multiple of 2")

  // R31: a non-blank code line cannot be indented more than two columns
  // deeper than the previous code line. This forbids "alignment" indents
  // that line up under a name on the previous line — when a continuation
  // needs deeper indent, the previous line should be split so the rule
  // is satisfied by ordinary +2 stepping.
  //
  // Two exceptions allow +4 instead of +2:
  //   - the previous line opened a quote or splice context (`' {` or `$ {`
  //     as its trailing tokens);
  //   - the previous line is a chain method-application opener — it begins
  //     with `.` and ends with `:` or `=>` (e.g. `. within:` or
  //     `. map: x =>`).
  //
  // Suspended when the line is inside an unclosed `(`, `[`, or `{` from an
  // earlier line: alignment inside parameter / selector lists (e.g. the
  // multi-line `import …. { A, B, /\n      C, D }` and `export` patterns)
  // is governed by R12/R22/R30 and the surrounding bracket structure.
  private def checkR31ContinuationIndent
    ( s:           State,
      leadingCols: Int,
      isBlank:     Boolean,
      firstReal:   Option[Token],
      emit:        (Int, String, String) => Unit )
  :   Unit =

    if isBlank then ()
    else if s.prevCodeLineIndent < 0 then ()
    else if s.bracketFormality.nonEmpty then ()
    else if s.quoteSpliceBraces.nonEmpty then ()
    else if s.inMultilineImport then ()
    else
      val isCommentOnly   = firstReal.exists(_.kind == Kind.Comment)
      val isStringContent = firstReal.exists(_.kind == Kind.Strs)
      if isCommentOnly || isStringContent then ()
      else
        val deepStep   = s.prevLineOpensQuoteSplice || s.prevLineOpensChain
        val step       = if deepStep then 4 else 2
        val maxAllowed = s.prevCodeLineIndent + step
        if leadingCols > maxAllowed then
          emit
            ( leadingCols + 1, "31.1",
              s"indent $leadingCols cannot exceed previous line's indent "
                +s"${s.prevCodeLineIndent} by more than $step" )

  // True if the semantic tokens of a line end with `' {` or `$ {` (the
  // opening of a quote/splice block whose body lives on the following lines).
  private def lineEndsWithQuoteSpliceBrace(sem: IndexedSeq[Token]): Boolean =
    if sem.length < 2 then false
    else
      val last = sem(sem.length - 1).text
      val prev = sem(sem.length - 2).text
      last == "{" && (prev == "'" || prev == "$")

  // True if the line is a chain method-application opener — the first
  // semantic token is `.` and the last is `:` or `=>` (e.g. `. within:` or
  // `. map: x =>`). Such lines deepen indentation by +4 rather than +2.
  private def lineIsChainOpener(sem: IndexedSeq[Token]): Boolean =
    if sem.isEmpty then false
    else
      val first = sem(0).text
      val last  = sem(sem.length - 1).text
      first == "." && (last == ":" || last == "=>")

  // Walk a line's tokens to maintain the quote/splice brace stack. The stack
  // holds an entry for every unclosed `{`: -1 for a regular brace, or the
  // column of `{` for a quote/splice opener whose body extends onto the
  // following lines (i.e. the `{` is the line's last semantic token, and
  // its matching `}` is therefore on a later line). When that `}` closes,
  // R31b checks that the closer's column matches the opener's column.
  private def trackQuoteSpliceBraces
    ( s:       State,
      line:    IndexedSeq[Token],
      ignored: Int,
      lineNum: Int,
      out:     mutable.ListBuffer[Violation] )
  :   Unit =

    val arr  = line.toArray
    val cols = scala.Array.ofDim[Int](arr.length + 1)
    cols(0) = 1
    var i = 0
    while i < arr.length do
      cols(i + 1) = cols(i) + arr(i).text.length
      i += 1
    val lastSemantic = arr.lastIndexWhere(t => t.kind != Kind.Space && t.kind != Kind.Comment)
    val stack = s.quoteSpliceBraces
    i = 0
    while i < arr.length do
      if arr(i).kind == Kind.Code then
        val text = arr(i).text
        if text == "{" then
          // A quote/splice opener that we care about is `' {` or `$ {`
          // appearing as the last semantic tokens of the line — its body
          // lives on the following lines. Inline `'{...}` patterns (case
          // patterns, type-quote patterns, single-line splices) close on
          // the same line and don't need cross-line tracking.
          val isLineEnd = i == lastSemantic
          var j = i - 1
          while j >= 0 && (arr(j).kind == Kind.Space || arr(j).kind == Kind.Comment) do j -= 1
          val precededByQuoteSplice =
            j >= 0 && arr(j).kind == Kind.Code && (arr(j).text == "'" || arr(j).text == "$")
          if isLineEnd && precededByQuoteSplice then stack.push(cols(i))
          else stack.push(-1)
        else if text == "}" then
          if stack.nonEmpty then
            val openerCol = stack.pop()
            if openerCol >= 0 && cols(i) != openerCol then
              out += Violation
                      ( s.file, lineNum, cols(i), "31.2",
                        s"closing `}` of a quote/splice block at column ${cols(i)} "
                          +s"does not align with its opening `{` at column $openerCol" )
      i += 1

  private def checkR4TrailingWs
    ( line: IndexedSeq[Token], emit: (Int, String, String) => Unit )
  :   Unit =

    line.lastOption match
      case Some(token) if token.kind == Kind.Space && token.text.length > 0 =>
        val hasNonWs = line.exists(t => t.kind != Kind.Space && t.kind != Kind.Comment)
        if hasNonWs then
          val col = line.iterator.map(_.text.length).sum - token.text.length + 1
          emit(col, "4", "line has trailing whitespace")

      case _ =>
        ()

  // R32: continuation lines of a multi-line `given` signature that begin
  // with `=>` must align vertically with the leading modifier or `given`
  // keyword on the first line of the signature.
  private def checkR32GivenArrowAlign
    ( s:           State,
      leadingCols: Int,
      isBlank:     Boolean,
      rest:        IndexedSeq[Token],
      emit:        (Int, String, String) => Unit )
  :   Unit =

    if isBlank then ()
    else if s.givenSignatureIndent < 0 then ()
    else
      val sem = rest.filter(t => t.kind != Kind.Space && t.kind != Kind.Comment)
      if sem.headOption.exists(_.text == "=>") && leadingCols != s.givenSignatureIndent then
        emit
          ( leadingCols + 1, "32",
            s"`=>` continuation of a `given` signature should align at column "
              +s"${s.givenSignatureIndent + 1} (found ${leadingCols + 1})" )

  private def checkLicense
    ( s: State, lineNum: Int, line: IndexedSeq[Token], emit: (Int, String, String) => Unit )
  :   Unit =

    val text = line.iterator.map(_.text).mkString

    if lineNum == 1 && !text.contains("/*") then
      emit(1, "6", "line 1 must open the license-header block comment with `/*`")

    if lineNum == 32 then
      if !text.contains("*/") then
        emit(1, "6", "line 32 must close the license-header block comment with `*/`")
      s.phase = Phase.Package

  private def checkPackage
    ( s:       State,
      lineNum: Int,
      rest:    IndexedSeq[Token],
      emit:    (Int, String, String) => Unit )
  :   Unit =

    if lineNum != PackageLine then
      emit
        ( 1, "7",
          s"expected `package` declaration on line 33, found content on line $lineNum" )
      s.phase = Phase.Body
    else
      val nonWs = rest.filter(t => t.kind != Kind.Space && t.kind != Kind.Comment).toList
      nonWs match
        case keyword :: ident :: tail
          if keyword.text == "package" && ident.kind == Kind.Code =>

          if !ident.text.matches("[A-Za-z_][A-Za-z0-9_]*") then
            emit
              ( 1, "7",
                s"package declaration must be a single identifier segment, not `${ident.text}`" )

          s.expectedModule.foreach: expected =>
            if ident.text != expected then
              emit
                ( 1, "7",
                  s"package `${ident.text}` does not match expected module `$expected`" )

          if tail.nonEmpty then
            emit
              ( 1, "7",
                "package declaration must contain only `package <ident>` on line 33" )

        case _ =>
          emit(1, "7", "line 33 must be `package <module>`")

      s.phase = Phase.AfterPackage

  private def checkAfterPackage
    ( s: State, isBlank: Boolean, emit: (Int, String, String) => Unit )
  :   Unit =

    if !isBlank then
      emit(1, "8", "line 34 must be a single blank line after `package`")

    s.phase = Phase.Imports

  private def checkImports
    ( s:         State,
      isBlank:   Boolean,
      firstReal: Option[Token],
      rest:      IndexedSeq[Token],
      emit:      (Int, String, String) => Unit )
  :   Unit =

    if isBlank then s.inMultilineImport = false
    else if s.inMultilineImport then
      // Continuation of a multi-line `import …,\n  more, more` or
      // `import …{a, b,\n  c}` statement. Update state and skip checks.
      s.inMultilineImport = importContinues(rest)
    else firstReal.foreach: head =>
      if head.text == "import" then
        val path  = importPath(rest)
        val group = classifyImport(path)
        // Top-level aliases are forbidden only for soundness libs (group 4
        // and 5). Standard-library aliases (`import scala.collection.mutable
        // as scm`, `import java.util.concurrent as juc`, etc.) and
        // language-feature aliases are an established convention.
        if importHasAlias(rest) && group >= 5 then
          emit
            ( 1, "9.1",
              "top-level imports must not use aliases (`as` or `=>`); write the full path" )

        s.prevImportGroup match
          case Some(prevGroup) =>
            // Groups 5 and 6 are "third-party" siblings: lowercase wildcard
            // (`import contingency.*`) and other named imports
            // (`import filesystemOptions.x`, `import AsyncError.Reason`)
            // routinely interleave in soundness code, so we don't require
            // ordering or blank lines between them. Alphabetical and
            // blank-within-group checks still apply within strictly the same
            // group (5 with 5, 6 with 6).
            val areSiblings = group >= 5 && prevGroup >= 5
            if !areSiblings && group < prevGroup then
              emit
                ( 1, "9.2",
                  s"import group $group appears after group $prevGroup" )
            else if !areSiblings && group > prevGroup then
              if !s.prevLineWasBlank then
                emit
                  ( 1, "9.3",
                    "import groups must be separated by exactly one blank line" )
            else if group == prevGroup then
              if s.prevLineWasBlank then
                emit
                  ( 1, "9.3",
                    "unexpected blank line within an import group" )
              s.prevImportName.foreach: prevName =>
                if path < prevName then
                  emit
                    ( 1, "9.2",
                      s"import `$path` is out of alphabetical order (after `$prevName`)" )

          case None => ()

        s.prevImportGroup = Some(group)
        s.prevImportName  = Some(path)
        s.inMultilineImport = importContinues(rest)
      else
        if !s.prevLineWasBlank && s.prevImportGroup.isDefined then
          emit
            ( 1, "10",
              "missing blank line between imports and first declaration" )
        s.phase = Phase.Body

  private def importContinues(rest: IndexedSeq[Token]): Boolean =
    val nonWs = rest.filter(t => t.kind != Kind.Space && t.kind != Kind.Comment)
    if nonWs.isEmpty then false
    else
      val opens  = nonWs.count(_.text == "{")
      val closes = nonWs.count(_.text == "}")
      val unbalanced = opens > closes
      val endsWithComma = nonWs.last.text == ","
      unbalanced || endsWithComma

  // Detects top-level `as` / `=>` aliases on an import statement, e.g.
  // `import scala.collection.immutable as sci`. Selector-renaming inside
  // braces (`import java.nio.file.{Files, Path as JPath}`) is permitted —
  // an `as` token at depth > 0 (inside `{…}`) does not count.
  private def importHasAlias(rest: IndexedSeq[Token]): Boolean =
    var depth = 0
    var found = false
    rest.foreach: t =>
      if t.kind == Kind.Code then
        if t.text == "{" then depth += 1
        else if t.text == "}" then depth -= 1
        else if depth == 0 && (t.text == "as" || t.text == "=>") then found = true
    found

  private def importPath(rest: IndexedSeq[Token]): String =
    val nonWs = rest.filter(t => t.kind != Kind.Space && t.kind != Kind.Comment).toList
    nonWs match
      case _ :: tail =>
        // Stop at a top-level `as` (rename of the whole import). An `as`
        // inside braces (`{Float as _, *}`) is a selector rename and is
        // part of the import path.
        val sb    = new StringBuilder
        var depth = 0
        var done  = false
        tail.foreach: t =>
          if !done then
            if t.text == "{" then { depth += 1; sb.append(t.text) }
            else if t.text == "}" then { depth -= 1; sb.append(t.text) }
            else if depth == 0 && t.text == "as" then done = true
            else sb.append(t.text)
        sb.toString

      case _ =>
        ""

  private def classifyImport(path: String): Int =
    // For multi-import lines (`import a.*, b.x, c.y`), classify by the first
    // import only — additional comma-separated imports are sub-imports of
    // the same group. A braced wildcard `{… , *}` counts as a wildcard
    // import (group 4), since the `*` selector still pulls in everything.
    // Stop the first-import scan at the first top-level `,` (commas inside
    // `{…}` are part of the same import).
    val firstImport =
      val sb = new StringBuilder
      var depth = 0
      var done  = false
      path.foreach: c =>
        if !done then
          if c == '{' then { depth += 1; sb.append(c) }
          else if c == '}' then { depth -= 1; sb.append(c) }
          else if depth == 0 && c == ',' then done = true
          else sb.append(c)
      sb.toString

    val firstSegment = firstImport.takeWhile(c => c != '.' && c != '{' && c != ' ')

    val wildcardImport =
      firstImport.endsWith(".*") || firstImport.endsWith("*}") || firstImport.endsWith("*, *}")
    firstSegment match
      case "language"        => 1
      case "java" | "javax"  => 2
      case "scala"           => 3
      // Compiler / JVM-internals and JEE: dotty (compiler API), `com.sun.*`
      // (Oracle JVM internals), `sun.*` (raw JVM internals), and `jakarta.*`
      // (JEE). These are alias-friendly like the JDK but conceptually
      // distinct from `scala.*`, so they form their own group.
      case "dotty" | "com" | "sun" | "jakarta" => 4

      case _ =>
        if firstSegment.headOption.exists(_.isUpper) then 6
        else if !wildcardImport then 6
        else 5

  private def checkTokens
    ( s:       State,
      lineNum: Int,
      line:    IndexedSeq[Token],
      prevTok: String,
      emit:    (Int, String, String) => Unit )
  :   Unit =

    val arr  = line.toArray
    val cols = scala.Array.ofDim[Int](arr.length + 1)
    cols(0) = 1
    var i = 0
    while i < arr.length do
      cols(i + 1) = cols(i) + arr(i).text.length
      i += 1

    // Comma checks need state for alignment-run detection.
    // We pass them through directly via the State held in the closure.
    checkBracketInteriors(s, arr, cols, prevTok, emit)
    checkComments(lineNum, arr, cols, emit)
    checkOperatorSpacing(arr, cols, emit)
    checkSymbolicMethodNames(arr, cols, emit)

  private def checkCommas
    ( s:       State,
      lineNum: Int,
      line:    IndexedSeq[Token],
      isBlank: Boolean,
      out:     mutable.ListBuffer[Violation] )
  :   Unit =

    val arr  = line.toArray
    val cols = scala.Array.ofDim[Int](arr.length + 1)
    cols(0) = 1
    var k = 0
    while k < arr.length do
      cols(k + 1) = cols(k) + arr(k).text.length
      k += 1

    val deferred = mutable.ListBuffer[Violation]()
    var hasAlignment = false

    var i = 0
    while i < arr.length do
      if arr(i).text == "," && arr(i).kind == Kind.Code then
        if i > 0 && arr(i - 1).kind == Kind.Space && arr(i - 1).text.length > 0 then
          out +=
            Violation
              ( s.file, lineNum, cols(i), "11.1",
                "no space is permitted before a comma" )

        if i + 1 < arr.length then
          val next = arr(i + 1)
          if next.kind != Kind.Space then
            out +=
              Violation
                ( s.file, lineNum, cols(i + 1), "11.2",
                  "exactly one space is required after a comma" )
          else if next.text != " " then
            // Extra spaces after comma — possibly an alignment-run column.
            // Defer until we know whether neighbouring lines also have alignment.
            hasAlignment = true
            if !next.text.startsWith("\n") then
              deferred +=
                Violation
                  ( s.file, lineNum, cols(i + 1), "11.2",
                    "exactly one space is required after a comma" )
      i += 1

    // Validate previously deferred (from line N-1) against current line:
    // if current line also has alignment, the previous run continues — suppress.
    if hasAlignment then s.pendingR11 = Nil
    else
      s.pendingR11.foreach(out += _)
      s.pendingR11 = Nil

    // Current candidates: keep if previous line also had alignment (then run is
    // valid by both directions). Otherwise defer to next line.
    if s.prevLineHadAlignment && hasAlignment then
      // Already in a run — current line's deferred violations are part of run.
      ()
    else if hasAlignment then
      // Need to confirm with next line.
      s.pendingR11 = deferred.toList
    else
      // No alignment on current — emit any candidates immediately.
      deferred.foreach(out += _)

    if !isBlank then s.prevLineHadAlignment = hasAlignment
    else s.prevLineHadAlignment = false

  private def checkBracketInteriors
    ( s:       State,
      arr:     Array[Token],
      cols:    Array[Int],
      prevTok: String,
      emit:    (Int, String, String) => Unit )
  :   Unit =

    val firstSemantic = arr.indexWhere(t => t.kind != Kind.Space && t.kind != Kind.Comment)
    val lastSemantic  = arr.lastIndexWhere(t => t.kind != Kind.Space && t.kind != Kind.Comment)

    val lineStartsWithBracket =
      firstSemantic >= 0 && (arr(firstSemantic).text == "(" || arr(firstSemantic).text == "[")

    val secondSemantic =
      if firstSemantic < 0 then -1
      else arr.indexWhere(t => t.kind != Kind.Space && t.kind != Kind.Comment, firstSemantic + 1)

    val arrowParen =
      firstSemantic >= 0 && arr(firstSemantic).text == "=>"
        && secondSemantic > 0
        && (arr(secondSemantic).text == "(" || arr(secondSemantic).text == "[")

    // A line beginning with `:` followed by `(`/`[` is a heavy-signature
    // continuation (typically anonymous given chains: `:   [ ... ]\n  =>  (
    // ... )\n  =>  ...`). Treat the bracket as a formal block candidate.
    val colonParen =
      firstSemantic >= 0 && arr(firstSemantic).text == ":"
        && secondSemantic > 0
        && (arr(secondSemantic).text == "(" || arr(secondSemantic).text == "[")

    // A line-start opener is a "formal-block candidate" when it continues a
    // declaration or method call from the previous line. Two sub-cases:
    //
    //   1. **Indent step**: the current line is more indented than the prev
    //      code line, AND prev line's last token is application-like
    //      (identifier, `)`, `]`, modifier keyword). This catches the first
    //      `(` of a heavy signature: `def name\n  ( a, b )`.
    //
    //   2. **Multi-clause continuation**: the current line is at the same
    //      indent as the prev line, AND the prev line ended with a formal
    //      `)`/`]` (i.e. the prev line was itself a formal-block clause).
    //      This catches subsequent param clauses: `( a )\n( using b )`.
    //
    // A blank line before the opener rules out continuation in either case.
    val leadingCols = arr.takeWhile(_.kind == Kind.Space).iterator.map(_.text.length).sum
    val moreIndentedThanPrev = leadingCols > s.prevCodeLineIndent
    val sameIndentAsPrev = leadingCols == s.prevCodeLineIndent

    val isMultiClauseContinuation =
      (prevTok == ")" || prevTok == "]") && (
        // Continuation of a previous formal block (multi-line `( ... )`):
        // align under that block's opener line.
        (s.prevFormalOpenerIndent >= 0 && leadingCols == s.prevFormalOpenerIndent)
          // Continuation of a declaration signature (`def name(args)\n(
          // using ... )`): prev line started with a declaration keyword and
          // is at the same indent as this line.
          || (s.prevLineStartedDecl && leadingCols == s.prevCodeLineIndent)
      )

    // A `(`/`[` at the start of a line whose preceding line ended with `=>`
    // is a lambda or context-function body. The brackets are a tuple
    // expression, not a method application — skip both R30 and R12 by
    // suppressing the rule entirely for the line.
    val isArrowBodyContinuation = prevTok == "=>" && !s.prevLineWasBlank

    val prevIsApplication =
      if s.prevLineWasBlank then false
      else if isMultiClauseContinuation then true
      else if !moreIndentedThanPrev then false
      else if prevTok == ")" || prevTok == "]" then true
      else if prevTok.isEmpty then false
      else if ModifierWords.contains(prevTok) then true
      else
        val c = prevTok.head
        (c.isLetter || c == '_' || c == '`') && !NonOperandWords.contains(prevTok)

    val stack = mutable.Stack[(Int, Boolean)]()
    s.currentFormalOpenerIndent = -1
    var i = 0
    while i < arr.length do
      if arr(i).kind == Kind.Code then
        val text = arr(i).text
        if text == "(" || text == "[" then
          val isLineStartOpener = lineStartsWithBracket && i == firstSemantic
          val isArrowOpener     = arrowParen && i == secondSemantic
          val isColonOpener     = colonParen && i == secondSemantic
          // Line-start `(`/`[` requires an application-like predecessor.
          // An `=> (`/`=> [` opener after a leading `=>` is always formal —
          // the `=>` itself signals continuation (e.g. anonymous-given
          // chains, `=>  ( param: Type )`).
          // A `:   [`/`:   (` after a leading `:` is a heavy-signature
          // continuation (anonymous given chains starting with type params
          // or context params).

          val formalCandidate =
            (isLineStartOpener && prevIsApplication) || isArrowOpener || isColonOpener
          stack.push((i, formalCandidate))
        else if text == ")" || text == "]" then
          if stack.nonEmpty then
            val (opener, formalCandidate) = stack.pop()
            val closer = i

            // Formal block: opener is a candidate AND the closer is the
            // line's last semantic token or only followed by a body opener.
            val nextAfterCloser =
              arr.indexWhere(t => t.kind != Kind.Space && t.kind != Kind.Comment, closer + 1)

            // For colon-opener brackets (`:   ( ... )` / `:   [ ... ]`) the
            // bracket is a formal context-parameter / type-parameter list
            // only when its closer is the line's last semantic token (the
            // chain continues on the next line) or is immediately followed
            // by an arrow `=>` (anonymous given continuation). Anything
            // else — `=`, `throws`, `~`, etc. — means the bracket is part
            // of the return type expression itself. Additionally a `(...)`
            // colon-opener bracket must contain a `name: type` annotation
            // (top-level `:`) — otherwise it is a tuple type, not a
            // context-parameter list.
            val isColonOpenerBracket = colonParen && opener == secondSemantic

            val colonParenBracketIsParamList =
              !isColonOpenerBracket
                || arr(opener).text == "[" || bracketHasTopColon(arr, opener, closer)

            val followedByBodyOpener =
              if !colonParenBracketIsParamList then false
              else if nextAfterCloser < 0 then true
              else
                val t = arr(nextAfterCloser).text
                if isColonOpenerBracket then t == "=>"
                else
                  t == ":" || t == "=" || t == "extends" || t == "derives"
                    || t == ")" || t == "]" || t == "}" || t == ","

            // Inspect contents at depth 0 (relative to this bracket): if
            // there is no top-level comma but there is at least one binary
            // operator, the brackets are a grouping expression, not an
            // argument list — R30 does not apply.
            val hasArgListShape = bracketHasArgListShape(arr, opener, closer)
            val isFormalBlock = formalCandidate && followedByBodyOpener && hasArgListShape
            if isFormalBlock then
              if nextAfterCloser < 0 then s.currentFormalOpenerIndent = leadingCols
              if closer > opener + 1 then
                val firstInside = arr(opener + 1)
                if firstInside.kind != Kind.Space then
                  emit
                    ( cols(opener + 1), "30",
                      s"a space is required after `${arr(opener).text}` in a multi-line block" )
                val lastInside = arr(closer - 1)
                if lastInside.kind != Kind.Space then
                  emit
                    ( cols(closer), "30",
                      s"a space is required before `$text` in a multi-line block" )
            else if closer > opener + 1 then
              // Suppress R12 for tuples appearing as a lambda/match-case
              // body on a fresh line after `=>` — author may use either
              // tight `(a, b)` or formal-style `( a, b )`.
              val isLambdaBodyTuple =
                isArrowBodyContinuation && opener == firstSemantic && closer == lastSemantic
              if !isLambdaBodyTuple then
                val firstInside = arr(opener + 1)
                if firstInside.kind == Kind.Space && firstInside.text.length > 0 then
                  emit
                    ( cols(opener + 1), "12",
                      s"no space is permitted directly after `${arr(opener).text}`" )
                val lastInside = arr(closer - 1)
                if lastInside.kind == Kind.Space && lastInside.text.length > 0
                  && (closer - 1) != (opener + 1)
                then
                  emit
                    ( cols(closer - 1), "12",
                      s"no space is permitted directly before `$text`" )
          else
            // Closer with no opener on this line: the opener was on an
            // earlier line. Pop its formal-candidate flag and opener-line
            // indent from the cross-line stack and only fire R30 if it was a
            // formal candidate.
            val (wasFormal, openerIndent) =
              if s.bracketFormality.nonEmpty then s.bracketFormality.pop() else (false, -1)
            if wasFormal then
              val nextAfterCloser =
                arr.indexWhere(t => t.kind != Kind.Space && t.kind != Kind.Comment, i + 1)
              if nextAfterCloser < 0 then s.currentFormalOpenerIndent = openerIndent
              if i == firstSemantic then
                emit
                  ( cols(i), "30",
                    s"`$text` cannot appear alone on its line; the closing bracket of a "
                      +s"multi-line block goes on the same line as the last parameter" )
              else if i > 0 && arr(i - 1).kind != Kind.Space then
                emit
                  ( cols(i), "30",
                    s"a space is required before `$text` in a multi-line block" )
      i += 1

    // Any opener left on the stack didn't match on this line: multi-line
    // opener. Push its formality flag and opener-line indent onto the
    // cross-line stack so the closer (on a later line) can decide whether to
    // fire R30 and what indent a multi-clause continuation must match.
    val leftover = stack.toList.reverse
    leftover.foreach: (opener, formalCandidate) =>
      val openerText = arr(opener).text
      if formalCandidate then
        if opener + 1 >= arr.length || arr(opener + 1).kind != Kind.Space then
          emit
            ( cols(opener + 1), "30",
              s"a space is required after `$openerText` in a multi-line block" )
      s.bracketFormality.push((formalCandidate, leadingCols))

  // Single-line bracket between `opener` and `closer` (exclusive). Returns
  // true iff the contents look like an argument list: a top-level comma or
  // type-annotation `:` is present, or there are no top-level binary
  // operators. Returns false when the contents are a pure grouping
  // expression (no commas, no `:`, has operators like `||`, `&&`, `==`,
  // `+`, etc.).
  private def bracketHasArgListShape
    ( arr: Array[Token], opener: Int, closer: Int )
  :   Boolean =

    var depth = 0
    var hasTopComma = false
    var hasTopColon = false
    var hasTopOperator = false
    var k = opener + 1
    while k < closer do
      val t = arr(k)
      if t.kind == Kind.Code then
        val text = t.text
        if text == "(" || text == "[" then depth += 1
        else if text == ")" || text == "]" then depth -= 1
        else if depth == 0 then
          if text == "," then hasTopComma = true
          else if text == ":" then hasTopColon = true
          else if isExpressionOperator(text) then hasTopOperator = true
      k += 1
    hasTopComma || hasTopColon || !hasTopOperator

  // Operators that almost exclusively appear in expression position, not in
  // type signatures or argument annotations: arithmetic, comparison, boolean.
  // Type operators (`<:`, `>:`, `&`, `|`, `=>`, `?=>`, `+:`, `:+`, etc.) are
  // intentionally excluded — they appear in parameter lists too.
  private val ExpressionOperators: Set[String] =
    Set("||", "&&", "==", "!=", "<=", ">=", "<", ">", "+", "-", "*", "/", "%")

  private def isExpressionOperator(t: String): Boolean = ExpressionOperators.contains(t)

  // True iff the bracket between `opener` and `closer` (exclusive) contains
  // a top-level `:` token at depth 0 — indicating a `name: Type`
  // annotation (parameter list). Used to distinguish `:   ( a: T )`
  // (param list) from `:   (T1, T2)` (tuple type).
  private def bracketHasTopColon
    ( arr: Array[Token], opener: Int, closer: Int )
  :   Boolean =

    var depth = 0
    var found = false
    var k = opener + 1
    while k < closer && !found do
      val t = arr(k)
      if t.kind == Kind.Code then
        val text = t.text
        if text == "(" || text == "[" then depth += 1
        else if text == ")" || text == "]" then depth -= 1
        else if depth == 0 && text == ":" then found = true
      k += 1
    found

  private def checkComments
    ( lineNum: Int,
      arr:     Array[Token],
      cols:    Array[Int],
      emit:    (Int, String, String) => Unit )
  :   Unit =

    val inLicense = lineNum >= 1 && lineNum <= 32
    var i = 0
    while i < arr.length do
      if arr(i).kind == Kind.Comment then
        val text = arr(i).text
        if text.startsWith("/**") then
          emit
            ( cols(i), "14.2",
              "`/** ... */` block comments are not permitted; use `doc/` markdown instead" )
        else if text.startsWith("/*") && !inLicense then
          emit
            ( cols(i), "14.1",
              "`/* ... */` block comments are reserved for the license header (lines 1-32)" )
      i += 1

  private val CheckedOps: Set[String] = Set
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

  private val NonOperandWords: Set[String] = Set
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
  private val BoundaryWords: Set[String] = Set
    ( "if", "then", "else", "match", "case", "do", "while", "for", "yield",
      "return", "throw", "try", "catch", "finally" )

  private def caseIsModifier(arr: Array[Token], i: Int): Boolean =
    var j = i + 1
    while j < arr.length && (arr(j).kind == Kind.Space || arr(j).kind == Kind.Comment) do
      j += 1
    j < arr.length && arr(j).kind == Kind.Code
      && (arr(j).text == "class" || arr(j).text == "object")

  private def isBinaryContext(arr: Array[Token], i: Int): Boolean =
    val left =
      var j = i - 1
      while j >= 0 && (arr(j).kind == Kind.Space || arr(j).kind == Kind.Comment) do j -= 1
      if j < 0 then false
      else if arr(j).kind == Kind.Strs then true
      else if arr(j).kind == Kind.Code then
        val t = arr(j).text
        if t == ")" || t == "]" then true
        else if t.isEmpty then false
        else
          val c = t.head
          (c.isLetterOrDigit || c == '_' || c == '`') && !NonOperandWords.contains(t)
      else false

    val right =
      var j = i + 1
      while j < arr.length && (arr(j).kind == Kind.Space || arr(j).kind == Kind.Comment) do j += 1
      if j >= arr.length then false
      else if arr(j).kind == Kind.Strs then true
      else if arr(j).kind == Kind.Code then
        val t = arr(j).text
        // The next token must look like an operand (not a closing bracket or
        // separator). This excludes postfix usages like `xs*`, `tuple*`, etc.
        if t == ")" || t == "]" || t == "}" || t == "," || t == ";" then false
        else if t.isEmpty then false
        else
          val c = t.head
          (c.isLetterOrDigit || c == '_' || c == '`' || c == '"' || c == '\'')
            && !NonOperandWords.contains(t)
      else false

    left && right

  private def checkOperatorSpacing
    ( arr:  Array[Token],
      cols: Array[Int],
      emit: (Int, String, String) => Unit )
  :   Unit =

    val firstSemantic = arr.indexWhere(t => t.kind != Kind.Space && t.kind != Kind.Comment)
    val lastSemantic  = arr.lastIndexWhere(t => t.kind != Kind.Space && t.kind != Kind.Comment)
    val frames = mutable.Stack[mutable.ArrayBuffer[OpHit]]()
    frames.push(mutable.ArrayBuffer.empty)

    var i = 0
    while i < arr.length do
      val tok = arr(i)
      if tok.kind == Kind.Code then
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
          val leftSpace  = i > 0 && arr(i - 1).kind == Kind.Space
          val rightSpace = i + 1 < arr.length && arr(i + 1).kind == Kind.Space
          if isBinary then
            // Symmetry: if both edges have a token of the same kind around, the
            // spaces should be matched. Skip at line edges.
            if !isAtStart && !isAtEnd && leftSpace != rightSpace then
              emit
                ( cols(i), "16",
                  s"`$text` has asymmetric spacing — use 0 or 1 space on both sides" )
            // Multi-char operators must have one space around (zero is reserved
            // for single-character operators).
            if text.length > 1 && !isAtStart && !isAtEnd
              && (!leftSpace || !rightSpace)
            then
              emit
                ( cols(i), "16",
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

  private def checkOpFrame
    ( ops: mutable.ArrayBuffer[OpHit], emit: (Int, String, String) => Unit )
  :   Unit =

    if ops.isEmpty then ()
    else
      // Same-precedence consistency: every operator at a given precedence
      // must use the same spacing within this frame.
      ops.groupBy(op => operatorPrecedence(op.text)).foreach: (_, group) =>
        val mixed = group.exists(op => op.leftSpace || op.rightSpace)
          && group.exists(op => !(op.leftSpace || op.rightSpace))
        if mixed then
          group.foreach: op =>
            emit
              ( op.col, "16",
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
              ( op.col, "16",
                s"`${op.text}` cannot have more spacing than lower-precedence "
                  +"operators in the same expression" )

  private def isSymbolicOperator(text: String): Boolean =
    text.nonEmpty && text.forall: c =>
      c match
        case '+' | '-' | '*' | '/' | '%' | '&' | '|' | '^' | '~' => true
        case '<' | '>' | '=' | '!' | '?' | ':' | '@' | '#'       => true
        case _                                                   => false

  private def checkSymbolicMethodNames
    ( arr:  Array[Token],
      cols: Array[Int],
      emit: (Int, String, String) => Unit )
  :   Unit =

    var i = 0
    while i < arr.length do
      if arr(i).kind == Kind.Code && arr(i).text == "def" then
        var j = i + 1
        while j < arr.length && (arr(j).kind == Kind.Space || arr(j).kind == Kind.Comment) do
          j += 1
        if j < arr.length && arr(j).kind == Kind.Code && isSymbolicOperator(arr(j).text) then
          val opIdx = j
          val nextIdx = opIdx + 1
          if nextIdx < arr.length && arr(nextIdx).kind == Kind.Code
            && (arr(nextIdx).text == "(" || arr(nextIdx).text == "[")
          then
            emit
              ( cols(nextIdx), "18",
                s"a single space is required between `${arr(opIdx).text}` and "
                  +s"`${arr(nextIdx).text}`" )
      i += 1

  private def checkHardSpace
    ( rest: IndexedSeq[Token], leadingCols: Int, emit: (Int, String, String) => Unit )
  :   Unit =

    rest.headOption match
      case Some(tok) if tok.text == "=>" =>
        if rest.length >= 2 then
          val next = rest(1)
          if next.kind != Kind.Space || next.text != "  " then
            emit
              ( leadingCols + 3, "25",
                "`=>` continuation line must be followed by exactly two spaces" )

      case Some(tok) if tok.text == ":" && lineEndsWithEqualsToken(rest) =>
        if rest.length >= 2 then
          val next = rest(1)
          if next.kind != Kind.Space || next.text != "   " then
            emit
              ( leadingCols + 2, "25",
                "heavy-signature return type `:` must be followed by exactly three spaces" )
      case _ => ()

  private def lineEndsWithEqualsToken(rest: IndexedSeq[Token]): Boolean =
    val nonWs = rest.filter(t => t.kind != Kind.Space && t.kind != Kind.Comment)
    nonWs.lastOption.exists(_.text == "=")

  private def checkChainContinuation
    ( s:           State,
      lineNum:     Int,
      leadingCols: Int,
      isBlank:     Boolean,
      firstReal:   Option[Token],
      emit:        (Int, String, String) => Unit )
  :   Unit =

    if isBlank then ()
    else firstReal.foreach: tok =>
      if tok.text == "." && s.prevCodeLineIndent >= 0 then
        if s.prevCodeLineIndent > leadingCols && !s.prevLineWasBlank then
          emit
            ( leadingCols + 1, "26.1",
              "blank line is required before `. method` continuation following a "
                +"more-indented line" )
        else if s.prevCodeLineIndent == leadingCols && s.prevLineWasBlank then
          emit
            ( leadingCols + 1, "26.2",
              "no blank line is permitted before `. method` continuation at the same indent" )

  private def checkReturnTypeBlank
    ( s:       State,
      lineNum: Int,
      isBlank: Boolean,
      rest:    IndexedSeq[Token],
      emit:    (Int, String, String) => Unit )
  :   Unit =

    if s.prevWasReturnType then
      if !isBlank then
        emit
          ( 1, "21",
            "a blank line is required between a heavy-signature return type and the body" )
      s.prevWasReturnType = false
    if !isBlank && isReturnTypeLine(rest) then s.prevWasReturnType = true

  private def isReturnTypeLine(rest: IndexedSeq[Token]): Boolean =
    rest.length >= 2 && rest(0).text == ":" && rest(1).kind == Kind.Space
      && rest(1).text == "   " && rest.lastOption.exists(_.text == "=")

  private def recordDeclarations(s: State, lineNum: Int, rest: IndexedSeq[Token]): Unit =
    val nonWs = rest.filter(t => t.kind != Kind.Space && t.kind != Kind.Comment)
    if nonWs.nonEmpty then
      val (kw, idx) = skipModifiers(nonWs, 0)
      if idx < nonWs.length then
        val keyword = nonWs(idx).text
        val nameIdx = idx + 1
        if nameIdx < nonWs.length && nonWs(nameIdx).kind == Kind.Code then
          val name = nonWs(nameIdx).text
          if keyword == "class" || keyword == "trait" || keyword == "enum" then
            if !s.typeDecls.contains(name) then s.typeDecls(name) = lineNum
          else if keyword == "object" then
            if !s.objectDecls.contains(name) then s.objectDecls(name) = lineNum

  private val ModifierWords: Set[String] = Set
    ( "private", "protected", "public", "final", "sealed", "abstract",
      "implicit", "lazy", "override", "case", "inline", "transparent",
      "infix", "open", "opaque", "erased", "tracked", "given" )

  private def skipModifiers(tokens: IndexedSeq[Token], start: Int): (Option[String], Int) =
    var i = start
    var lastModifier: Option[String] = None
    while i < tokens.length && tokens(i).kind == Kind.Code
      && ModifierWords.contains(tokens(i).text)
      && tokens(i).text != "given"
    do
      lastModifier = Some(tokens(i).text)
      i += 1
    (lastModifier, i)

  private def checkCompanionOrdering
    ( file: String, s: State, out: mutable.ListBuffer[Violation] )
  :   Unit =

    s.objectDecls.foreach: (name, objLine) =>
      s.typeDecls.get(name).foreach: typeLine =>
        if objLine > typeLine then
          out +=
            Violation
              ( file, objLine, 1, "28",
                s"object `$name` must appear before class/trait/enum `$name`" )

  private def checkFileNaming(file: String, out: mutable.ListBuffer[Violation]): Unit =
    val segments = file.split("/").nn
    val name     = segments(segments.length - 1).nn
    if !name.endsWith(".scala") then ()
    else
      val base   = name.substring(0, name.length - ".scala".length).nn
      val parts  = file.split("/lib/").nn
      val module = if parts.length < 2 then None else Some(parts(1).nn.split("/").nn(0).nn)
      val ok = module.fold(true): m =>
        base == s"${m}_core"
        || base == s"soundness_${m}_core"
        || base.startsWith(s"$m.")
        || base.startsWith(s"soundness_") && base.endsWith("_core")
        || base.startsWith(s"${m}_") && base.endsWith("_core")
        || isCrossModuleExport(base)
      if !ok then
        out +=
          Violation
            ( file, 1, 1, "29",
              s"file name `$name` does not match a documented soundness convention" )

  private def isCrossModuleExport(base: String): Boolean =
    val u = base.indexOf('_')
    val d = base.indexOf('.')
    val firstCut = List(u, d).filter(_ >= 0)
    if firstCut.isEmpty then false
    else
      val prefix = base.substring(0, firstCut.min).nn
      // Cross-module export pattern: <other-module>_<this-module>_core or <other-module>.<TypeName>
      prefix.headOption.exists(_.isLower)

  private def lineEndsWithMatch(rest: IndexedSeq[Token]): Boolean =
    val sem = rest.filter(t => t.kind != Kind.Space && t.kind != Kind.Comment)
    sem.lastOption.exists(_.text == "match")
      || (sem.length >= 2 && sem(sem.length - 2).text == "match" && sem.last.text == ":")

  private case class CaseLine(arrowCol: Int, isSingleLine: Boolean, isCase: Boolean)

  private def parseCaseLine(rest: IndexedSeq[Token], leadingCols: Int): CaseLine =
    if rest.isEmpty || rest(0).text != "case" then CaseLine(-1, false, false)
    else
      val secondSem = rest.indexWhere(t => t.kind != Kind.Space && t.kind != Kind.Comment, 1)
      if secondSem >= 0 && (rest(secondSem).text == "class" || rest(secondSem).text == "object")
      then CaseLine(-1, false, false)
      else
        val arrowIdx = rest.indexWhere(t => t.text == "=>")
        // No `=>` on this line — it's an enum case (or a match case with a
        // multi-line pattern, which is uncommon). Either way, skip both R19 and R20.
        if arrowIdx < 0 then CaseLine(-1, false, false)
        else
          var col = leadingCols + 1
          var k = 0
          while k < arrowIdx do
            col += rest(k).text.length
            k += 1
          val hasBodyAfter = rest.drop(arrowIdx + 1).exists: t =>
            t.kind != Kind.Space && t.kind != Kind.Comment
          CaseLine(col, hasBodyAfter, true)

  private def checkMatchCases
    ( s:           State,
      lineNum:     Int,
      leadingCols: Int,
      isBlank:     Boolean,
      rest:        IndexedSeq[Token],
      line:        IndexedSeq[Token],
      out:         mutable.ListBuffer[Violation] )
  :   Unit =

    inline def emit(col: Int, rule: String, msg: String): Unit =
      out += Violation(s.file, lineNum, col, rule, msg)

    if isBlank then
      finalizeCaseRun(s, out)
      return

    val info = parseCaseLine(rest, leadingCols)
    if !info.isCase then
      // Non-case line: close any active run
      finalizeCaseRun(s, out)
    else
      // Multi-line case isolation (Rule 20)
      if !info.isSingleLine then
        val isFirstInScope = s.prevCodeLineIndent < leadingCols
        if !s.prevLineWasBlank && !s.prevLineEndedMatch && !isFirstInScope then
          emit
            ( leadingCols + 1, "20",
              "a blank line is required before a multi-line case (except for the first case)" )

        // R33: a multi-line case must have exactly one space before `=>` —
        // alignment-padding only applies to runs of single-line cases (R19).
        val arrowIdx = rest.indexWhere(_.text == "=>")
        if arrowIdx > 0 then
          val before = rest(arrowIdx - 1)
          if before.kind != Kind.Space || before.text != " " then
            var c = leadingCols + 1
            var k = 0
            while k < arrowIdx do
              c += rest(k).text.length
              k += 1
            emit
              ( c, "R33-multiline-case-arrow-space",
                "exactly one space is required before `=>` in a multi-line case" )

        finalizeCaseRun(s, out)
      else
        // Single-line case
        s.caseRun match
          case Some(run) if run.indent == leadingCols && !s.prevLineWasBlank =>
            run.cases += CaseEntry(lineNum, info.arrowCol)

          case _ =>
            finalizeCaseRun(s, out)
            val newRun = CaseRun(leadingCols, mutable.ListBuffer.empty)
            newRun.cases += CaseEntry(lineNum, info.arrowCol)
            s.caseRun = Some(newRun)

  private def finalizeCaseRun(s: State, out: mutable.ListBuffer[Violation]): Unit =
    s.caseRun.foreach: run =>
      if run.cases.size >= 2 then
        val expected = run.cases.iterator.map(_.arrowCol).max
        run.cases.foreach: entry =>
          if entry.arrowCol != expected then
            out +=
              Violation
                ( s.file, entry.lineNum, entry.arrowCol, "19",
                  s"`=>` should be at column $expected to align with the case run" )
    s.caseRun = None

  private val DeclKeywords: Set[String] =
    Set("def", "val", "var", "type", "class", "trait", "object", "enum", "given")

  private def declKeywordSequence(rest: IndexedSeq[Token]): Option[String] =
    val nonWs = rest.filter(t => t.kind != Kind.Space && t.kind != Kind.Comment).toList
    val (kwsBuf, declOpt) = collectKeywords(nonWs, mutable.ListBuffer.empty)
    declOpt.map: kw =>
      (kwsBuf :+ kw).mkString(" ")

  private def collectKeywords
    ( tokens: List[Token], buf: mutable.ListBuffer[String] )
  :   (mutable.ListBuffer[String], Option[String]) =

    tokens match
      case t :: rest if t.kind == Kind.Code && ModifierWords.contains(t.text)
        && !DeclKeywords.contains(t.text) =>
        buf += t.text
        collectKeywords(rest, buf)

      case t :: _ if t.kind == Kind.Code && DeclKeywords.contains(t.text) =>
        (buf, Some(t.text))

      case _ =>
        (buf, None)

  private def lineOpensBody(rest: IndexedSeq[Token]): Boolean =
    val nonWs = rest.filter(t => t.kind != Kind.Space && t.kind != Kind.Comment)
    nonWs.lastOption.exists(t => t.text == "=" || t.text == ":")

  private def checkSiblingPadding
    ( s:           State,
      lineNum:     Int,
      leadingCols: Int,
      isBlank:     Boolean,
      rest:        IndexedSeq[Token],
      out:         mutable.ListBuffer[Violation] )
  :   Unit =

    if isBlank then s.blanksSinceDecl += 1
    else
      val firstTok = rest.headOption
      val isAnnotation = firstTok.exists(_.text.startsWith("@"))
      val isCommentOnly = firstTok.exists(_.kind == Kind.Comment)

      // Drop entries from scopes we've exited. This must fire on every code
      // line, not just declaration lines: a non-declaration line at a smaller
      // indent (e.g. `else if c == '\'' then`) signals we have left the inner
      // scope, and any declarations recorded inside that scope must not be
      // treated as siblings of the next declaration we encounter.
      if !isCommentOnly && !isAnnotation then
        s.prevDeclByIndent.keysIterator.toList.foreach: k =>
          if k > leadingCols then s.prevDeclByIndent.remove(k)

      // R27 does not apply inside an open `(…)` parameter list: `val name:
      // type,` rows there are constructor parameters, not body declarations.
      if s.openParens > 0 then ()
      else declKeywordSequence(rest) match
        case Some(kwSeq) =>
          val padding =
            if lineOpensBody(rest) then 1 else 0

          val cur = DeclShape(lineNum, leadingCols, kwSeq, padding)
          val isFirstInScope = s.prevCodeLineIndent < leadingCols

          // First declaration in a new scope: blank line is enforced separately by R21
          // (heavy signatures only); regular non-heavy scopes have no requirement.
          if isFirstInScope then s.prevDeclByIndent.remove(leadingCols)
          s.prevDeclByIndent.get(leadingCols).foreach: prev =>
            val sameKw   = prev.kwSeq == kwSeq

            val expected =
              if sameKw && prev.padding == 0 && cur.padding == 0 then 0
              else math.max(prev.padding, cur.padding)

            val actual = s.blanksSinceDecl
            if actual < expected then
              out +=
                Violation
                  ( s.file, lineNum, 1, "27",
                    s"expected $expected blank line(s) between sibling declarations (saw $actual)" )

          s.prevDeclByIndent(leadingCols) = cur
          s.blanksSinceDecl = 0

        case None =>
          // Annotation and comment-only lines belong to the next declaration;
          // they don't consume the blank-line counter that separates the
          // previous declaration from the upcoming one. Anything else
          // (free-floating statements like method calls) breaks the sibling
          // chain — the next declaration is not a sibling of whatever came
          // before the statement.
          if !isAnnotation && !isCommentOnly then
            s.blanksSinceDecl = 0
            s.prevDeclByIndent.remove(leadingCols)

  private def checkUsingAlignment
    ( s:           State,
      lineNum:     Int,
      leadingCols: Int,
      rest:        IndexedSeq[Token],
      emit:        (Int, String, String) => Unit )
  :   Unit =

    // Alignment check first, using state from the prior line. Only fresh
    // parameter rows are checked: a row is fresh iff the previous line ended
    // with `,`, `(`, or `using` (the row-separator tokens). Otherwise the
    // current line is a wrapped continuation of the previous parameter's type
    // and is intentionally aligned to the type column, not the name column.
    if s.openParens > 0 && rest.nonEmpty then
      s.usingNameColumn.foreach: expected =>
        val freshRow =
          s.prevCodeLineLastTok == "," || s.prevCodeLineLastTok == "("
            || s.prevCodeLineLastTok == "using"

        val firstSemIdx = rest.indexWhere(t => t.kind != Kind.Space && t.kind != Kind.Comment)
        if freshRow && firstSemIdx >= 0 && rest(firstSemIdx).text != ")" then
          var c = leadingCols + 1
          var k = 0
          while k < firstSemIdx do
            c += rest(k).text.length
            k += 1
          if c != expected then
            emit
              ( c, "22",
                s"using-clause parameter should align at column $expected (found $c)" )

    // Then update state by walking tokens.
    var depth = s.openParens
    var i     = 0
    while i < rest.length do
      val t = rest(i)
      if t.kind == Kind.Code then
        if t.text == "(" then
          if depth == 0 then
            val nextSem = nextSemantic(rest, i + 1)
            if nextSem >= 0 && rest(nextSem).text == "using" then
              // Per-parameter modifiers (e.g. `inline`) are part of the
              // parameter, so subsequent rows align under the FIRST token
              // of the parameter — including the modifier — not under the
              // parameter's name. So `inline commensurable: …,` followed
              // by `addable: …,` aligns `addable` under `inline`.
              val firstTokIdx = nextSemantic(rest, nextSem + 1)
              if firstTokIdx >= 0 then
                var c = leadingCols + 1
                var k = 0
                while k < firstTokIdx do
                  c += rest(k).text.length
                  k += 1
                s.usingNameColumn = Some(c)
          depth += 1
        else if t.text == ")" then
          depth -= 1
          if depth <= 0 then s.usingNameColumn = None
      i += 1

    s.openParens = depth max 0

  private def nextSemantic(rest: IndexedSeq[Token], from: Int): Int =
    var k = from
    while k < rest.length && (rest(k).kind == Kind.Space || rest(k).kind == Kind.Comment) do
      k += 1
    if k < rest.length then k else -1

  private def lineStartsAnnotation(firstReal: Option[Token]): Boolean =
    firstReal.exists(_.text.startsWith("@"))

  // -----------------------------------------------------------------------
  // R33: keyword-sequence layout (`if`/`then`/`else`, `for`/`yield`,
  // `for`/`do`, `while`/`do`, `try`/`catch`/`finally`).
  //
  //   * Keyword cascade (33.1): once any K_i (i ≥ 2) starts a new line, every
  //     later keyword must also start a new line.
  //   * Body cascade (33.2): once any B_i (i ≥ 2) is indented onto its own
  //     line(s), every later body must be indented too.
  //   * Keyword alignment (33.3): a keyword that starts a new line must sit
  //     in the column of K_1.
  //
  // R34: for-comprehension generator alignment.
  //
  //   * Operator alignment (34.1): `<-` and `=` operators in generators are
  //     vertically aligned within the generator block.
  //   * LHS alignment (34.2): the left-hand side of every generator/binding
  //     line aligns with the first generator's LHS.
  //   * Filter alignment (34.3): an `if` filter aligns with the `<-`/`=`
  //     column of the generators.
  //
  // The cascades fire forward only — a break in K_i does not retroactively
  // require K_1..K_{i-1} to break.
  // -----------------------------------------------------------------------

  private case class Pos(text: String, line: Int, col: Int)

  private def flattenCode(lines: IndexedSeq[IndexedSeq[Token]]): IndexedSeq[Pos] =
    val buf = mutable.ArrayBuffer[Pos]()
    var lineIdx = 0
    while lineIdx < lines.length do
      val line = lines(lineIdx)
      var col  = 1
      var j    = 0
      while j < line.length do
        val tok = line(j)
        // Strings count as body content (they are valid expressions), but
        // their text never matches a keyword or a bracket so they don't
        // disturb keyword-search or depth-tracking logic.
        if tok.kind == Kind.Code || tok.kind == Kind.Strs then
          buf += Pos(tok.text, lineIdx + 1, col)
        col += tok.text.length
        j += 1
      lineIdx += 1
    buf.toIndexedSeq

  // True iff the token at `idx` is the first code token on its line — i.e.
  // there is no preceding code token on the same line.
  private def startsNewLine(toks: IndexedSeq[Pos], idx: Int): Boolean =
    idx == 0 || toks(idx - 1).line < toks(idx).line

  // True iff the body that follows the keyword at `kwIdx` is indented onto
  // a new line (its first code token sits below the keyword's line).
  private def isBodyIndented(toks: IndexedSeq[Pos], kwIdx: Int): Boolean =
    kwIdx + 1 < toks.length && toks(kwIdx + 1).line > toks(kwIdx).line

  // Find the first occurrence of any of `targets` at depth 0 from `from`,
  // tracking parens/brackets and (for `if`/`else` matching) nested if-else
  // pairs. Returns the token index, or -1 if not found before the end of
  // the enclosing scope.
  private def findKeyword
    ( toks:       IndexedSeq[Pos],
      from:       Int,
      targets:    Set[String],
      nestIfElse: Boolean )
  :   Int =

    var i     = from
    var depth = 0
    while i < toks.length do
      val t = toks(i).text
      if t == "(" || t == "[" || t == "{" then depth += 1
      else if t == ")" || t == "]" || t == "}" then
        if depth == 0 then return -1
        depth -= 1
      else if nestIfElse && t == "if" then depth += 1
      else if nestIfElse && t == "else" then
        if depth == 0 && targets.contains("else") then return i
        else if depth > 0 then depth -= 1
      else if depth == 0 && targets.contains(t) then return i
      i += 1
    -1

  // Apply the cascade and alignment rules to a sequence of keywords. `seq`
  // gives the (Pos, idx-in-toks) of each keyword in order; `seq.head` is K_1.
  private def applySequenceRules
    ( file: String,
      seq:  List[(Pos, Int)],
      toks: IndexedSeq[Pos],
      out:  mutable.ListBuffer[Violation] )
  :   Unit =

    if seq.length < 2 then return
    val k1 = seq.head._1

    var splitMode    = false
    var indentedMode = false
    var i            = 1
    while i < seq.length do
      val (cur, _)   = seq(i)
      val broke      = startsNewLine(toks, seq(i)._2)
      if broke then
        if cur.col != k1.col then
          out += Violation
                  ( file, cur.line, cur.col, "33.3",
                    s"keyword `${cur.text}` on a new line should align with `${k1.text}` "
                      +s"at column ${k1.col} (found ${cur.col})" )
        splitMode = true
      else if splitMode then
        out += Violation
                ( file, cur.line, cur.col, "33.1",
                  s"keyword `${cur.text}` must start a new line because an earlier "
                    +s"keyword in this sequence does" )
      // Body cascade applies to bodies after K_2 onward (i.e. body following
      // each K_i for i ≥ 2). The body following K_1 (the condition or
      // generator block) does not trigger the cascade.
      val curBodyIndented = isBodyIndented(toks, seq(i)._2)
      if indentedMode && !curBodyIndented then
        // The cur keyword's body should be indented but isn't.
        val bodyCol = if seq(i)._2 + 1 < toks.length then toks(seq(i)._2 + 1).col else cur.col
        out += Violation
                ( file, cur.line, bodyCol, "33.2",
                  s"body after `${cur.text}` must be indented onto a new line because "
                    +s"an earlier body in this sequence is" )
      if curBodyIndented then indentedMode = true
      i += 1

  private def processIfSeq
    ( toks: IndexedSeq[Pos], start: Int, file: String, out: mutable.ListBuffer[Violation] )
  :   Unit =

    val k2Idx = findKeyword(toks, start + 1, Set("then"), nestIfElse = false)
    if k2Idx < 0 then return
    val k1    = toks(start)
    val k2    = toks(k2Idx)
    val k3Idx = findKeyword(toks, k2Idx + 1, Set("else"), nestIfElse = true)

    if k3Idx >= 0 then
      val k3 = toks(k3Idx)
      applySequenceRules(file, List((k1, start), (k2, k2Idx), (k3, k3Idx)), toks, out)
    else
      applySequenceRules(file, List((k1, start), (k2, k2Idx)), toks, out)

  private def processForSeq
    ( toks: IndexedSeq[Pos], start: Int, file: String, out: mutable.ListBuffer[Violation] )
  :   Unit =

    val k2Idx = findKeyword(toks, start + 1, Set("yield", "do"), nestIfElse = false)
    if k2Idx < 0 then return
    val k1 = toks(start)
    val k2 = toks(k2Idx)
    applySequenceRules(file, List((k1, start), (k2, k2Idx)), toks, out)
    checkGeneratorAlignment(toks, start, k2Idx, file, out)

  private def processWhileSeq
    ( toks: IndexedSeq[Pos], start: Int, file: String, out: mutable.ListBuffer[Violation] )
  :   Unit =

    val k2Idx = findKeyword(toks, start + 1, Set("do"), nestIfElse = false)
    if k2Idx < 0 then return
    val k1 = toks(start)
    val k2 = toks(k2Idx)
    applySequenceRules(file, List((k1, start), (k2, k2Idx)), toks, out)

  private def processTrySeq
    ( toks: IndexedSeq[Pos], start: Int, file: String, out: mutable.ListBuffer[Violation] )
  :   Unit =

    val k1     = toks(start)
    val firstIdx = findKeyword(toks, start + 1, Set("catch", "finally"), nestIfElse = false)
    if firstIdx < 0 then return
    val first  = toks(firstIdx)
    if first.text == "catch" then
      val finallyIdx = findKeyword(toks, firstIdx + 1, Set("finally"), nestIfElse = false)
      if finallyIdx >= 0 then
        applySequenceRules
          ( file, List((k1, start), (first, firstIdx), (toks(finallyIdx), finallyIdx)),
            toks, out )
      else
        applySequenceRules(file, List((k1, start), (first, firstIdx)), toks, out)
    else
      applySequenceRules(file, List((k1, start), (first, firstIdx)), toks, out)

  // R34: alignment within a for-comprehension's generator block. The block
  // spans tokens between `for` (at `forIdx`) and the matching `yield`/`do`
  // (at `kwIdx`). For every gen/bind/filter line that begins at depth-0
  // within the block, check that the LHS column and `<-`/`=` column match
  // the first generator's, and that an `if` filter sits at the operator
  // column.
  private def checkGeneratorAlignment
    ( toks:   IndexedSeq[Pos],
      forIdx: Int,
      kwIdx:  Int,
      file:   String,
      out:    mutable.ListBuffer[Violation] )
  :   Unit =

    case class GenLine(line: Int, startCol: Int, opCol: Int, isFilter: Boolean)

    // Group token indices by their line, in order of appearance.
    val byLine = mutable.LinkedHashMap[Int, mutable.ArrayBuffer[Int]]()
    var i      = forIdx + 1
    while i < kwIdx do
      val ln = toks(i).line
      byLine.getOrElseUpdate(ln, mutable.ArrayBuffer[Int]()) += i
      i += 1

    // Walk lines in order, tracking paren/bracket depth so we can identify
    // continuation lines (depth > 0 at line start).
    val genLines = mutable.ArrayBuffer[GenLine]()
    var depth    = 0
    byLine.foreach: (ln, idxs) =>
      val depthAtStart = depth
      // Update depth across this line's tokens for the next iteration.
      var j = 0
      while j < idxs.length do
        val t = toks(idxs(j)).text
        if t == "(" || t == "[" || t == "{" then depth += 1
        else if t == ")" || t == "]" || t == "}" then depth -= 1
        j += 1

      if depthAtStart == 0 && idxs.nonEmpty then
        val first = toks(idxs(0))
        if first.text == "if" then
          genLines += GenLine(ln, first.col, first.col, isFilter = true)
        else
          // Find first `<-` or `=` at line-relative depth 0.
          var d     = 0
          var opCol = -1
          var k     = 0
          while k < idxs.length && opCol < 0 do
            val t = toks(idxs(k)).text
            if t == "(" || t == "[" || t == "{" then d += 1
            else if t == ")" || t == "]" || t == "}" then d -= 1
            else if d == 0 && (t == "<-" || t == "=") then opCol = toks(idxs(k)).col
            k += 1
          if opCol >= 0 then
            genLines += GenLine(ln, first.col, opCol, isFilter = false)

    // Need at least two generator/binding/filter lines for alignment to bite.
    if genLines.length < 2 then return

    // The first non-filter line establishes the LHS and operator columns.
    val firstGen = genLines.find(!_.isFilter).getOrElse(genLines.head)
    val refLhs   = firstGen.startCol
    val refOp    = firstGen.opCol

    var idx = 0
    while idx < genLines.length do
      val gl = genLines(idx)
      if !(gl.line == firstGen.line && gl.startCol == firstGen.startCol) then
        if gl.isFilter then
          if gl.startCol != refOp then
            out += Violation
                    ( file, gl.line, gl.startCol, "34.3",
                      s"`if` filter should align with `<-`/`=` at column $refOp "
                        +s"(found ${gl.startCol})" )
        else
          if gl.startCol != refLhs then
            out += Violation
                    ( file, gl.line, gl.startCol, "34.2",
                      s"generator should align with the first generator's LHS at column "
                        +s"$refLhs (found ${gl.startCol})" )
          if gl.opCol != refOp then
            out += Violation
                    ( file, gl.line, gl.opCol, "34.1",
                      s"`<-`/`=` should be vertically aligned at column $refOp "
                        +s"(found ${gl.opCol})" )
      idx += 1

  private def checkSequences
    ( file:  String,
      lines: IndexedSeq[IndexedSeq[Token]],
      out:   mutable.ListBuffer[Violation] )
  :   Unit =

    val toks = flattenCode(lines)
    var i    = 0
    while i < toks.length do
      toks(i).text match
        case "if"    => processIfSeq(toks, i, file, out)
        case "for"   => processForSeq(toks, i, file, out)
        case "while" => processWhileSeq(toks, i, file, out)
        case "try"   => processTrySeq(toks, i, file, out)
        case _       => ()
      i += 1
