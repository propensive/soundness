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
    var prevLineWasBlank:     Boolean                    = false
    var prevWasAnnotation:    Boolean                    = false
    var prevLineNum:          Int                        = 0
    var prevWasReturnType:    Boolean                    = false
    var prevCodeLineIndent:   Int                        = -1
    var prevCodeLineLastTok:  String                     = ""
    var openParens:           Int                        = 0
    var usingNameColumn:      Option[Int]                = None
    // Columns of every multi-space-after-comma site on the previous
    // line. R37 (529.2) uses this to decide whether the current line
    // genuinely continues a multi-row alignment (one of *its* extra-
    // space comma columns must match a column from the previous row),
    // rather than blindly suppressing whenever both lines happen to
    // have extra padding somewhere.
    var prevAlignmentCols:    Set[Int]                   = Set.empty
    var pendingR11:           List[Violation]            = Nil
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
    // Indent of the first line of the current `given` declaration whose
    // signature spans multiple lines, or -1 if we are not inside one. Set
    // when a line begins with `given` (after any modifiers) and reset when
    // the body opener (`=`) is reached. Used by R32 to require that any
    // `=>` continuation line align vertically with the leading modifier or
    // `given` keyword on the first signature line.
    var givenSignatureIndent:     Int                     = -1
    // True iff the immediately preceding non-blank code line was a "tight
    // expression" — one whose top-level (depth 0) tokens contain no
    // whitespace between code tokens, with at most one allowed space after
    // a leading expression-introducing keyword. Used by R33 (833.4) to
    // require that a heavy `(`/`[` continuation attach to a tight anchor.
    var prevLineIsTight:          Boolean                  = false

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
    checkR33HeavyBracketAnchor(s, leadingCols, isBlank, firstReal, rest, emit)

    if isBlank then
      s.consecutiveBlanks += 1
      if s.consecutiveBlanks > 2 then
        emit(1, "783", "more than two consecutive blank lines")
    else
      s.consecutiveBlanks = 0

    checkTokens(s, lineNum, line, s.prevCodeLineLastTok, emit)
    checkCommas(s, lineNum, line, isBlank, out)
    checkHardSpace(rest, leadingCols, emit)
    checkChainContinuation(s, lineNum, leadingCols, isBlank, firstReal, emit)
    checkR32GivenArrowAlign(s, leadingCols, isBlank, rest, emit)
    checkReturnTypeBlank(s, lineNum, isBlank, rest, emit)
    // R28 chunk separation (315) is enforced tree-based by
    // `checkChunkBlanks`; the old line-by-line sibling-padding check has
    // been removed as redundant.
    checkUsingAlignment(s, lineNum, leadingCols, rest, emit)
    if !isBlank then
      val sem = rest.filter{ t => t.kind != Sort.Space && t.kind != Sort.Comment }
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
        sem.headOption.exists: t => DeclKeywords.contains(t.text) || ModifierWords.contains(t.text)

      val isContinuationOfDecl =
        s.prevLineStartedDecl
          && sem.headOption.exists{ t => t.text == "(" || t.text == "[" }
      s.prevLineStartedDecl = !hasTopLevelEq && (startsWithDecl || isContinuationOfDecl)

      s.prevLineIsTight = isTightExpression(line.lexemes)

      // R32 anchor: a line that begins a `given` declaration (after any
      // modifiers) records its leading-cols as the anchor for arrow
      // continuation. The anchor clears when the body opener (`=`) appears
      // at top level or when an unrelated declaration begins.
      val (_, kwIdx) = skipModifiers(sem, 0)

      val startsGiven =
        kwIdx < sem.length && sem(kwIdx).kind == Sort.Code && sem(kwIdx).text == "given"

      if startsGiven then s.givenSignatureIndent = leadingCols
      else if s.givenSignatureIndent >= 0 && hasTopLevelEq then s.givenSignatureIndent = -1

      // R32 given-signature termination: any line that's neither an `=>`
      // continuation nor part of the initial signature ends the
      // given-signature region. (The R33.3 type-annotation anchor used
      // to be tracked here too, but is now driven from the untyped tree
      // — see `Definitions.extract` and `checkDefnAnchors`.)
      if s.givenSignatureIndent >= 0 && !sem.headOption.exists(_.text == "=>") then
        if !startsWithDecl && !isContinuationOfDecl then s.givenSignatureIndent = -1

    if isBlank then
      if s.prevWasAnnotation then
        emit
          ( 1, "551.2",
            "blank line is not permitted between an annotation and the declaration it annotates" )
        s.prevWasAnnotation = false
    else
      s.prevWasAnnotation = s.annotationEndLines.contains(lineNum)
      s.prevLineNum = lineNum
      // Comment-only and annotation-only lines belong to the next declaration:
      // they must not update `prevCodeLineIndent`, otherwise sibling-scope
      // detection mis-classifies the next declaration as a same-scope sibling
      // of whatever appeared before the comment.
      val isCommentOnly = firstReal.exists(_.kind == Sort.Comment)
      val isAnnotationOnly = lineStartsAnnotation(firstReal)
      // Continuation lines inside a multi-line triple-quoted string are
      // tokenised as a single Strs token with no leading Space token, so
      // their leadingCols is always 0. Don't let that corrupt the indent
      // state used by chain-continuation / bracket / sibling / R31 checks.
      // A normal code line that *starts* with a string interpolation (e.g.
      // `sh"…".exec()`) does have a leading Space token and is not affected.

      val isStringContinuation =
        firstReal.exists(_.kind == Sort.Strs) && leadingWs.isEmpty
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

  // R32: continuation lines of a multi-line `given` signature that begin
  // with `=>` must align vertically with the leading modifier or `given`
  // keyword on the first line of the signature.
  private def checkR32GivenArrowAlign
    ( s:           State,
      leadingCols: Int,
      isBlank:     Boolean,
      rest:        IndexedSeq[Lexeme],
      emit:        (Int, String, String) => Unit )
  :   Unit =

    if isBlank then ()
    else if s.givenSignatureIndent < 0 then ()
    else
      val sem = rest.filter{ t => t.kind != Sort.Space && t.kind != Sort.Comment }
      if sem.headOption.exists(_.text == "=>") && leadingCols != s.givenSignatureIndent then
        emit
          ( leadingCols + 1, "140",
            s"`=>` continuation of a `given` signature should align at column "
              +s"${s.givenSignatureIndent + 1} (found ${leadingCols + 1})" )

  // The keywords that may introduce a tight expression — `new T`,
  // `throw E`, `return E`, `yield E`, `then E`, `else E`, `do E`, `try E`,
  // `catch …`, `finally …`. The space following one of these at the head
  // of a line does not break "tightness": grammatically the keyword and
  // the expression that follows it are one production, not an infix
  // application.
  private val ExprIntroKeywords: Set[String] =
    Set
      ( "new", "throw", "return", "yield",
        "then", "else", "do", "try", "catch", "finally",
        // Class/trait parent-spec introducers: `extends Foo(args)` and
        // `with Foo(args)` take an expression-shaped tail just like `new`.
        "extends", "with" )

  // Tokens that, at the end of a code line, signal "the next line is a
  // body" (lambda body, assignment RHS, block content, keyword-sequence
  // body, etc.) rather than a heavy-bracket continuation of the current
  // line's expression. Used to skip R33.4's anchor check on those lines.
  private val BodyOpenerTerminators: Set[String] =
    Set
      ( "=", "=>", ":", ";", "match",
        "then", "else", "do", "yield",
        "try", "catch", "finally",
        "for", "if", "while",
        "with", "extends", "derives", "case" )

  // A **tight expression** has no top-level whitespace between code
  // tokens: at bracket depth zero, the only whitespace allowed is a
  // single space directly after one leading expression-introducing
  // keyword. Parenthesising any expression makes it tight, since the
  // interior moves to depth > 0 where the rule does not reach.
  //
  // Examples — tight: `recur`, `foo.bar(baz).quux`, `new Exception`,
  // `(x: Int)`, `Some(x)`, `( arg )` (a whole-line bracketed clause).
  //
  // Examples — not tight: `head :: recur`, `val foo = bar`,
  // `if x then y else z`, `x: Int`.
  private def isTightExpression(line: IndexedSeq[Lexeme]): Boolean =
    val arr = line.toArray
    var i = 0
    while i < arr.length && arr(i).kind == Sort.Space do i += 1
    if i >= arr.length then return false
    var depth             = 0
    var sawCodeAtTopLevel = false
    // Optional leading expression-introducing keyword followed by one space.
    if arr(i).kind == Sort.Code && ExprIntroKeywords.contains(arr(i).text) then
      sawCodeAtTopLevel = true
      i += 1
      if i < arr.length && arr(i).kind == Sort.Space && arr(i).text == " " then
        i += 1
    // OR: optional leading `.` (chain continuation) followed by one space.
    else if arr(i).kind == Sort.Code && arr(i).text == "." then
      sawCodeAtTopLevel = true
      i += 1
      if i < arr.length && arr(i).kind == Sort.Space && arr(i).text == " " then
        i += 1
    while i < arr.length do
      val tok = arr(i)
      tok.kind match
        case Sort.Space =>
          if depth == 0 && sawCodeAtTopLevel then return false

        case Sort.Comment => ()

        case _ =>
          if depth == 0 then sawCodeAtTopLevel = true
          tok.text match
            case "(" | "[" | "{" => depth += 1
            case ")" | "]" | "}" => depth -= 1
            case _               => ()
      i += 1
    true

  // R33.4: a line whose first semantic token is `(` or `[` is a "heavy
  // argument block" applied to the previous line's expression. That
  // expression must be **tight** so the `(`/`[` attaches unambiguously
  // to the entire previous line's content rather than to some mid-line
  // subexpression.
  //
  // Three other ways a line is a valid anchor for a heavy continuation:
  // - The previous line is a declaration signature in progress (no
  //   top-level `=` yet) — its `(args)` is a parameter list, governed
  //   by its own rules; the anchor check is exempt.
  // - The previous line was itself a closed heavy bracket continuation
  //   (multi-clause currying like `f` / `(x)` / `(y)`): a whole-line
  //   `( ... )` is itself tight, so this case is naturally handled by
  //   the tight check.
  // - The current line is inside an open multi-line bracket (`openParens
  //   > 0` from a previous line) — the `(` is not a heavy continuation
  //   but interior content of an enclosing bracket.
  // A line whose leading `(`/`[` group is immediately followed by `=>` is a
  // lambda (or polymorphic-function) parameter list, not a heavy argument
  // continuation, so R33 must not flag it.
  private def lineOpensLambda(rest: IndexedSeq[Lexeme]): Boolean =
    val sem = rest.filter { t => t.kind != Sort.Space && t.kind != Sort.Comment }
    sem.headOption.exists { t => t.text == "(" || t.text == "[" } && {
      var depth = 0
      var i     = 0
      var close = -1
      while i < sem.length && close < 0 do
        sem(i).text match
          case "(" | "[" => depth += 1

          case ")" | "]" =>
            depth -= 1
            if depth == 0 then close = i

          case _ => ()

        i += 1

      close >= 0 && sem.lift(close + 1).exists { t => t.text == "=>" }
    }

  private def checkR33HeavyBracketAnchor
    ( s:           State,
      leadingCols: Int,
      isBlank:     Boolean,
      firstReal:   Option[Lexeme],
      rest:        IndexedSeq[Lexeme],
      emit:        (Int, String, String) => Unit )
  :   Unit =

    if isBlank then ()
    else if s.openParens > 0 then ()
    else if s.prevLineWasBlank then ()
    else if !firstReal.exists { t => t.kind == Sort.Code && (t.text == "(" || t.text == "[") }
    then ()
    // A lambda parameter list (`(params) =>`) is not a heavy argument
    // continuation; the `(…)` belongs to the lambda. Skip it.
    else if lineOpensLambda(rest) then ()
    // A heavy continuation is indented *more* than its anchor. If the
    // current line's indent is ≤ the previous code line's indent, the
    // `(`/`[` is a sibling statement (a tuple, parenthesised expression
    // or type-application standing on its own), not a continuation.
    else if s.prevCodeLineIndent < 0 || leadingCols <= s.prevCodeLineIndent then ()
    // If the previous line ends with a "body opener" — `=`, `=>`, `:`,
    // `then`, `else`, etc. — the current line is the body of that
    // construct (assignment RHS, lambda body, case body, etc.), not a
    // heavy continuation. Skip the check.
    else if BodyOpenerTerminators.contains(s.prevCodeLineLastTok) then ()
    // If the previous line ends with a symbolic infix operator, the current
    // `(`/`[` begins the right-hand operand of that operator — an operator
    // continuation governed by R616, not a heavy argument block. Skip.
    else if isSymbolicOperator(s.prevCodeLineLastTok) then ()
    // If the previous line opened a block, quote or splice (ending in `{`), the
    // current line is the first expression *inside* that scope — a tuple or
    // parenthesised value standing on its own, not an argument continuation of
    // a mid-line receiver. Skip.
    else if s.prevCodeLineLastTok == "{" then ()
    else if s.prevLineIsTight then ()
    else if s.prevLineStartedDecl then ()
    else
      emit
        ( leadingCols + 1, "833.4",
          "heavy `(`/`[` continuation must follow a tight expression on its "
            +"own line; the preceding line contains a top-level operator or "
            +"assignment, so the anchor is mid-line" )

  private def checkTokens
    ( s:       State,
      lineNum: Int,
      line:    Line,
      prevTok: String,
      emit:    (Int, String, String) => Unit )
  :   Unit =

    val arr  = line.arr
    val cols = line.cols

    // Comma checks need state for alignment-run detection.
    // We pass them through directly via the State held in the closure.
    checkBracketInteriors(s, arr, cols, prevTok, emit)
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

  private def checkBracketInteriors
    ( s:       State,
      arr:     Array[Lexeme],
      cols:    Array[Int],
      prevTok: String,
      emit:    (Int, String, String) => Unit )
  :   Unit =

    val firstSemantic = arr.indexWhere{ t => t.kind != Sort.Space && t.kind != Sort.Comment }
    val lastSemantic  = arr.lastIndexWhere{ t => t.kind != Sort.Space && t.kind != Sort.Comment }

    val lineStartsWithBracket =
      firstSemantic >= 0 && (arr(firstSemantic).text == "(" || arr(firstSemantic).text == "[")

    val secondSemantic =
      if firstSemantic < 0 then -1
      else arr.indexWhere(t => t.kind != Sort.Space && t.kind != Sort.Comment, firstSemantic + 1)

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
    val leadingCols = arr.takeWhile(_.kind == Sort.Space).iterator.map(_.text.length).sum
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
      if arr(i).kind == Sort.Code then
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
              arr.indexWhere(t => t.kind != Sort.Space && t.kind != Sort.Comment, closer + 1)

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
                    || t == "=>"

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
                if firstInside.kind != Sort.Space then
                  emit
                    ( cols(opener + 1), "811",
                      s"a space is required after `${arr(opener).text}` in a multi-line block" )
                val lastInside = arr(closer - 1)
                if lastInside.kind != Sort.Space then
                  emit
                    ( cols(closer), "811",
                      s"a space is required before `$text` in a multi-line block" )
            else if closer > opener + 1 then
              // Suppress R12 for tuples appearing as a lambda/match-case
              // body on a fresh line after `=>` — author may use either
              // tight `(a, b)` or formal-style `( a, b )`.
              val isLambdaBodyTuple =
                isArrowBodyContinuation && opener == firstSemantic && closer == lastSemantic
              if !isLambdaBodyTuple then
                val firstInside = arr(opener + 1)
                if firstInside.kind == Sort.Space && firstInside.text.length > 0 then
                  emit
                    ( cols(opener + 1), "402",
                      s"no space is permitted directly after `${arr(opener).text}`" )
                val lastInside = arr(closer - 1)
                if lastInside.kind == Sort.Space && lastInside.text.length > 0
                  && (closer - 1) != (opener + 1)
                then
                  emit
                    ( cols(closer - 1), "402",
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
                arr.indexWhere(t => t.kind != Sort.Space && t.kind != Sort.Comment, i + 1)
              if nextAfterCloser < 0 then s.currentFormalOpenerIndent = openerIndent
              if i == firstSemantic then
                emit
                  ( cols(i), "811",
                    s"`$text` cannot appear alone on its line; the closing bracket of a "
                      +s"multi-line block goes on the same line as the last parameter" )
              else if i > 0 && arr(i - 1).kind != Sort.Space then
                emit
                  ( cols(i), "811",
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
        if opener + 1 >= arr.length || arr(opener + 1).kind != Sort.Space then
          emit
            ( cols(opener + 1), "811",
              s"a space is required after `$openerText` in a multi-line block" )
      s.bracketFormality.push((formalCandidate, leadingCols))

  // Single-line bracket between `opener` and `closer` (exclusive). Returns
  // true iff the contents look like an argument list: a top-level comma or
  // type-annotation `:` is present, or there are no top-level binary
  // operators. Returns false when the contents are a pure grouping
  // expression (no commas, no `:`, has operators like `||`, `&&`, `==`,
  // `+`, etc.).
  private def bracketHasArgListShape
    ( arr: Array[Lexeme], opener: Int, closer: Int )
  :   Boolean =

    var depth = 0
    var hasTopComma = false
    var hasTopColon = false
    var hasTopOperator = false
    var k = opener + 1
    while k < closer do
      val t = arr(k)
      if t.kind == Sort.Code then
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
    ( arr: Array[Lexeme], opener: Int, closer: Int )
  :   Boolean =

    var depth = 0
    var found = false
    var k = opener + 1
    while k < closer && !found do
      val t = arr(k)
      if t.kind == Sort.Code then
        val text = t.text
        if text == "(" || text == "[" then depth += 1
        else if text == ")" || text == "]" then depth -= 1
        else if depth == 0 && text == ":" then found = true
      k += 1
    found

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

  private val NonOperandWords: Set[String] =
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

  private def checkHardSpace
    ( rest: IndexedSeq[Lexeme], leadingCols: Int, emit: (Int, String, String) => Unit )
  :   Unit =

    rest.headOption match
      case Some(tok) if tok.text == "=>" =>
        if rest.length >= 2 then
          val next = rest(1)
          if next.kind != Sort.Space || next.text != "  " then
            emit
              ( leadingCols + 3, "444",
                "`=>` continuation line must be followed by exactly two spaces" )

      case Some(tok) if tok.text == ":" && lineEndsWithEqualsToken(rest) =>
        if rest.length >= 2 then
          val next = rest(1)
          if next.kind != Sort.Space || next.text != "   " then
            emit
              ( leadingCols + 2, "444",
                "heavy-signature return type `:` must be followed by exactly three spaces" )

      case _ => ()

  private def lineEndsWithEqualsToken(rest: IndexedSeq[Lexeme]): Boolean =
    val nonWs = rest.filter{ t => t.kind != Sort.Space && t.kind != Sort.Comment }
    nonWs.lastOption.exists(_.text == "=")

  private def checkChainContinuation
    ( s:           State,
      lineNum:     Int,
      leadingCols: Int,
      isBlank:     Boolean,
      firstReal:   Option[Lexeme],
      emit:        (Int, String, String) => Unit )
  :   Unit =

    if isBlank then ()
    else firstReal.foreach: tok =>
      if tok.text == "." && s.prevCodeLineIndent >= 0 then
        if s.prevCodeLineIndent > leadingCols && !s.prevLineWasBlank then
          emit
            ( leadingCols + 1, "163.1",
              "blank line is required before `. method` continuation following a "
                +"more-indented line" )
        else if s.prevCodeLineIndent == leadingCols && s.prevLineWasBlank then
          emit
            ( leadingCols + 1, "163.2",
              "no blank line is permitted before `. method` continuation at the same indent" )

  private def checkReturnTypeBlank
    ( s:       State,
      lineNum: Int,
      isBlank: Boolean,
      rest:    IndexedSeq[Lexeme],
      emit:    (Int, String, String) => Unit )
  :   Unit =

    if s.prevWasReturnType then
      if !isBlank then
        emit
          ( 1, "677",
            "a blank line is required between a heavy-signature return type and the body" )
      s.prevWasReturnType = false
    if !isBlank && isReturnTypeLine(rest) then s.prevWasReturnType = true

  private def isReturnTypeLine(rest: IndexedSeq[Lexeme]): Boolean =
    rest.length >= 2 && rest(0).text == ":" && rest(1).kind == Sort.Space
      && rest(1).text == "   " && rest.lastOption.exists(_.text == "=")

  private[decorum] val ModifierWords: Set[String] =
    Set
      ( "private", "protected", "public", "final", "sealed", "abstract",
        "implicit", "lazy", "override", "case", "inline", "transparent",
        "infix", "open", "opaque", "erased", "tracked", "given", "into" )

  private def skipModifiers(tokens: IndexedSeq[Lexeme], start: Int): (Option[String], Int) =
    var i = start
    var lastModifier: Option[String] = None
    while i < tokens.length && tokens(i).kind == Sort.Code
      && ModifierWords.contains(tokens(i).text)
      && tokens(i).text != "given"
    do
      lastModifier = Some(tokens(i).text)
      i += 1
    (lastModifier, i)

  private[decorum] val DeclKeywords: Set[String] =
    Set("def", "val", "var", "type", "class", "trait", "object", "enum", "given")

  private def checkUsingAlignment
    ( s:           State,
      lineNum:     Int,
      leadingCols: Int,
      rest:        IndexedSeq[Lexeme],
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

        val firstSemIdx = rest.indexWhere{ t => t.kind != Sort.Space && t.kind != Sort.Comment }
        if freshRow && firstSemIdx >= 0 && rest(firstSemIdx).text != ")" then
          var c = leadingCols + 1
          var k = 0
          while k < firstSemIdx do
            c += rest(k).text.length
            k += 1
          if c != expected then
            emit
              ( c, "946",
                s"using-clause parameter should align at column $expected (found $c)" )

    // Then update state by walking tokens.
    var depth = s.openParens
    var i     = 0
    while i < rest.length do
      val t = rest(i)
      if t.kind == Sort.Code then
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

  private def nextSemantic(rest: IndexedSeq[Lexeme], from: Int): Int =
    var k = from
    while k < rest.length && (rest(k).kind == Sort.Space || rest(k).kind == Sort.Comment) do
      k += 1
    if k < rest.length then k else -1

  private def lineStartsAnnotation(firstReal: Option[Lexeme]): Boolean =
    firstReal.exists(_.text.startsWith("@"))

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

