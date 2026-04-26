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
      out += Violation
              ( file, state.prevLineNum, 1, "R15-annotation-orphan",
                "annotation is not followed by a declaration" )
    state.pendingR11.foreach(out += _)
    state.pendingR11 = Nil
    finalizeCaseRun(state, out)
    checkCompanionOrdering(file, state, out)
    LazyList.from(out)

  def expectedModule(filePath: String): Option[String] =
    val parts    = filePath.split("/lib/").nn
    val segments = filePath.split("/").nn
    val fileName = segments(segments.length - 1).nn
    val cuts     = List(fileName.indexOf('_'), fileName.indexOf('.')).filter(_ >= 0)
    if cuts.nonEmpty then Some(fileName.substring(0, cuts.min).nn)
    else if parts.length < 2 then None
    else Some(parts(1).nn.split("/").nn(0).nn)

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
    var caseRun:              Option[CaseRun]            = None
    var blanksSinceDecl:      Int                        = 0
    var prevDeclByIndent:     mutable.Map[Int, DeclShape] = mutable.Map.empty
    var openParens:           Int                        = 0
    var usingNameColumn:      Option[Int]                = None
    var prevLineHadAlignment: Boolean                    = false
    var pendingR11:           List[Violation]            = Nil
    val typeDecls:            mutable.Map[String, Int]   = mutable.Map.empty
    val objectDecls:          mutable.Map[String, Int]   = mutable.Map.empty

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

    checkR2LineLength(visibleLen, emit)
    checkR3IndentWidth(isBlank, leadingCols, emit)
    checkR4TrailingWs(line, emit)

    if isBlank then
      s.consecutiveBlanks += 1
      if s.consecutiveBlanks > 2 then
        emit(1, "R5-blank-cap", "more than two consecutive blank lines")
    else s.consecutiveBlanks = 0

    s.phase match
      case Phase.License      => checkLicense(s, lineNum, line, emit)
      case Phase.Package      => checkPackage(s, lineNum, rest, emit)
      case Phase.AfterPackage => checkAfterPackage(s, isBlank, emit)
      case Phase.Imports      => checkImports(s, isBlank, firstReal, rest, emit)
      case Phase.Body         => ()

    checkTokens(lineNum, line, emit)
    checkCommas(s, lineNum, line, isBlank, out)
    checkHardSpace(rest, leadingCols, emit)
    checkChainContinuation(s, lineNum, leadingCols, isBlank, firstReal, emit)
    checkReturnTypeBlank(s, lineNum, isBlank, rest, emit)
    checkMatchCases(s, lineNum, leadingCols, isBlank, rest, line, out)
    checkSiblingPadding(s, lineNum, leadingCols, isBlank, rest, out)
    checkUsingAlignment(s, lineNum, leadingCols, rest, emit)
    recordDeclarations(s, lineNum, rest)
    if !isBlank then s.prevLineEndedMatch = lineEndsWithMatch(rest)

    if isBlank then
      if s.prevWasAnnotation then
        emit
         ( 1, "R15-annotation-blank",
           "blank line is not permitted between an annotation and the declaration it annotates" )
        s.prevWasAnnotation = false
    else
      s.prevWasAnnotation = lineStartsAnnotation(firstReal)
      s.prevLineNum = lineNum
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
        out += Violation
                ( file, line, col, "R1-no-tabs", "tab character is not permitted; use spaces" )
      if ch == '\n' then
        line += 1
        col = 1
      else col += 1
      i += 1

  private def checkR2LineLength(visibleLen: Int, emit: (Int, String, String) => Unit): Unit =
    if visibleLen > MaxColumns then
      emit
       ( MaxColumns + 1, "R2-line-length",
         s"line exceeds 100 columns (is $visibleLen columns)" )

  private def checkR3IndentWidth
    ( isBlank: Boolean, leadingCols: Int, emit: (Int, String, String) => Unit )
  :   Unit =

    if !isBlank && leadingCols%2 != 0 then
      emit(1, "R3-indent", s"indent width $leadingCols is not a multiple of 2")

  private def checkR4TrailingWs
    ( line: IndexedSeq[Token], emit: (Int, String, String) => Unit )
  :   Unit =

    line.lastOption match
      case Some(token) if token.kind == Kind.Space && token.text.length > 0 =>
        val hasNonWs = line.exists(t => t.kind != Kind.Space && t.kind != Kind.Comment)
        if hasNonWs then
          val col = line.iterator.map(_.text.length).sum - token.text.length + 1
          emit(col, "R4-trailing-ws", "line has trailing whitespace")
      case _ =>
        ()

  private def checkLicense
    ( s: State, lineNum: Int, line: IndexedSeq[Token], emit: (Int, String, String) => Unit )
  :   Unit =

    val text = line.iterator.map(_.text).mkString

    if lineNum == 1 && !text.contains("/*") then
      emit(1, "R6-license", "line 1 must open the license-header block comment with `/*`")

    if lineNum == 32 then
      if !text.contains("*/") then
        emit(1, "R6-license", "line 32 must close the license-header block comment with `*/`")
      s.phase = Phase.Package

  private def checkPackage
    ( s:       State,
      lineNum: Int,
      rest:    IndexedSeq[Token],
      emit:    (Int, String, String) => Unit )
  :   Unit =

    if lineNum != PackageLine then
      emit
       ( 1, "R7-package",
         s"expected `package` declaration on line 33, found content on line $lineNum" )
      s.phase = Phase.Body
    else
      val nonWs = rest.filter(t => t.kind != Kind.Space && t.kind != Kind.Comment).toList
      nonWs match
        case keyword :: ident :: tail
          if keyword.text == "package" && ident.kind == Kind.Code =>

          if !ident.text.matches("[A-Za-z_][A-Za-z0-9_]*") then
            emit
             ( 1, "R7-package",
               s"package declaration must be a single identifier segment, not `${ident.text}`" )

          s.expectedModule.foreach: expected =>
            if ident.text != expected then
              emit
               ( 1, "R7-package",
                 s"package `${ident.text}` does not match expected module `$expected`" )

          if tail.nonEmpty then
            emit
             ( 1, "R7-package",
               "package declaration must contain only `package <ident>` on line 33" )

        case _ =>
          emit(1, "R7-package", "line 33 must be `package <module>`")

      s.phase = Phase.AfterPackage

  private def checkAfterPackage
    ( s: State, isBlank: Boolean, emit: (Int, String, String) => Unit )
  :   Unit =

    if !isBlank then
      emit(1, "R8-package-blank", "line 34 must be a single blank line after `package`")

    s.phase = Phase.Imports

  private def checkImports
    ( s:         State,
      isBlank:   Boolean,
      firstReal: Option[Token],
      rest:      IndexedSeq[Token],
      emit:      (Int, String, String) => Unit )
  :   Unit =

    if isBlank then ()
    else firstReal.foreach: head =>
      if head.text == "import" then
        if importHasAlias(rest) then
          emit
           ( 1, "R9-aliased-import",
             "top-level imports must not use aliases (`as` or `=>`); write the full path" )
        val path  = importPath(rest)
        val group = classifyImport(path)

        s.prevImportGroup match
          case Some(prevGroup) =>
            if group < prevGroup then
              emit
               ( 1, "R9-import-order",
                 s"import group $group appears after group $prevGroup" )
            else if group > prevGroup then
              if !s.prevLineWasBlank then
                emit
                 ( 1, "R9-import-group-blank",
                   "import groups must be separated by exactly one blank line" )
            else
              if s.prevLineWasBlank then
                emit
                 ( 1, "R9-import-group-blank",
                   "unexpected blank line within an import group" )
              s.prevImportName.foreach: prevName =>
                if path < prevName then
                  emit
                   ( 1, "R9-import-order",
                     s"import `$path` is out of alphabetical order (after `$prevName`)" )
          case None => ()

        s.prevImportGroup = Some(group)
        s.prevImportName  = Some(path)
      else
        if !s.prevLineWasBlank && s.prevImportGroup.isDefined then
          emit
           ( 1, "R10-after-imports",
             "missing blank line between imports and first declaration" )
        s.phase = Phase.Body

  private def importHasAlias(rest: IndexedSeq[Token]): Boolean =
    rest.exists(t => t.kind == Kind.Code && (t.text == "as" || t.text == "=>"))

  private def importPath(rest: IndexedSeq[Token]): String =
    val nonWs = rest.filter(t => t.kind != Kind.Space && t.kind != Kind.Comment).toList
    nonWs match
      case _ :: tail =>
        tail.takeWhile(_.text != "as").iterator.map(_.text).mkString
      case _ =>
        ""

  private def classifyImport(path: String): Int =
    val firstSegment = path.takeWhile(c => c != '.' && c != ',' && c != '{' && c != ' ')
    firstSegment match
      case "language"        => 1
      case "java" | "javax"  => 2
      case "scala"           => 3
      case _ =>
        if firstSegment.headOption.exists(_.isUpper) then 5
        else if !path.endsWith(".*") then 5
        else 4

  private def checkTokens
    ( lineNum: Int,
      line:    IndexedSeq[Token],
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
    checkBracketInteriors(arr, cols, emit)
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
          out += Violation
                  ( s.file, lineNum, cols(i), "R11-comma-space-before",
                    "no space is permitted before a comma" )

        if i + 1 < arr.length then
          val next = arr(i + 1)
          if next.kind != Kind.Space then
            out += Violation
                    ( s.file, lineNum, cols(i + 1), "R11-comma-space-after",
                      "exactly one space is required after a comma" )
          else if next.text != " " then
            // Extra spaces after comma — possibly an alignment-run column.
            // Defer until we know whether neighbouring lines also have alignment.
            hasAlignment = true
            if !next.text.startsWith("\n") then
              deferred += Violation
                           ( s.file, lineNum, cols(i + 1), "R11-comma-space-after",
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
    ( arr:  Array[Token],
      cols: Array[Int],
      emit: (Int, String, String) => Unit )
  :   Unit =

    val firstSemantic = arr.indexWhere(t => t.kind != Kind.Space && t.kind != Kind.Comment)
    val lineStartsWithBracket =
      firstSemantic >= 0 && (arr(firstSemantic).text == "(" || arr(firstSemantic).text == "[")
    val secondSemantic =
      if firstSemantic < 0 then -1
      else arr.indexWhere(t => t.kind != Kind.Space && t.kind != Kind.Comment, firstSemantic + 1)
    val arrowParen =
      firstSemantic >= 0 && arr(firstSemantic).text == "=>"
        && secondSemantic > 0
        && (arr(secondSemantic).text == "(" || arr(secondSemantic).text == "[")
    val stack = mutable.Stack[(Int, Boolean)]()
    var i = 0
    while i < arr.length do
      if arr(i).kind == Kind.Code then
        val text = arr(i).text
        if text == "(" || text == "[" then
          val exempt = (lineStartsWithBracket && i == firstSemantic)
                         || (arrowParen && i == secondSemantic)
          stack.push((i, exempt))
        else if text == ")" || text == "]" then
          if stack.nonEmpty then
            val (opener, exempt) = stack.pop()
            val closer = i
            if !exempt && closer > opener + 1 then
              val firstInside = arr(opener + 1)
              if firstInside.kind == Kind.Space && firstInside.text.length > 0 then
                emit
                 ( cols(opener + 1), "R12-bracket-interior",
                   s"no space is permitted directly after `${arr(opener).text}`" )

              val lastInside = arr(closer - 1)
              if lastInside.kind == Kind.Space && lastInside.text.length > 0
                && (closer - 1) != (opener + 1)
              then
                emit
                 ( cols(closer - 1), "R12-bracket-interior",
                   s"no space is permitted directly before `$text`" )
      i += 1

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
           ( cols(i), "R14-scaladoc",
             "`/** ... */` block comments are not permitted; use `doc/` markdown instead" )
        else if text.startsWith("/*") && !inLicense then
          emit
           ( cols(i), "R14-block-comment",
             "`/* ... */` block comments are reserved for the license header (lines 1-32)" )
        else if text.startsWith("//") && !inLicense then
          if text.length == 2 then ()
          else if text.charAt(2) == ' ' then
            if text.length > 3 && text.charAt(3) == ' ' then
              emit
               ( cols(i), "R13-comment-space",
                 "a line comment must be followed by exactly one space" )
          else
            emit
             ( cols(i), "R13-comment-space",
               "a line comment must be followed by exactly one space" )
      i += 1

  private val CheckedOps: Set[String] =
    Set("+", "-", "*", "/", "%", "&", "|", "^", "<", ">",
        "<<", ">>", ">>>", "&&", "||", "==", "!=", "<=", ">=",
        "=>", "->", "<-", "<:", ">:", "&~", "?=>")

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

  private val NonOperandWords: Set[String] = Set(
    "case", "if", "then", "else", "do", "while", "for", "yield", "return",
    "match", "with", "extends", "derives", "given", "using", "new", "throw",
    "try", "catch", "finally", "import", "package", "def", "val", "var",
    "lazy", "object", "class", "trait", "enum", "type", "private", "protected",
    "public", "final", "sealed", "abstract", "implicit", "override", "inline",
    "transparent", "infix", "open", "opaque", "erased", "tracked",
    // Custom infix words from the syntax doc + common Range/iterator infixes
    "is", "of", "in", "by", "to", "under", "on", "raises", "until"
  )

  private def isBinaryContext(arr: Array[Token], i: Int): Boolean =
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
               ( cols(i), "R16-operator-spacing",
                 s"`$text` has asymmetric spacing — use 0 or 1 space on both sides" )
            // Multi-char operators must have one space around (zero is reserved
            // for single-character operators).
            if text.length > 1 && !isAtStart && !isAtEnd
              && (!leftSpace || !rightSpace)
            then
              emit
               ( cols(i), "R16-operator-spacing",
                 s"multi-character `$text` requires one space on each side" )
            frames.top += OpHit(text, i, cols(i), leftSpace, rightSpace)
      i += 1

    while frames.nonEmpty do checkOpFrame(frames.pop(), emit)

  private def checkOpFrame
    ( ops: mutable.ArrayBuffer[OpHit], emit: (Int, String, String) => Unit )
  :   Unit =

    if ops.isEmpty then ()
    else
      val withSpacing = ops.map: op =>
        val s = if op.leftSpace || op.rightSpace then 1 else 0
        (op, s)
      val byPrec = withSpacing.groupBy((op, _) => operatorPrecedence(op.text))

      // Same-precedence consistency
      byPrec.values.foreach: opPairs =>
        val spacings = opPairs.map(_._2).distinct
        if spacings.length > 1 then
          opPairs.foreach: (op, _) =>
            emit
             ( op.col, "R16-operator-spacing",
               s"`${op.text}` has inconsistent spacing with same-precedence operators" )

      // Cross-precedence ordering: higher precedence should have ≤ spacing
      val precSpacings = byPrec.toList.map: (p, pairs) =>
        (p, pairs.head._2)
      val sortedPrecs = precSpacings.sortBy(_._1)
      var i = 0
      while i < sortedPrecs.length do
        val (pLow, sLow) = sortedPrecs(i)
        var j = i + 1
        while j < sortedPrecs.length do
          val (pHigh, sHigh) = sortedPrecs(j)
          if sHigh > sLow then
            byPrec(pHigh).foreach: (op, _) =>
              emit
               ( op.col, "R16-operator-spacing",
                 s"`${op.text}` cannot have more spacing than lower-precedence operators in the same expression" )
          j += 1
        i += 1

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
             ( cols(nextIdx), "R18-symbolic-method",
               s"a single space is required between `${arr(opIdx).text}` and `${arr(nextIdx).text}`" )
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
             ( leadingCols + 3, "R25-hard-space",
               "`=>` continuation line must be followed by exactly two spaces" )
      case Some(tok) if tok.text == ":" && lineEndsWithEqualsToken(rest) =>
        if rest.length >= 2 then
          val next = rest(1)
          if next.kind != Kind.Space || next.text != "   " then
            emit
             ( leadingCols + 2, "R25-hard-space",
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
           ( leadingCols + 1, "R26-chain-blank-required",
             "blank line is required before `. method` continuation following a more-indented line" )
        else if s.prevCodeLineIndent == leadingCols && s.prevLineWasBlank then
          emit
           ( leadingCols + 1, "R26-chain-blank-forbidden",
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
         ( 1, "R21-heavy-body-blank",
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

  private val ModifierWords: Set[String] =
    Set("private", "protected", "public", "final", "sealed", "abstract", "implicit",
        "lazy", "override", "case", "inline", "transparent", "infix", "open",
        "opaque", "erased", "tracked", "given")

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
          out += Violation
                  ( file, objLine, 1, "R28-companion-ordering",
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
        out += Violation
                ( file, 1, 1, "R29-file-naming",
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
           ( leadingCols + 1, "R20-multiline-case-blank",
             "a blank line is required before a multi-line case (except for the first case)" )
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
            out += Violation
                    ( s.file, entry.lineNum, entry.arrowCol, "R19-case-arrow-align",
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
      declKeywordSequence(rest) match
        case Some(kwSeq) =>
          val padding =
            if lineOpensBody(rest) then 1 else 0
          val cur = DeclShape(lineNum, leadingCols, kwSeq, padding)
          val isFirstInScope = s.prevCodeLineIndent < leadingCols
          // Drop entries from scopes we've exited.
          s.prevDeclByIndent.keysIterator.toList.foreach: k =>
            if k > leadingCols then s.prevDeclByIndent.remove(k)
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
              out += Violation
                      ( s.file, lineNum, 1, "R27-sibling-padding",
                        s"expected $expected blank line(s) between sibling declarations (saw $actual)" )
          s.prevDeclByIndent(leadingCols) = cur
          s.blanksSinceDecl = 0
        case None =>
          // Annotation lines belong to the next declaration; don't consume the
          // blank-line counter that separates the previous declaration from the
          // upcoming one.
          val isAnnotation = rest.headOption.exists(_.text.startsWith("@"))
          if !isAnnotation then s.blanksSinceDecl = 0

  private def checkUsingAlignment
    ( s:           State,
      lineNum:     Int,
      leadingCols: Int,
      rest:        IndexedSeq[Token],
      emit:        (Int, String, String) => Unit )
  :   Unit =

    // Alignment check first, using state from the prior line.
    if s.openParens > 0 && rest.nonEmpty then
      s.usingNameColumn.foreach: expected =>
        val firstSemIdx = rest.indexWhere(t => t.kind != Kind.Space && t.kind != Kind.Comment)
        if firstSemIdx >= 0 && rest(firstSemIdx).text != ")" then
          var c = leadingCols + 1
          var k = 0
          while k < firstSemIdx do
            c += rest(k).text.length
            k += 1
          if c != expected then
            emit
             ( c, "R22-using-alignment",
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
              val nameIdx = nextSemantic(rest, nextSem + 1)
              if nameIdx >= 0 then
                var c = leadingCols + 1
                var k = 0
                while k < nameIdx do
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
