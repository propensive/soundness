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

object BalanceRules:
  // R30 (811): a formal multi-line block pads its interior — a space is
  // required directly inside both the opener and the closer — and its
  // closing bracket shares a line with the last parameter rather than
  // standing alone. A formal pair that opens and closes on one line
  // checks both interior edges on the spot; one that spans lines checks
  // the opener's inside edge on the opening line and the closer's
  // leading edge on the closing line. The pair inventory and formality
  // classification come from `ctx.brackets` (see `decorum.Brackets`).
  object FormalBlockSpacing extends Rule:
    def id: String = "811"
    def principle: Principle = Principle.Balance

    def check(ctx: Context): List[Violation] =
      val out = mutable.ListBuffer[Violation]()

      ctx.brackets.pairs.foreach: pair =>
        if pair.formal then
          if pair.singleLine then checkInline(ctx, pair, out)
          else
            checkOpener(ctx, pair, out)
            if pair.matched then checkCloser(ctx, pair, out)

      out.toList

    private def checkInline
      ( ctx: Context, pair: Brackets.Pair, out: mutable.ListBuffer[Violation] )
    :   Unit =

      val line = ctx.lines(pair.openLine - 1)
      val arr  = line.arr
      val cols = line.cols

      if pair.closeIdx > pair.openIdx + 1 then
        if arr(pair.openIdx + 1).kind != Sort.Space then
          out +=
            Violation
              ( ctx.file, pair.openLine, cols(pair.openIdx + 1), "811",
                s"a space is required after `${arr(pair.openIdx).text}` in a multi-line block" )

        if arr(pair.closeIdx - 1).kind != Sort.Space then
          out +=
            Violation
              ( ctx.file, pair.openLine, cols(pair.closeIdx), "811",
                s"a space is required before `${arr(pair.closeIdx).text}` in a " +
                  "multi-line block" )

    private def checkOpener
      ( ctx: Context, pair: Brackets.Pair, out: mutable.ListBuffer[Violation] )
    :   Unit =

      val line = ctx.lines(pair.openLine - 1)
      val arr  = line.arr
      val cols = line.cols

      if pair.openIdx + 1 >= arr.length || arr(pair.openIdx + 1).kind != Sort.Space then
        out +=
          Violation
            ( ctx.file, pair.openLine, cols(pair.openIdx + 1), "811",
              s"a space is required after `${arr(pair.openIdx).text}` in a multi-line block" )

    private def checkCloser
      ( ctx: Context, pair: Brackets.Pair, out: mutable.ListBuffer[Violation] )
    :   Unit =

      val line = ctx.lines(pair.closeLine - 1)
      val arr  = line.arr
      val cols = line.cols
      val text = arr(pair.closeIdx).text

      val firstSemantic = arr.indexWhere: t => t.kind != Sort.Space && t.kind != Sort.Comment

      if pair.closeIdx == firstSemantic then
        out +=
          Violation
            ( ctx.file, pair.closeLine, cols(pair.closeIdx), "811",
              s"`$text` cannot appear alone on its line; the closing bracket of a " +
                "multi-line block goes on the same line as the last parameter" )
      else if pair.closeIdx > 0 && arr(pair.closeIdx - 1).kind != Sort.Space then
        out +=
          Violation
            ( ctx.file, pair.closeLine, cols(pair.closeIdx), "811",
              s"a space is required before `$text` in a multi-line block" )

  // R12 (402): a compact single-line bracket pair — one that is not a
  // formal block — must not pad its interior with spaces. Tuples on a
  // fresh line after `=>` (lambda and match-case body tuples) are
  // exempt: the author may use either tight or formal-style spacing.
  object CompactBracketSpacing extends Rule:
    def id: String = "402"
    def principle: Principle = Principle.Balance

    def check(ctx: Context): List[Violation] =
      val out = mutable.ListBuffer[Violation]()

      ctx.brackets.pairs.foreach: pair =>
        if pair.singleLine && !pair.formal && !pair.lambdaBodyTuple &&
          pair.closeIdx > pair.openIdx + 1
        then
          val line = ctx.lines(pair.openLine - 1)
          val arr  = line.arr
          val cols = line.cols

          val firstInside = arr(pair.openIdx + 1)
          val lastInside  = arr(pair.closeIdx - 1)

          if firstInside.kind == Sort.Space && firstInside.text.length > 0 then
            out +=
              Violation
                ( ctx.file, pair.openLine, cols(pair.openIdx + 1), "402",
                  s"no space is permitted directly after `${arr(pair.openIdx).text}`" )

          if lastInside.kind == Sort.Space && lastInside.text.length > 0 &&
            (pair.closeIdx - 1) != (pair.openIdx + 1)
          then
            out +=
              Violation
                ( ctx.file, pair.openLine, cols(pair.closeIdx - 1), "402",
                  s"no space is permitted directly before `${arr(pair.closeIdx).text}`" )

      out.toList

  // R-376: binary-operator spacing. Within each **frame** — a bracket
  // level, further cut at every comma, control-flow keyword and `=>` —
  // an operator's spacing must be symmetric, multi-character operators
  // must be spaced, same-precedence operators must agree with each
  // other, and no spaced operator may bind tighter than an unspaced one.
  // Frames are strictly per-line: an opener carried across a line break
  // starts afresh, exactly as the old per-line walk did.
  object OperatorSpacing extends Rule:
    def id: String = "376"
    def principle: Principle = Principle.Balance

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

    private def isSpaced(op: OpHit): Boolean = op.leftSpace || op.rightSpace
    private def precedenceOf(op: OpHit): Int = operatorPrecedence(op.text)

    // Control-flow keywords that separate sub-expressions: encountering one
    // closes the current operator frame and opens a fresh one at the same
    // nesting depth, just like `,` does. `case` is excluded when used as a
    // modifier (`case class`, `case object`).
    private val BoundaryWords: Set[String] =
      Set
        ( "if", "then", "else", "match", "case", "do", "while", "for", "yield",
          "return", "throw", "try", "catch", "finally" )

    private def skippable(t: Lexeme): Boolean = t.kind == Sort.Space || t.kind == Sort.Comment

    private def caseIsModifier(arr: Array[Lexeme], i: Int): Boolean =
      var j = i + 1
      while j < arr.length && skippable(arr(j)) do j += 1

      j < arr.length && arr(j).kind == Sort.Code &&
        (arr(j).text == "class" || arr(j).text == "object")

    private def isBinaryContext(arr: Array[Lexeme], i: Int): Boolean =
      val left =
        var j = i - 1
        while j >= 0 && skippable(arr(j)) do j -= 1

        if j < 0 then false
        else if arr(j).kind == Sort.Strs then true
        else if arr(j).kind == Sort.Code then
          val t = arr(j).text

          if t == ")" || t == "]" then true
          else if t.isEmpty then false
          else
            val c = t.head
            (c.isLetterOrDigit || c == '_' || c == '`') && !Scans.NonOperandWords.contains(t)
        else
          false

      val right =
        var j = i + 1
        while j < arr.length && skippable(arr(j)) do j += 1

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

            (c.isLetterOrDigit || c == '_' || c == '`' || c == '"' || c == '\'') &&
              !Scans.NonOperandWords.contains(t)
        else
          false

      left && right

    def check(ctx: Context): List[Violation] =
      val out = mutable.ListBuffer[Violation]()
      var idx = 0

      while idx < ctx.lines.length do
        checkLine(ctx, idx + 1, ctx.lines(idx), out)
        idx += 1

      out.toList

    private def checkLine
      ( ctx: Context, lineNum: Int, line: Line, out: mutable.ListBuffer[Violation] )
    :   Unit =

      val arr  = line.arr
      val cols = line.cols

      def emit(col: Int, message: String): Unit =
        out += Violation(ctx.file, lineNum, col, "376", message)

      val firstSemantic = arr.indexWhere: t => !skippable(t)
      val lastSemantic  = arr.lastIndexWhere: t => !skippable(t)
      val frames        = mutable.Stack[mutable.ArrayBuffer[OpHit]]()
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
          else if BoundaryWords.contains(text) &&
            !(text == "case" && caseIsModifier(arr, i))
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
                emit(cols(i), s"`$text` has asymmetric spacing — use 0 or 1 space on both sides")

              // Multi-char operators must have one space around (zero is reserved
              // for single-character operators).
              if text.length > 1 && !isAtStart && !isAtEnd && (!leftSpace || !rightSpace) then
                emit(cols(i), s"multi-character `$text` requires one space on each side")

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
      ( ops: mutable.ArrayBuffer[OpHit], emit: (Int, String) => Unit )
    :   Unit =

      if ops.isEmpty then ()
      else
        // Same-precedence consistency: every operator at a given precedence
        // must use the same spacing within this frame.
        ops.groupBy(precedenceOf).foreach: (_, group) =>
          val mixed = group.exists(isSpaced) && !group.forall(isSpaced)

          if mixed then
            group.foreach: op =>
              emit
                ( op.col,
                  s"`${op.text}` has inconsistent spacing with same-precedence operators" )

        // Cross-precedence ordering: every spaced operator must have *strictly*
        // lower precedence than every unspaced operator.  Equivalently: the
        // highest-precedence spaced operator must be lower than the
        // lowest-precedence unspaced one.
        var maxSpacedPrec   = Int.MinValue
        var minUnspacedPrec = Int.MaxValue

        ops.foreach: op =>
          val prec = operatorPrecedence(op.text)
          if isSpaced(op) && prec > maxSpacedPrec then maxSpacedPrec = prec
          if !isSpaced(op) && prec < minUnspacedPrec then minUnspacedPrec = prec

        if maxSpacedPrec > minUnspacedPrec then
          ops.foreach: op =>
            if isSpaced(op) && operatorPrecedence(op.text) > minUnspacedPrec then
              emit
                ( op.col,
                  s"`${op.text}` cannot have more spacing than lower-precedence " +
                    "operators in the same expression" )

  // R-376.1: assignment and mutation operators (`=`, `+=`, `-=`, etc.)
  // require *at least* one space before and *exactly* one space after
  // when the right-hand side appears on the same line. The "at least one
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
  object AssignmentSpacing extends Rule:
    def id: String = "376.1"
    def principle: Principle = Principle.Balance

    private val AssignOps: Set[String] =
      Set("=", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "<<=", ">>=", ">>>=")

    def check(ctx: Context): List[Violation] =
      val out = mutable.ListBuffer[Violation]()
      var idx = 0

      while idx < ctx.lines.length do
        val lineNum = idx + 1
        val line    = ctx.lines(idx)
        val arr     = line.arr
        val cols    = line.cols

        val lastSemantic =
          arr.lastIndexWhere: t => t.kind != Sort.Space && t.kind != Sort.Comment

        var i = 0

        while i < arr.length do
          val t = arr(i)

          if t.kind == Sort.Code && AssignOps.contains(t.text) && i != lastSemantic then
            // `name_=` setter method-name suffix: skip. `=` follows an
            // identifier ending in `_` with no intervening space.
            val isSetterSuffix =
              t.text == "=" && i > 0 && arr(i - 1).kind == Sort.Code &&
                arr(i - 1).text.endsWith("_")

            if !isSetterSuffix then
              val leftHasSpace =
                i > 0 && arr(i - 1).kind == Sort.Space && arr(i - 1).text.length >= 1

              val rightExactSpace =
                i + 1 < arr.length && arr(i + 1).kind == Sort.Space && arr(i + 1).text == " "

              if !leftHasSpace then
                out +=
                  Violation
                    ( ctx.file, lineNum, cols(i), "376.1",
                      s"`${t.text}` requires at least one space before it when " +
                        "the right-hand side is on the same line" )
              else if !rightExactSpace then
                out +=
                  Violation
                    ( ctx.file, lineNum, cols(i), "376.1",
                      s"`${t.text}` requires exactly one space after it when " +
                        "the right-hand side is on the same line" )

          i += 1

        idx += 1

      out.toList

  // R-013: a symbolic method name (`def +`, `def <=`, …) must be
  // separated from a directly-following `(` or `[` by a single space —
  // otherwise the opener reads as part of the operator glyph.
  object SymbolicMethodNames extends Rule:
    def id: String = "013"
    def principle: Principle = Principle.Balance

    def check(ctx: Context): List[Violation] =
      val out = mutable.ListBuffer[Violation]()
      var idx = 0

      while idx < ctx.lines.length do
        val lineNum = idx + 1
        val line    = ctx.lines(idx)
        val arr     = line.arr
        val cols    = line.cols

        var i = 0

        while i < arr.length do
          if arr(i).kind == Sort.Code && arr(i).text == "def" then
            var j = i + 1

            while j < arr.length && (arr(j).kind == Sort.Space || arr(j).kind == Sort.Comment) do
              j += 1

            if j < arr.length && arr(j).kind == Sort.Code && Scans.isSymbolicOperator(arr(j).text)
            then
              val opIdx   = j
              val nextIdx = opIdx + 1

              if nextIdx < arr.length && arr(nextIdx).kind == Sort.Code &&
                (arr(nextIdx).text == "(" || arr(nextIdx).text == "[")
              then
                out +=
                  Violation
                    ( ctx.file, lineNum, cols(nextIdx), "013",
                      s"a single space is required between `${arr(opIdx).text}` and " +
                        s"`${arr(nextIdx).text}`" )

          i += 1

        idx += 1

      out.toList
