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

object AnchorRules:
  // -----------------------------------------------------------------------
  // R33: keyword-sequence layout (`if`/`then`/`else`, `for`/`yield`,
  // `for`/`do`, `while`/`do`, `try`/`catch`/`finally`).
  //
  // The chain elements (K₁, K₂, …) and their positions are extracted from
  // the untyped tree by `Sequences.extract`; this checker only applies the
  // rules. See `decorum.Sequences` for how the anchor and bridge concepts
  // map onto `untpd.If` / `WhileDo` / `ForYield` / `ForDo` / `Try`.
  //
  // A sequence K_1 B_1 K_2 B_2 … K_n B_n has an **anchor point** at the
  // (line L, column C) of K_1 (or the leftmost modifier prefix such as
  // `inline` / `transparent inline`, when present).
  //
  //   * Keyword placement (833.1): every subsequent keyword K_i (i ≥ 2) is
  //     either **inline** (starts on line L) or **broken** (starts a new
  //     line in column C). Once any K_i is broken, every later K_j must
  //     also be broken — i.e. the chain has a single break point,
  //     keywords before it on line L, keywords from it on their own lines
  //     in column C.
  //   * Body cascade (833.2): once any B_i (i ≥ 2) is indented onto its
  //     own line(s), every later body must be indented too. B_1 (the
  //     condition of `if`/`while`, the generators of `for`, the body of
  //     `try`) is exempt.
  //
  //   `else if` bridge: when an `else` is immediately followed by an `if`
  //   on the same line (possibly with modifiers between them), the trio
  //   `else if … then` is folded into a single chain element. Its
  //   placement column is the `else`'s, and the body whose layout is
  //   checked is the body after the inner `then`. This means
  //
  //       if cond1 then
  //         body1
  //       else if cond2 then
  //         body2
  //       else
  //         body3
  //
  //   is accepted: the inline `if cond2 then` does not register as an
  //   un-indented body for the outer `else`. The reference column for
  //   every broken keyword in the chain — every `then`, every `else`,
  //   every `else if` — remains the column of the first `if`.
  //
  //   If `else` is on its own line and the next `if` is on a later,
  //   more-indented line, the inner `if` is not a bridge: it starts a
  //   fresh chain with its own anchor, and the outer chain ends at
  //   `else`.
  //
  // The cascades fire forward only — a break in K_i does not retroactively
  // require K_1..K_{i-1} to break.
  // -----------------------------------------------------------------------
  object SequenceLayout extends Rule:
    def id: String = "833"
    def principle: Principle = Principle.Anchoring

    def check(ctx: Context): List[Violation] =
      val out = mutable.ListBuffer[Violation]()
      ctx.sequences.foreach(apply(ctx.file, _, out))
      out.toList

    // Apply the placement and body-cascade rules to one keyword sequence.
    // `seq.elements.head` is K₁; its (line, col) is the anchor point.
    private def apply(file: String, seq: Sequence, out: mutable.ListBuffer[Violation]): Unit =
      if seq.elements.length < 2 then return
      val anchor     = seq.anchor
      val anchorLine = anchor.line
      val anchorCol  = anchor.col

      var indentedMode = false
      var i            = 1
      while i < seq.elements.length do
        val elem         = seq.elements(i)
        val onAnchorLine = elem.line == anchorLine
        val brokenAtCol  = elem.startsLine && elem.col == anchorCol
        if !(onAnchorLine || brokenAtCol) then
          out +=
            Violation
              ( file, elem.line, elem.col, "833.1",
                s"keyword `${elem.label}` must start on the same line as "
                  +s"`${anchor.label}` (line $anchorLine) or on a new line in "
                  +s"column $anchorCol (found line ${elem.line}, column ${elem.col})" )
        // Body cascade applies to bodies after K₂ onward. K₁'s body (the
        // condition/generators/try-body) doesn't trigger the cascade.
        if indentedMode && !elem.bodyIndented then
          out +=
            Violation
              ( file, elem.line, elem.bodyCol, "833.2",
                s"body after `${elem.label}` must be indented onto a new line because "
                  +s"an earlier body in this sequence is" )
        if elem.bodyIndented then indentedMode = true
        i += 1

  // R33.3: a definition's type-annotation `:` must sit either on the same
  // line as the definition's anchor or in the anchor's column.
  //
  // Definition anchors are extracted from the untyped tree (see
  // `decorum.Definitions`), so the rule does not depend on the token
  // stream's bracket-nesting heuristics. The parser pairs each `:`
  // with its containing `ValOrDefDef` for us.
  object DefinitionAnchors extends Rule:
    def id: String = "833.3"
    def principle: Principle = Principle.Anchoring

    def check(ctx: Context): List[Violation] =
      ctx.definitions.flatMap: a =>
        if a.colonLine != a.anchorLine && a.colonCol != a.anchorCol then
          List
            ( Violation
                ( ctx.file, a.colonLine, a.colonCol, "833.3",
                  s"type-annotation `:` should align with the definition's anchor at "
                    +s"column ${a.anchorCol} (found ${a.colonCol})" ) )
        else
          Nil

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

    // An optional leading expression-introducing keyword followed by one
    // space, or an optional leading `.` (chain continuation) followed by
    // one space.
    if arr(i).kind == Sort.Code && ExprIntroKeywords.contains(arr(i).text) then
      sawCodeAtTopLevel = true
      i += 1

      if i < arr.length && arr(i).kind == Sort.Space && arr(i).text == " " then
        i += 1
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

  // A line whose leading `(`/`[` group is immediately followed by `=>` is a
  // lambda (or polymorphic-function) parameter list, not a heavy argument
  // continuation, so R33.4 must not flag it.
  private def lineOpensLambda(rest: IndexedSeq[Lexeme]): Boolean =
    val sem = rest.filter: t => t.kind != Sort.Space && t.kind != Sort.Comment

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

      close >= 0 && sem.lift(close + 1).exists(_.text == "=>")
    }

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
  // - The current line is inside an open multi-line bracket (a carried
  //   `(`-depth from a previous line) — the `(` is not a heavy
  //   continuation but interior content of an enclosing bracket.
  object HeavyBracketAnchor extends Rule:
    def id: String = "833.4"
    def principle: Principle = Principle.Anchoring

    def check(ctx: Context): List[Violation] =
      val out    = mutable.ListBuffer[Violation]()
      val depths = CarriedDepths(ctx)

      var prevLineWasBlank    = false
      var prevCodeLineIndent  = -1
      var prevTok             = ""
      var prevLineIsTight     = false
      var prevLineStartedDecl = false
      var idx = 0

      while idx < ctx.lines.length do
        val line    = ctx.lines(idx)
        val lineNum = idx + 1

        val headIsBracket =
          line.firstReal.exists: t =>
            t.kind == Sort.Code && (t.text == "(" || t.text == "[")

        if line.isBlank then ()
        else if depths.parens(idx) > 0 then ()
        else if prevLineWasBlank then ()
        else if !headIsBracket then ()
        // A lambda parameter list (`(params) =>`) is not a heavy argument
        // continuation; the `(…)` belongs to the lambda. Skip it.
        else if lineOpensLambda(line.rest) then ()
        // A heavy continuation is indented *more* than its anchor. If the
        // current line's indent is ≤ the previous code line's indent, the
        // `(`/`[` is a sibling statement (a tuple, parenthesised expression
        // or type-application standing on its own), not a continuation.
        else if prevCodeLineIndent < 0 || line.leadingCols <= prevCodeLineIndent then ()
        // If the previous line ends with a "body opener" — `=`, `=>`, `:`,
        // `then`, `else`, etc. — the current line is the body of that
        // construct (assignment RHS, lambda body, case body, etc.), not a
        // heavy continuation. Skip the check.
        else if BodyOpenerTerminators.contains(prevTok) then ()
        // If the previous line ends with a symbolic infix operator, the
        // current `(`/`[` begins the right-hand operand of that operator —
        // an operator continuation governed by R616, not a heavy argument
        // block. Skip.
        else if Scans.isSymbolicOperator(prevTok) then ()
        // If the previous line opened a block, quote or splice (ending in
        // `{`), the current line is the first expression *inside* that
        // scope — a tuple or parenthesised value standing on its own, not
        // an argument continuation of a mid-line receiver. Skip.
        else if prevTok == "{" then ()
        else if prevLineIsTight then ()
        else if prevLineStartedDecl then ()
        else
          out +=
            Violation
              ( ctx.file, lineNum, line.leadingCols + 1, "833.4",
                "heavy `(`/`[` continuation must follow a tight expression on its " +
                  "own line; the preceding line contains a top-level operator or " +
                  "assignment, so the anchor is mid-line" )

        if !line.isBlank then
          val sem = semTokens(line)

          sem.lastOption.foreach: t => prevTok = t.text

          prevLineStartedDecl = Scans.declStep(sem, prevLineStartedDecl).startedDecl
          prevLineIsTight = isTightExpression(line.lexemes)

          val isCommentOnly    = line.firstReal.exists(_.kind == Sort.Comment)
          val isAnnotationOnly = line.firstReal.exists(_.text.startsWith("@"))

          val isStringContinuation =
            line.firstReal.exists(_.kind == Sort.Strs) && line.leadingWs.isEmpty

          if !isCommentOnly && !isAnnotationOnly && !isStringContinuation then
            prevCodeLineIndent = line.leadingCols

        prevLineWasBlank = line.isBlank
        idx += 1

      out.toList

  // ------------------------------------------------------------------------
  // Shared lexical tables for the indentation rules (473.8/473.1). Both rules
  // suspend inside brackets and braces left open by an earlier line, so a
  // single linear pre-scan computes, for each line, the bracket state carried
  // *into* it: the count of unclosed `(`/`[` openers from earlier lines, the
  // count of unclosed `{` braces, and the net `(`-depth (clamped at zero, as
  // the old checker's `openParens` was).
  // ------------------------------------------------------------------------
  private final class CarriedDepths(ctx: Context):
    val length: Int = ctx.lines.length
    val brackets: Array[Int] = new Array[Int](length)
    val braces:   Array[Int] = new Array[Int](length)
    val parens:   Array[Int] = new Array[Int](length)

    private var crossBrackets = 0
    private var crossBraces   = 0
    private var openParens    = 0
    private var idx           = 0

    while idx < length do
      brackets(idx) = crossBrackets
      braces(idx)   = crossBraces
      parens(idx)   = openParens

      val arr = ctx.lines(idx).arr
      var bracketOpens = 0
      var bracketPops  = 0
      var braceOpens   = 0
      var bracePops    = 0
      var parenDepth   = openParens
      var i = 0

      while i < arr.length do
        if arr(i).kind == Sort.Code then
          arr(i).text match
            case "(" | "[" =>
              bracketOpens += 1
              if arr(i).text == "(" then parenDepth += 1

            case ")" | "]" =>
              if bracketOpens > 0 then bracketOpens -= 1 else bracketPops += 1
              if arr(i).text == ")" then parenDepth -= 1

            case "{" =>
              braceOpens += 1

            case "}" =>
              if braceOpens > 0 then braceOpens -= 1 else bracePops += 1

            case _ =>
              ()

        i += 1

      crossBrackets = 0.max(crossBrackets - bracketPops) + bracketOpens
      crossBraces   = 0.max(crossBraces - bracePops) + braceOpens
      openParens    = parenDepth.max(0)
      idx += 1

  // The semantic (non-whitespace, non-comment) tokens of a line.
  private def semTokens(line: Line): IndexedSeq[Lexeme] =
    line.rest.filter: t => t.kind != Sort.Space && t.kind != Sort.Comment

  // True if the semantic tokens of a line end with `' {` or `$ {` (the
  // opening of a quote/splice block whose body lives on the following lines).
  private def endsWithQuoteSpliceBrace(sem: IndexedSeq[Lexeme]): Boolean =
    if sem.length < 2 then false
    else
      val last = sem(sem.length - 1).text
      val prev = sem(sem.length - 2).text
      last == "{" && (prev == "'" || prev == "$")

  // True if the line is a chain method-application opener — the first
  // semantic token is `.` and the last is `:` or `=>` (e.g. `. within:` or
  // `. map: x =>`). Such lines deepen indentation by +4 rather than +2.
  private def isChainOpener(sem: IndexedSeq[Lexeme]): Boolean =
    if sem.isEmpty then false
    else
      val first = sem(0).text
      val last  = sem(sem.length - 1).text
      first == "." && (last == ":" || last == "=>")

  // Keyword-sequence heads (`if/then/else`, `while/do`, `for/yield`,
  // `try/catch/finally`). Lines headed by one of these keep their own anchor
  // (833.x) and never open a 473.8 scope of their own.
  private val SequenceStarters: Set[String] =
    Set("if", "while", "for", "try", "then", "else", "do", "yield", "catch", "finally")

  private final class Scope(val anchorCol: Int, val openLine: Int):
    var bodySeen: Boolean = false

  // The core scan shared by 473.8 (which emits its violations) and 473.1
  // (which suppresses itself on each scope's first body line). The scope
  // inventory comes from the tree-derived anchor model (`ctx.anchors`):
  // every frame's opener line opens one scope, except `ChainCall` frames
  // (their +4 body indent is governed by 473.1's deep-step allowance),
  // `QuoteSplice` frames (governed by 473.2–.6), keyword-sequence starter
  // lines (governed by 833.x), and `=>`-led given-signature continuation
  // lines (governed by R32/140).
  //
  // Anchor semantics: the scope's anchor is the *opener* line's leading
  // column (for `DefnRhs` frames this is the `=` line, not the model's
  // keyword-line `anchorCol`; behaviour preservation of the old checker
  // pins this choice — revisiting it is a rule change for a later step).
  // A scope is popped when a later checkable line dedents to or past its
  // anchor; only the first checkable interior line of the innermost open
  // scope is checked, and must sit at exactly anchor + 2. Lines inside
  // carried-over brackets or braces are neither checked nor pop scopes.
  //
  // Lexical fallback: a line with no tree-derived frame still opens a scope
  // when its final semantic token is `:` / `match` / `=>` / `=` (subject to
  // the same exclusions). Two corners need this: assignment statements
  // (`x(i) =` with the value on the next line — `untpd.Assign` is not part
  // of the anchor model), and regions the standalone parser cannot fully
  // parse (capture-checking syntax such as `^{...}` mangles the enclosing
  // definitions' trees, taking their frames with them).
  private def bodyScopeScan
    ( ctx:    Context,
      depths: CarriedDepths )
  :   (List[Violation], Set[Int]) =

    val out       = mutable.ListBuffer[Violation]()
    val bodyLines = mutable.Set[Int]()

    val scopeLines: Map[Int, Int] =
      val builder = Map.newBuilder[Int, Int]

      ctx.anchors.frames.foreach: frame =>
        if frame.kind != Anchors.FrameKind.ChainCall &&
          frame.kind != Anchors.FrameKind.QuoteSplice
        then
          val opener = ctx.lines(frame.openLine - 1)
          val head   = semTokens(opener).headOption.map(_.text).getOrElse("")

          if !SequenceStarters.contains(head) && head != "=>" then
            builder += frame.openLine -> opener.leadingCols

      builder.result()

    val stack = mutable.Stack[Scope]()
    var idx   = 0

    while idx < ctx.lines.length do
      val line    = ctx.lines(idx)
      val lineNum = idx + 1

      val checkable =
        !line.isBlank &&
          depths.brackets(idx) == 0 && depths.braces(idx) == 0 && depths.parens(idx) == 0 &&
          !line.firstReal.exists(_.kind == Sort.Comment) &&
          !(line.firstReal.exists(_.kind == Sort.Strs) && line.leadingCols == 0)

      if checkable then
        while stack.nonEmpty && line.leadingCols <= stack.top.anchorCol do stack.pop()

        if stack.nonEmpty then
          val top = stack.top

          if lineNum != top.openLine && !top.bodySeen then
            top.bodySeen = true
            bodyLines += lineNum

            if line.leadingCols != top.anchorCol + 2 then
              out +=
                Violation
                  ( ctx.file, lineNum, line.leadingCols + 1, "473.8",
                    s"indented scope body must be indented to column ${top.anchorCol + 3} " +
                      s"(anchor + 2); found column ${line.leadingCols + 1}" )

      if !line.isBlank then
        val anchorCol =
          scopeLines.get(lineNum).orElse:
            val sem  = semTokens(line)
            val head = sem.headOption.map(_.text).getOrElse("")

            val opens =
              sem.lastOption.exists: t =>
                t.text == ":" || t.text == "match" || t.text == "=>" || t.text == "="

            if opens && !SequenceStarters.contains(head) && head != "=>" && !isChainOpener(sem)
            then Some(line.leadingCols)
            else None

        anchorCol.foreach: col => stack.push(Scope(col, lineNum))

      idx += 1

    (out.toList, bodyLines.toSet)

  // R473.8: the body of an indented scope (a colon-block `recv:`, a `match`,
  // a lambda/`case` arrow `=>`, or a definition `=` RHS) must sit at exactly
  // anchor + 2. Bracket and quote interiors are governed by R30/R12/R44.
  object BodyScopeIndent extends Rule:
    def id: String = "473.8"
    def principle: Principle = Principle.Anchoring

    def check(ctx: Context): List[Violation] =
      bodyScopeScan(ctx, CarriedDepths(ctx))(0)

  // R473.1: a non-blank code line cannot be indented more than two columns
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
  // earlier line (alignment there is governed by R12/R22/R30 and the
  // surrounding bracket structure), inside a multi-line import's selector
  // list, on a lone `for`-filter row (governed by 924.3), and on the first
  // body line of an indented scope (governed by 473.8).
  object ContinuationIndent extends Rule:
    def id: String = "473.1"
    def principle: Principle = Principle.Anchoring

    def check(ctx: Context): List[Violation] =
      val out    = mutable.ListBuffer[Violation]()
      val depths = CarriedDepths(ctx)
      val (_, bodyLines) = bodyScopeScan(ctx, depths)

      val importLineSet = ctx.imports.iterator.flatMap{ i => i.startLine to i.endLine }.toSet

      val forFilterLines: Set[Int] =
        val builder = Set.newBuilder[Int]

        ctx.forGroups.foreach: gens =>
          gens.foreach: gl =>
            if gl.isFilter then
              val shared =
                gens.exists: other =>
                  (other ne gl) && other.line == gl.line

              if !shared then builder += gl.line

        builder.result()

      var prevCodeLineIndent   = -1
      var prevOpensQuoteSplice = false
      var prevOpensChain       = false
      var idx = 0

      while idx < ctx.lines.length do
        val line    = ctx.lines(idx)
        val lineNum = idx + 1

        if !line.isBlank then
          val isCommentOnly   = line.firstReal.exists(_.kind == Sort.Comment)
          val isStringContent = line.firstReal.exists(_.kind == Sort.Strs)

          val suppressed =
            bodyLines.contains(lineNum) || prevCodeLineIndent < 0 ||
              depths.brackets(idx) > 0 || depths.braces(idx) > 0 ||
              importLineSet.contains(lineNum) || forFilterLines.contains(lineNum) ||
              isCommentOnly || isStringContent

          if !suppressed then
            val deepStep   = prevOpensQuoteSplice || prevOpensChain
            val step       = if deepStep then 4 else 2
            val maxAllowed = prevCodeLineIndent + step

            if line.leadingCols > maxAllowed then
              out +=
                Violation
                  ( ctx.file, lineNum, line.leadingCols + 1, "473.1",
                    s"indent ${line.leadingCols} cannot exceed previous line's indent " +
                      s"$prevCodeLineIndent by more than $step" )

          val sem = semTokens(line)
          prevOpensQuoteSplice = endsWithQuoteSpliceBrace(sem)
          prevOpensChain       = isChainOpener(sem)

          // Comment-only, annotation-only and triple-quoted-string
          // continuation lines don't update the previous-code-line indent.
          val isAnnotationOnly = line.firstReal.exists(_.text.startsWith("@"))

          val isStringContinuation =
            line.firstReal.exists(_.kind == Sort.Strs) && line.leadingWs.isEmpty

          if !isCommentOnly && !isAnnotationOnly && !isStringContinuation then
            prevCodeLineIndent = line.leadingCols

        idx += 1

      out.toList

  // R473.9: in a multi-line `def`/`given` signature, the body-introducing
  // top-level `=` must be the last code token on the final signature line.
  //
  // The trigger condition is lexical: the line has a top-level `=`, the
  // previous code line left a declaration signature in progress (it started
  // with a declaration keyword or modifier, or continued one with a `(`/`[`
  // clause, without reaching its own top-level `=`), and this line begins
  // with a genuine signature-continuation token (`:` return type, or a
  // `(`/`[` parameter clause). Template-body members (`class Foo:` followed
  // by `val a = 1`) do not qualify: their head token is the member's own
  // keyword.
  object SignatureEqLast extends Rule:
    def id: String = "473.9"
    def principle: Principle = Principle.Anchoring

    def check(ctx: Context): List[Violation] =
      val out = mutable.ListBuffer[Violation]()
      var prevLineStartedDecl = false
      var idx = 0

      while idx < ctx.lines.length do
        val line = ctx.lines(idx)
        val lineNum = idx + 1

        if !line.isBlank then
          val sem  = semTokens(line)
          val step = Scans.declStep(sem, prevLineStartedDecl)

          val wasDeclInProgress = prevLineStartedDecl
          prevLineStartedDecl = step.startedDecl
          val headTok = sem.headOption.map(_.text).getOrElse("")

          if step.hasTopLevelEq && wasDeclInProgress &&
            (headTok == ":" || headTok == "(" || headTok == "[")
          then
            var col     = line.leadingCols + 1
            var depth   = 0
            var seenEq  = false
            var emitCol = -1

            line.rest.foreach: t =>
              if t.kind == Sort.Code then
                if seenEq && emitCol < 0 then emitCol = col
                else if t.text == "(" || t.text == "[" || t.text == "{" then depth += 1
                else if t.text == ")" || t.text == "]" || t.text == "}" then depth -= 1
                else if depth == 0 && t.text == "=" then seenEq = true

              col += t.text.length

            if emitCol >= 0 then
              out +=
                Violation
                  ( ctx.file, lineNum, emitCol, "473.9",
                    "the body-introducing `=` of a multi-line definition signature must " +
                      "be the last token on the signature's final line" )

        idx += 1

      out.toList

  // R32 (140): continuation lines of a multi-line `given` signature that
  // begin with `=>` must align vertically with the leading modifier or
  // `given` keyword on the first line of the signature. The signature anchor
  // is tracked lexically: a line whose first semantic token (after any
  // modifiers) is `given` records its indent as the anchor; the anchor
  // clears when the body opener (`=`) appears at top level, or when a line
  // that is neither an `=>` continuation nor part of the signature begins.
  object GivenArrowAlign extends Rule:
    def id: String = "140"
    def principle: Principle = Principle.Anchoring

    def check(ctx: Context): List[Violation] =
      val out = mutable.ListBuffer[Violation]()
      var givenSignatureIndent = -1
      var prevLineStartedDecl  = false
      var idx = 0

      while idx < ctx.lines.length do
        val line    = ctx.lines(idx)
        val lineNum = idx + 1

        if !line.isBlank then
          val sem = semTokens(line)

          if givenSignatureIndent >= 0 && sem.headOption.exists(_.text == "=>") &&
            line.leadingCols != givenSignatureIndent
          then
            out +=
              Violation
                ( ctx.file, lineNum, line.leadingCols + 1, "140",
                  s"`=>` continuation of a `given` signature should align at column " +
                    s"${givenSignatureIndent + 1} (found ${line.leadingCols + 1})" )

          val step = Scans.declStep(sem, prevLineStartedDecl)
          prevLineStartedDecl = step.startedDecl

          // A line that begins a `given` declaration (after any modifiers)
          // records its leading-cols as the anchor for arrow continuation.
          val kwIdx = skipModifierTokens(sem)

          val startsGiven =
            kwIdx < sem.length && sem(kwIdx).kind == Sort.Code && sem(kwIdx).text == "given"

          if startsGiven then givenSignatureIndent = line.leadingCols
          else if givenSignatureIndent >= 0 && step.hasTopLevelEq then givenSignatureIndent = -1

          // Any line that's neither an `=>` continuation nor part of the
          // initial signature ends the given-signature region.
          if givenSignatureIndent >= 0 && !sem.headOption.exists(_.text == "=>") then
            if !step.startsWithDecl && !step.continuesDecl then givenSignatureIndent = -1

        idx += 1

      out.toList

    private def skipModifierTokens(tokens: IndexedSeq[Lexeme]): Int =
      var i = 0

      while i < tokens.length && tokens(i).kind == Sort.Code &&
        Scans.ModifierWords.contains(tokens(i).text) && tokens(i).text != "given"
      do i += 1

      i

  // R560: a multi-line `m`/`j`/`x`/`y`/`tel` triple-quoted string is laid out as
  // a block — the opening quotes end their line, the content is indented two
  // columns beyond the prefix, no content line is indented less than the first,
  // and the closing `"""` is alone on its line aligned with the prefix. A
  // leading `( ` before the opener and a trailing `,`/`)` after the closer are
  // permitted (the surrounding application syntax), so "alone" means the string
  // content does not share the opener/closer line.
  object InterpolationLayout extends Rule:
    def id: String = "560"
    def principle: Principle = Principle.Anchoring

    // Interpolators whose leading/trailing whitespace is insignificant, so a
    // multi-line `"""…"""` may be laid out as a block (R560). Other interpolators
    // (`t`, `s`, `sh`, …) and raw `"""` strings carry significant whitespace and
    // are exempt — see the R2/R4 relaxation in `checkLine`.
    private val LayoutInterpolators: Set[String] = Set("m", "j", "x", "y", "tel")

    def check(ctx: Context): List[Violation] =
      val file = ctx.file
      val out = mutable.ListBuffer[Violation]()
      val lines = ctx.text.split("\n", -1).nn
      def lineText(n: Int): String = if n >= 1 && n <= lines.length then lines(n - 1).nn else ""
      // 1-based column of the first non-space character, or 0 if the line is blank.
      def firstCol(text: String): Int =
        var i = 0
        while i < text.length && (text.charAt(i) == ' ' || text.charAt(i) == '\t') do i += 1
        if i >= text.length then 0 else i + 1

      ctx.interpolations.foreach: info =>
        if LayoutInterpolators.contains(info.prefix) && info.triple
          && info.closeLine > info.openLine
        then
          val col      = info.openCol         // column of the prefix character
          val expected = col + 2              // required column of the content
          val q        = "\"\"\""
          val openText = lineText(info.openLine)
          val afterOpen = (info.openCol - 1) + info.prefix.length + 3

          val openerEndsLine =
            afterOpen >= openText.length || openText.substring(afterOpen).nn.trim.nn.isEmpty

          if !openerEndsLine then
            out +=
              Violation
                ( file, info.openLine, afterOpen + 1, "560.1",
                  s"the content of a multi-line `${info.prefix}$q` string must begin on the "
                    +"line after the opening quotes" )
          else
            var first = -1
            var ln    = info.openLine + 1
            while ln < info.closeLine do
              val fc = firstCol(lineText(ln))
              if fc != 0 then
                if first < 0 then first = fc
                if fc < expected then
                  out +=
                    Violation
                      ( file, ln, fc, "560.3",
                        s"a line of a multi-line `${info.prefix}$q` string must not be indented "
                          +s"less than the first content line (column $expected)" )
              ln += 1
            if first >= 0 && first != expected then
              out +=
                Violation
                  ( file, info.openLine + 1, first, "560.2",
                    s"the content of a multi-line `${info.prefix}$q` string must be indented to "
                      +s"column $expected" )

          val closeText  = lineText(info.closeLine)
          val closeFirst = firstCol(closeText)
          val afterClose = (info.closeCol - 1) + 3
          val trailing   =
            if afterClose <= closeText.length then closeText.substring(afterClose).nn else ""
          val trailingOk = trailing.forall(c => c == ' ' || c == '\t' || c == ',' || c == ')')
          if closeFirst != info.closeCol || info.closeCol != col || !trailingOk then
            out +=
              Violation
                ( file, info.closeLine, (if closeFirst == 0 then 1 else closeFirst), "560.4",
                  s"the closing `$q` of a multi-line `${info.prefix}$q` string must be alone on "
                    +s"its line, aligned with the opening quotes (column $col)" )

      out.toList
