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

// Whole-file lexical extraction of `(`/`[` bracket pairs for the
// bracket-shaped rules: 811 (interior spacing of formal multi-line
// blocks) and 402 (no interior padding in compact single-line brackets).
// Bracket pairing is lexical — the same whole-file `(`/`[` stack the
// original per-line walk maintained, split there between an inline stack
// and a cross-line formality stack — so the extraction is deliberately
// token-based, like `QuoteSites`. Braces are ignored: the frozen
// behaviour pairs only round and square brackets, with no kind-matching
// between opener and closer.
//
// Each opener is classified at open time as a **formal candidate**: a
// line-start `(`/`[` continuing an application-like previous line, an
// `=> (` opener after a leading arrow, or a `:   (` opener after a
// leading colon (heavy-signature continuations). For a pair that closes
// on its opening line the classification is refined to a **formal
// block** — the closer must be followed only by a body opener, and the
// interior must have argument-list shape; otherwise the pair is a
// compact bracket governed by 402. A pair that closes on a later line
// keeps the candidate flag alone, exactly as the old cross-line stack
// did.
object Brackets:

  // One bracket pair. `closeLine`/`closeIdx` are -1 when the opener
  // never closes. `formal` is the full formal-block classification for a
  // single-line pair, and the opener's formal-candidate flag for a
  // multi-line or unmatched one. `openerIndent` is the leading-columns
  // value of the opener's line; `lambdaBodyTuple` marks a compact pair
  // spanning a whole line that follows a line ending in `=>` (a lambda
  // or match-case body tuple, exempt from 402).
  case class Pair
    ( openLine:        Int,
      openIdx:         Int,
      closeLine:       Int,
      closeIdx:        Int,
      formal:          Boolean,
      openerIndent:    Int,
      lambdaBodyTuple: Boolean ):

    def singleLine: Boolean = openLine == closeLine

    def matched: Boolean = closeLine >= 0

  case class Model(pairs: List[Pair])

  // One unclosed opener: its position, formal-candidate flag and the
  // leading columns of the line it opened on.
  private final case class Open(line: Int, idx: Int, formal: Boolean, indent: Int)

  def extract(lines: IndexedSeq[Line]): Model =
    val pairs = mutable.ListBuffer[Pair]()
    val stack = mutable.Stack[Open]()

    var prevTok            = ""
    var prevLineWasBlank   = false
    var prevCodeLineIndent = -1
    var prevStartedDecl    = false
    var prevFormalIndent   = -1
    var idx                = 0

    while idx < lines.length do
      val line    = lines(idx)
      val lineNum = idx + 1
      val arr     = line.arr

      val firstSemantic = arr.indexWhere: t => t.kind != Sort.Space && t.kind != Sort.Comment
      val lastSemantic  = arr.lastIndexWhere: t => t.kind != Sort.Space && t.kind != Sort.Comment

      val secondSemantic =
        if firstSemantic < 0 then -1
        else arr.indexWhere(t => t.kind != Sort.Space && t.kind != Sort.Comment, firstSemantic + 1)

      val lineStartsWithBracket =
        firstSemantic >= 0 &&
          (arr(firstSemantic).text == "(" || arr(firstSemantic).text == "[")

      // A line beginning `=> (`/`=> [` continues an anonymous-given chain;
      // one beginning `:   (`/`:   [` is a heavy-signature continuation.
      // Both make the bracket a formal-block candidate.
      val arrowParen =
        firstSemantic >= 0 && arr(firstSemantic).text == "=>" && secondSemantic > 0 &&
          (arr(secondSemantic).text == "(" || arr(secondSemantic).text == "[")

      val colonParen =
        firstSemantic >= 0 && arr(firstSemantic).text == ":" && secondSemantic > 0 &&
          (arr(secondSemantic).text == "(" || arr(secondSemantic).text == "[")

      val leadingCols = line.leadingCols

      // A multi-clause continuation aligns a fresh `(`/`[` clause under a
      // just-closed formal block's opener line, or under a declaration
      // signature still in progress at the same indent.
      val isMultiClauseContinuation =
        (prevTok == ")" || prevTok == "]") &&
          ((prevFormalIndent >= 0 && leadingCols == prevFormalIndent) ||
            (prevStartedDecl && leadingCols == prevCodeLineIndent))

      // A `(`/`[` after a line ending in `=>` is a lambda or
      // context-function body: a tuple expression, not an application.
      val isArrowBodyContinuation = prevTok == "=>" && !prevLineWasBlank

      // A line-start opener is a formal candidate only when the previous
      // line's last token is application-like — an identifier, closer or
      // modifier keyword — and the line steps in (or continues clauses).
      val prevIsApplication =
        if prevLineWasBlank then false
        else if isMultiClauseContinuation then true
        else if leadingCols <= prevCodeLineIndent then false
        else if prevTok == ")" || prevTok == "]" then true
        else if prevTok.isEmpty then false
        else if Checker.ModifierWords.contains(prevTok) then true
        else
          val c = prevTok.head
          (c.isLetter || c == '_' || c == '`') && !Checker.NonOperandWords.contains(prevTok)

      var currentFormalIndent = -1
      var i = 0

      while i < arr.length do
        if arr(i).kind == Sort.Code then
          val text = arr(i).text

          if text == "(" || text == "[" then
            val isLineStartOpener = lineStartsWithBracket && i == firstSemantic
            val isArrowOpener     = arrowParen && i == secondSemantic
            val isColonOpener     = colonParen && i == secondSemantic

            val formalCandidate =
              (isLineStartOpener && prevIsApplication) || isArrowOpener || isColonOpener

            stack.push(Open(lineNum, i, formalCandidate, leadingCols))
          else if text == ")" || text == "]" then
            if stack.nonEmpty then
              val open = stack.pop()

              val nextAfterCloser =
                arr.indexWhere(t => t.kind != Sort.Space && t.kind != Sort.Comment, i + 1)

              if open.line == lineNum then
                // For a colon-opener bracket, `(...)` must contain a
                // `name: type` annotation — otherwise it is a tuple type,
                // not a context-parameter list.
                val isColonOpenerBracket = colonParen && open.idx == secondSemantic

                val paramListShaped =
                  !isColonOpenerBracket || arr(open.idx).text == "[" ||
                    hasTopColon(arr, open.idx, i)

                // Formal block: the closer is the line's last semantic
                // token or is followed only by a body opener. Colon-opener
                // brackets additionally require an arrow continuation.
                val followedByBodyOpener =
                  if !paramListShaped then false
                  else if nextAfterCloser < 0 then true
                  else
                    val t = arr(nextAfterCloser).text

                    if isColonOpenerBracket then t == "=>"
                    else
                      t == ":" || t == "=" || t == "extends" || t == "derives" ||
                        t == ")" || t == "]" || t == "}" || t == "," || t == "=>"

                val isFormalBlock =
                  open.formal && followedByBodyOpener && hasArgListShape(arr, open.idx, i)

                if isFormalBlock && nextAfterCloser < 0 then currentFormalIndent = leadingCols

                val lambdaBodyTuple =
                  !isFormalBlock && isArrowBodyContinuation &&
                    open.idx == firstSemantic && i == lastSemantic

                pairs +=
                  Pair(lineNum, open.idx, lineNum, i, isFormalBlock, open.indent, lambdaBodyTuple)
              else
                if open.formal && nextAfterCloser < 0 then currentFormalIndent = open.indent

                pairs += Pair(open.line, open.idx, lineNum, i, open.formal, open.indent, false)

        i += 1

      // End-of-line state updates, mirroring the old per-line walk: the
      // last semantic token, the formal-opener indent transfer, the
      // declaration-signature state, and the previous code-line indent
      // (which comment-only, annotation-only and triple-quoted-string
      // continuation lines do not update).
      if !line.isBlank then
        val sem = line.rest.filter: t => t.kind != Sort.Space && t.kind != Sort.Comment

        sem.lastOption.foreach: t => prevTok = t.text

        prevFormalIndent = currentFormalIndent
        prevStartedDecl = Scans.declStep(sem, prevStartedDecl).startedDecl

        val isCommentOnly    = line.firstReal.exists(_.kind == Sort.Comment)
        val isAnnotationOnly = line.firstReal.exists(_.text.startsWith("@"))

        val isStringContinuation =
          line.firstReal.exists(_.kind == Sort.Strs) && line.leadingWs.isEmpty

        if !isCommentOnly && !isAnnotationOnly && !isStringContinuation then
          prevCodeLineIndent = leadingCols

      prevLineWasBlank = line.isBlank
      idx += 1

    stack.toList.reverse.foreach: open =>
      pairs += Pair(open.line, open.idx, -1, -1, open.formal, open.indent, false)

    Model(pairs.toList)

  // Single-line bracket between `opener` and `closer` (exclusive).
  // Returns true iff the contents look like an argument list: a
  // top-level comma or type-annotation `:` is present, or there are no
  // top-level binary operators. Returns false when the contents are a
  // pure grouping expression (no commas, no `:`, has operators like
  // `||`, `&&`, `==`, `+`, etc.).
  private def hasArgListShape(arr: Array[Lexeme], opener: Int, closer: Int): Boolean =
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
          else if ExpressionOperators.contains(text) then hasTopOperator = true

      k += 1

    hasTopComma || hasTopColon || !hasTopOperator

  // Operators that almost exclusively appear in expression position, not
  // in type signatures or argument annotations: arithmetic, comparison,
  // boolean. Type operators (`<:`, `>:`, `&`, `|`, `=>`, `?=>`, `+:`,
  // `:+`, etc.) are intentionally excluded — they appear in parameter
  // lists too.
  private val ExpressionOperators: Set[String] =
    Set("||", "&&", "==", "!=", "<=", ">=", "<", ">", "+", "-", "*", "/", "%")

  // True iff the bracket between `opener` and `closer` (exclusive)
  // contains a top-level `:` token at depth 0 — indicating a
  // `name: Type` annotation (parameter list). Used to distinguish
  // `:   ( a: T )` (param list) from `:   (T1, T2)` (tuple type).
  private def hasTopColon(arr: Array[Lexeme], opener: Int, closer: Int): Boolean =
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
