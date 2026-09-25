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
package harlequin

import anticipation.*
import denominative.{Ordinal, nil}
import denominative.dysasymptotics.linearSize
import gossamer.*
import prophesy.ScalaKeywords
import proscenium.*
import rudiments.*
import vacuous.*

// Analysis of a code fragment around a cursor: the pure-text and tokenized splitting a
// completion host performs before any compiler runs — where the partial identifier begins,
// whether it selects a member, and whether the token before it is a value receiver awaiting an
// infix method. Lifted from the Flame REPL, so any completion host — a REPL, a debugger
// console — shares one implementation.
object Fragment:
  // A character which may form part of an alphanumeric identifier.
  def identifierChar(char: Char): Boolean = char.isLetterOrDigit || char == '_'

  // The offset at which the partial identifier ending at `offset` begins.
  def identifierStart(code: Text, offset: Int): Int =
    var start: Int = offset
    while start > 0 && identifierChar(code.s.charAt(start - 1)) do start -= 1
    start

  // Splits `code` at the cursor into the member-selection base — everything up to and
  // including the `.` immediately before the partial member name — and that partial. An
  // `Unset` base means the cursor is not selecting a member (a first-token identifier, the
  // first segment of an import, …), so there is no fixed type to enumerate against.
  def memberBase(code: Text, offset: Int): (Optional[Text], Text) =
    val start: Int = identifierStart(code, offset)
    val prefix: Text = code.keep(offset).skip(start)

    if start > 0 && code.s.charAt(start - 1) == '.' then (code.keep(start), prefix)
    else (Unset, prefix)

  // Keywords that make a following identifier a name, type or path rather than a value, so it
  // is not an infix receiver (`val x`, `def f`, `import p`, `case P`, `new T`, …).
  val introducers: Set[Text] =
    Set(t"val", t"var", t"def", t"type", t"class", t"object", t"trait", t"enum", t"given",
        t"package", t"import", t"export", t"case", t"extension", t"new")

  private val valueAccents: Set[Accent] =
    Set(Accent.Term, Accent.Number, Accent.String, Accent.Typal)

  // The lexer tags a symbolic operator (`+`, `::`, `<=`, …) as an identifier, just as Scala
  // treats it, and a closing bracket as a symbol — so accent alone cannot tell an operator
  // (after which an expression is expected) from a value; text distinguishes them.
  private def symbolic(text: Text): Boolean =
    text.s.length > 0 && text.s.forall { char => !identifierChar(char) && !char.isWhitespace }

  // The standalone lexer's view of the fragment, flattened and stripped of noise.
  private def tokens(text: Text): List[Token] =
    val source = Scala.highlight(text)(using highlighting.tokenizedScala)

    def significant(token: Token): Boolean =
      token.accent != Accent.Unparsed && token.text.s.trim.nn != ""

    val lines: List[List[Token]] = source.lines.to[List]
    val all:   List[Token]       = lines.flat

    all.filter(significant(_))

  // The infix-completion receiver: when the cursor sits at `<value-expr> <space> <partial>` — a
  // value followed by whitespace, not a member selection — the value expression with a
  // synthetic trailing `.` (so the member-completion path serves it) and the partial method
  // name. `Unset` when there is no value receiver: the token before the space is a keyword
  // (hard or soft: the standalone lexer tags `inline`, `using`, … as identifiers, as Scala
  // does), an operator, comma or open bracket, or a name in a definition or import position.
  def infixBase(code: Text, offset: Int): (Optional[Text], Text) =
    val s = code.s
    val start: Int = identifierStart(code, offset)
    val prefix: Text = code.keep(offset).skip(start)

    if start == 0 || !s.charAt(start - 1).isWhitespace then (Unset, prefix) else
      val before: Text = code.keep(start)
      val sig = tokens(before)

      sig.last.lay((Unset, prefix)): last =>
        val text = last.text
        val closeBracket = text == t")" || text == t"]" || text == t"}"

        val valueEnding =
          !ScalaKeywords.all.has(text)
          && (closeBracket || text == t"_" || (valueAccents.has(last.accent) && !symbolic(text)))

        if !valueEnding then (Unset, prefix) else
          var end = start
          while end > 0 && s.charAt(end - 1).isWhitespace do end -= 1
          val baseStart = expressionStart(code.keep(end))
          val base: Text = code.keep(end).skip(baseStart)

          val preceding: Text =
            tokens(code.keep(baseStart)).last.let(_.text).or(t"")

          if introducers.has(preceding) then (Unset, prefix) else (t"$base.", prefix)

  // The character index where the value expression ending at the last character of `text`
  // begins: scans back over identifiers, `.` and balanced bracket groups, stopping at an
  // operator, a space, or a boundary at depth zero.
  def expressionStart(text: Text): Int =
    val str = text.s
    var i = str.length - 1
    var depth = 0
    var scanning = true

    while i >= 0 && scanning do
      val c = str.charAt(i)
      if c == ')' || c == ']' || c == '}' then { depth += 1; i -= 1 }
      else if c == '(' || c == '[' || c == '{' then
        if depth == 0 then { i += 1; scanning = false } else { depth -= 1; i -= 1 }
      else if depth > 0 then i -= 1
      else if identifierChar(c) || c == '.' then i -= 1
      else { i += 1; scanning = false }

    if i < 0 then 0 else i

  // The closing bracket that matches an opening one.
  private def closer(bracket: Char): Char = bracket match
    case '(' => ')'
    case '[' => ']'
    case _   => '}'

  // Walks the lexical skeleton of `code`: every character outside a string or character
  // literal and outside a comment, in order, with the bracket nesting at which it sits — an
  // opening bracket sits outside the group it opens, as does its closer, so `(` and `)` are
  // both reported at the depth of the surrounding code. The walk is an approximation good
  // enough for an unfinished line: it knows `"…"` with escapes, `"""…"""`, `'x'` and `'\x'`,
  // `//` to the end of the line, and `/* … */` with nesting, but not the code inside an
  // interpolation's `${…}`, which passes as string. A closing bracket with nothing open is
  // ignored, so a fragment cut from inside a group still scans. `step` folds the state over
  // each reported character, given its offset, the character and its depth; `settled` ends
  // the walk early once the state holds what the caller needs.
  private def walk[state](code: Text)(initial: state)(settled: state => Boolean)
    ( step: (state, Int, Char, Int) => state )
  :   state =

    val length: Int = code.length

    // NUL for a read past the end, which no clause below matches, so every lookahead is total.
    def at(offset: Int): Char = code(Ordinal.zerary(offset)).or('\u0000')

    // The offset just past the `"` closing a string literal opened before `offset`, or past
    // the end for an unterminated one.
    def string(offset: Int): Int =
      if offset >= length then length
      else at(offset) match
        case '"'  => offset + 1
        case '\\' => string(offset + 2)
        case _    => string(offset + 1)

    // The offset just past the `"""` (plus any further `"`s, which belong to the content)
    // closing a triple-quoted string opened before `offset`.
    def multiline(offset: Int): Int =
      if offset >= length then length
      else if at(offset) == '"' && at(offset + 1) == '"' && at(offset + 2) == '"' then
        quotes(offset + 3)
      else multiline(offset + 1)

    // The offset past a run of `"`s.
    def quotes(offset: Int): Int =
      if offset < length && at(offset) == '"' then quotes(offset + 1) else offset

    // The offset of the newline ending a line comment (which is then reported as a character
    // in its own right), or the end.
    def line(offset: Int): Int =
      if offset >= length || at(offset) == '\n' then offset else line(offset + 1)

    // The offset just past the `*/` closing a block comment, honouring Scala's nesting.
    def block(offset: Int, nesting: Int): Int =
      if offset >= length then length
      else if at(offset) == '*' && at(offset + 1) == '/' then
        if nesting == 1 then offset + 2 else block(offset + 2, nesting - 1)
      else if at(offset) == '/' && at(offset + 1) == '*' then block(offset + 2, nesting + 1)
      else block(offset + 1, nesting)

    def recur(offset: Int, depth: Int, state: state): state =
      if settled(state) || offset >= length then state
      else at(offset) match
        case '"' if at(offset + 1) == '"' && at(offset + 2) == '"' =>
          recur(multiline(offset + 3), depth, state)

        case '"' =>
          recur(string(offset + 1), depth, state)

        // A character literal, plain or escaped; any other `'` (a quote, `'{…}`) is reported
        // as itself, so the bracket following it counts.
        case '\'' if at(offset + 2) == '\'' =>
          recur(offset + 3, depth, state)

        case '\'' if at(offset + 1) == '\\' && at(offset + 3) == '\'' =>
          recur(offset + 4, depth, state)

        case '/' if at(offset + 1) == '/' =>
          recur(line(offset + 2), depth, state)

        case '/' if at(offset + 1) == '*' =>
          recur(block(offset + 2, 1), depth, state)

        case char @ ('(' | '[' | '{') =>
          recur(offset + 1, depth + 1, step(state, offset, char, depth))

        case char @ (')' | ']' | '}') =>
          val outer: Int = if depth > 0 then depth - 1 else 0
          recur(offset + 1, outer, step(state, offset, char, outer))

        case char =>
          recur(offset + 1, depth, step(state, offset, char, depth))

    recur(0, 0, initial)

  // The closing delimiters for the brackets left open in `code`, innermost first, so that
  // appending them in order completes the fragment: `foo(bar, List[Int` yields `]` then `)`.
  // String literals and comments are skipped (see `walk`), so a bracket inside either does
  // not count. A closer with nothing open is ignored; a closer of the wrong kind closes the
  // innermost open bracket regardless, as the depth-zero scanners assume.
  def unclosed(code: Text): List[Char] =
    walk(code)(Nil: List[Char])(_ => false): (stack, _, char, _) =>
      char match
        case '(' | '[' | '{'       => closer(char) :: stack
        case ')' | ']' | '}'       => if stack.nil then stack else stack.tail
        case _                     => stack

  // The offset of the first character of `code` at bracket depth zero — outside every
  // `(…)`, `[…]` and `{…}`, and outside any string literal or comment — which satisfies
  // `predicate`, given its offset; `Unset` if there is none. The scanner behind "the `:` of a
  // return type, not of a parameter" and "the `=` opening a body, not of a default argument".
  def outermost(code: Text)(predicate: Int => Boolean): Optional[Int] =
    walk(code)(Unset: Optional[Int])(_.present): (found, offset, _, depth) =>
      if depth == 0 && predicate(offset) then offset else found

  // The offset of the first `char` at bracket depth zero in `code`, or `Unset`.
  def outermost(code: Text, char: Char): Optional[Int] =
    walk(code)(Unset: Optional[Int])(_.present): (found, offset, current, depth) =>
      if depth == 0 && current == char then offset else found

  // The parts of `code` between its occurrences of `separator` at bracket depth zero, in
  // order: `a.*, b.{c, d}, e.given` split on `,` yields `a.*`, ` b.{c, d}` and ` e.given`.
  // Parts are neither trimmed nor dropped when empty, so the separators' positions can be
  // recovered, and `code` without a top-level separator is returned as its single part.
  def split(code: Text, separator: Char): List[Text] =
    val cuts: List[Int] =
      walk(code)(Nil: List[Int])(_ => false): (cuts, offset, char, depth) =>
        if depth == 0 && char == separator then offset :: cuts else cuts

    val (parts, start) =
      cuts.reverse.fuse((Nil: List[Text], 0)):
        val (parts, start) = state
        (code.keep(next).skip(start) :: parts, next + 1)

    (code.skip(start) :: parts).reverse

  // The contents of each bracket group opened at depth zero by `bracket`, in order, each
  // without its brackets: the parameter clauses of `def f(a: Int)(using b: B)` are `a: Int`
  // and `using b: B`, and its type parameters are its `[` groups. A group opened at depth
  // zero and never closed is omitted, being unfinished.
  def groups(code: Text, bracket: Char = '('): List[Text] =
    val (found, _) =
      walk(code)((Nil: List[Text], Unset: Optional[Int]))(_ => false):
        case ((found, open), offset, char, depth) =>
          if depth == 0 && char == bracket then (found, offset + 1)
          else if depth == 0 && open.present && (char == ')' || char == ']' || char == '}') then
            (open.let { start => code.keep(offset).skip(start) :: found }.or(found), Unset)
          else (found, open)

    found.reverse
