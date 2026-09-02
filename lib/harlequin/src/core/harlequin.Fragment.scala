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
import denominative.nil
import denominative.dysasymptotics.linearSize
import gossamer.*
import proscenium.*
import rudiments.*
import vacuous.*

// Analysis of a code fragment around a cursor: the pure-text and tokenized splitting a
// completion host performs before any compiler runs — where the partial identifier begins,
// whether it selects a member, and whether the token before it is a value receiver awaiting an
// infix method. Lifted from the Flame REPL, so any completion host — a REPL, a debugger
// console — shares one implementation.
object Fragment:
  private def identifierChar(char: Char): Boolean = char.isLetterOrDigit || char == '_'

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
  private val infixExcluded: Set[Text] =
    Set(t"val", t"var", t"def", t"type", t"class", t"object", t"trait", t"enum", t"given",
        t"package", t"import", t"export", t"case", t"extension", t"new")

  private val valueAccents: Set[Accent] =
    Set(Accent.Term, Accent.Number, Accent.String, Accent.Typal)

  // Every Scala 3 keyword, hard and soft. The standalone lexer tags soft keywords (`inline`,
  // `transparent`, `opaque`, `open`, `using`, `extension`, …) as identifiers — as Scala does —
  // so they would otherwise pass as infix receivers; this set excludes them all.
  private val allKeywords: Set[Text] =
    Set(t"abstract", t"case", t"catch", t"class", t"def", t"do", t"else", t"enum", t"export",
        t"extends", t"false", t"final", t"finally", t"for", t"given", t"if", t"implicit",
        t"import", t"lazy", t"match", t"new", t"null", t"object", t"override", t"package",
        t"private", t"protected", t"return", t"sealed", t"super", t"then", t"this", t"throw",
        t"trait", t"true", t"try", t"type", t"val", t"var", t"while", t"with", t"yield",
        t"as", t"derives", t"end", t"extension", t"infix", t"inline", t"opaque", t"open",
        t"transparent", t"using")

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
  // name. `Unset` when there is no value receiver: the token before the space is a keyword,
  // operator, comma or open bracket, or a name in a definition or import position.
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
          !allKeywords.has(text)
          && (closeBracket || text == t"_" || (valueAccents.has(last.accent) && !symbolic(text)))

        if !valueEnding then (Unset, prefix) else
          var end = start
          while end > 0 && s.charAt(end - 1).isWhitespace do end -= 1
          val baseStart = expressionStart(code.keep(end))
          val base: Text = code.keep(end).skip(baseStart)

          val preceding: Text =
            tokens(code.keep(baseStart)).last.let(_.text).or(t"")

          if infixExcluded.has(preceding) then (Unset, prefix) else (t"$base.", prefix)

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
