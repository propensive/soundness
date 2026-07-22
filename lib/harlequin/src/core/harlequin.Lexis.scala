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
import denominative.*
import gossamer.*
import prophesy.*
import rudiments.*
import vacuous.*

// Maps Harlequin's token stream onto Prophesy's `Lexeme` alphabet, and extracts the reversed
// lexeme context at a caret — the input to `KeywordPattern` lookup. The same mapping serves
// completion at runtime and the corpus-analysis tool that derives the pattern tree, so the
// two always agree on what a context looks like.
object Lexis:
  // Soft modifiers read as plain identifiers immediately before a caret (`soften`'s lookahead
  // requires a *following* hard keyword), so lexeme mapping identifies them by text
  // everywhere, keeping corpus statistics and completion-time behaviour consistent.
  private val soft: Set[Text] =
    Set(t"inline", t"opaque", t"open", t"transparent", t"infix", t"update", t"erased",
        t"tracked", t"using")

  private[harlequin] def identifierChar(char: Char): Boolean =
    char.isLetterOrDigit || char == '_'

  // True of a token whose text is pure punctuation: no identifier characters and no quotes.
  // Such a token is classified by its text alone, never by its accent: the parse-tree overlay
  // that refines accents works from error-recovery trees on incomplete input — the norm at a
  // completion caret — and a recovery tree's span can land exactly on a punctuation token,
  // re-accenting a `:` as a type. A glyph's identity must not depend on that.
  private def glyphic(text: Text): Boolean =
    text.length > 0 && text.s.forall: char =>
      !char.isLetterOrDigit && char != '_' && char != '`' && char != '"' && char != '\''

  // The lexeme of a single token, or `Unset` for tokens (whitespace) that contribute nothing.
  def lexeme(token: Token): Optional[Lexeme] = token.accent match
    case Accent.Unparsed => Unset

    case _ if glyphic(token.text) && token.accent != Accent.String &&
      token.accent != Accent.Error =>
      token.text match
        case t"(" => Lexeme.Open(Lexeme.Bracket.Round)
        case t")" => Lexeme.Close(Lexeme.Bracket.Round)
        case t"[" => Lexeme.Open(Lexeme.Bracket.Square)
        case t"]" => Lexeme.Close(Lexeme.Bracket.Square)
        case t"{" => Lexeme.Open(Lexeme.Bracket.Brace)
        case t"}" => Lexeme.Close(Lexeme.Bracket.Brace)
        case _    => Lexeme.Symbol(token.text)

    case Accent.Keyword | Accent.Modifier =>
      Lexeme.Keyword(token.text)

    case Accent.Term =>
      if soft.has(token.text) then Lexeme.Keyword(token.text) else Lexeme.Term

    case Accent.Typal  => Lexeme.Typal
    case Accent.Number => Lexeme.Literal
    case Accent.String => Lexeme.Literal
    case Accent.Error  => Lexeme.Error

    // Non-glyphic symbol-accented tokens (e.g. a backquoted identifier the scanner reports
    // in the symbol range) keep their text as an uninterpreted symbol.
    case Accent.Symbol | Accent.Parens => Lexeme.Symbol(token.text)

  // The whole-stream lexeme sequence of a `SourceCode`, `Start` first, with `Break` inserted
  // at each line boundary that begins a new statement: a non-blank line indented at-or-below
  // the previous non-blank line. A more deeply indented line continues its enclosing
  // expression, so it contributes no boundary.
  def lexemes(code: SourceCode): List[Lexeme] =
    val builder = List.newBuilder[Lexeme]
    builder += Lexeme.Start

    var previous: Optional[Int] = Unset

    code.lines.foreach: line =>
      if line.exists(_.accent != Accent.Unparsed) then
        val indent = line.head.pipe: token =>
          if token.accent == Accent.Unparsed then token.length else 0

        if previous.lay(false)(indent <= _) then builder += Lexeme.Break
        previous = indent

        line.foreach: token =>
          lexeme(token).let(builder += _)

    builder.result()

  // The completion prefix at `caret` — the identifier fragment touching it, `Unset` when the
  // caret does not follow an identifier character — and the reversed lexeme context that
  // precedes it, nearest-first, limited to `limit` lexemes. The text before the prefix is
  // tokenized afresh (always at tokenized depth: no compiler run), so the fragment itself
  // never contributes a `Term` lexeme and the scanner's error recovery handles the truncated
  // input. A caret on a blank line emits the `Break` that `lexemes` would emit once the
  // statement is typed.
  def context(text: Text, caret: Ordinal, limit: Int = 8): (Optional[Text], List[Lexeme]) =
    val point = caret.n0.min(text.length)
    var start = point

    while start > 0 && identifierChar(text.s.charAt(start - 1)) do start -= 1

    val prefix: Optional[Text] =
      if start == point then Unset else text.s.substring(start, point).nn.tt

    val truncated: Text = text.keep(start)

    val code = SourceCode(Scala, truncated, Unset)(using highlighting.tokenizedScala)
    val reversed = lexemes(code).reverse

    // If the caret's line holds nothing but whitespace before the prefix, `lexemes` saw no
    // significant line to attach a boundary to; emit the `Break` here when that line's indent
    // is at-or-below the last significant line's.
    val lastNewline = truncated.s.lastIndexOf('\n')

    val breakAtCaret =
      lastNewline >= 0 && {
        val caretLine = truncated.s.substring(lastNewline + 1).nn

        caretLine.forall(_.isWhitespace) && {
          val before = truncated.s.substring(0, lastNewline).nn.split('\n').nn

          before.reverse.find(!_.nn.forall(_.isWhitespace)) match
            case Some(line) => caretLine.length <= line.nn.takeWhile(_.isWhitespace).nn.length
            case None       => false
        }
      }

    val context = if breakAtCaret then Lexeme.Break :: reversed else reversed
    (prefix, context.take(limit))
