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

import dotty.tools.dotc.util.SourceFile

// Shared linear-scan helpers for checks that track, line by line, whether
// the previous code line left a declaration signature "in progress" — a
// line that started with a declaration keyword or modifier (or continued
// such a signature with a `(`/`[` clause) without reaching its top-level
// `=`. Four consumers share this state machine: `AnchorRules
// .SignatureEqLast`, `AnchorRules.GivenArrowAlign`, `AnchorRules
// .HeavyBracketAnchor` and the bracket-pair extraction in `Brackets`.
object Scans:

  // The shared token vocabulary: modifier keywords, declaration keywords
  // and the words that can never be an operand of a binary operator.
  // These sets were originally private to the per-line walk in `Checker`;
  // they now serve the extractors (`Brackets`), the scan helpers below
  // and several registry rules.
  private[decorum] val ModifierWords: Set[String] =
    Set
      ( "private", "protected", "public", "final", "sealed", "abstract",
        "implicit", "lazy", "override", "case", "inline", "transparent",
        "infix", "open", "opaque", "erased", "tracked", "given", "into" )

  private[decorum] val DeclKeywords: Set[String] =
    Set("def", "val", "var", "type", "class", "trait", "object", "enum", "given")

  private[decorum] val NonOperandWords: Set[String] =
    Set
      ( "case", "if", "then", "else", "do", "while", "for", "yield", "return",
        "match", "with", "extends", "derives", "given", "using", "new", "throw",
        "try", "catch", "finally", "import", "package", "def", "val", "var",
        "lazy", "object", "class", "trait", "enum", "type", "private", "protected",
        "public", "final", "sealed", "abstract", "implicit", "override", "inline",
        "transparent", "infix", "open", "opaque", "erased", "tracked",
        "is", "of", "in", "by", "to", "under", "on", "raises", "until" )

  private[decorum] def isSymbolicOperator(text: String): Boolean =
    text.nonEmpty && text.forall: c =>
      c match
        case '+' | '-' | '*' | '/' | '%' | '&' | '|' | '^' | '~' => true
        case '<' | '>' | '=' | '!' | '?' | ':' | '@' | '#'       => true
        case _                                                   => false

  // Walk each line index `l` strictly between `endLine` and `startLine`
  // (1-indexed) — those are the lines that lie *between* the two
  // statements. If any is blank (whitespace only), there's a separator.
  private[decorum] def hasBlankLineBetween
    ( endLine: Int, startLine: Int, content: String, source: SourceFile )
  :   Boolean =

    var l = endLine + 1

    while l < startLine do
      val from = source.lineToOffset(l - 1)

      val to =
        if l < content.split('\n').length + 1
        then source.lineToOffset(l).min(content.length)
        else content.length

      var i     = from
      var blank = true

      while blank && i < to do
        val c = content.charAt(i)
        if c != ' ' && c != '\t' && c != '\n' && c != '\r' then blank = false
        i += 1

      if blank then return true
      l += 1

    false

  // The declaration-signature facts of one line: whether it has a
  // top-level `=` (a completed declaration), whether it starts with a
  // declaration keyword or modifier, whether it continues an in-progress
  // signature with a `(`/`[` clause, and the resulting carried state
  // (`startedDecl`) for the next line.
  case class DeclStep
    ( hasTopLevelEq:  Boolean,
      startsWithDecl: Boolean,
      continuesDecl:  Boolean,
      startedDecl:    Boolean )

  // Advance the declaration-signature state machine by one non-blank
  // line. `sem` is the line's semantic (non-whitespace, non-comment)
  // tokens; `prevStartedDecl` is the state carried from the previous
  // non-blank line.
  def declStep(sem: IndexedSeq[Lexeme], prevStartedDecl: Boolean): DeclStep =
    val hasTopLevelEq =
      var depth = 0
      var found = false

      sem.foreach: t =>
        if t.text == "(" || t.text == "[" || t.text == "{" then depth += 1
        else if t.text == ")" || t.text == "]" || t.text == "}" then depth -= 1
        else if depth == 0 && t.text == "=" then found = true

      found

    val startsWithDecl =
      sem.headOption.exists: t =>
        DeclKeywords.contains(t.text) || ModifierWords.contains(t.text)

    val continuesDecl =
      prevStartedDecl && sem.headOption.exists: t => t.text == "(" || t.text == "["

    DeclStep
      ( hasTopLevelEq, startsWithDecl, continuesDecl,
        !hasTopLevelEq && (startsWithDecl || continuesDecl) )
