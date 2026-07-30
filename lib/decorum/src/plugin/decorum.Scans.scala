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

// Shared linear-scan helpers for checks that track, line by line, whether
// the previous code line left a declaration signature "in progress" — a
// line that started with a declaration keyword or modifier (or continued
// such a signature with a `(`/`[` clause) without reaching its top-level
// `=`. Four consumers share this state machine: `AnchorRules
// .SignatureEqLast`, `AnchorRules.GivenArrowAlign`, `AnchorRules
// .HeavyBracketAnchor` and the bracket-pair extraction in `Brackets`.
object Scans:

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
        Checker.DeclKeywords.contains(t.text) || Checker.ModifierWords.contains(t.text)

    val continuesDecl =
      prevStartedDecl && sem.headOption.exists: t => t.text == "(" || t.text == "["

    DeclStep
      ( hasTopLevelEq, startsWithDecl, continuesDecl,
        !hasTopLevelEq && (startsWithDecl || continuesDecl) )
