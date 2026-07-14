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
┃    Soundness, version 0.63.0.                                                                    ┃
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
package stratiform

import anticipation.*
import contingency.*
import vacuous.*

object TelReader:
  // Only stratiform's read path (`Tel.parseDirect`) constructs readers, so
  // the parser-pool invariants (one exclusive `Parser` per thread) and the
  // resolution scope of the carried tactic are preserved by construction.
  // The wrapped capabilities travel as neutral carriers (jacinta's
  // `JsonReader` pattern): the fields stay pure, and each accessor reasserts
  // the type at the rim — the audited point.
  private[stratiform] def apply(parser: Tel.Parser^, tactic: Tactic[TelError]): TelReader^ =
    new TelReader(parser.asInstanceOf[AnyRef], tactic.asInstanceOf[AnyRef])

// The public, restricted rim of the TEL parser, handed to `Tel.Parsable`
// instances so they can consume compound entries straight off the input
// without an intermediate document AST. An entry is a keyword line plus its
// subtree; `keyword(indent)` steps to the next entry at the given indent
// level (transparently consuming blanks, comments and tabulation headers),
// and exactly one of the other methods must then consume that entry in
// full. The reader carries its own `Tactic[TelError]`, so instance `parse`
// bodies need no error vocabulary: malformed input aborts through the read
// call's ambient tactic.
//
// An exclusive, stateful capability, like the parser it wraps: it is owned
// by one `Tel.Parsable.parse` call at a time, for the duration of that
// call, and nothing of it may be retained afterwards.
final class TelReader private (parser0: AnyRef, tactic0: AnyRef)
extends caps.ExclusiveCapability, caps.Stateful:
  private inline def parser: Tel.Parser^ = parser0.asInstanceOf[Tel.Parser^]

  private[stratiform] inline def errorTactic: Tactic[TelError] =
    tactic0.asInstanceOf[Tactic[TelError]]

  // The next entry's keyword at exactly `indent`, with the reader left
  // positioned right after it, or `Unset` once the entry region ends (a
  // shallower line, a document separator, or the end of the input).
  update def keyword(indent: Int): Optional[Text] =
    val next = parser.directKeyword(indent)(using errorTactic)
    if next == null then Unset else next.nn

  // Peeks (without consuming) whether the entry has substance — an inline
  // atom or a child compound — the AST `optionalDecodable`'s emptiness test,
  // for optional wrappers that map a bare keyword to an absent value.
  update def hasSubstance: Boolean = parser.directEntrySubstance()(using errorTactic)

  // ── Entry consumers: each takes the whole entry (line remainder,
  // source/literal continuation and child subtree), so the reader is left
  // at the next sibling entry. ──

  // The entry's primary atom — the first inline atom's text, `Unset` when
  // the line carries none — mirroring `Tel#primaryAtom`. Backs the
  // primitive parsers.
  update def atom(): Optional[Text] = parser.directAtomText()(using errorTactic)

  // Consumes only the entry's own line (and any source/literal
  // continuation), leaving its children for the caller to parse one level
  // deeper — the step before a nested record's field loop.
  update def finishLine(): Unit = parser.directFinishLine()(using errorTactic)

  // Skips the entry entirely: unknown keywords, and duplicate occurrences
  // of a non-repeatable field (the AST's first-match-wins semantics).
  update def skipEntry(indent: Int): Unit = parser.directSkipEntry(indent)(using errorTactic)

  // ── The fallback seam: materialize one entry (or the whole remaining
  // document) as a `Tel`, for field types that only have a
  // `Tel.Decodable`. ──
  update def value(indent: Int): Tel = parser.directValue(indent)(using errorTactic)

  private[stratiform] update def document(): Tel = parser.directDocument()(using errorTactic)

  // Raise through the reader's tactic — for leaf instances that reject a
  // well-formed entry's content, continuing with a sentinel under an
  // accruing boundary exactly like the AST primitives.
  update def fault(reason: TelError.Reason): Unit =
    raise(TelError(reason))(using errorTactic)
