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
package stratiform

import scala.caps

import anticipation.*
import contingency.*
import denominative.Span
import vacuous.*

object TelReader:
  // Sentinels of `keywordWord()`; impossible as packed keywords, whose bytes
  // are all printable ASCII.
  inline final val KeywordEnd = -1L
  inline final val KeywordOpaque = -2L

  // Only stratiform's read path (`Tel.parseDirect`) constructs readers, so
  // the parser-pool invariants (one exclusive `Parser` per thread) and the
  // resolution scope of the carried tactic are preserved by construction.
  // The wrapped capabilities travel as neutral carriers (jacinta's
  // `JsonReader` pattern): the fields stay pure, and each accessor reasserts
  // the type at the rim — the audited point.
  private[stratiform] def apply(parser: Tel.Parser^, tactic: Tactic[Tel.Error]): TelReader^ =
    new TelReader(parser.asInstanceOf[AnyRef], tactic.asInstanceOf[AnyRef])

  // The rim cast as a static helper: inlined forwarders on a capture-erased
  // reader (macro splices) would otherwise bind the accessor's fresh `^` to
  // the erased receiver, whose capture set is read-only. Public because the
  // forwarders inline into staged parsers generated in user modules.
  def reveal(parser0: AnyRef): Tel.Parser^ = parser0.asInstanceOf[Tel.Parser^]

// The public, restricted rim of the TEL parser, handed to `Tel.Parsable`
// instances so they can consume compound entries straight off the input
// without an intermediate document AST. An entry is a keyword line plus its
// subtree; `keyword(indent)` steps to the next entry at the given indent
// level (transparently consuming blanks, comments and tabulation headers),
// and exactly one of the other methods must then consume that entry in
// full. The reader carries its own `Tactic[Tel.Error]`, so instance `parse`
// bodies need no error vocabulary: malformed input aborts through the read
// call's ambient tactic.
//
// An exclusive, stateful capability, like the parser it wraps: it is owned
// by one `Tel.Parsable.parse` call at a time, for the duration of that
// call, and nothing of it may be retained afterwards.
final class TelReader private (parser0: AnyRef, tactic0: AnyRef)
extends caps.ExclusiveCapability, caps.Stateful:
  private inline def parser: Tel.Parser^ = TelReader.reveal(parser0)

  private[stratiform] inline def errorTactic: Tactic[Tel.Error] =
    tactic0.asInstanceOf[Tactic[Tel.Error]]

  // The next entry's keyword at exactly `indent`, with the reader left
  // positioned right after it, or `Unset` once the entry region ends (a
  // shallower line, a document separator, or the end of the input).
  // The quote-free forwarders are `inline` (enabled by the parser rim's
  // `(using Tactic)` conventions and the toolchain's inline-update receiver
  // fix); methods that appear inside stratiform's macro quotes stay
  // non-inline — the spliced reader there is capture-erased, and an inline
  // update method requires an exclusive receiver.
  inline update def keyword(indent: Int): Optional[Text] =
    Tel.Parser.erasedDirectKeyword(parser0, indent)(using errorTactic) match
      case null       => Unset
      case next: Text => next

  // The next keyword step in packed form, for parsers that compare keywords
  // against literal constants (staged parsers compile wire keywords to
  // immediates): the keyword's bytes packed LSB-first (at most eight
  // printable-ASCII bytes), `KeywordEnd` once the entry region ends, or
  // `KeywordOpaque` for a keyword that cannot pack — the keyword is still
  // consumed, and `keywordText` identifies it for a general dispatch. Public
  // because staged parsers are generated into user modules.
  update def keywordWord(indent: Int): Long =
    parser.directKeywordWord(indent)(using errorTactic)

  // The keyword most recently stepped to — the `KeywordOpaque` complement of
  // `keywordWord`.
  update def keywordText: Text = parser.directKeywordText

  // Peeks (without consuming) whether the entry has substance — an inline
  // atom or a child compound — the AST `optionalDecodable`'s emptiness test,
  // for optional wrappers that map a bare keyword to an absent value.
  inline update def hasSubstance: Boolean =
    Tel.Parser.erasedDirectEntrySubstance(parser0)(using errorTactic)

  // ── Entry consumers: each takes the whole entry (line remainder,
  // source/literal continuation and child subtree), so the reader is left
  // at the next sibling entry. ──

  // The entry's primary atom — the first inline atom's text, `Unset` when
  // the line carries none — mirroring `Tel#primaryAtom`. Backs the
  // primitive parsers.
  update def atom(): Optional[Text] = parser.directAtomText()(using errorTactic)

  // The entry's primary atom parsed straight from its arena bytes as an
  // integer / boolean, or `Unset` for a missing or wrong-shaped atom — the
  // number/boolean readers, saving the value `String` the `atom()` path would
  // materialize only for the primitive to re-scan. After an `Unset`,
  // `primaryPresent` says whether an atom was there (missing → `Absent`,
  // present-but-unparseable → `NotScalar`) and `primaryText` gives that atom's
  // text for the error, matching the AST path byte-for-byte.
  update def int(): Optional[Int] = parser.directAtomInt()(using errorTactic)
  update def long(): Optional[Long] = parser.directAtomLong()(using errorTactic)
  update def boolean(): Optional[Boolean] = parser.directAtomBoolean()(using errorTactic)

  update def primaryPresent: Boolean = parser.directPrimaryPresent

  update def primaryText: Optional[Text] =
    val text = parser.directPrimaryText
    if text == null then Unset else Optional(Text(text))

  // Consumes only the entry's own line (and any source/literal
  // continuation), leaving its children for the caller to parse one level
  // deeper — the step before a nested record's field loop.
  update def finishLine(): Unit = parser.directFinishLine()(using errorTactic)

  // As `finishLine`, but returning the entry line's atoms (all presentation
  // forms) for the derived record parser's §19.2 positional pre-pass.
  update def lineAtoms(): Array[Tel.Atom]^{} = parser.directFinishLineAtoms()(using errorTactic)

  // Skips the entry entirely: unknown keywords, and duplicate occurrences
  // of a non-repeatable field (the AST's first-match-wins semantics).
  update def skipEntry(indent: Int): Unit = parser.directSkipEntry(indent)(using errorTactic)

  // ── The fallback seam: materialize one entry (or the whole remaining
  // document) as a `Tel`, for field types that only have a
  // `Tel.Decodable`. ──
  inline update def value(indent: Int): Tel =
    Tel.Parser.erasedDirectValue(parser0, indent)(using errorTactic)

  private[stratiform] inline update def document(): Tel =
    Tel.Parser.erasedDirectDocument(parser0)(using errorTactic)

  // ── Spans ──
  //
  // A direct parse builds no AST, so a focus cannot be located after the fact
  // the way `Tel#as` locates its own; it takes its span from here, while the
  // parser is still on the entry. `entryOrdinal` numbers the entries, so a
  // caller can tell whether the value extent currently on record still belongs
  // to the entry it is interested in, or to something read since. All three are
  // pure values — no capability leaves through the rim.

  update def entryOrdinal: Int = parser.directEntrySeq

  // The current entry's keyword.
  update def keywordSpan: Span = parser.directKeywordSpan

  // The inline-atom run of entry `ordinal`, or empty if what is on record
  // describes a different entry, or that entry had no inline atoms.
  update def valueSpan(ordinal: Int): Span = parser.directValueSpan(ordinal)

  // Raise through the reader's tactic — for leaf instances that reject a
  // well-formed entry's content, continuing with a sentinel under an
  // accruing boundary exactly like the AST primitives. The error is located at
  // the entry's value, or at its keyword when it has none, so a direct-path
  // decode error carries a span just as a parse error does.
  update def fault(reason: Tel.Error.Reason): Unit =
    raise(Tel.Error(reason, parser.directEntrySpan))(using errorTactic)

  // As `fault`, but for a field whose keyword never arrived: there is no entry
  // of its own to point at, so it carries no span, exactly as an unresolvable
  // pointer does on the AST path.
  update def absentFault(reason: Tel.Error.Reason): Unit =
    raise(Tel.Error(reason))(using errorTactic)
