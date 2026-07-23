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
package breviloquence

import scala.caps

import anticipation.*
import contingency.*
import vacuous.*

object CborReader:
  // Sentinel of `keyWord()`; impossible as a packed key, whose bytes are all
  // 7-bit ASCII.
  inline final val KeyOpaque = -2L

  // Only breviloquence's read path (`Cbor.parseDirect`) constructs readers,
  // so the exclusivity of the wrapped parser and the resolution scope of the
  // carried tactic are preserved by construction. The wrapped tactic travels
  // as a neutral carrier (jacinta's `JsonReader` pattern): the field stays
  // pure, and each accessor reasserts the type at the rim — the audited
  // point.
  private[breviloquence] def apply(parser: Cbor.Parser, tactic: Tactic[CborError])
  :   CborReader^ =

    new CborReader(parser, tactic.asInstanceOf[AnyRef])

// The public, restricted rim of the CBOR parser, handed to `Cbor.Parsable`
// instances so they can consume data items straight off the input without an
// intermediate `Cbor.Ast`. Each method consumes exactly one item (or one
// structural step). The reader carries its own `Tactic[CborError]` — CBOR's
// single error type covers both malformed input and mistyped items — so
// instance `parse` bodies need no error vocabulary: failures abort through
// the read call's ambient tactic.
//
// An exclusive, stateful capability, like the parser it wraps: it is owned
// by one `Cbor.Parsable.parse` call at a time, for the duration of that
// call, and nothing of it may be retained afterwards.
final class CborReader private (parser0: AnyRef, tactic0: AnyRef)
extends caps.ExclusiveCapability, caps.Stateful:
  private inline def parser: Cbor.Parser = parser0.asInstanceOf[Cbor.Parser]

  // The sealed conduit for generated parsers: package-private, so the only
  // path to the wrapped capabilities from outside breviloquence is through
  // the accessor the compiler synthesizes for breviloquence's own
  // macro-generated splices — hand-written code cannot name it. Generated
  // code binds the parser once per record and reads through `Cbor.Parser`'s
  // direct rim without this class's per-item forwarders.
  private[breviloquence] def rawParser: AnyRef = parser0
  private[breviloquence] def rawTactic: AnyRef = tactic0
  private inline def tactic: Tactic[CborError] = tactic0.asInstanceOf[Tactic[CborError]]

  // ── Scalars: one data item each. Values and failures agree with the
  // `Cbor.Ast` accessors exactly, so direct and AST reads yield equal
  // values — integers coerce to floats and vice versa, as `.long` and
  // `.double` do. ──
  inline update def long(): Long = parser.directLong()(using tactic)
  inline update def int(): Int = parser.directLong()(using tactic).toInt
  inline update def double(): Double = parser.directDouble()(using tactic)
  inline update def boolean(): Boolean = parser.directBoolean()(using tactic)
  update def text(): Text = parser.directString()(using tactic).tt
  update def string(): String = parser.directString()(using tactic)
  update def byteString(): IArray[Byte] = parser.directBytes()(using tactic)

  // ── Undefined handling: `hasUndefined` peeks without consuming, for
  // optional wrappers that map a wire `undefined` (0xF7) to an absent
  // value, exactly as the AST path's `optional`. ──
  update def hasUndefined: Boolean = parser.directIsUndefined
  update def undefined(): Unit = parser.directUndefined()

  // ── Structure. `openMap()` and `openArray()` yield the entry or element
  // count, or -1 for an indefinite-length item, whose end is a Break stop
  // code consumed by `breakEnd()`. A non-map item under `openMap()` reads
  // as an empty map (the AST record decoder's semantics); a non-array item
  // under `openArray()` fails as the AST `.array` accessor. ──
  inline update def openMap(): Int = parser.directOpenMap()(using tactic)
  inline update def openArray(): Int = parser.directOpenArray()(using tactic)
  inline update def breakEnd(): Boolean = parser.directBreak()(using tactic)

  // The next map key in packed form, for parsers that compare keys against
  // literal constants (generated parsers compile field names to
  // immediates): the packed low word of the key (its high word from
  // `keyHigh`), or `KeyOpaque` when the key cannot be packed — the caller
  // then takes the `keyName` step instead, which consumes it generally.
  update def keyWord(): Long = parser.directKeyWord()

  update def keyHigh: Long = parser.directKeyHigh

  // The general key step: a text key's content, or `null` for a non-text
  // key, whose entry is ignored — the caller skips its value.
  update def keyName(): String | Null = parser.directKeyName()(using tactic)

  // ── The fallback seam: parse one whole item into an AST (for field types
  // that only have a `Decodable in Cbor`), or skip one whole item (for
  // unknown keys). ──
  update def value(): Cbor = Cbor.ast(parser.value()(using tactic))
  inline update def skipValue(): Unit = parser.directSkipValue()(using tactic)

  // Scans the upcoming map for the given key and returns its text value,
  // leaving the reader where it started — the dispatch primitive for a
  // sum's discriminant entry, which may appear anywhere in the map. `Unset`
  // when the item has no such key or its value is not text.
  update def discriminant(key: Text): Optional[Text] =
    parser.directDiscriminant(key.s)(using tactic) match
      case null        => Unset
      case tag: String => tag.tt
