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
package jacinta

import anticipation.*
import contingency.*
import gossamer.*
import vacuous.*
import zephyrine.*

object JsonReader:
  // Only jacinta's read path (`Json.parseDirect`) constructs readers, so the
  // parser-pool invariants (one exclusive `Parser` per thread) and the
  // resolution scope of the carried tactic are preserved by construction. The
  // wrapped capabilities travel as neutral carriers (the `FrameReader`
  // pattern): the fields stay pure, and each accessor reasserts the type at
  // the rim — the audited point.
  private[jacinta] def apply(parser: Parser^, tactic: Tactic[ParseError]): JsonReader^ =
    new JsonReader(parser.asInstanceOf[AnyRef], tactic.asInstanceOf[AnyRef])

// The public, restricted rim of the JSON tokenizer, handed to `Json.Parsable`
// instances so they can consume JSON tokens straight off the input without an
// intermediate `Json.Ast`. Each method consumes exactly one token (or one
// structural step), skipping any leading whitespace. The reader carries its
// own `Tactic[ParseError]`, so instance `parse` bodies need no error
// vocabulary: malformed input and mistyped values abort through the read
// call's ambient tactic.
//
// An exclusive, stateful capability, like the parser it wraps: it is owned by
// one `Json.Parsable.parse` call at a time, for the duration of that call,
// and nothing of it may be retained afterwards.
final class JsonReader private (parser0: AnyRef, tactic0: AnyRef)
extends caps.ExclusiveCapability, caps.Stateful:
  private inline def parser: Parser^ = parser0.asInstanceOf[Parser^]
  private inline def tactic: Tactic[ParseError] = tactic0.asInstanceOf[Tactic[ParseError]]

  // ── Scalars: one JSON value each. Numbers coerce exactly as the
  // `Json.Ast` accessors do, so direct and AST reads yield equal values. ──
  update def string(): Text = parser.directString()(using tactic).tt
  update def boolean(): Boolean = parser.directBoolean()(using tactic)
  update def long(): Long = parser.directLong()(using tactic)
  update def int(): Int = parser.directLong()(using tactic).toInt
  update def double(): Double = parser.directDouble()(using tactic)
  update def bcd(): Bcd = parser.directBcd()(using tactic)

  // ── Null handling: `hasNull` peeks without consuming, for optional
  // wrappers that map a wire `null` to an absent value. ──
  update def hasNull: Boolean = parser.directIsNull()
  update def nullValue(): Unit = parser.directNull()(using tactic)

  // ── Structure. After `openObject()`, `key()` yields each key in turn
  // (consuming the separator and the colon) and `Unset` once the closing
  // brace is consumed. After `openArray()`, `element()` is true while
  // another element follows (positioned at it) and false once the closing
  // bracket is consumed. ──
  update def openObject(): Unit = parser.directOpenObject()(using tactic)

  update def key(): Optional[Text] = parser.directKey()(using tactic) match
    case null        => Unset
    case key: String => key.tt

  // As `key()`, but exposing the tokenizer's interned `String` (repeated keys
  // return the same reference), so the derived product parser's key lookup is
  // a reference-equality scan in the steady state.
  private[jacinta] update def keyName(): String | Null = parser.directKey()(using tactic)

  // As `key()`, but resolving against a precomputed key table without
  // materializing the key at all: the table index, `KeyTable.Unknown`, or
  // `KeyTable.End` once the closing brace is consumed.
  private[jacinta] update def keyIndex(table: Json.KeyTable): Int =
    val fast = parser.directKeyIndexFast(table)
    if fast != Int.MinValue then fast else parser.directKeyIndexGeneral(table)(using tactic)

  update def openArray(): Unit = parser.directOpenArray()(using tactic)
  update def element(): Boolean = parser.directElement()(using tactic)

  // ── The fallback seam: parse one whole value into an AST (for field types
  // that only have a `Decodable in Json`), or skip one whole value (for
  // unknown keys). ──
  update def value(): Json = Json.ast(Json.Ast(parser.directValue()(using tactic)))
  update def skipValue(): Unit = parser.directSkipValue()(using tactic)

  // Scans the upcoming object for the given key and returns its string
  // value, leaving the reader where it started — the dispatch primitive for
  // an internal discriminator field that may appear anywhere in the object.
  // `Unset` when the object has no such key or its value is not a string.
  update def discriminant(key: Text): Optional[Text] =
    parser.directDiscriminant(key.s)(using tactic) match
      case null        => Unset
      case tag: String => tag.tt

  // Abort through the reader's tactic, positioned at the current input
  // offset — for leaf instances that reject a well-formed token's content.
  update def fail(issue: Json.Ast.Issue): Nothing = parser.directFail(issue)(using tactic)
