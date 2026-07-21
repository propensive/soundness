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
package locomotion

import anticipation.*
import contingency.*

object ProtobufReader:
  // Only locomotion's read path (`Protobuf.parseDirect`) constructs readers,
  // so the exclusivity of the wrapped parser and the resolution scope of the
  // carried tactic are preserved by construction. The wrapped tactic travels
  // as a neutral carrier (jacinta's `JsonReader` pattern): the field stays
  // pure, and each accessor reasserts the type at the rim — the audited
  // point.
  private[locomotion] def apply(parser: ProtobufParser, tactic: Tactic[ProtobufError])
  :   ProtobufReader^ =

    new ProtobufReader(parser, tactic.asInstanceOf[AnyRef])

// The public, restricted rim of the Protobuf wire parser, handed to
// `Protobuf.Parsable` instances so they can consume fields straight off the
// input without the intermediate number-keyed `Protobuf` map. A `parse` call
// receives the reader with its *window* set to the value's payload (the
// whole input at the top level; one field's wire value in a field position)
// and must consume to the window's end. The reader carries its own
// `Tactic[ProtobufError]`, so instance `parse` bodies need no error
// vocabulary: malformed input aborts through the read call's ambient tactic.
//
// An exclusive, stateful capability, like the parser it wraps: it is owned
// by one `Protobuf.Parsable.parse` call at a time, for the duration of that
// call, and nothing of it may be retained afterwards.
final class ProtobufReader private (parser0: AnyRef, tactic0: AnyRef)
extends caps.ExclusiveCapability, caps.Stateful:
  private inline def parser: ProtobufParser = parser0.asInstanceOf[ProtobufParser]

  // The sealed conduit for generated parsers: package-private, so the only
  // path to the wrapped capabilities from outside locomotion is through the
  // accessor the compiler synthesizes for locomotion's own macro-generated
  // splices — hand-written code cannot name it. Generated code binds the
  // parser once per record and reads through `ProtobufParser`'s direct rim
  // without this class's per-field forwarders.
  private[locomotion] def rawParser: AnyRef = parser0
  private[locomotion] def rawTactic: AnyRef = tactic0
  private inline def tactic: Tactic[ProtobufError] = tactic0.asInstanceOf[Tactic[ProtobufError]]

  // ── The message steps: the next field's tag while the window has
  // content, then per-field reads. `enterField` narrows the window to one
  // field's wire value (returning the token `leaveField` restores). ──
  update def more: Boolean = !parser.directAtLimit
  update def tag(): Int = parser.directTag()(using tactic)
  update def enterField(code: Int): Int = parser.directEnterField(code)(using tactic)
  update def leaveField(saved: Int): Unit = parser.directLeaveField(saved)
  update def skipField(code: Int): Unit = parser.directSkipField(code)(using tactic)

  // ── Scalars, reading the window's content exactly as the AST accessors
  // interpret a field's recorded payload — for hand-written instances that
  // compose over one wire value. ──
  update def varint(): Long = parser.directVarint()(using tactic)
  update def fixed32(): Int = parser.directFixed32()(using tactic)
  update def fixed64(): Long = parser.directFixed64()(using tactic)
  update def text(): Text = Text(parser.directStringWindow())
  update def data(): Data = parser.directDataWindow()

  // ── The fallback seam: one field's wire value (for gathered occurrences
  // decoded through a `Decodable in Protobuf`), or the remaining window as
  // a message (for `Parsable.fromDecodable`). ──
  update def wire(code: Int): Protobuf = parser.directWire(code)(using tactic)
  update def message(): Protobuf = parser.directMessage()
