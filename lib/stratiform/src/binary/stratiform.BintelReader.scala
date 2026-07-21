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

import anticipation.*
import contingency.*
import vacuous.*

object BintelReader:
  // Only stratiform's read path (`Bintel.parse`) constructs readers, so the
  // exclusivity of the wrapped parser and the resolution scope of the
  // carried tactic are preserved by construction. The wrapped tactic
  // travels as a neutral carrier (jacinta's `JsonReader` pattern): the
  // field stays pure, and each accessor reasserts the type at the rim —
  // the audited point.
  private[stratiform] def apply(parser: BintelParser, tactic: Tactic[BintelError])
  :   BintelReader^ =

    new BintelReader(parser, tactic.asInstanceOf[AnyRef])

// The public, restricted rim of the BinTEL body parser, handed to
// `Bintel.Parsable` instances so they can consume elements straight off the
// input without the intermediate `Tel.Element` tree, its `Tel`
// presentation, or the text-format decode that follows. The structure is
// count-driven: a `parse` call is invoked positioned at a struct body (a
// child count, then that many `keyword-index`-prefixed elements) and must
// consume it in full. The reader carries its own `Tactic[BintelError]`, so
// malformed input aborts through the read call's ambient tactic; *decode*
// errors (mistyped or absent values) accrue through the read site's
// `Tactic[TelError]` and `Foci`, exactly as the AST path's text-format
// decode.
//
// An exclusive, stateful capability, like the parser it wraps: it is owned
// by one `Bintel.Parsable.parse` call at a time, for the duration of that
// call, and nothing of it may be retained afterwards.
final class BintelReader private (parser0: AnyRef, tactic0: AnyRef)
extends caps.ExclusiveCapability, caps.Stateful:
  private inline def parser: BintelParser = parser0.asInstanceOf[BintelParser]

  // The sealed conduit for generated parsers: package-private, so the only
  // path to the wrapped capabilities from outside stratiform is through the
  // accessor the compiler synthesizes for stratiform's own macro-generated
  // splices — hand-written code cannot name it. Generated code binds the
  // parser once per read and steps through `BintelParser`'s direct rim
  // without this class's per-step forwarders.
  private[stratiform] def rawParser: AnyRef = parser0
  private[stratiform] def rawTactic: AnyRef = tactic0
  private inline def tactic: Tactic[BintelError] = tactic0.asInstanceOf[Tactic[BintelError]]

  // ── The struct-body steps: the child count, then per child a keyword
  // index — the flat position of the member it fills, matching the
  // members' declaration order — and the member-typed payload. ──
  update def count(): Int = parser.directCount()(using tactic)
  update def index(): Int = parser.directCount()(using tactic)

  // ── One scalar payload, as text: BinTEL scalars are the TEL atom's
  // UTF-8 bytes, so a leaf's value semantics are the text format's. ──
  update def scalar(): Text = Text(parser.directScalar()(using tactic))
  update def skipScalar(): Unit = parser.directSkipScalar()(using tactic)
