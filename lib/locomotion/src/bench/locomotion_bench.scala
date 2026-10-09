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
import contingency.*, strategies.throwUnsafely
import distillate.*
import prepositional.*
import proscenium.*

// The benchmark message schema. These types live at the top level of the package
// (rather than nested in the `Benchmarks` object) so Wisteria derivation resolves
// cleanly when the benchmark bodies are staged and recompiled in a separate
// compilation unit, and in their own file so they do not share a synthetic
// top-level `…$package` class with the `Benchmarks` object.

case class Small(@field(1) id: Long, @field(2) name: Text, @field(3) active: Boolean)
derives CanEqual

case class User
   ( @field(1) id:       Long,
     @field(2) username: Text,
     @field(3) email:    Text,
     @field(4) active:   Boolean,
     @field(5) role:     Text )
derives CanEqual

case class Users(@field(1) users: List[User]) derives CanEqual

case class LogEntry
   ( @field(1) timestamp: Long,
     @field(2) level:     Text,
     @field(3) service:   Text,
     @field(4) requestId: Text,
     @field(5) userId:    Long,
     @field(6) message:   Text )
derives CanEqual

case class Logs(@field(1) logs: List[LogEntry]) derives CanEqual

case class Ints(@field(1) values: List[Long]) derives CanEqual

case class Attributes(@field(1) entries: Map[Text, Text]) derives CanEqual

// A chain of distinct wrapper messages giving a fixed five-level nesting depth.
// (Distinct classes rather than a self-referential type so the inline derivation
// has a finite, non-recursive shape to expand.)
case class Deep5(@field(1) label: Text) derives CanEqual
case class Deep4(@field(1) label: Text, @field(2) child: Deep5) derives CanEqual
case class Deep3(@field(1) label: Text, @field(2) child: Deep4) derives CanEqual
case class Deep2(@field(1) label: Text, @field(2) child: Deep3) derives CanEqual
case class Deep1(@field(1) label: Text, @field(2) child: Deep2) derives CanEqual

// Anchor the intermediate nested types with explicit derivations; a deep
// case-class graph otherwise overflows the inline-derivation budget.
given deep5Encodable: Deep5 is Encodable in Protobuf = Protobuf.EncodableDerivation.derived
given deep4Encodable: Deep4 is Encodable in Protobuf = Protobuf.EncodableDerivation.derived
given deep3Encodable: Deep3 is Encodable in Protobuf = Protobuf.EncodableDerivation.derived
given deep2Encodable: Deep2 is Encodable in Protobuf = Protobuf.EncodableDerivation.derived
given deep5Decodable: Deep5 is Decodable in Protobuf = Protobuf.DecodableDerivation.derived
given deep4Decodable: Deep4 is Decodable in Protobuf = Protobuf.DecodableDerivation.derived
given deep3Decodable: Deep3 is Decodable in Protobuf = Protobuf.DecodableDerivation.derived
given deep2Decodable: Deep2 is Decodable in Protobuf = Protobuf.DecodableDerivation.derived

// Each message type's codec, derived once, here, rather than wherever it is used: the staged
// benchmark bodies, the corpora and `TimingMain` share these instances, so an operation measures
// encoding or decoding and never the expansion or allocation of its codec.
given smallEncodable:      Small is Encodable in Protobuf = Protobuf.EncodableDerivation.derived
given usersEncodable:      Users is Encodable in Protobuf = Protobuf.EncodableDerivation.derived
given logsEncodable:       Logs is Encodable in Protobuf = Protobuf.EncodableDerivation.derived
given intsEncodable:       Ints is Encodable in Protobuf = Protobuf.EncodableDerivation.derived
given attributesEncodable: Attributes is Encodable in Protobuf =
  Protobuf.EncodableDerivation.derived
given deep1Encodable:      Deep1 is Encodable in Protobuf = Protobuf.EncodableDerivation.derived
given smallDecodable:      Small is Decodable in Protobuf = Protobuf.DecodableDerivation.derived
given usersDecodable:      Users is Decodable in Protobuf = Protobuf.DecodableDerivation.derived
given logsDecodable:       Logs is Decodable in Protobuf = Protobuf.DecodableDerivation.derived
given intsDecodable:       Ints is Decodable in Protobuf = Protobuf.DecodableDerivation.derived
given attributesDecodable: Attributes is Decodable in Protobuf =
  Protobuf.DecodableDerivation.derived
given deep1Decodable:      Deep1 is Decodable in Protobuf = Protobuf.DecodableDerivation.derived

// Generated direct parsers, as plain `val`s rather than givens: the "typed" rows above resolve
// through the derived `Decodable` (the `Protobuf` ADT path), and the direct rows bring one of
// these into scope *inside* the staged body (`given … = locomotion.logsParsable`), so each row
// measures one path and neither measures the generation of its parser.
val usersParsable: Users is Protobuf.Parsable = Inlinable.parsable[Users]
val logsParsable:  Logs is Protobuf.Parsable  = Inlinable.parsable[Logs]
val deep1Parsable: Deep1 is Protobuf.Parsable = Inlinable.parsable[Deep1]

// A hand-written streaming consumer with a tiny live set: counts the occurrences of field 1
// (the `Logs.logs` entries) without decoding any of them, so a constrained-heap stress run over
// a large message measures the parser's buffering alone, not the decoded value.
val countEntries: Long is Protobuf.Parsable = Protobuf.Parsable: reader =>
  var count = 0L

  while reader.more do
    val tag = reader.tag()
    val saved = reader.enterField(tag & 7)
    if (tag >>> 3) == 1 then count += 1
    reader.leaveField(saved)

  count
