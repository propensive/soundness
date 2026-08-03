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
