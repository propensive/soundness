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
┃    Soundness, version 0.54.0.                                                                    ┃
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
import contextual.*
import prepositional.*
import contingency.*
import vacuous.*
import distillate.*

// Encodes any value with an `Encodable in Tel` instance to its `Tel` form.
// Mirrors jacinta's `.json`, xylophone's `.xml`, ypsiloid's `.yaml`, etc.
extension [entity: Encodable in Tel](value: entity) def tel: Tel = value.encode

// `tel"…"` extension on StringContext routes through contextual's
// interpolation framework; the actual macro lives in `stratiform.internal`.
// Mirrors `extension (inline context: StringContext) def j` from
// `lib/jacinta/src/core/jacinta_core.scala:230`.
extension (inline context: StringContext)
  transparent inline def tel: Interpolation = interpolation[Tel](context)


extension (tel: Tel)
  def edited(edit: Edit): Tel raises MutationError = edit(tel)


extension (tel: Tel)
  // Runtime-checks `tel` against `topic`, then re-types it as a schema-typed
  // `Tel of topic from topic`. The phantom `Topic` records the current position
  // within the schema (initially the root, `topic`) and `Origin` records the
  // root schema, so the `Dynamic` navigation macros can resolve fields and
  // child indices against `topic` at compile time — with no `dynamicTelAccess`
  // import required.
  //
  // The runtime conformance check is a decode-and-discard against `topic`'s
  // `Tel.Decodable` (a nonconformant value raises `TelError`): TEL scalars are
  // untyped text, so a structural walk over the schema cannot catch type mismatches
  // (e.g. a non-numeric `Int` field) that decoding does — hence decoding remains the
  // strict check. The carrying `Tel.Decodable` also supplies the schema for the
  // phantom anchor, so the schema is guaranteed coherent with the decoder used.
  def verify[topic](using topic is Tel.Decodable)
  :   (Tel of topic from topic) raises TelError =
    tel.as[topic]
    tel.asInstanceOf[Tel of topic from topic]
