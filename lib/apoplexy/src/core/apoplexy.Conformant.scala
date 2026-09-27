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
package apoplexy

import anticipation.*
import contingency.*
import distillate.*
import fulminate.*
import hieroglyph.*
import prepositional.*
import telekinesis.*
import turbulence.*

import errorDiagnostics.emptyDiagnostics
import zephyrine.{Parse, memoize}

// Interprets an `Http.Response` as a value of `Self`, reading the body as the carrier type
// `Transport` the spec's media type construes (see `gesticulate.Construable`).
// `Api.Response.call[T]()` checks at compile time that `T` conforms to the response schema,
// then summons `(T is Conformant) over <Transport>` at the concrete call site — so only the
// carrier's own `Aggregable`, and the value's `Decodable` in it, are demanded, and
// `List[T]`-style collection decoders resolve.
//
// The instances are layered by priority: the raw response and `Unit` first; then the carrier
// itself, read from bytes; then the carrier read from text (for carriers such as `Xml` whose
// `Aggregable` is by `Text`); then any value decodable from a carrier, by bytes and by text.
// The layering keeps a carrier which is both `Aggregable by Data` and `by Text` (`Text` is)
// from being ambiguous, and keeps a carrier from decoding through its own identity `Decodable`.
trait Conformant3:
  // A 2xx body decoded to any value decodable from a text-read carrier
  given decodableText: [value, carrier]
  =>  ( aggregable: carrier is Aggregable by Text,
        decodable:  value is Decodable in carrier,
        decoder:    CharDecoder,
        tactic:     Tactic[Api.Error] )
  =>  (value is Conformant) over carrier =
    response =>
      Conformant.successful(response)
      val data: Data = response.body.stream.memoize
      decodable.decoded(aggregable.aggregate(Chain(decoder.decoded(data))))

trait Conformant2 extends Conformant3:
  // A 2xx body decoded to any value decodable from a byte-read carrier
  given decodable: [value, carrier]
  =>  ( aggregable: carrier is Aggregable by Data,
        decodable:  value is Decodable in carrier,
        tactic:     Tactic[Api.Error] )
  =>  (value is Conformant) over carrier =
    response =>
      Conformant.successful(response)
      val data: Data = response.body.stream.memoize
      decodable.decoded(aggregable.aggregate(Chain(data)))

  // The raw 2xx body as a carrier read from text. The bytes are decoded to `Text` (through the
  // `CharDecoder`) before the carrier's parser sees them.
  given carrierText: [carrier]
  =>  ( aggregable: carrier is Aggregable by Text, decoder: CharDecoder, tactic: Tactic[Api.Error] )
  =>  (carrier is Conformant) over carrier =
    response =>
      Conformant.successful(response)
      val data: Data = response.body.stream.memoize
      aggregable.aggregate(Chain(decoder.decoded(data)))

object Conformant extends Conformant2:
  // Raise `Api.Error` unless the response status is in the 2xx range.
  def successful(response: Http.Response)(using Tactic[Api.Error]): Unit =
    if response.status.category != Http.Status.Category.Successful
    then abort(Api.Error(Api.Error.Reason.Status(response.status.code)))

  // The escape hatch: the raw response, with no status check (any transport).
  given response: [transport] => (Http.Response is Conformant) over transport =
    response => response

  // Just check for success and discard the body — for no-content (204) endpoints
  // such as `delete`, and the default target of a bare `.call()` on them. Never reads the
  // body, so an empty one is fine; transport-agnostic.
  given unit: [transport] => Tactic[Api.Error] => (Unit is Conformant) over transport =
    response => successful(response)

  // The raw 2xx body as a carrier read from bytes
  given carrier: [carrier]
  =>  ( aggregable: carrier is Aggregable by Data, tactic: Tactic[Api.Error] )
  =>  (carrier is Conformant) over carrier =
    response =>
      successful(response)
      val data: Data = response.body.stream.memoize
      aggregable.aggregate(Chain(data))

trait Conformant extends Typeclass, Transportive:
  def read(response: Http.Response): Self
