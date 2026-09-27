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
import distillate.*
import hieroglyph.*
import prepositional.*
import telekinesis.*
import turbulence.*

import zephyrine.{Parse, memoize}

// Interprets an `Http.Response` as a value of `Self`, reading the body as the carrier type
// `Transport` the spec's media type construes (see `gesticulate.Construable`). It reads only:
// the status is checked by `Api.Response.call` beforehand, which raises `Api.Error` with the
// failure body construed by the same means.
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
  // A body decoded to any value decodable from a text-read carrier
  given decodableText: [value, carrier]
  =>  ( aggregable: carrier is Aggregable by Text,
        decodable:  value is Decodable in carrier,
        decoder:    Charset )
  =>  (value is Conformant) over carrier =
    response =>
      val data: Data = response.body.stream.memoize
      decodable.decoded(aggregable.aggregate(Chain(decoder.decoded(data))))

trait Conformant2 extends Conformant3:
  // A body decoded to any value decodable from a byte-read carrier
  given decodable: [value, carrier]
  =>  ( aggregable: carrier is Aggregable by Data, decodable: value is Decodable in carrier )
  =>  (value is Conformant) over carrier =
    response =>
      val data: Data = response.body.stream.memoize
      decodable.decoded(aggregable.aggregate(Chain(data)))

  // The body as a carrier read from text. The bytes are decoded to `Text` (through the
  // `Charset`) before the carrier's parser sees them.
  given carrierText: [carrier]
  =>  ( aggregable: carrier is Aggregable by Text, decoder: Charset )
  =>  (carrier is Conformant) over carrier =
    response =>
      val data: Data = response.body.stream.memoize
      aggregable.aggregate(Chain(decoder.decoded(data)))

object Conformant extends Conformant2:
  // The escape hatch: the raw response (any transport)
  given response: [transport] => (Http.Response is Conformant) over transport =
    response => response

  // Discard the body — for no-content (204) endpoints such as `delete`, and the default target
  // of a bare `.call()` on them; transport-agnostic.
  given unit: [transport] => (Unit is Conformant) over transport = response => ()

  // The body as a carrier read from bytes
  given carrier: [carrier] => (aggregable: carrier is Aggregable by Data)
  =>  (carrier is Conformant) over carrier =
    response =>
      val data: Data = response.body.stream.memoize
      aggregable.aggregate(Chain(data))

trait Conformant extends Typeclass, Transportive:
  def read(response: Http.Response): Self
