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
package telekinesis

import language.dynamics

import anticipation.*
import contingency.*
import distillate.*
import fulminate.*
import gesticulate.*
import gossamer.*
import hieroglyph.*
import legerdemain.*
import monotonous.*
import prepositional.*
import rudiments.*
import spectacular.*
import vacuous.*

import alphabets.hexLowerCase

object Postable:
  def apply[response](mediaType0: MediaType, stream0: response => Stream[Data])
  :   response is Postable =

    // `Typeclass` instances are `Pure` by infrastructure, but the streaming lambda may capture
    // capabilities with the same lifetime as this instance's given resolution; laundered pure —
    // the jacinta codec-thunk seal pattern (see rep/DECISIONS.md).
    val stream1: response -> Stream[Data] = caps.unsafe.unsafeAssumePure(stream0)

    new Postable:
      type Self = response
      def mediaType(response: response): MediaType = mediaType0
      def stream(response: response): Stream[Data] = stream1(response)


  given text: (encoder: CharEncoder) => Text is Postable =
    Postable(media"text/plain", value => Stream(IArray.from(value.data)))

  given textStream: (encoder: CharEncoder) => Stream[Text] is Postable =
    Postable(media"application/octet-stream", _.map(_.data))

  given unit: Unit is Postable = Postable(media"text/plain", unit => Stream())
  given data: Data is Postable = Postable(media"application/octet-stream", Stream(_))
  given byteStream: Stream[Data] is Postable = Postable(media"application/octet-stream", identity)

  given query: Query is Postable =
    import charEncoders.utf8Encoder
    Postable(media"application/x-www-form-urlencoded", query => Stream(query.queryString.data))


  given dataStream: [response: Abstractable across HttpStreams to HttpStreams.Content]
  =>  ( tactic: Tactic[MediaTypeError] )
  =>  response is Postable =

    // See `apply`: the instance's `mediaType` raises through the resolution-scoped tactic,
    // which shares the instance's lifetime; the media-type decoder is resolved once under a
    // laundered tactic so the instance stays pure, per the codec-thunk seal pattern.
    val decoder: MediaType is Decodable in Text =
      given Tactic[MediaTypeError] = caps.unsafe.unsafeAssumePure(tactic)
      caps.unsafe.unsafeAssumePure(summon[(MediaType is Decodable in Text)^])

    new Postable:
      type Self = response

      def mediaType(content: response): MediaType =
        content.generic(0).decode[MediaType](using decoder)

      def stream(content: response): Stream[Data] = content.generic(1)

trait Postable extends Typeclass:
  def mediaType(content: Self): MediaType
  def stream(content: Self): Stream[Data]

  def preview(value: Self): Text = stream(value).prim.lay(t""): data =>
    val sample = data.take(1024)
    val string: Text = sample.serialize[Hex]
    if data.length > 128 then t"$string..." else string
