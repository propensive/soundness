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
import turbulence.*
import vacuous.*
import zephyrine.*

import alphabets.hexLowerCase

object Postable:
  // The streaming payload as a named SAM rather than the function type
  // `response => (Stream[Data] over Credit)^`: a lambda may not mint a fresh
  // scoped capability as its result, but a SAM's method may (see
  // `zephyrine.Spring`). Lambda call sites are unchanged by SAM conversion.
  trait Streamer[content]:
    def stream(content: content): (Stream[Data] over Credit)^

  // `stream0` must construct a fresh pull endpoint on each call: the request
  // body may be materialized more than once — for a redirect that preserves
  // the method, and independently by `preview` for logging — and a `Stream`,
  // being a single-use mutable buffer, cannot be re-pulled.
  def apply[response](mediaType0: MediaType, stream0: Streamer[response]^)
  :   response is Postable =

    // `Typeclass` instances are `Pure` by infrastructure, but the streaming lambda may capture
    // capabilities with the same lifetime as this instance's given resolution; laundered pure —
    // the jacinta codec-thunk seal pattern (see rep/DECISIONS.md).
    val stream1: Streamer[response] = caps.unsafe.unsafeAssumePure(stream0)

    new Postable:
      type Self = response
      def mediaType(response: response): MediaType = mediaType0
      def stream(response: response): (Stream[Data] over Credit)^ = stream1.stream(response)


  given text: (encoder: CharEncoder) => Text is Postable =
    // Sealed: a fresh `IArray` is immutable; fresh-ness is the opaque-Array artifact.
    Postable(media"text/plain", value => Stream(caps.unsafe.unsafeAssumePure(IArray.from(value.data))))

  given textStream: (encoder: CharEncoder) => LazyList[Text] is Postable =
    Postable(media"application/octet-stream", lazyList => Stream(lazyList.map(_.data).iterator))

  given unit: Unit is Postable = Postable(media"text/plain", _ => Stream(Iterator.empty[Data]))
  given data: Data is Postable = Postable(media"application/octet-stream", Stream(_))

  given byteStream: LazyList[Data] is Postable =
    Postable(media"application/octet-stream", lazyList => Stream(lazyList.iterator))

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
      def stream(content: response): (Stream[Data] over Credit)^ = content.generic(1).stream

trait Postable extends Typeclass:
  def mediaType(content: Self): MediaType
  def stream(content: Self): (Stream[Data] over Credit)^

  // A sample of the body for logging: a single bounded refill, not a full drain —
  // the body may be large, and `stream` mints a fresh endpoint anyway.
  def preview(value: Self): Text =
    val endpoint = stream(value)

    try endpoint.refill(Credit(1024)) match
      case count: Int =>
        val sample =
          endpoint.addressable.materialize
            ( endpoint.window(using Unsafe), endpoint.start, count )

        val string: Text = sample.serialize[Hex]
        if count > 128 then t"$string..." else string

      case _ => t""
    finally endpoint.close()
