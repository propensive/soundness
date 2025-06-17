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
┃    Soundness, version 0.35.0.                                                                    ┃
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
import proscenium.*
import rudiments.*
import spectacular.*
import vacuous.*

import alphabets.base256.alphanumericOrBraille

object Postable:
  def apply[response](mediaType0: MediaType, stream0: response => Stream[Bytes])
  : response is Postable =

      new Postable:
        type Self = response
        def mediaType(response: response): MediaType = mediaType0
        def stream(response: response): Stream[Bytes] = stream0(response)


  given text: (encoder: CharEncoder) => Text is Postable =
    Postable(media"text/plain", value => Stream(IArray.from(value.bytes)))

  given textStream: (encoder: CharEncoder) => Stream[Text] is Postable =
    Postable(media"application/octet-stream", _.map(_.bytes))

  given unit: Unit is Postable = Postable(media"text/plain", unit => Stream())
  given bytes: Bytes is Postable = Postable(media"application/octet-stream", Stream(_))

  given byteStream: Stream[Bytes] is Postable = Postable(media"application/octet-stream", identity)

  given query: Query is Postable =
    import charEncoders.utf8
    Postable(media"application/x-www-form-urlencoded", query => Stream(query.queryString.bytes))

  given dataStream: [response: Abstractable across HttpStreams into HttpStreams.Content]
        =>  Tactic[MediaTypeError]
        =>  response is Postable =

    new Postable:
      type Self = response

      def mediaType(content: response): MediaType = content.generic(0).decode[MediaType]
      def stream(content: response): Stream[Bytes] = content.generic(1)

trait Postable:
  type Self
  def mediaType(content: Self): MediaType
  def stream(content: Self): Stream[Bytes]

  def preview(value: Self): Text = stream(value).prim.lay(t""): bytes =>
    val sample = bytes.take(1024)
    val string: Text = sample.serialize[Base256]
    if bytes.length > 128 then t"$string..." else string
