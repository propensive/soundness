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
┃    Soundness, version 0.27.0.                                                                    ┃
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

import anticipation.*
import contingency.*
import distillate.*
import fulminate.*
import gesticulate.*
import gossamer.*
import hieroglyph.*
import monotonous.*, alphabets.base256.alphanumericOrBraille
import prepositional.*
import proscenium.*
import rudiments.*
import spectacular.*
import vacuous.*

import language.dynamics

object Postable:
  def apply[ResponseType](medium0: Medium, stream0: ResponseType => Stream[Bytes])
  :     ResponseType is Postable =
    new Postable:
      type Self = ResponseType
      def medium(response: ResponseType): Medium = medium0
      def stream(response: ResponseType): Stream[Bytes] = stream0(response)

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

  given dataStream: [ResponseType: Abstractable across HttpStreams into HttpStreams.Content]
  =>    Tactic[MediumError]
  =>    ResponseType is Postable =

    new Postable:
      type Self = ResponseType

      def medium(content: ResponseType): Medium = content.generic(0).decode[Medium]
      def stream(content: ResponseType): Stream[Bytes] = content.generic(1)

trait Postable:
  type Self
  def medium(content: Self): Medium
  def stream(content: Self): Stream[Bytes]

  def preview(value: Self): Text = stream(value).prim.lay(t""): bytes =>
    val sample = bytes.take(1024)
    val string: Text = sample.serialize[Base256]
    if bytes.length > 128 then t"$string..." else string
