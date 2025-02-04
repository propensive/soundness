/*
    Telekinesis, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package telekinesis

import anticipation.*
import contingency.*
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

trait FallbackPostable:
  given [QueryType] => (serializer: QueryEncoder[QueryType]) => QueryType is Postable =
    Postable(media"application/x-www-form-urlencoded", value =>
        Stream(serializer.params(value).queryString.bytes(using charEncoders.utf8)))

object Postable extends FallbackPostable:
  def apply[ResponseType](mediaType0: MediaType, stream0: ResponseType => Stream[Bytes])
  :     ResponseType is Postable =
    new Postable:
      type Self = ResponseType
      def mediaType(response: ResponseType): MediaType = mediaType0
      def stream(response: ResponseType): Stream[Bytes] = stream0(response)

  given text: (encoder: CharEncoder) => Text is Postable =
    Postable(media"text/plain", value => Stream(IArray.from(value.bytes)))

  given textStream: (encoder: CharEncoder) => Stream[Text] is Postable =
    Postable(media"application/octet-stream", _.map(_.bytes))

  given unit: Unit is Postable = Postable(media"text/plain", unit => Stream())
  given bytes: Bytes is Postable = Postable(media"application/octet-stream", Stream(_))

  given byteStream: Stream[Bytes] is Postable =
    Postable(media"application/octet-stream", identity(_))

  given dataStream: [ResponseType: Abstractable across HttpStreams into HttpStreams.Content]
  =>    Tactic[MediaTypeError]
  =>    ResponseType is Postable =

    new Postable:
      type Self = ResponseType

      def mediaType(content: ResponseType): MediaType = content.generic(0).decode[MediaType]
      def stream(content: ResponseType): Stream[Bytes] = content.generic(1)

trait Postable:
  type Self
  def mediaType(content: Self): MediaType
  def stream(content: Self): Stream[Bytes]

  def preview(value: Self): Text = stream(value).prim.lay(t""): bytes =>
    val sample = bytes.take(1024)
    val string: Text = sample.serialize[Base256]
    if bytes.length > 128 then t"$string..." else string
