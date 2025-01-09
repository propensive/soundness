/*
    Telekinesis, version 0.25.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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
import rudiments.*
import spectacular.*
import vacuous.*

import language.dynamics

trait FallbackPostable:
  given [QueryType](using serializer: QueryEncoder[QueryType]): Postable[QueryType] =
    Postable(media"application/x-www-form-urlencoded", value =>
        LazyList(serializer.params(value).queryString.bytes(using charEncoders.utf8)))

object Postable extends FallbackPostable:
  given text(using encoder: CharEncoder): Postable[Text] =
    Postable(media"text/plain", value => LazyList(IArray.from(value.bytes)))

  given textStream(using encoder: CharEncoder): Postable[LazyList[Text]] =
    Postable(media"application/octet-stream", _.map(_.bytes))

  given unit: Postable[Unit] = Postable(media"text/plain", unit => LazyList())
  given bytes: Postable[Bytes] = Postable(media"application/octet-stream", LazyList(_))
  given byteStream: Postable[LazyList[Bytes]] = Postable(media"application/octet-stream", _.map(identity(_)))

  given [ResponseType: GenericHttpResponseStream](using mediaType: Tactic[MediaTypeError])
      => Postable[ResponseType] as dataStream =

    // FIXME: Check if mapping `identity` is necessary
    Postable(Media.parse(ResponseType.mediaType.show), ResponseType.content(_).map(identity))

class Postable[PostType](val contentType: MediaType, val content: PostType => LazyList[Bytes]):
  def preview(value: PostType): Text = content(value).prim.lay(t""): bytes =>
    val sample = bytes.take(1024)
    val string: Text = sample.serialize[Base256]
    if bytes.length > 128 then t"$string..." else string
