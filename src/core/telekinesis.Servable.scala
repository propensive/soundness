/*
    Telekinesis, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

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
import gesticulate.*
import prepositional.*
import rudiments.*
import spectacular.*
import turbulence.*

object Servable:
  def apply[ResponseType](mediaType: MediaType)(lambda: ResponseType => LazyList[Bytes])
          : ResponseType is Servable = response =>
    val headers = List(ResponseHeader.ContentType.header -> mediaType.show)
    HttpResponse(1.1, HttpStatus.Ok, headers, lambda(response))

  given Content is Servable as content:
    def serve(content: Content): HttpResponse =
      val headers = List(ResponseHeader.ContentType.header -> content.media.show)

      HttpResponse(1.1, HttpStatus.Ok, headers, content.stream)

  given [ResponseType: GenericHttpResponseStream] => ResponseType is Servable as bytes =
    Servable(unsafely(Media.parse(ResponseType.mediaType))): value =>
      ResponseType.content(value)

  given Bytes is Servable as data = Servable(media"application/octet-stream")(LazyList(_))

  inline given [ValueType: Media] => ValueType is Servable as media =
    scala.compiletime.summonFrom:
      case encodable: (ValueType is Encodable in Bytes) => value =>
        val headers = List(ResponseHeader.ContentType.header -> ValueType.mediaType(value).show)
        HttpResponse(1.1, HttpStatus.Ok, headers, LazyList(encodable.encode(value)))
      case given (ValueType is Readable by Bytes)       => value =>
        val headers = List(ResponseHeader.ContentType.header -> ValueType.mediaType(value).show)
        HttpResponse(1.1, HttpStatus.Ok, headers, value.stream[Bytes])

trait Servable:
  type Self
  def serve(content: Self): HttpResponse
