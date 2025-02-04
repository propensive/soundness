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
import gesticulate.*
import prepositional.*
import proscenium.*
import spectacular.*
import turbulence.*

object Servable:
  def apply[ResponseType](mediaType: ResponseType => MediaType)
     (lambda: ResponseType => Stream[Bytes])
  :     ResponseType is Servable = response =>
    val headers = List(ResponseHeader.ContentType.header -> mediaType(response).show)
    HttpResponse(1.1, HttpStatus.Ok, headers, lambda(response))

  given content: Content is Servable:
    def serve(content: Content): HttpResponse =
      val headers = List(ResponseHeader.ContentType.header -> content.media.show)

      HttpResponse(1.1, HttpStatus.Ok, headers, content.stream)

  given bytes: [ResponseType: Abstractable across HttpStreams into HttpStreams.Content]
  =>    ResponseType is Servable =
    Servable[ResponseType](value => unsafely(Media.parse(ResponseType.generic(value)(0)))): value =>
      ResponseType.generic(value)(1)

  given data: Bytes is Servable = Servable[Bytes](_ => media"application/octet-stream")(Stream(_))

  inline given media: [ValueType: Media] => ValueType is Servable =
    scala.compiletime.summonFrom:
      case encodable: (ValueType is Encodable in Bytes) => value =>
        val headers = List(ResponseHeader.ContentType.header -> ValueType.mediaType(value).show)
        HttpResponse(1.1, HttpStatus.Ok, headers, Stream(encodable.encode(value)))
      case given (ValueType is Readable by Bytes)       => value =>
        val headers = List(ResponseHeader.ContentType.header -> ValueType.mediaType(value).show)
        HttpResponse(1.1, HttpStatus.Ok, headers, value.stream[Bytes])

trait Servable:
  type Self
  def serve(content: Self): HttpResponse
