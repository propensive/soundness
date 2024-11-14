/*
    Scintillate, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package scintillate

import rudiments.*
import prepositional.*
import turbulence.*
import contingency.*
import gesticulate.*
import telekinesis.*
import anticipation.*
import spectacular.*

object Servable:
  def apply[ResponseType](mediaType: MediaType)(lambda: ResponseType => LazyList[Bytes])
          : ResponseType is Servable =
    new Servable:
      type Self = ResponseType

      def process(content: Self, status: Int, headers: Map[Text, Text], responder: Responder)
              : Unit =
        responder.addHeader(ResponseHeader.ContentType.header, mediaType.show)
        headers.each(responder.addHeader)
        responder.sendBody(status, lambda(content))

  given Content is Servable as content:
    def process(content: Content, status: Int, headers: Map[Text, Text], responder: Responder)
            : Unit =
      responder.addHeader(ResponseHeader.ContentType.header, content.media.show)
      headers.each(responder.addHeader)
      responder.sendBody(200, content.stream)

  given [ResponseType: GenericHttpResponseStream] => ResponseType is Servable as bytes =
    Servable(unsafely(Media.parse(ResponseType.mediaType))): value =>
      ResponseType.content(value)

  given Redirect is Servable as redirect:
    def process(content: Redirect, status: Int, headers: Map[Text, Text], responder: Responder)
            : Unit =
      responder.addHeader(ResponseHeader.Location.header, content.location)
      headers.each(responder.addHeader)
      responder.sendBody(301, LazyList())

  given [ResponseType] => NotFound[ResponseType] is Servable as notFound:
    def process
       (notFound:  NotFound[ResponseType],
        status:    Int,
        headers:   Map[Text, Text],
        responder: Responder)
            : Unit =
      notFound.serve(headers, responder)

  given [ResponseType: Retrievable] => Unfulfilled[ResponseType] is Servable as retrievable:
    def process
       (notFound:  Unfulfilled[ResponseType],
        status:    Int,
        headers:   Map[Text, Text],
        responder: Responder)
            : Unit =
      responder.addHeader(ResponseHeader.ContentType.header, ResponseType.mediaType.show)
      headers.each(responder.addHeader)
      responder.sendBody(500, ResponseType.stream(notFound.content))

  given Bytes is Servable as data = Servable(media"application/octet-stream")(LazyList(_))

  inline given [ValueType: Media] => ValueType is Servable as media =
    scala.compiletime.summonFrom:
      case encodable: (ValueType is Encodable in Bytes) =>
        (value, status, headers, responder) =>
          responder.addHeader(ResponseHeader.ContentType.header, ValueType.mediaType(value).show)
          headers.each(responder.addHeader)
          responder.sendBody(200, LazyList(encodable.encode(value)))
      case readable: (ValueType is Readable by Bytes) =>
        (value, status, headers, responder) =>
          responder.addHeader(ResponseHeader.ContentType.header, ValueType.mediaType(value).show)
          headers.each(responder.addHeader)
          responder.sendBody(200, value.stream[Bytes])

trait Servable:
  type Self
  def process(content: Self, status: Int, headers: Map[Text, Text], responder: Responder): Unit
