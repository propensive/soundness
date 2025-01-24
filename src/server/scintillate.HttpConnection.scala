/*
    Scintillate, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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

import anticipation.*
import contingency.*
import gossamer.*
import nettlesome.*
import rudiments.*
import spectacular.*
import telekinesis.*
import vacuous.*

import com.sun.net.httpserver as csnh

object HttpConnection:
  def apply(exchange: csnh.HttpExchange): HttpConnection logs HttpServerEvent =
    val uri = exchange.getRequestURI.nn
    val query = Optional(uri.getQuery)
    val target = uri.getPath.nn.tt+query.let(t"?"+_.tt).or(t"")
    val method = exchange.getRequestMethod.nn.show.decode[HttpMethod]

    val headers: List[RequestHeader.Value] =
      exchange.getRequestHeaders.nn.asScala.view.mapValues(_.nn.asScala.to(List)).flatMap: pair =>
        pair.absolve match
          case (RequestHeader(header), values) => values.map: value =>
            header(value.tt)

      . to(List)

    val version: HttpVersion = HttpVersion.parse(exchange.getProtocol.nn.tt)

    val host = unsafely:
       Hostname.parse:
         Optional(uri.getHost).let(_.tt).or:
           exchange.getLocalAddress.nn.getAddress.nn.getCanonicalHostName.nn.tt

    val in = exchange.getRequestBody.nn
    val buffer = new Array[Byte](65536)

    def stream(): Stream[Bytes] =
      val len = in.read(buffer)
      if len > 0 then buffer.slice(0, len).snapshot #:: stream() else Stream.empty

    val request =
      HttpRequest
       (method  = method,
        version = version,
        host    = host,
        target  = target,
        body    = stream(),
        headers = headers)

    Log.fine(HttpServerEvent.Received(request))

    val port = Option(exchange.getRequestURI.nn.getPort).filter(_ > 0).getOrElse:
      exchange.getLocalAddress.nn.getPort

    def respond(response: HttpResponse): Unit =
      response.headers.each: (key, value) =>
        exchange.getResponseHeaders.nn.add(key.s, value.s)

      val length = response.body match
        case Stream()     => -1
        case Stream(data) => data.length
        case _              => 0

      exchange.sendResponseHeaders(response.status.code, length)

      val responseBody = exchange.getResponseBody.nn

      response.body.map(_.mutable(using Unsafe)).each(responseBody.write(_))
      responseBody.flush()
      exchange.close()

    HttpConnection(false, port, request, respond)

case class HttpConnection
   (secure: Boolean, port: Int, request: HttpRequest, respond: HttpResponse => Unit)
