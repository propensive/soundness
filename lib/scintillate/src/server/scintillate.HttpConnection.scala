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
┃    Soundness, version 0.43.0.                                                                    ┃
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
package scintillate

import anticipation.*
import contingency.*
import distillate.*
import gossamer.*
import proscenium.*
import rudiments.*
import spectacular.*
import symbolism.*
import telekinesis.*
import turbulence.*
import urticose.*
import vacuous.*

import com.sun.net.httpserver as csnh

object HttpConnection:
  def apply(exchange: csnh.HttpExchange): HttpConnection logs HttpServerEvent =
    val uri = exchange.getRequestURI.nn
    val query = Optional(uri.getQuery)
    val target = uri.getPath.nn.tt+query.let(t"?"+_.tt).or(t"")
    val method = exchange.getRequestMethod.nn.show.decode[Http.Method]

    val headers: List[Http.Header] =
      exchange.getRequestHeaders.nn.asScala.view.mapValues(_.nn.asScala.to(List)).flatMap: pair =>
        pair.absolve match
          case (key, values) => values.map: value =>
            Http.Header(key, value.tt)

      . to(List)

    val version: Http.Version = Http.Version.parse(exchange.getProtocol.nn.tt)

    val host = unsafely:
      Optional(uri.getHost).let(_.tt).or:
        exchange.getLocalAddress.nn.getAddress.nn.getCanonicalHostName.nn.tt
      . decode[Hostname]

    lazy val in = exchange.getRequestBody.nn

    val buffer = new Array[Byte](65536)

    def stream(): Stream[Bytes] =
      val len = in.read(buffer)
      if len > 0 then buffer.slice(0, len).snapshot #:: stream() else Stream.empty

    val request =
      Http.Request
       (method      = method,
        version     = version,
        host        = host,
        target      = target,
        body        = () => stream(),
        textHeaders = headers)

    Log.fine(HttpServerEvent.Received(request))

    val port = Option(exchange.getRequestURI.nn.getPort).filter(_ > 0).getOrElse:
      exchange.getLocalAddress.nn.getPort

    def respond(response: Http.Response): Unit =
      var chunked = false
      response.textHeaders.each:
        case Http.Header(key, value) =>
          if key.lower == t"transfer-encoding" && value.lower == t"chunked" then chunked = true
          exchange.getResponseHeaders.nn.add(key.s, value.s)

      val length = if chunked then 0 else response.body match
        case Stream()     => -1
        case Stream(data) => data.length
        case _            => 0

      exchange.sendResponseHeaders(response.status.code, length)
      val responseBody = exchange.getResponseBody.nn

      response.body.map(_.mutable(using Unsafe)).each(responseBody.write(_))
      responseBody.flush()
      exchange.close()

    new HttpConnection(request, false, port, respond)

class HttpConnection
   (request: Http.Request, val tls: Boolean, val port: Int, val respond: Http.Response => Unit)
extends Http.Request
   (request.method,
    request.version,
    request.host,
    request.target,
    request.textHeaders,
    request.body)
