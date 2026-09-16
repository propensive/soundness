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
┃    Soundness, version 0.64.0.                                                                    ┃
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
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package scintillate

import io.undertow.{Handlers, Undertow}
import io.undertow.server.HttpServerExchange
import io.undertow.util.Headers

import java.nio.ByteBuffer

// Undertow with its `RoutingHandler`, answering on the XNIO I/O threads (as its own
// non-blocking handlers do); the echo body is read with the non-blocking receiver.
object RivalUndertow:
  private def send(exchange: HttpServerExchange, contentType: String, body: ByteBuffer): Unit =
    exchange.getResponseHeaders.nn.put(Headers.CONTENT_TYPE, contentType)
    exchange.getResponseSender.nn.send(body)

  lazy val server: Unit =
    val routing = Handlers.routing().nn
      . get("/bench", exchange => send(exchange, "text/plain", ByteBuffer.wrap(RivalJackson.hello)))
      . get("/json", exchange =>
          send(exchange, "application/json", ByteBuffer.wrap(RivalJackson.greeting())))
      . get("/large", exchange =>
          send(exchange, "application/octet-stream", ByteBuffer.wrap(HttpWorkload.largeBody)))
      . post("/echo", exchange =>
          exchange.getRequestReceiver.nn.receiveFullBytes: (exchange, bytes) =>
            send(exchange, "application/octet-stream", ByteBuffer.wrap(bytes)))
      . get("/user/{id}", exchange =>
          val id = exchange.getQueryParameters.nn.get("id").nn.getFirst
          val requestId = exchange.getRequestHeaders.nn.getFirst(HttpWorkload.requestIdHeader)
          exchange.getResponseHeaders.nn.put(Headers.CONTENT_TYPE, "text/plain")
          exchange.getResponseSender.nn.send(s"$id:${if requestId == null then "" else requestId}"))

    Undertow.builder().nn
    . addHttpListener(HttpRivals.port(HttpRivals.Undertow), "127.0.0.1").nn
    . setHandler(routing).nn
    . build().nn
    . start()

    HttpRivals.ready(HttpRivals.Undertow)
