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

import com.sun.net.httpserver.{HttpExchange, HttpServer}

// The JDK's built-in `com.sun.net.httpserver`, handing each exchange to a virtual thread:
// the zero-dependency baseline, with one context dispatching on the path by hand.
object RivalJdk:
  private def send(exchange: HttpExchange, status: Int, contentType: String, body: Array[Byte])
  :   Unit =

    exchange.getResponseHeaders.nn.set("Content-Type", contentType)
    exchange.sendResponseHeaders(status, if body.length == 0 then -1 else body.length)
    exchange.getResponseBody.nn.write(body)
    exchange.close()

  private def handle(exchange: HttpExchange): Unit =
    val path = exchange.getRequestURI.nn.getPath.nn

    path match
      case "/bench" => send(exchange, 200, "text/plain", RivalJackson.hello)
      case "/json"  => send(exchange, 200, "application/json", RivalJackson.greeting())
      case "/large" => send(exchange, 200, "application/octet-stream", HttpWorkload.largeBody)

      case "/echo" =>
        val body = exchange.getRequestBody.nn.readAllBytes().nn
        send(exchange, 200, "application/octet-stream", body)

      case _ if path.startsWith(RivalJackson.userPrefix) =>
        val id = path.substring(RivalJackson.userPrefix.length)
        val requestId = exchange.getRequestHeaders.nn.getFirst(HttpWorkload.requestIdHeader)
        val body = s"$id:${if requestId == null then "" else requestId}"
        send(exchange, 200, "text/plain", body.getBytes("UTF-8").nn)

      case _ =>
        send(exchange, 404, "text/plain", Array.emptyByteArray)

  lazy val server: Unit =
    val address = java.net.InetSocketAddress("127.0.0.1", HttpRivals.port(HttpRivals.JdkHttpServer))
    val server = HttpServer.create(address, 1024).nn
    server.setExecutor(java.util.concurrent.Executors.newVirtualThreadPerTaskExecutor())
    server.createContext("/", handle(_))
    server.start()

    HttpRivals.ready(HttpRivals.JdkHttpServer)
