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

import io.helidon.http.HeaderNames
import io.helidon.webserver.WebServer
import io.helidon.webserver.http.HttpRouting

// Helidon SE 4 ("Níma"): a blocking-style server that runs every request on its own
// virtual thread, with Helidon's own routing.
object RivalHelidon:
  private val requestId = HeaderNames.create(HttpWorkload.requestIdHeader).nn

  lazy val server: Unit =
    java.util.logging.LogManager.getLogManager.nn.reset()

    def routing(routes: HttpRouting.Builder): Unit =
      routes
      . get("/bench", (request, response) =>
          response.header(HeaderNames.CONTENT_TYPE, "text/plain").nn.send(RivalJackson.hello))
      . get("/json", (request, response) =>
          response.header(HeaderNames.CONTENT_TYPE, "application/json").nn
          . send(RivalJackson.greeting()))
      . get("/large", (request, response) =>
          response.header(HeaderNames.CONTENT_TYPE, "application/octet-stream").nn
          . send(HttpWorkload.largeBody))
      . post("/echo", (request, response) =>
          response.header(HeaderNames.CONTENT_TYPE, "application/octet-stream").nn
          . send(request.content.nn.as(classOf[Array[Byte]])))
      . get("/user/{id}", (request, response) =>
          val id = request.path.nn.pathParameters.nn.get("id")
          val value = request.headers.nn.first(requestId).nn.orElse("")
          response.header(HeaderNames.CONTENT_TYPE, "text/plain").nn.send(s"$id:$value"))

    WebServer.builder().nn
    . host("127.0.0.1").nn
    . port(HttpRivals.port(HttpRivals.Helidon)).nn
    . routing(routing(_)).nn
    . build().nn
    . start()

    HttpRivals.ready(HttpRivals.Helidon)
