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

import io.vertx.core.Vertx
import io.vertx.core.buffer.Buffer
import io.vertx.core.json.JsonObject
import io.vertx.ext.web.Router
import io.vertx.ext.web.handler.BodyHandler

// Vert.x 5 with vertx-web's `Router`, handlers running on the event loop, and Vert.x's own
// `JsonObject` for the JSON row.
object RivalVertx:
  private val hello = Buffer.buffer(RivalJackson.hello).nn
  private val large = Buffer.buffer(HttpWorkload.largeBody).nn

  lazy val server: Unit =
    java.util.logging.LogManager.getLogManager.nn.reset()
    val vertx = Vertx.vertx().nn
    val router = Router.router(vertx).nn

    router.get("/bench").nn.handler: context =>
      context.response.nn.putHeader("Content-Type", "text/plain").nn.end(hello)

    router.get("/json").nn.handler: context =>
      context.json(JsonObject().put("message", HttpWorkload.hello))

    router.get("/large").nn.handler: context =>
      context.response.nn.putHeader("Content-Type", "application/octet-stream").nn.end(large)

    router.post("/echo").nn.handler(BodyHandler.create()).nn.handler: context =>
      context.response.nn.putHeader("Content-Type", "application/octet-stream").nn
      . end(context.body.nn.buffer)

    router.get("/user/:id").nn.handler: context =>
      val requestId = context.request.nn.getHeader(HttpWorkload.requestIdHeader)
      context.response.nn.putHeader("Content-Type", "text/plain").nn
      . end(s"${context.pathParam("id")}:${if requestId == null then "" else requestId}")

    vertx.createHttpServer().nn
    . requestHandler(router).nn
    . listen(HttpRivals.port(HttpRivals.Vertx), "127.0.0.1").nn
    . toCompletionStage.nn.toCompletableFuture.nn.get()

    HttpRivals.ready(HttpRivals.Vertx)
