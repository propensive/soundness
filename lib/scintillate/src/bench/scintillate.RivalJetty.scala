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

import org.eclipse.jetty.http.HttpHeader
import org.eclipse.jetty.io.Content
import org.eclipse.jetty.server.{Handler, Request, Response, Server, ServerConnector}
import org.eclipse.jetty.util.Callback
import org.eclipse.jetty.util.thread.QueuedThreadPool

import java.nio.ByteBuffer

// Jetty 12's core `Handler` API (no servlets), dispatching handlers onto virtual threads,
// which is how Jetty recommends running blocking handlers such as the echo's body read.
object RivalJetty:
  private def send(response: Response, callback: Callback, contentType: String, body: ByteBuffer)
  :   Boolean =

    response.getHeaders.nn.put(HttpHeader.CONTENT_TYPE, contentType)
    response.write(true, body, callback)
    true

  final class Routes extends Handler.Abstract:
    override def handle(request: Request, response: Response, callback: Callback): Boolean =
      val path = Request.getPathInContext(request).nn

      path match
        case "/bench" => send(response, callback, "text/plain", ByteBuffer.wrap(RivalJackson.hello))

        case "/json" =>
          send(response, callback, "application/json", ByteBuffer.wrap(RivalJackson.greeting()))

        case "/large" =>
          send(response, callback, "application/octet-stream", ByteBuffer.wrap(HttpWorkload.largeBody))

        case "/echo" =>
          send(response, callback, "application/octet-stream", Content.Source.asByteBuffer(request))

        case _ if path.startsWith(RivalJackson.userPrefix) =>
          val id = path.substring(RivalJackson.userPrefix.length)
          val requestId = request.getHeaders.nn.get(HttpWorkload.requestIdHeader)
          val body = s"$id:${if requestId == null then "" else requestId}"
          send(response, callback, "text/plain", ByteBuffer.wrap(body.getBytes("UTF-8")))

        case _ =>
          false

  lazy val server: Unit =
    val threads = QueuedThreadPool()
    threads.setVirtualThreadsExecutor(java.util.concurrent.Executors.newVirtualThreadPerTaskExecutor())
    val server = Server(threads)
    val connector = ServerConnector(server)
    connector.setHost("127.0.0.1")
    connector.setPort(HttpRivals.port(HttpRivals.Jetty))
    server.addConnector(connector)
    server.setHandler(Routes())
    server.start()

    HttpRivals.ready(HttpRivals.Jetty)
