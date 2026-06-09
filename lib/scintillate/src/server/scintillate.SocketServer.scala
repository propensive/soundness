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
┃    Soundness, version 0.54.0.                                                                    ┃
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

import java.io as ji
import java.net as jn

import anticipation.*
import contingency.*
import parasite.*
import rudiments.*
import telekinesis.*
import turbulence.*
import urticose.*
import vacuous.*
import zephyrine.*

// A raw-TCP HTTP/1.1 server backend, built on a `java.net.ServerSocket` rather
// than `com.sun.net.httpserver`. Each accepted socket is handled on its own
// Loom virtual thread (`daemon`). The handler API is identical to `HttpServer`'s
// — a `HttpConnection ?=> Http.Response` — so the two backends are
// interchangeable. This first cut serves a single request per connection and
// closes it; keep-alive, request-body framing and connection upgrades build on
// top of this skeleton.
case class SocketServer(port: Int, local: Boolean = true)(using errorPage: WebserverErrorPage)
extends RequestServable:
  def handle(handler: HttpConnection ?=> Http.Response)(using Monitor, Codicil)
  :   Service logs HttpServerEvent raises ServerError =

    def startServer(): jn.ServerSocket raises ServerError =
      try
        val host = if local then "localhost" else "0.0.0.0"
        jn.ServerSocket(port, 0, jn.InetAddress.getByName(host).nn)
      catch case error: jn.BindException => abort(ServerError(port))

    def writeAll(out: ji.OutputStream, stream: Stream[Data]): Unit raises StreamError =
      var count: Int = 0

      stream.each: block =>
        try
          out.write(block.mutable(using Unsafe))
          count += block.length
        catch case _: ji.IOException => abort(StreamError(count.b))

      try out.flush() catch case _: ji.IOException => abort(StreamError(count.b))

    def serve(socket: jn.Socket): Unit =
      try
        val in = socket.getInputStream.nn
        val out = socket.getOutputStream.nn

        whereas:
          case StreamError(length) =>
            Log.warn(HttpServerEvent.BrokenStream(length))

          case error: HttpRequestError =>
            safely(writeAll(out, Http.Response.serialize(Http.Response(Http.BadRequest)())))

        . recover:
            val cursor = Cursor[Data](Streamable.inputStream.stream(in).filter(_.nonEmpty).iterator)
            val head = Http.Request.parseHead(cursor)

            val request =
              Http.Request
                ( head.method, head.version, head.host, head.target, head.headers,
                  () => cursor.remainder )

            def respond(response: Http.Response): Unit raises StreamError =
              val includeBody = head.method != Http.Head
              writeAll(out, Http.Response.serialize(response, includeBody))

            val connection = new HttpConnection(request, false, port, respond)
            Log.fine(HttpServerEvent.Received(request))

            connection.respond:
              try handler(using connection)
              catch case throwable: Throwable => errorPage.handle(throwable, connection)

      catch case NonFatal(exception) => exception.printStackTrace()
      finally safely(socket.close())

    val serverSocket = startServer()

    val acceptLoop = loop:
      safely(serverSocket.accept().nn).let: socket =>
        daemon(serve(socket))

    val acceptTask = daemon(acceptLoop.run())
    val cancel: Promise[Unit] = Promise[Unit]()
    val stopTask = async(cancel.attend() yet { acceptLoop.stop(); safely(serverSocket.close()) })

    Service: () =>
      safely(cancel.fulfill(()))

