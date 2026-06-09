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
import gossamer.*
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
// interchangeable. The connection loop supports keep-alive and pipelining,
// `Content-Length` and chunked request bodies, and `101` protocol upgrades.
case class SocketServer(port: Int, local: Boolean = true)(using errorPage: WebserverErrorPage)
extends RequestServable:

  private def writeAll(out: ji.OutputStream, stream: Stream[Data]): Unit raises StreamError =
    var count: Int = 0

    stream.each: block =>
      try
        out.write(block.mutable(using Unsafe))
        count += block.length
      catch case _: ji.IOException => abort(StreamError(count.b))

    try out.flush() catch case _: ji.IOException => abort(StreamError(count.b))

  // Frame the request body off the shared connection cursor: chunked decoding
  // for `Transfer-Encoding: chunked`, otherwise `Content-Length` bytes, or empty
  // when neither is given. The framed stream stops exactly at the body's end so
  // the cursor is left at the next pipelined request.
  private def requestBody(cursor: Cursor[Data], head: Http.Request.Head): Stream[Data] =
    val chunked: Boolean = head.headers.exists: header =>
      header.key.lower == t"transfer-encoding" && header.value.lower.contains(t"chunked")

    if chunked then Http.Request.chunkedBody(cursor) else
      val length: Optional[Int] =
        head.headers.filter(_.key.lower == t"content-length").prim.let(_.value)
        . lay(Unset: Optional[Int]): text =>
            safely(Integer.parseInt(text.s.trim.nn))

      length.lay(Stream())(Http.Request.fixedBody(cursor, _))

  // RFC 7230 §6.3: HTTP/1.1 keeps connections alive unless `Connection: close`;
  // HTTP/1.0 closes unless `Connection: keep-alive`.
  private def keepAlive(head: Http.Request.Head): Boolean =
    val connection = head.headers.filter(_.key.lower == t"connection").map(_.value.lower)

    head.version match
      case 1.1 => !connection.exists(_.contains(t"close"))
      case _   => connection.exists(_.contains(t"keep-alive"))

  // A request asks to upgrade the protocol (e.g. to WebSocket) when it carries
  // `Connection: Upgrade` together with an `Upgrade` header. Such a request's
  // body is the unbounded remainder of the connection, so a frame reader can
  // keep receiving bytes after the handshake.
  private def isUpgrade(head: Http.Request.Head): Boolean =
    head.headers.filter(_.key.lower == t"connection").exists(_.value.lower.contains(t"upgrade"))
    && head.headers.exists(_.key.lower == t"upgrade")

  // Drive the HTTP/1.1 keep-alive loop over an arbitrary byte source and sink,
  // independent of any socket. `handle` calls this once per accepted connection;
  // it is also the in-process entry point for tests and benchmarks, which feed a
  // `ByteArrayInputStream` of requests and capture the responses with no network
  // involvement. The caller owns the streams (closing, read timeouts).
  def serveConnection(handler: HttpConnection ?=> Http.Response)
    ( in: ji.InputStream, out: ji.OutputStream )
    ( using HttpServerEvent is Loggable )
  :   Unit =

    // Handle one request off the cursor; return whether to keep the connection
    // alive for a further request.
    def serveRequest(cursor: Cursor[Data]): Boolean raises StreamError =
      whereas:
        case error: HttpRequestError =>
          val response = Http.Response(Http.BadRequest)() + Http.Header(t"connection", t"close")
          safely(writeAll(out, Http.Response.serialize(response)))
          false

      . recover:
          val head = Http.Request.parseHead(cursor)
          val upgrade = isUpgrade(head)
          val body = if upgrade then cursor.remainder else requestBody(cursor, head)
          val keep = keepAlive(head)

          val request =
            Http.Request
              ( head.method, head.version, head.host, head.target, head.headers, () => body )

          var upgraded = false

          def respond(response: Http.Response): Unit raises StreamError =
            if response.status == Http.SwitchingProtocols then
              // Switch to the upgraded protocol: write the handshake headers, then
              // pipe its raw stream until it ends. This blocks for the lifetime of
              // the upgraded connection (e.g. a WebSocket session).
              upgraded = true
              writeAll(out, Http.Response.serialize(response))
            else
              val response2 =
                if keep then response else response + Http.Header(t"connection", t"close")

              writeAll(out, Http.Response.serialize(response2, head.method != Http.Head))

          val connection = new HttpConnection(request, false, port, respond)
          Log.fine(HttpServerEvent.Received(request))

          connection.respond:
            try handler(using connection)
            catch case throwable: Throwable => errorPage.handle(throwable, connection)

          if upgraded then false else
            // Drain any body the handler did not consume, so the cursor is left
            // at the start of the next request.
            body.each(_ => ())
            keep

    try
      whereas:
        case StreamError(length) =>
          Log.warn(HttpServerEvent.BrokenStream(length))

      . recover:
          val cursor = Cursor[Data](Streamable.inputStream.stream(in).filter(_.nonEmpty).iterator)

          var continue = true
          while continue && !cursor.finished do continue = serveRequest(cursor)

    catch case NonFatal(exception) => exception.printStackTrace()

  def handle(handler: HttpConnection ?=> Http.Response)(using Monitor, Codicil)
  :   Service logs HttpServerEvent raises ServerError =

    val idleTimeout: Int = 30000

    def startServer(): jn.ServerSocket raises ServerError =
      try
        val host = if local then "localhost" else "0.0.0.0"
        jn.ServerSocket(port, 0, jn.InetAddress.getByName(host).nn)
      catch case error: jn.BindException => abort(ServerError(port))

    val serverSocket = startServer()

    val acceptLoop = loop:
      safely(serverSocket.accept().nn).let: socket =>
        daemon:
          try
            socket.setSoTimeout(idleTimeout)
            serveConnection(handler)(socket.getInputStream.nn, socket.getOutputStream.nn)
          finally safely(socket.close())

    val acceptTask = daemon(acceptLoop.run())
    val cancel: Promise[Unit] = Promise[Unit]()
    val stopTask = async(cancel.attend() yet { acceptLoop.stop(); safely(serverSocket.close()) })

    Service: () =>
      safely(cancel.fulfill(()))

