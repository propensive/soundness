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
┃    Soundness, version 0.63.0.                                                                    ┃
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
import javax.net.ssl as jns

import anticipation.*
import contingency.*
import denominative.*
import gossamer.*
import hieroglyph.charEncoders.asciiEncoder
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
// `Content-Length` and chunked request bodies, `100-continue`, request-size
// limits, and `101` protocol upgrades. Supplying an `SSLContext` as `ssl` serves
// over TLS instead of cleartext.
case class SocketServer
  ( port: Int, local: Boolean = true, ssl: Optional[jns.SSLContext] = Unset )
  ( using errorPage: WebserverErrorPage )
extends RequestServable:

  // Cap on the bytes an unconsumed request body will be drained before the
  // connection is closed rather than read in full to reach the next request.
  private val drainLimit: Int = 65536

  private val continueResponse: Data = t"HTTP/1.1 100 Continue\r\n\r\n".data

  private def writeAll(out: ji.OutputStream, stream: Stream[Data]): Unit raises StreamError =
    var count: Int = 0

    // Consume the stream one block at a time and flush after each: a streaming
    // body (chunked response, SSE, or an upgraded WebSocket) may never end, so
    // its bytes must reach the client as they are produced rather than be forced
    // into memory or sit unflushed in the buffer.
    def recur(stream: Stream[Data]): Unit = stream.flow(()):
      try
        out.write(next.mutable(using Unsafe))
        out.flush()
        count += next.length
      catch case _: ji.IOException => abort(StreamError(count.b))

      recur(more)

    recur(stream)

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
    head.headers.filter(_.key.lower == t"connection").exists(_.value.lower.contains(t"upgrade")) &&
      head.headers.exists(_.key.lower == t"upgrade")

  // A client may withhold a request body until the server agrees to receive it
  // with an interim `100 Continue` (RFC 7231 §5.1.1).
  private def expectsContinue(head: Http.Request.Head): Boolean =
    head.version == 1.1 && head.headers.exists: header =>
      header.key.lower == t"expect" && header.value.lower.contains(t"100-continue")

  private def streaming(response: Http.Response): Boolean = response.body match
    case Http.Body.Streaming(_) => true
    case _                      => false

  // Map a request-parsing failure to the status the client should see.
  private def errorStatus(reason: HttpRequestError.Reason): Http.Status =
    import HttpRequestError.Reason

    reason match
      case Reason.UriTooLong      => Http.UriTooLong
      case Reason.HeadersTooLarge => Http.RequestHeaderFieldsTooLarge
      case _                      => Http.BadRequest

  // Drive the HTTP/1.1 keep-alive loop over an arbitrary byte source and sink,
  // independent of any socket. `handle` calls this once per accepted connection;
  // it is also the in-process entry point for tests and benchmarks, which feed a
  // `ByteArrayInputStream` of requests and capture the responses with no network
  // involvement. The caller owns the streams (closing, read timeouts).
  def serveConnection(handler: HttpConnection ?=> Http.Response)
    ( in: ji.InputStream, out: ji.OutputStream )
    ( using HttpServerEvent is Loggable )
  :   Unit =

    val closeHeader: Http.Header = Http.Header(t"connection", t"close")

    // Handle one request off the cursor; return whether to keep the connection
    // alive for a further request.
    def serveRequest(cursor: Cursor[Data]): Boolean =
      recover:
        case error: HttpRequestError =>
          val response = Http.Response(errorStatus(error.reason))() + closeHeader
          safely(writeAll(out, Http.Response.serialize(response)))
          false

        case StreamError(_) =>
          // A read error or timeout while a request is in flight: best-effort 408
          // (the socket may still be writable on a read timeout), then close.
          val response = Http.Response(Http.RequestTimeout)() + closeHeader
          safely(writeAll(out, Http.Response.serialize(response)))
          false

      . protect:
          val head = Http.Request.parseHead(cursor)
          val upgrade = isUpgrade(head)

          // Tell a waiting client it may send the body before we read it.
          if expectsContinue(head) then writeAll(out, Stream(continueResponse))

          val body = if upgrade then cursor.remainder else requestBody(cursor, head)

          val request =
            Http.Request
              ( head.method, head.version, head.host, head.target, head.headers, () => body )

          var upgraded = false
          var keep = keepAlive(head)

          def respond(response: Http.Response): Unit raises StreamError =
            if response.status == Http.SwitchingProtocols then
              // Switch to the upgraded protocol: write the handshake headers, then
              // pipe its raw stream until it ends. This blocks for the lifetime of
              // the upgraded connection (e.g. a WebSocket session).
              upgraded = true
              writeAll(out, Http.Response.serialize(response))
            else
              // A streaming body to a pre-1.1 client can't be chunked, so it must
              // be delimited by closing the connection.
              if head.version != 1.1 && streaming(response) then keep = false
              val response2 = if keep then response else response + closeHeader
              val bytes = Http.Response.serialize(response2, head.method != Http.Head, head.version)
              writeAll(out, bytes)

          val connection = new HttpConnection(request, ssl.present, port, respond)
          Log.fine(HttpServerEvent.Received(request))
          val started = System.currentTimeMillis

          connection.respond:
            try handler(using connection)
            catch case throwable: Throwable => errorPage.handle(throwable, connection)

          Log.info(HttpServerEvent.Processed(request, System.currentTimeMillis - started))

          if upgraded || !keep then false else
            // Drain any body the handler did not consume so the cursor reaches the
            // next request — but bound it: past `drainLimit` unread bytes, close
            // rather than read a large ignored upload. Walking already-consumed
            // (memoised) blocks doesn't move the cursor, so the position delta
            // measures only what the drain itself reads.
            val drainStart = cursor.position.n0

            def drained(stream: Stream[Data]): Boolean = stream match
              case _ #:: tail => cursor.position.n0 - drainStart <= drainLimit && drained(tail)
              case _          => true

            drained(body)

    // A stream error tearing down the connection is logged and ends it; any other
    // unexpected failure is left to propagate out of the per-connection daemon, where
    // the `trap` installed in `handle` logs it and isolates it to this connection.
    recover:
      case StreamError(length) =>
        Log.warn(HttpServerEvent.BrokenStream(length))

    . protect:
        val cursor = Cursor[Data](Streamable.inputStream.stream(in).filter(_.nonEmpty).iterator)

        var continue = true
        while continue && !cursor.finished do continue = serveRequest(cursor)

  def handle(handler: HttpConnection ?=> Http.Response)(using Monitor, Probate)
  :   Service logs HttpServerEvent raises ServerError =

    val idleTimeout: Int = 30000

    def startServer(): jn.ServerSocket raises ServerError =
      try
        val address = jn.InetAddress.getByName(if local then "localhost" else "0.0.0.0").nn

        ssl.lay(jn.ServerSocket(port, 0, address)): context =>
          context.getServerSocketFactory.nn.createServerSocket(port, 0, address).nn
      catch case error: jn.BindException => abort(ServerError(port))

    val serverSocket = startServer()

    // A failure in a per-connection daemon (anything not already turned into an HTTP
    // response) is logged and accepted, isolating it to that connection: the server
    // keeps accepting, and the error neither escalates nor is dumped to stderr.
    contain:
      case error => Log.fail(HttpServerEvent.ConnectionFailed(error)); Remedy.Accept

    . protect:
        val acceptLoop = loop:
          safely(serverSocket.accept().nn).let: socket =>
            daemon:
              try
                socket.setSoTimeout(idleTimeout)
                serveConnection(handler)(socket.getInputStream.nn, socket.getOutputStream.nn)
              finally safely(socket.close())

        val acceptTask = daemon(acceptLoop.run())
        val cancel: Promise[Unit] = Promise[Unit]()

        val stopTask = async:
          cancel.attend()
          acceptLoop.stop()
          safely(serverSocket.close())

        Service: () =>
          safely(cancel.fulfill(()))

