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
import prepositional.*
import rudiments.*
import telekinesis.*
import turbulence.*
import urticose.*
import vacuous.*
import zephyrine.*

// A raw-TCP HTTP/1.1 server backend, built on a `java.net.ServerSocket` rather
// than `com.sun.net.httpserver`. Each accepted socket is handled on its own
// Loom virtual thread (`daemon`). The handler API is identical to `HttpServer`'s
// — a `(connection: HttpConnection) ?=> Http.Response^{connection}` — so the two
// backends are interchangeable. The connection loop supports keep-alive and pipelining,
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

  private val continueResponse: Data = t"HTTP/1.1 100 Continue\r\n\r\n".in[Data]

  private def writeAll(out: ji.OutputStream, stream: (Stream[Data] over Credit)^)
  :   Unit raises StreamError =

    var count: Int = 0

    // Consume the stream one block at a time and flush after each: a streaming
    // body (chunked response, SSE, or an upgraded WebSocket) may never end, so
    // its bytes must reach the client as they are produced rather than be forced
    // into memory or sit unflushed in the buffer.
    var failed: Boolean = false

    stream.sweep: (storage, start, size) =>
      if !failed then
        try
          out.write(storage.asInstanceOf[Array[Byte]], start, size)
          out.flush()
          count += size
        catch case error: ji.IOException => failed = true

    if failed then abort(StreamError(count.b))

  // Frame the request body off the shared connection cursor: chunked decoding
  // for `Transfer-Encoding: chunked`, otherwise `Content-Length` bytes, or empty
  // when neither is given. The framed stream stops exactly at the body's end so
  // the cursor is left at the next pipelined request.
  // Upstream #1570's kernel-stream body with this branch's cursor convention
  // (`Cursor[Data, {}]^`: a concrete cap argument, never `?`, which collapses inline
  // receiver proxies to read-only).
  private def requestBody(cursor: Cursor[Data, {}]^, head: Http.Request.Head)
  :   Stream[Data] over Credit =

    val chunked: Boolean = head.headers.exists: header =>
      header.key.lower == t"transfer-encoding" && header.value.lower.contains(t"chunked")

    if chunked then Http.Request.chunkedBody(cursor) else
      val length: Optional[Int] =
        head.headers.filter(_.key.lower == t"content-length").prim.let(_.value)
        . lay(Unset: Optional[Int]): text =>
            safely(Integer.parseInt(text.s.trim.nn))

      length.lay(Http.emptyBody())(Http.Request.fixedBody(cursor, _))

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

  private def streaming(response: Http.Response^): Boolean = response.body match
    case Http.Body.Flowing(_) => true
    case _                    => false

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
  def serveConnection(handler: (connection: HttpConnection) ?=> Http.Response^{connection})
    ( in: ji.InputStream, out: ji.OutputStream )
    ( using (HttpServerEvent is Loggable)^ )
  :   Unit =

    val closeHeader: Http.Header = Http.Header(t"connection", t"close")

    // Handle one request off the cursor; return whether to keep the connection
    // alive for a further request.
    def serveRequest(cursor: Cursor[Data, {}]^): Boolean =
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
          if expectsContinue(head) then writeAll(out, continueResponse.stream)

          // The framed body, minted once and lent single-owner: the handler and
          // the post-response drain share it, so the handler pulls what it
          // reads and the drain consumes the rest. A second `body()` call
          // RESUMES rather than replaying — explicit `memoize` replaces the
          // former Progression view's implicit caching.
          val bodyStream: Stream[Data] over Credit =
            if upgrade then streamOf(cursor) else requestBody(cursor, head)

          // Neutral carrier: the spring re-lends the same single-owner stream.
          val bodyRef: AnyRef = bodyStream.asInstanceOf[AnyRef]

          val request =
            Http.Request
              ( head.method,
                head.version,
                head.host,
                head.target,
                head.headers,
                () => bodyRef.asInstanceOf[Stream[Data] over Credit] )

          var upgraded = false
          var keep = keepAlive(head)

          val respond: HttpConnection.Respond^ = new HttpConnection.Respond:
            def apply(response: Http.Response^)(using Tactic[StreamError]): Unit =
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
            val drain = bodyRef.asInstanceOf[Stream[Data] over Credit]
            var intact = true
            var draining = true

            while draining do drain.refill(Credit(drainLimit)) match
              case count: Int =>
                drain.skip(count)

                if cursor.position.n0 - drainStart > drainLimit then
                  intact = false
                  draining = false

              case _ =>
                draining = false

            intact

    // A stream error tearing down the connection is logged and ends it; any other
    // unexpected failure is left to propagate out of the per-connection daemon, where
    // the `trap` installed in `handle` logs it and isolates it to this connection.
    recover:
      case StreamError(length) =>
        Log.warn(HttpServerEvent.BrokenStream(length))

    . protect:
        // The connection cursor pulls straight from the socket's endpoint;
        // construction is deferred until the first read (live-socket rule).
        val cursor = Cursor[Data](Streamable.inputStream.stream(in))

        var continue = true
        while continue && !cursor.finished do continue = serveRequest(cursor)

  // A per-request server: handle every request (HTTP/1.1) or stream (HTTP/2)
  // with `handler`. The degenerate session with no per-connection setup.
  def handle(handler: (connection: HttpConnection) ?=> Http.Response^{connection})
    ( using Monitor, Probate )
    ( using (HttpServerEvent is Loggable)^, Tactic[ServerError] )
  :   Service^ =

    handleSession: session ?=>
      session.handle(handler)

  // A per-connection session server: `scope` runs once when a connection is
  // established (an HTTP/2 connection, or an HTTP/1.1 keep-alive socket) and may
  // set up per-connection state before calling `session.handle` to serve that
  // connection's requests/streams with the state in scope. Capture checking
  // confines the state to its connection.
  def handleSession(scope: (session: Http2Session^) ?=> Unit)
    ( using Monitor, Probate )
    ( using (HttpServerEvent is Loggable)^, Tactic[ServerError] )
  :   Service^ =

    val idleTimeout: Int = 30000

    def startServer(): jn.ServerSocket raises ServerError =
      try
        val address = jn.InetAddress.getByName(if local then "localhost" else "0.0.0.0").nn

        ssl.lay(jn.ServerSocket(port, 0, address)): context =>
          val socket = context.getServerSocketFactory.nn.createServerSocket(port, 0, address).nn

          // Offer `h2` then `http/1.1` by ALPN, so a client that speaks HTTP/2
          // negotiates it during the TLS handshake; accepted sockets inherit
          // these parameters. Plaintext servers have no ALPN.
          socket match
            case ssl: jns.SSLServerSocket =>
              val params = ssl.getSSLParameters.nn
              params.setApplicationProtocols(Array[String | Null]("h2", "http/1.1"))
              ssl.setSSLParameters(params)

            case _ =>
              ()

          socket
      catch case error: jn.BindException => abort(ServerError(port))

    val serverSocket = startServer()

    // A failure in a per-connection daemon (anything not already turned into an HTTP
    // response) is logged and accepted, isolating it to that connection: the server
    // keeps accepting, and the error neither escalates nor is dumped to stderr.
    contain:
      case error => Log.fail(HttpServerEvent.ConnectionFailed(error)); Remedy.Accept

    . protect:
        // Daemon bodies must be pure context functions, so the server, the handler and each
        // socket cross into them as `AnyRef` rims (the cordillera recipe).
        val self: AnyRef = this
        // Eta-wrapped into a capture-neutral `AnyRef => Unit` (capability-typed function
        // types re-hide when crossed through a rim; the kernel-module-sep finding), since a
        // context-function value applies itself in any non-context-function position.
        val scope1: AnyRef => Unit =
          session => scope(using session.asInstanceOf[Http2Session^])
        val scope0: AnyRef = scope1.asInstanceOf[AnyRef]
        // The (capability-typed) Loggable evidence crosses as an `AnyRef` rim too.
        val loggable0: AnyRef = summon[(HttpServerEvent is Loggable)^].asInstanceOf[AnyRef]

        val acceptLoop = loop:
          safely(serverSocket.accept().nn).let: socket =>
            val socket0: AnyRef = socket

            daemon:
              val socket1 = socket0.asInstanceOf[jn.Socket]

              try
                socket1.setSoTimeout(idleTimeout)

                val in = socket1.getInputStream.nn
                val out = socket1.getOutputStream.nn
                given HttpServerEvent is Loggable = loggable0.asInstanceOf[HttpServerEvent is Loggable]
                val scope2 = scope0.asInstanceOf[AnyRef => Unit]
                val self1 = self.asInstanceOf[SocketServer]

                // A TLS socket may have negotiated `h2` by ALPN; force the
                // handshake to learn the protocol, then dispatch to the native
                // HTTP/2 engine. Everything else (plaintext, or ALPN `http/1.1`)
                // takes the HTTP/1.1 keep-alive path. Either way the connection is
                // one session scope.
                val protocol: Text = socket1 match
                  case tls: jns.SSLSocket =>
                    safely(tls.startHandshake())
                    Optional(tls.getApplicationProtocol).let(_.tt).or(t"")

                  case _ =>
                    t""

                if protocol == t"h2" then Http2Serve.serveSession(scope0, in, out, port)
                else
                  // An HTTP/1.1 keep-alive connection is also a per-connection
                  // scope; its session `handle` serves the connection's requests.
                  val session: Http2Session^ = new Http2Session:
                    def handle(handler: (connection: HttpConnection) ?=> Http.Response^{connection})
                    :   Unit =
                      self1.serveConnection(handler)(in, out)

                  scope2(session.asInstanceOf[AnyRef])

              finally safely(socket1.close())

        val acceptLoop0: AnyRef = acceptLoop.asInstanceOf[AnyRef]
        val acceptTask = daemon(acceptLoop0.asInstanceOf[Loop].run())
        val cancel: Promise[Unit] = Promise[Unit]()

        val stopTask = async:
          cancel.attend()
          acceptLoop.stop()
          safely(serverSocket.close())

        Service: () =>
          safely(cancel.fulfill(()))

