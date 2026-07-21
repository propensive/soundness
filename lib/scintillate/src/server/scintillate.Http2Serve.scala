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
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package scintillate

import java.io as ji

import anticipation.*
import coaxial.*
import contingency.*
import cordillera.*
import gossamer.*
import parasite.*
import prepositional.*
import proscenium.*
import rudiments.*
import telekinesis.*
import turbulence.*
import vacuous.*
import zephyrine.*

// Serves an HTTP/2 connection (already negotiated via ALPN) over a socket's raw
// streams, driving the same handler contract the HTTP/1.1 backend uses. Each
// client-initiated stream is handled on its own virtual thread, so requests
// multiplexed on the one connection run concurrently. Runs until the connection
// ends (client GOAWAY or socket close). Blocks the calling (accept-loop) daemon
// for the connection's lifetime, matching `serveConnection`.
object Http2Serve:
  // Serve every stream on `connection` with the request handler `handler0` (a
  // neutral `AnyRef => AnyRef` rim of the context-function handler), each on its
  // own virtual thread. Blocks until the connection ends. The per-connection
  // state a session sets up is captured by `handler0` and shared here across
  // all streams.
  private def runStreams
    ( connection: Http2ServerConnection^, handler0: AnyRef, port: Int )
    ( using Monitor, Probate, (HttpServerEvent is Loggable)^ )
  :   Unit =

    given Tactic[Http2Error] = strategies.throwUnsafely
    given Tactic[StreamError] = strategies.throwUnsafely

    val connectionRef: AnyRef = connection.asInstanceOf[AnyRef]

    connection.eachStream: stream =>
      val streamRef: AnyRef = stream.asInstanceOf[AnyRef]

      daemon:
        val stream0 = streamRef.asInstanceOf[Http2Stream]
        val connection0 = connectionRef.asInstanceOf[Http2ServerConnection]
        val handler1 = handler0.asInstanceOf[AnyRef => AnyRef]
        val streamId = stream0.id

        safely:
          val entries = stream0.headers.await()

          // The request body streams the stream's inbound DATA frames, per the
          // `Spring` re-lending contract.
          val body: Spring[Data]^ = () =>
            zephyrine.Stream(stream0.body.stream.records.iterator)

          val request = PseudoHeaders.requestOf(entries, body)

          // The response sink: frame the handler's `Http.Response` as HEADERS
          // (+ DATA), driving the body block-by-block off its pull endpoint
          // exactly as the HTTP/1.1 `writeAll` does (never `memoize`, which a
          // transforming body such as a text encoder does not terminate under).
          val respond: HttpConnection.Respond^ = new HttpConnection.Respond:
            def apply(response: Http.Response^)(using Tactic[StreamError]): Unit =
              // A `Trailer` header (RFC 7230 §4.4) names response headers to be
              // sent as HTTP/2 trailers — a trailing HEADERS block after the body
              // (e.g. gRPC's `grpc-status`) — rather than in the initial block.
              val trailerNames: Set[Text] =
                response.textHeaders
                  . filter(_.key.lower == t"trailer")
                  . flatMap(_.value.cut(t",").map(_.trim.lower))
                  . to(Set)

              val (trailerEntries, headEntries) =
                PseudoHeaders.entries(response).partition: entry =>
                  trailerNames.contains(entry.name)

              val trailing: Boolean = !trailerEntries.isEmpty

              def sendTrailers(): Unit =
                if trailing then connection0.sendTrailers(streamId, trailerEntries)

              response.body match
                case Http.Body.Empty =>
                  connection0.sendHeaders(streamId, headEntries, endStream = !trailing)
                  sendTrailers()

                case Http.Body.Fixed(data) =>
                  val headEnd = data.isEmpty && !trailing
                  connection0.sendHeaders(streamId, headEntries, endStream = headEnd)
                  if !data.isEmpty then connection0.sendData(streamId, data, endStream = !trailing)
                  sendTrailers()

                case Http.Body.Flowing(source) =>
                  connection0.sendHeaders(streamId, headEntries, endStream = false)

                  source().sweep: (storage, start, size) =>
                    val block = storage.asInstanceOf[Array[Byte]]
                      . slice(start, start + size).immutable(using Unsafe)

                    connection0.sendData(streamId, block, endStream = false)

                  // Trailers close the stream; otherwise an empty END_STREAM DATA.
                  if trailing then sendTrailers()
                  else connection0.sendData(streamId, IArray.empty[Byte], endStream = true)

          val connection1 = new HttpConnection(request, true, port, respond)
          connection1.respond(handler1(connection1.asInstanceOf[AnyRef]).asInstanceOf[Http.Response])

  // Open the HTTP/2 connection over the socket's streams and run its session
  // scope. `serve` (per-request) is the degenerate session that immediately
  // handles with no per-connection setup; `serveSession` lends the caller an
  // `Http2Session` so it can set up per-connection state first.
  private def open(in: ji.InputStream, out: ji.OutputStream)(using Monitor)
  :   (Http2ServerConnection^, Probate) =

    // A local (pure) Probate rather than one captured from the accept daemon:
    // capturing the caller's `Probate` capability would make this call — and so
    // the accept-daemon body — impure.
    import probates.cancelProbate
    given Tactic[AsyncError] = strategies.throwUnsafely
    given Tactic[StreamError] = strategies.throwUnsafely
    val connection = Http2ServerConnection(StreamDuplex(in, out))
    connection.start()
    (connection, cancelProbate)

  def serve
    ( handler: AnyRef => AnyRef, in: ji.InputStream, out: ji.OutputStream, port: Int )
    ( using Monitor, (HttpServerEvent is Loggable)^ )
  :   Unit =

    val (connection, probate) = open(in, out)
    runStreams(connection, handler.asInstanceOf[AnyRef], port)(using summon, probate)

  // Serve one HTTP/2 connection as a session: `scope0` — the (rimmed) session
  // scope — runs once when the connection is established and may set up
  // per-connection state (auth, a rate limiter, …) before calling
  // `session.handle` to serve every stream with that state in scope. Capture
  // checking confines the state to this connection — the server dual of the
  // client's `Http2.Endpoint.session`. `scope0` crosses the accept daemon as an
  // `AnyRef => Unit` rim (a session-scope function value re-hides otherwise).
  def serveSession
    ( scope0: AnyRef, in: ji.InputStream, out: ji.OutputStream, port: Int )
    ( using Monitor, (HttpServerEvent is Loggable)^ )
  :   Unit =

    val (connection, probate) = open(in, out)

    val session: Http2Session^ = new Http2Session:
      def handle(handler: (connection: HttpConnection) ?=> Http.Response^{connection}): Unit =
        // Rim the context-function handler to a neutral `AnyRef => AnyRef`, as
        // the accept loop does for the per-request path.
        val handler0: AnyRef =
          ((ref: AnyRef) => handler(using ref.asInstanceOf[HttpConnection])).asInstanceOf[AnyRef]

        runStreams(connection, handler0, port)(using summon, probate)

    scope0.asInstanceOf[AnyRef => Unit](session.asInstanceOf[AnyRef])

// A per-connection serve scope. Runs once when an HTTP/2 connection is
// established; `handle` registers the per-stream request handler and serves
// every stream on the connection with it (concurrently), sharing whatever state
// the enclosing scope set up. That state is confined by capture checking to the
// connection, so it cannot leak to another client's connection.
trait Http2Session:
  def handle(handler: (connection: HttpConnection) ?=> Http.Response^{connection}): Unit

// A `Duplex` over a socket's raw byte streams: reads frame the inbound endpoint,
// each `send` writes the whole chunk and flushes (the writer serialises one frame
// per send). Used to drive `Http2ServerConnection` over a scintillate socket.
class StreamDuplex(in: ji.InputStream, out: ji.OutputStream)(using Tactic[StreamError])
extends Duplex:
  def source(using Buffering): (Stream[Data] over Credit)^ = Streamable.inputStream.stream(in)

  def send(consume data: (Stream[Data] over Credit)^): Unit =
    data.sweep: (storage, start, size) =>
      out.write(storage.asInstanceOf[Array[Byte]], start, size)

    out.flush()

  def close(): Unit = safely(out.close())
