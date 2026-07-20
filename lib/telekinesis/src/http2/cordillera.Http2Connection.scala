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
package cordillera

import java.util.concurrent.atomic as juca

import scala.collection.concurrent as scc

import anticipation.{Data as Bytes, *}
import coaxial.*
import contingency.*
import gossamer.*
import hieroglyph.*, charEncoders.asciiEncoder
import parasite.*
import proscenium.*
import rudiments.*
import telekinesis.*
import turbulence.*
import vacuous.*
import zephyrine.*

import Http2.*

object Http2Connection:
  // The client connection preface (RFC 7540 §3.5): a fixed octet sequence that
  // precedes the first SETTINGS frame in prior-knowledge h2c.
  private[cordillera] val connectionPreface: Bytes = t"PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n".in[Bytes]

  // Our advertised SETTINGS: disable server push; a generous stream window.
  private[cordillera] val initialSettings: List[Setting] =
    List
      ( Setting(SettingId.EnablePush.id, 0),
        Setting(SettingId.InitialWindowSize.id, 0x7fffffff) )

  // Dispatch one decoded frame. Lives on the companion — taking the connection as a
  // plain parameter — so the reader daemon's body stays free of `this` captures.
  // The tactic is a plain using-parameter: a context-function result may not hide it.
  private def dispatch(conn: Http2Connection, frame: Frame, decoder: Hpack)
    ( using Tactic[Http2Error] )
  :   Boolean =
    frame match
      case Frame.Settings(_, ack) =>
        if !ack then
          conn.send(Frame.Settings(Nil, ack = true))
          conn.started.offer(())

        true

      case Frame.Ping(opaque, ack) =>
        if !ack then conn.send(Frame.Ping(opaque, ack = true))
        true

      case Frame.GoAway(lastStreamId, _, _) =>
        Log.warn(Http2Event.GoAway(lastStreamId))
        false

      case Frame.Headers(id, block, endStream, _) =>
        conn.streams.get(id).foreach: stream =>
          stream.acceptHeaders(decoder.decode(block))

          if endStream then
            stream.end()
            conn.streams.remove(id)

        true

      case Frame.Data(id, payload, endStream) =>
        conn.streams.get(id).foreach: stream =>
          stream.acceptData(payload)
          // Replenish the peer's flow-control window for what we consumed.
          if payload.length > 0 then
            conn.send(Frame.WindowUpdate(0, payload.length))
            conn.send(Frame.WindowUpdate(id, payload.length))

          if endStream then
            stream.end()
            conn.streams.remove(id)

        true

      case Frame.RstStream(id, _) =>
        conn.streams.get(id).foreach: stream =>
          stream.end()
          conn.streams.remove(id)

        true

      case Frame.WindowUpdate(_, _) | Frame.Continuation(_, _, _) | Frame.Ignored(_, _) =>
        true


// One outbound request stream's receive side: a promise for its response header
// block (resolved on the first HEADERS frame), a spool feeding the response body
// (fed by DATA frames), and a promise for trailers (resolved on a second, end-stream
// HEADERS frame — gRPC's status). The reader daemon populates these; the caller
// awaits `headers` and consumes `body.stream`.
class Http2Stream(val id: Int):
  val headers: Promise[List[HpackEntry]] = Promise()
  val trailers: Promise[List[HpackEntry]] = Promise()
  val body: Relay[Bytes] = Relay()
  // Untracked: written only by the connection's single reader daemon.
  @caps.unsafe.untrackedCaptures
  private var headersSeen: Boolean = false

  // Record an incoming HEADERS block: the first becomes the response headers, a
  // subsequent one (always end-stream) becomes the trailers.
  def acceptHeaders(block: List[HpackEntry]): Unit =
    if !headersSeen then
      headersSeen = true
      headers.offer(block)
    else
      trailers.offer(block)

  def acceptData(data: Bytes): Unit = body.put(data)

  // Close the receive side; resolve any unfulfilled promises so awaiters don't hang.
  def end(): Unit =
    if !headers.ready then headers.offer(Nil)
    if !trailers.ready then trailers.offer(Nil)
    body.stop()

// A multiplexed HTTP/2 connection over a persistent `Duplex` (cleartext h2c, with
// prior knowledge — no upgrade, no TLS). A single writer daemon drains an outbound
// `Spool[Frame]` to the socket, serialising all writes (so no lock is needed); a
// reader daemon parses inbound frames and dispatches them by stream id. Must be
// created within a `supervise`-provided `Monitor`.
class Http2Connection(duplex: Duplex)(using Monitor, Probate):
  import Http2Connection.*

  private val streams: scc.TrieMap[Int, Http2Stream] = scc.TrieMap()
  private val nextId: juca.AtomicInteger = juca.AtomicInteger(1)
  private val outbound: Relay[Frame] = Relay()
  private val started: Promise[Unit] = Promise()

  private def send(frame: Frame): Unit = outbound.put(frame)


  // Tear the connection down after an unrecoverable reader/writer failure: unblock a
  // pending handshake, end every open stream so awaiters of its headers/body/trailers
  // don't hang on a connection that can no longer make progress, and stop the outbound
  // spool so the writer exits.
  private def tearDown(): Unit =
    started.cancel()
    streams.values.foreach(_.end())
    outbound.stop()

  // The writer drains the outbound spool to the socket, serialising all writes (so no
  // lock is needed); the reader decodes inbound frames and dispatches them until the
  // socket ends, a GOAWAY arrives, or a protocol error occurs. Both run under a `trap`
  // that tears the connection down on failure, so a write, parse or HPACK error is
  // isolated to this connection — it neither escalates nor leaves a request awaiter
  // hanging — rather than being swallowed or escaping the daemon.
  private val (writer, reader): (Daemon, Daemon) =
    contain:
      case _ => tearDown(); Remedy.Accept

    . protect:
        // Everything the fibers touch is bound to locals (or neutral carriers)
        // before they spawn: a daemon body may not capture the instance under
        // construction, and its context function must stay pure.
        val duplex0: Duplex = duplex
        val outbound0: Relay[Frame] = outbound
        val self: AnyRef = this.asInstanceOf[AnyRef]

        val writer = daemon:
          duplex0.send(zephyrine.Stream(connectionPreface))

          outbound0.stream.records.each: frame =>
            duplex0.send(zephyrine.Stream(frame.serialize))

        val frameReaderRef: AnyRef = FrameReader(duplex0.source).asInstanceOf[AnyRef]

        val reader = daemon:
          // A protocol error tears down just this connection; throw it to the enclosing
          // `contain`, which runs `tearDown()` and stops the reader.
          given Tactic[Http2Error] = AsyncTactic()

          val frameReader = frameReaderRef.asInstanceOf[FrameReader^]
          val decoder = Hpack()
          var continue = true

          while continue do (frameReader.next(): @unchecked) match
            case Unset        => continue = false
            case frame: Frame =>
              continue = dispatch(self.asInstanceOf[Http2Connection], frame, decoder)

        (writer, reader)

  // Perform the connection handshake: emit our SETTINGS and await the peer's.
  // Plain using-parameters, de-sugared from `raises`: a context-function result may
  // not hide `this`.
  def start()(using Tactic[AsyncError]): Unit =
    send(Frame.Settings(initialSettings, ack = false))
    started.await()

  // Open a new client stream, send its header block (and optional body), and return
  // the stream handle whose promises/spool the reader will populate.
  def request(headerBlock: List[HpackEntry], body: Optional[Bytes]): Http2Stream =
    val id = nextId.getAndAdd(2)
    val stream = Http2Stream(id)
    streams(id) = stream
    val encoder = Hpack()
    val noBody = body.absent

    send(Frame.Headers(id, encoder.encode(headerBlock), endStream = noBody, endHeaders = true))

    body.let: payload =>
      send(Frame.Data(id, payload, endStream = true))

    stream

  // Issue a telekinesis `Http.Request` over this connection and return the
  // `Http.Response`, blocking only until the response HEADERS arrive; the body
  // streams lazily from the stream's spool. `scheme`/`authority` supply the
  // pseudo-headers the request type doesn't carry. Trailers (e.g. gRPC status) are
  // available afterwards via `stream.trailers`.
  def fetch(request: Http.Request, scheme: Text, authority: Text)
    ( using Tactic[Http2Error], Tactic[AsyncError] )
  :   (Http2Stream, Http.Response) =

    Log.fine(Http2Event.RequestSent(authority))
    val headerBlock = PseudoHeaders.request(request, scheme, authority)
    val data = request.body().memoize

    val payload: Optional[Bytes] = if data.isEmpty then Unset else data

    val stream = this.request(headerBlock, payload)
    val responseHeaders = stream.headers.await()

    (stream, PseudoHeaders.response(responseHeaders, LazyList.from(stream.body.stream.records)))

  def close(): Unit =
    send(Frame.GoAway(0, ErrorCode.NoError.code, IArray.empty[Byte]))
    outbound.stop()
    reader.cancel()
    writer.cancel()
    duplex.close()

