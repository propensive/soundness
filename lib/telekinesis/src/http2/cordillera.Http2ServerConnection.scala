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
package cordillera

import scala.collection.concurrent as scc

import anticipation.{Data as Bytes, *}
import coaxial.*
import contingency.*
import gossamer.*
import parasite.*
import prepositional.*
import proscenium.*
import rudiments.*
import turbulence.*
import vacuous.*
import zephyrine.*

import Http2.*

object Http2ServerConnection:
  // Our advertised SETTINGS: a generous stream window. (`EnablePush` is a
  // client-only setting, so the server sends only the window.)
  private val serverSettings: List[Setting] =
    List(Setting(SettingId.InitialWindowSize.id, 0x7fffffff))

  // Dispatch one decoded frame, in the server role: the peer is a client, so a
  // HEADERS frame for an unknown (client-initiated, odd) stream id CREATES the
  // stream — its header block is the request head — and announces it on the
  // `accepted` relay for the serve loop to handle; a second HEADERS block on a
  // known stream carries request trailers. Lives on the companion — taking the
  // connection as a plain parameter — so the reader daemon's body stays free of
  // `this` captures. Returns false to stop the reader.
  private def dispatch(conn: Http2ServerConnection, frame: Frame, decoder: Hpack)
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
        conn.streams.get(id) match
          case Some(stream) =>
            // A second HEADERS block on a live stream: request trailers.
            stream.acceptHeaders(decoder.decode(block))

            if endStream then
              stream.end()
              conn.streams.remove(id)

          case None =>
            val stream = Http2Stream(id)
            conn.streams(id) = stream
            stream.acceptHeaders(decoder.decode(block))
            if endStream then stream.end() else ()
            conn.accepted.put(stream)

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

// The server role of a multiplexed HTTP/2 connection over a persistent `Duplex`.
// The reader daemon first validates the 24-byte client connection preface, then
// parses inbound frames, creating a stream per client-initiated HEADERS block
// and announcing it on `accepted`; the writer daemon drains the outbound spool.
// The caller consumes `accepted` (one handler per stream, on its own virtual
// thread) and writes responses back with `sendHeaders`/`sendData`, which may be
// called concurrently for different streams — frames interleave by design, and
// each header block is encoded with a fresh (always-literal) HPACK encoder, so
// no encoder state is shared. Must be created within a `supervise`-provided
// `Monitor`.
class Http2ServerConnection(duplex: Duplex^)(using Monitor, Probate):
  import Http2ServerConnection.*

  // A socket-backed `Duplex` captures its I/O capabilities, so it crosses into
  // the reader/writer daemons (and reaches `close`) as a neutral `AnyRef` rim.
  private val duplexRef: AnyRef = duplex.asInstanceOf[AnyRef]

  private[cordillera] val streams: scc.TrieMap[Int, Http2Stream] = scc.TrieMap()
  private val outbound: Relay[Frame] = Relay()
  private[cordillera] val started: Promise[Unit] = Promise()

  // Streams opened by the client, in arrival order; the serve loop takes each
  // and runs its handler. Stopped when the connection ends.
  private[cordillera] val accepted: Relay[Http2Stream] = Relay()

  private[cordillera] def send(frame: Frame): Unit = outbound.put(frame)

  // Tear the connection down after an unrecoverable reader/writer failure or a
  // bad preface: unblock a pending handshake, end every open stream, and stop
  // the spools so the writer and the serve loop exit.
  private def tearDown(): Unit =
    started.cancel()
    streams.values.foreach(_.end())
    outbound.stop()
    accepted.stop()

  private val (writer, reader) =
    contain:
      case _ => tearDown(); Remedy.Accept

    . protect:
        // Everything the fibers touch is bound to locals (or neutral carriers)
        // before they spawn: a daemon body may not capture the instance under
        // construction, and its context function must stay pure.
        val duplex0Ref: AnyRef = duplexRef
        val outbound0: Relay[Frame] = outbound
        val self: AnyRef = this.asInstanceOf[AnyRef]

        val writer = daemon:
          val duplex0 = duplex0Ref.asInstanceOf[Duplex^]

          outbound0.stream.records.each: frame =>
            duplex0.send(zephyrine.Stream(frame.serialize))

        val frameReaderRef: AnyRef =
          FrameReader(duplexRef.asInstanceOf[Duplex^].source).asInstanceOf[AnyRef]

        val reader = daemon:
          // A protocol error tears down just this connection; throw it to the
          // enclosing `contain`, which runs `tearDown()` and stops the reader.
          given Tactic[Http2Error] = AsyncTactic()

          val frameReader = frameReaderRef.asInstanceOf[FrameReader^]

          // The server's first read: consume and validate the client
          // connection preface before frame-parsing.
          frameReader.expectPreface(Http2Connection.connectionPreface)

          val decoder = Hpack()
          var continue = true

          while continue do (frameReader.next(): @unchecked) match
            case Unset        => continue = false
            case frame: Frame =>
              continue = dispatch(self.asInstanceOf[Http2ServerConnection], frame, decoder)

          self.asInstanceOf[Http2ServerConnection].accepted.stop()

        (writer, reader)

  // Perform the server side of the connection handshake: emit our SETTINGS and
  // await the client's (which the dispatch acks). Plain using-parameters,
  // de-sugared from `raises`: a context-function result may not hide `this`.
  def start()(using Tactic[AsyncError]): Unit =
    send(Frame.Settings(serverSettings, ack = false))
    started.await()

  // Run `handler` for each client-initiated stream as it arrives, on the
  // reader-driven serve loop; returns when the connection ends. The handler
  // typically spawns a per-stream fiber so requests multiplexed on the one
  // connection are served concurrently.
  def eachStream(handler: Http2Stream => Unit): Unit =
    accepted.stream.records.each(handler)

  // Send a response header block on `streamId`, encoded with a fresh
  // (always-literal) HPACK encoder. `endStream` marks a bodiless response.
  def sendHeaders(streamId: Int, entries: List[HpackEntry], endStream: Boolean): Unit =
    val encoder = Hpack()
    send(Frame.Headers(streamId, encoder.encode(entries), endStream, endHeaders = true))

  // Send one DATA frame on `streamId`; `endStream` closes the response.
  def sendData(streamId: Int, payload: Bytes, endStream: Boolean): Unit =
    send(Frame.Data(streamId, payload, endStream))

  // Send a trailing HEADERS block (always end-stream) on `streamId` — the
  // response trailers, e.g. gRPC's `grpc-status`. A response with trailers must
  // leave `endStream` unset on its HEADERS and DATA, so this block closes the
  // stream. Encoded with a fresh (always-literal) HPACK encoder.
  def sendTrailers(streamId: Int, entries: List[HpackEntry]): Unit =
    val encoder = Hpack()
    send(Frame.Headers(streamId, encoder.encode(entries), endStream = true, endHeaders = true))

  def close(): Unit =
    send(Frame.GoAway(0, ErrorCode.NoError.code, IArray.empty[Byte]))
    outbound.stop()
    accepted.stop()
    reader.cancel()
    writer.cancel()
    duplexRef.asInstanceOf[Duplex^].close()
