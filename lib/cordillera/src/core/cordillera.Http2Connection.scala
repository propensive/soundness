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
package cordillera

import java.util.concurrent.atomic as juca

import scala.collection.concurrent as scc

import anticipation.{Data as Bytes, *}
import coaxial.*
import contingency.*
import gossamer.*
import hieroglyph.*, charEncoders.ascii
import parasite.*
import proscenium.*
import rudiments.*
import telekinesis.*
import turbulence.*
import vacuous.*

import Http2.*

object Http2Connection:
  // The client connection preface (RFC 7540 §3.5): a fixed octet sequence that
  // precedes the first SETTINGS frame in prior-knowledge h2c.
  private[cordillera] val connectionPreface: Bytes = t"PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n".data

  // Our advertised SETTINGS: disable server push; a generous stream window.
  private[cordillera] val initialSettings: List[Setting] =
    List
      ( Setting(SettingId.EnablePush.id, 0),
        Setting(SettingId.InitialWindowSize.id, 0x7fffffff) )

// One outbound request stream's receive side: a promise for its response header
// block (resolved on the first HEADERS frame), a spool feeding the response body
// (fed by DATA frames), and a promise for trailers (resolved on a second, end-stream
// HEADERS frame — gRPC's status). The reader daemon populates these; the caller
// awaits `headers` and consumes `body.stream`.
class Http2Stream(val id: Int):
  val headers: Promise[List[HpackEntry]] = Promise()
  val trailers: Promise[List[HpackEntry]] = Promise()
  val body: Spool[Bytes] = Spool()
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
  private val outbound: Spool[Frame] = Spool()
  private val started: Promise[Unit] = Promise()

  private def send(frame: Frame): Unit = outbound.put(frame)

  // Dispatch one decoded frame. Separated out so the read loop's `Tactic[Http2Error]`
  // (for HPACK decoding) is supplied in one place.
  private def dispatch(frame: Frame, decoder: Hpack): Boolean raises Http2Error =
    frame match
      case Frame.Settings(_, ack) =>
        if !ack then
          send(Frame.Settings(Nil, ack = true))
          started.offer(())

        true

      case Frame.Ping(opaque, ack) =>
        if !ack then send(Frame.Ping(opaque, ack = true))
        true

      case Frame.GoAway(_, _, _) =>
        false

      case Frame.Headers(id, block, endStream, _) =>
        streams.get(id).foreach: stream =>
          stream.acceptHeaders(decoder.decode(block))

          if endStream then
            stream.end()
            streams.remove(id)

        true

      case Frame.Data(id, payload, endStream) =>
        streams.get(id).foreach: stream =>
          stream.acceptData(payload)
          // Replenish the peer's flow-control window for what we consumed.
          if payload.length > 0 then
            send(Frame.WindowUpdate(0, payload.length))
            send(Frame.WindowUpdate(id, payload.length))

          if endStream then
            stream.end()
            streams.remove(id)

        true

      case Frame.RstStream(id, _) =>
        streams.get(id).foreach: stream =>
          stream.end()
          streams.remove(id)

        true

      case Frame.WindowUpdate(_, _) | Frame.Continuation(_, _, _) =>
        true

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
    trap:
      case _ => tearDown(); Remedy.Accept

    . within:
        val writer = daemon:
          duplex.send(Stream(connectionPreface))

          outbound.stream.each: frame =>
            duplex.send(Stream(frame.serialize))

        val reader = daemon:
          val frameReader = FrameReader(duplex.stream.iterator)
          val decoder = Hpack()
          var continue = true

          while continue do frameReader.next() match
            case Unset        => continue = false
            case frame: Frame => continue = dispatch(frame, decoder)

        (writer, reader)

  // Perform the connection handshake: emit our SETTINGS and await the peer's.
  def start(): Unit raises AsyncError =
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
  :   (Http2Stream, Http.Response) raises Http2Error raises AsyncError =

    val headerBlock = PseudoHeaders.request(request, scheme, authority)
    val chunks = request.body().to(List)

    val payload: Optional[Bytes] =
      if chunks.isEmpty then Unset else chunks.foldLeft(IArray.empty[Byte])(_ ++ _)

    val stream = this.request(headerBlock, payload)
    val responseHeaders = stream.headers.await()

    (stream, PseudoHeaders.response(responseHeaders, stream.body.stream))

  def close(): Unit =
    send(Frame.GoAway(0, ErrorCode.NoError.code, IArray.empty[Byte]))
    outbound.stop()
    reader.cancel()
    writer.cancel()
    duplex.close()

