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

import scala.collection.mutable as scm

import anticipation.{Data as Bytes, *}
import coaxial.*
import contingency.*
import fulminate.*
import gossamer.*
import parasite.*
import prepositional.*
import rudiments.*
import spectacular.*
import telekinesis.*
import vacuous.*

import Http2Error.Reason

// The HTTP/2 wire vocabulary (RFC 7540): frames, frame types, flag bits, error
// codes and settings. Grouped under `Http2` so its deliberately generic names —
// `Frame`, `Flags`, `Setting`, `ErrorCode` — don't crowd the top-level namespace,
// mirroring how `telekinesis` groups its types under `Http`.
object Http2:
  object FrameType:
    def fromId(id: Int): Optional[FrameType] = id match
      case 0x0 => FrameType.Data
      case 0x1 => FrameType.Headers
      case 0x3 => FrameType.RstStream
      case 0x4 => FrameType.Settings
      case 0x6 => FrameType.Ping
      case 0x7 => FrameType.GoAway
      case 0x8 => FrameType.WindowUpdate
      case 0x9 => FrameType.Continuation
      case _   => Unset

  // The frame types this client handles (RFC 7540 §6). PushPromise (5) is not
  // supported (the client disables server push) and Priority (2) is parsed only to
  // be ignored, so neither has a dedicated decode path.
  enum FrameType:
    case Data, Headers, RstStream, Settings, Ping, GoAway, WindowUpdate, Continuation

    def id: Int = this match
      case Data         => 0x0
      case Headers      => 0x1
      case RstStream    => 0x3
      case Settings     => 0x4
      case Ping         => 0x6
      case GoAway       => 0x7
      case WindowUpdate => 0x8
      case Continuation => 0x9

  // The frame flag bits used by the supported frame types (RFC 7540 §6). Tested with
  // `Flags.set(flags, Flags.EndStream)`.
  object Flags:
    val EndStream: Int = 0x1   // DATA, HEADERS
    val Ack: Int = 0x1         // SETTINGS, PING (same bit, different frames)
    val EndHeaders: Int = 0x4  // HEADERS, CONTINUATION
    val Padded: Int = 0x8      // DATA, HEADERS
    val Priority: Int = 0x20   // HEADERS

    def set(flags: Int, flag: Int): Boolean = (flags & flag) != 0

  // HTTP/2 error codes (RFC 7540 §7), carried by RST_STREAM and GOAWAY.
  enum ErrorCode(val code: Long):
    case NoError              extends ErrorCode(0x0)
    case ProtocolError        extends ErrorCode(0x1)
    case InternalError        extends ErrorCode(0x2)
    case FlowControlError     extends ErrorCode(0x3)
    case SettingsTimeout      extends ErrorCode(0x4)
    case StreamClosed         extends ErrorCode(0x5)
    case FrameSizeError       extends ErrorCode(0x6)
    case RefusedStream        extends ErrorCode(0x7)
    case Cancel               extends ErrorCode(0x8)
    case CompressionError     extends ErrorCode(0x9)
    case ConnectError         extends ErrorCode(0xa)
    case EnhanceYourCalm      extends ErrorCode(0xb)
    case InadequateSecurity   extends ErrorCode(0xc)
    case Http11Required       extends ErrorCode(0xd)

  object SettingId:
    def fromId(id: Int): Optional[SettingId] = id match
      case 0x1 => SettingId.HeaderTableSize
      case 0x2 => SettingId.EnablePush
      case 0x3 => SettingId.MaxConcurrentStreams
      case 0x4 => SettingId.InitialWindowSize
      case 0x5 => SettingId.MaxFrameSize
      case 0x6 => SettingId.MaxHeaderListSize
      case _   => Unset

  // An HTTP/2 SETTINGS parameter identifier (RFC 7540 §6.5.2). Unknown identifiers
  // must be ignored, so `Setting` keeps the raw id rather than rejecting it.
  enum SettingId(val id: Int):
    case HeaderTableSize      extends SettingId(0x1)
    case EnablePush           extends SettingId(0x2)
    case MaxConcurrentStreams extends SettingId(0x3)
    case InitialWindowSize    extends SettingId(0x4)
    case MaxFrameSize         extends SettingId(0x5)
    case MaxHeaderListSize    extends SettingId(0x6)

  // A 16-bit setting identifier paired with its 32-bit value.
  case class Setting(id: Int, value: Long)

  object Frame:
    private[cordillera] def uint24(data: Bytes, offset: Int): Int =
      ((data(offset) & 0xff) << 16) | ((data(offset + 1) & 0xff) << 8) | (data(offset + 2) & 0xff)

    private[cordillera] def uint32(data: Bytes, offset: Int): Long =
      ((data(offset).toLong & 0xff) << 24) | ((data(offset + 1).toLong & 0xff) << 16) |
        ((data(offset + 2).toLong & 0xff) << 8) | (data(offset + 3).toLong & 0xff)

    private def writeUint24(buf: ByteBuf^, value: Int): Unit =
      buf.add(((value >>> 16) & 0xff).toByte)
      buf.add(((value >>> 8) & 0xff).toByte)
      buf.add((value & 0xff).toByte)

    private def writeUint32(buf: ByteBuf^, value: Long): Unit =
      buf.add(((value >>> 24) & 0xff).toByte)
      buf.add(((value >>> 16) & 0xff).toByte)
      buf.add(((value >>> 8) & 0xff).toByte)
      buf.add((value & 0xff).toByte)

    // Decodes a single frame from `data` starting at `offset`; returns the frame and
    // the offset just past it. `data` must already contain the whole frame.
    def decode(data: Bytes, offset: Int): (Frame, Int) raises Http2Error =
      if offset + 9 > data.length then abort(Http2Error(Reason.Truncated))
      val length = uint24(data, offset)
      val typeId = data(offset + 3) & 0xff
      val flags = data(offset + 4) & 0xff
      val streamId = (uint32(data, offset + 5) & 0x7fffffffL).toInt
      val start = offset + 9
      val end = start + length

      if end > data.length then abort(Http2Error(Reason.Truncated))
      val body: anticipation.Data = data.slice(start, end)

      // An unrecognised frame type — PRIORITY or PUSH_PROMISE (which this stack
      // does not model), or an extension frame — must be ignored, not treated as
      // a connection error (RFC 7540 §4.1, §5.5): clients routinely send
      // PRIORITY, so aborting here would kill real connections.
      val frame = (FrameType.fromId(typeId): @unchecked) match
        case Unset => Frame.Ignored(typeId, streamId)

        case FrameType.Data =>
          Frame.Data(streamId, stripPadding(body, flags), Flags.set(flags, Flags.EndStream))

        case FrameType.Headers =>
          val unpadded = stripPadding(body, flags)
          // A PRIORITY prefix (5 bytes: 4-byte dependency + 1-byte weight) precedes
          // the header block when the PRIORITY flag is set; skip it.
          val block: anticipation.Data =
            if Flags.set(flags, Flags.Priority)
            then unpadded.slice(5, unpadded.length)
            else unpadded

          Frame.Headers
            ( streamId,
              block,
              Flags.set(flags, Flags.EndStream),
              Flags.set(flags, Flags.EndHeaders) )

        case FrameType.Continuation =>
          Frame.Continuation(streamId, body, Flags.set(flags, Flags.EndHeaders))

        case FrameType.RstStream =>
          Frame.RstStream(streamId, uint32(body, 0))

        case FrameType.Settings =>
          Frame.Settings(decodeSettings(body), Flags.set(flags, Flags.Ack))

        case FrameType.Ping =>
          Frame.Ping(body, Flags.set(flags, Flags.Ack))

        case FrameType.GoAway =>
          val lastStreamId = (uint32(body, 0) & 0x7fffffffL).toInt
          Frame.GoAway
            ( lastStreamId,
              uint32(body, 4),
              body.slice(8, body.length) )

        case FrameType.WindowUpdate =>
          Frame.WindowUpdate(streamId, (uint32(body, 0) & 0x7fffffffL).toInt)

      (frame, end)

    // DATA/HEADERS may carry a 1-byte pad length followed by that many trailing pad
    // bytes (RFC 7540 §6.1/§6.2); strip both when the PADDED flag is set.
    private def stripPadding(payload: Bytes, flags: Int): Bytes raises Http2Error =
      if !Flags.set(flags, Flags.Padded) then payload else
        if payload.length < 1 then abort(Http2Error(Reason.Truncated))
        val padLength = payload(0) & 0xff
        if 1 + padLength > payload.length then abort(Http2Error(Reason.Protocol(t"bad padding")))
        payload.slice(1, payload.length - padLength)

    private def decodeSettings(payload: Bytes): List[Setting] raises Http2Error =
      if payload.length%6 != 0 then abort(Http2Error(Reason.Protocol(t"bad SETTINGS length")))
      val builder = List.newBuilder[Setting]
      var i = 0

      while i < payload.length do
        val id = ((payload(i) & 0xff) << 8) | (payload(i + 1) & 0xff)
        builder += Setting(id, uint32(payload, i + 2))
        i += 6

      builder.result()

    private def frameType(frame: Frame): FrameType = frame match
      case _: Frame.Headers      => FrameType.Headers
      case _: Frame.Continuation => FrameType.Continuation
      case _: Frame.Data         => FrameType.Data
      case _: Frame.RstStream    => FrameType.RstStream
      case _: Frame.Settings     => FrameType.Settings
      case _: Frame.Ping         => FrameType.Ping
      case _: Frame.GoAway       => FrameType.GoAway
      case _: Frame.WindowUpdate => FrameType.WindowUpdate
      case _: Frame.Ignored      => panic(m"an ignored frame cannot be serialized")

    private def frameFlags(frame: Frame): Int = frame match
      case Frame.Headers(_, _, endStream, endHeaders) =>
        (if endStream then Flags.EndStream else 0) | (if endHeaders then Flags.EndHeaders else 0)

      case Frame.Continuation(_, _, endHeaders) => if endHeaders then Flags.EndHeaders else 0
      case Frame.Data(_, _, endStream)          => if endStream then Flags.EndStream else 0
      case Frame.Settings(_, ack)               => if ack then Flags.Ack else 0
      case Frame.Ping(_, ack)                   => if ack then Flags.Ack else 0
      case _                                    => 0

    private def frameBuilder(lambda: ByteBuf^ => Unit): Bytes =
      val buf: ByteBuf^ = ByteBuf()
      lambda(buf)
      buf.data

    private[cordillera] def serializeFrame(frame: Frame): Bytes =
      val body = payload(frame)
      val buf: ByteBuf^ = ByteBuf(9 + body.length)
      writeUint24(buf, body.length)
      buf.add(frameType(frame).id.toByte)
      buf.add(frameFlags(frame).toByte)
      writeUint32(buf, frame.stream.toLong & 0x7fffffffL)
      buf.addAll(body)
      buf.data

    private def payload(frame: Frame): Bytes = frame match
      case Frame.Headers(_, block, _, _)   => block
      case Frame.Continuation(_, block, _) => block
      case Frame.Data(_, payload, _)       => payload
      case Frame.Ping(opaque, _)           => opaque
      case Frame.RstStream(_, errorCode)   => frameBuilder(writeUint32(_, errorCode))
      case Frame.Ignored(_, _)             => panic(m"an ignored frame cannot be serialized")

      case Frame.WindowUpdate(_, increment) =>
        frameBuilder(writeUint32(_, increment.toLong & 0x7fffffffL))

      case Frame.Settings(settings, _) =>
        frameBuilder: buf =>
          // A while-loop rather than `each`: the closure may not capture the
          // exclusive buffer.
          var rest = settings

          while !rest.isEmpty do
            val setting = rest.head
            buf.add(((setting.id >>> 8) & 0xff).toByte)
            buf.add((setting.id & 0xff).toByte)
            writeUint32(buf, setting.value)
            rest = rest.tail

      case Frame.GoAway(lastStreamId, errorCode, debug) =>
        frameBuilder: buf =>
          writeUint32(buf, lastStreamId.toLong & 0x7fffffffL)
          writeUint32(buf, errorCode)
          buf.addAll(debug)

  // An HTTP/2 frame (RFC 7540 §6): a 9-byte header — 24-bit length, 8-bit type, 8-bit
  // flags, 1 reserved bit + 31-bit stream id — followed by a type-specific payload.
  // Models the frame types a cleartext-h2c client needs; PUSH_PROMISE is unsupported
  // and PRIORITY is not represented (its bits are skipped where they appear on
  // HEADERS). The byte type is aliased `Bytes` so the `Data` case does not shadow
  // `anticipation.Data`.
  enum Frame:
    // `endStream` closes the sending half; `headerBlock` is the (already
    // padding-stripped, priority-stripped) HPACK fragment.
    case Headers
      ( streamId:    Int,
        headerBlock: Bytes,
        endStream:   Boolean,
        endHeaders:  Boolean )

    // A continuation of a header block too large for one HEADERS frame.
    case Continuation(streamId: Int, headerBlock: Bytes, endHeaders: Boolean)

    case Data(streamId: Int, payload: Bytes, endStream: Boolean)
    case RstStream(streamId: Int, errorCode: Long)
    case Settings(settings: List[Setting], ack: Boolean)
    case Ping(opaque: Bytes, ack: Boolean)
    case GoAway(lastStreamId: Int, errorCode: Long, debug: Bytes)
    case WindowUpdate(streamId: Int, increment: Int)

    // An inbound frame of a type this stack does not model — PRIORITY,
    // PUSH_PROMISE or an extension — decoded only to be discarded (RFC 7540
    // §4.1, §5.5). Never serialised.
    case Ignored(typeId: Int, streamId: Int)

    // The stream this frame belongs to; connection-level frames (SETTINGS, PING,
    // GOAWAY) use stream 0.
    def stream: Int = this match
      case Headers(id, _, _, _)   => id
      case Continuation(id, _, _) => id
      case Data(id, _, _)         => id
      case RstStream(id, _)       => id
      case Settings(_, _)         => 0
      case Ping(_, _)             => 0
      case GoAway(_, _, _)        => 0
      case WindowUpdate(id, _)    => id
      case Ignored(_, id)         => id

    // Serialise the frame, including its 9-byte header.
    // Delegates to the companion: builder mutation inside an enum-class method
    // would force a `uses` clause onto the class itself.
    def serialize: Bytes = Frame.serializeFrame(this)

  // A cleartext-h2c endpoint: a connectable address plus the `:authority` to send.
  // Used as the `Target` of the HTTP/2 `HttpClient` given, distinct from the
  // `DomainSocket` target telekinesis binds to its HTTP/1.1 client.
  case class Endpoint[endpoint: {Connectable as connectable, Showable}]
    ( endpoint: endpoint, authority: Text ):
    // Establish a multiplexed connection whose underlying `Duplex` is closed when the
    // enclosing `supervise` scope ends, rather than being leaked. The connection (and
    // its reader/writer daemons) runs on a daemon that holds the `duplex` loan open —
    // parked until the scope is torn down — so the response bodies it streams lazily
    // stay readable for the scope's lifetime instead of being closed per request.
    // Returns once the HTTP/2 handshake has completed.
    // Named using-parameters, de-sugared from `raises`: the result retains the
    // connection's capabilities, so it must name its evidence rather than hide it.
    def connect()(using monitor: Monitor, probate: Probate, asyncError: Tactic[AsyncError])
    :   Http2Connection^{monitor, caps.any} =

      // A neutral carrier: the connection (a capability) crosses the daemon and the
      // promise as an `AnyRef`.
      val ready: Promise[AnyRef] = Promise()

      daemon:
        try
          endpoint.duplex: duplex =>
            val connection = Http2Connection(duplex)
            connection.start()
            ready.offer(connection.asInstanceOf[AnyRef])
            Promise[Unit]().await()

        finally if !ready.ready then ready.cancel()

      ready.await().asInstanceOf[Http2Connection^{monitor, caps.any}]

  // A scoped session on an `Http2.Endpoint`: the multiplexed connection is opened
  // and its handshake completed, then the live `Http2Connection` is lent to the
  // lambda; when the lambda ends, the connection (with its reader/writer daemons)
  // is torn down and the transport closed — no parked daemon holds the loan open,
  // because the loan and the scope coincide. `result` is quantified outside the
  // lambda, so a value borrowing the connection — a lazily-streaming response
  // body, an open `Http2Stream` — cannot escape the scope; memoized values may.
  // Concurrent `fetch`es within the scope multiplex on the one connection.
  // A named instance class rather than an anonymous given: an anonymous
  // subclass would freshen the capability types in its inferred `Session`
  // member, which then fails to match the declared refinement.
  class EndpointSessionable[endpoint: {Connectable, Showable}]
    ( using monitor:    Monitor,
            probate:    Probate,
            asyncError: Tactic[AsyncError],
            loggable:   (SocketEvent is Loggable)^ )
  extends Sessionable:
    type Self = Endpoint[endpoint]
    type Session = Http2Connection^{caps.any}

    def session[result](target: Endpoint[endpoint])(lambda: (session: Session) ?=> result)
    :   result =

      target.endpoint.duplex: duplex =>
        val connection = Http2Connection(duplex)
        connection.start()
        try lambda(using connection) finally connection.close()

  given sessionable: [endpoint: {Connectable, Showable}]
  =>  ( monitor:    Monitor,
        probate:    Probate,
        asyncError: Tactic[AsyncError],
        loggable:   (SocketEvent is Loggable)^ )
  =>  (EndpointSessionable[endpoint]^{monitor, asyncError, loggable, caps.any}) =
    EndpointSessionable[endpoint]()

  // An `HttpClient` that speaks HTTP/2 (prior-knowledge h2c) to an `Http2.Endpoint`.
  // It captures the ambient `Monitor`/`Probate` from this given's context — the
  // connection's daemons (and the scope-tied teardown) need them — so it can only be
  // summoned inside a `supervise` scope. A fresh connection is opened per request for
  // now; pooling is a later refinement.
  object Client:
    given http2: [endpoint]
    =>  ( monitor:    Monitor,
          probate:    Probate,
          http2Error: Tactic[Http2Error],
          asyncError: Tactic[AsyncError] )
    =>  ((HttpClient onto Endpoint[endpoint])^{monitor, http2Error, asyncError, caps.any}) =

      new HttpClient:
        type Target = Endpoint[endpoint]

        def request(request: Http.Request, target: Endpoint[endpoint])(using (HttpEvent is Loggable)^)
        :   Http.Response =

          target.connect().fetch(request, t"http", target.authority)(1)
