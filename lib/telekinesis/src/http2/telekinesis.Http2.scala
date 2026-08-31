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
package telekinesis

import scala.caps

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

import Http2.Error.Reason
import anticipation.*
import denominative.*
import java.util.concurrent.atomic as juca
import scala.collection.concurrent as scc
import hieroglyph.*, charEncoders.asciiEncoder
import proscenium.*
import turbulence.*
import zephyrine.*
import Http2.*

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
    private[telekinesis] def uint24(data: Bytes, offset: Int): Int =
      ((data.readUnchecked(offset) & 0xff) << 16) | ((data.readUnchecked(offset + 1) & 0xff) << 8) | (data.readUnchecked(offset + 2) & 0xff)

    private[telekinesis] def uint32(data: Bytes, offset: Int): Long =
      ((data.readUnchecked(offset).toLong & 0xff) << 24) | ((data.readUnchecked(offset + 1).toLong & 0xff) << 16) |
        ((data.readUnchecked(offset + 2).toLong & 0xff) << 8) | (data.readUnchecked(offset + 3).toLong & 0xff)

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
    def decode(data: Bytes, offset: Int): (Frame, Int) raises Http2.Error =
      if offset + 9 > data.length then abort(Http2.Error(Reason.Truncated))
      val length = uint24(data, offset)
      val typeId = data.readUnchecked(offset + 3) & 0xff
      val flags = data.readUnchecked(offset + 4) & 0xff
      val streamId = (uint32(data, offset + 5) & 0x7fffffffL).toInt
      val start = offset + 9
      val end = start + length

      if end > data.length then abort(Http2.Error(Reason.Truncated))
      val body: anticipation.Data = data.segment((start).z till (end).z)

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
            then unpadded.segment((5).z till (unpadded.length).z)
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
              body.segment((8).z till (body.length).z) )

        case FrameType.WindowUpdate =>
          Frame.WindowUpdate(streamId, (uint32(body, 0) & 0x7fffffffL).toInt)

      (frame, end)

    // DATA/HEADERS may carry a 1-byte pad length followed by that many trailing pad
    // bytes (RFC 7540 §6.1/§6.2); strip both when the PADDED flag is set.
    private def stripPadding(payload: Bytes, flags: Int): Bytes raises Http2.Error =
      if !Flags.set(flags, Flags.Padded) then payload else
        if payload.length < 1 then abort(Http2.Error(Reason.Truncated))
        val padLength = payload.readUnchecked(0) & 0xff
        if 1 + padLength > payload.length then abort(Http2.Error(Reason.Protocol(t"bad padding")))
        payload.segment((1).z till (payload.length - padLength).z)

    private def decodeSettings(payload: Bytes): List[Setting] raises Http2.Error =
      if payload.length%6 != 0 then abort(Http2.Error(Reason.Protocol(t"bad SETTINGS length")))
      val builder = scala.collection.immutable.List.newBuilder[Setting]
      var i = 0

      while i < payload.length do
        val id = ((payload.readUnchecked(i) & 0xff) << 8) | (payload.readUnchecked(i + 1) & 0xff)
        builder += Setting(id, uint32(payload, i + 2))
        i += 6

      builder.result().to(List)

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

    private[telekinesis] def serializeFrame(frame: Frame): Bytes =
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
          var rest = settings.stdlib

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
  // Used as the `Target` of the HTTP/2 `Http.Client` given, distinct from the
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
    def connect()(using monitor: Monitor, probate: Probate, asyncError: Tactic[Async.Error])
    :   Http2.Connection^{monitor, caps.any} =

      // A neutral carrier: the connection (a capability) crosses the daemon and the
      // promise as an `AnyRef`.
      val ready: Promise[AnyRef] = Promise()

      daemon:
        try
          endpoint.duplex: duplex =>
            val connection = Http2.Connection(duplex)
            connection.start()
            ready.offer(connection.asInstanceOf[AnyRef])
            Promise[Unit]().await()

        finally if !ready.ready then ready.cancel()

      ready.await().asInstanceOf[Http2.Connection^{monitor, caps.any}]

  // A scoped session on an `Http2.Endpoint`: the multiplexed connection is opened
  // and its handshake completed, then the live `Http2.Connection` is lent to the
  // lambda; when the lambda ends, the connection (with its reader/writer daemons)
  // is torn down and the transport closed — no parked daemon holds the loan open,
  // because the loan and the scope coincide. `result` is quantified outside the
  // lambda, so a value borrowing the connection — a lazily-streaming response
  // body, an open `Http2.Stream` — cannot escape the scope; memoized values may.
  // Concurrent `fetch`es within the scope multiplex on the one connection.
  // A named instance class rather than an anonymous given: an anonymous
  // subclass would freshen the capability types in its inferred `Result`
  // member, which then fails to match the declared refinement.
  class EndpointSessional[endpoint: {Connectable, Showable}]
    ( using monitor:    Monitor,
            probate:    Probate,
            asyncError: Tactic[Async.Error],
            loggable:   (Socket.Event is Loggable)^ )
  extends Sessional:
    type Self = Endpoint[endpoint]
    type Result = Http2.Connection^{caps.any}

    def session[result](target: Endpoint[endpoint])(lambda: (session: Result) ?=> result)
    :   result =

      target.endpoint.duplex: duplex =>
        val connection = Http2.Connection(duplex)
        connection.start()
        try lambda(using connection) finally connection.close()

  given sessional: [endpoint: {Connectable, Showable}]
  =>  ( monitor:    Monitor,
        probate:    Probate,
        asyncError: Tactic[Async.Error],
        loggable:   (Socket.Event is Loggable)^ )
  =>  (EndpointSessional[endpoint]^{monitor, asyncError, loggable, caps.any}) =
    EndpointSessional[endpoint]()

  // An `Http.Client` that speaks HTTP/2 (prior-knowledge h2c) to an `Http2.Endpoint`.
  // It captures the ambient `Monitor`/`Probate` from this given's context — the
  // connection's daemons (and the scope-tied teardown) need them — so it can only be
  // summoned inside a `supervise` scope. A fresh connection is opened per request for
  // now; pooling is a later refinement.
  object Client:
    given http2: [endpoint]
    =>  ( monitor:    Monitor,
          probate:    Probate,
          http2Error: Tactic[Http2.Error],
          asyncError: Tactic[Async.Error] )
    =>  ((Http.Client onto Endpoint[endpoint])^{monitor, http2Error, asyncError, caps.any}) =

      new Http.Client:
        type Target = Endpoint[endpoint]

        def request(request: Http.Request, target: Endpoint[endpoint])(using (Http.Event is Loggable)^)
        :   Http.Response =

          target.connect().fetch(request, t"http", target.authority)(1)

  // Http2Error → Http2.Error
  object Error:
    object Reason:
      given communicable: Reason is Communicable =
        case Truncated         => m"the HTTP/2 data ended unexpectedly"
        case BadPreface        => m"the connection preface was not valid"
        case BadHuffman        => m"the Huffman-coded string was not valid"
        case BadInteger        => m"an HPACK integer was malformed or too large"
        case BadIndex(index)   => m"the HPACK table index $index was out of range"
        case BadFrameType(id)  => m"the frame type $id was not recognized"
        case FrameTooLarge     => m"a frame exceeded the maximum permitted size"
        case FlowControl       => m"a flow-control window was exceeded"
        case Protocol(message) => m"a protocol error occurred: $message"
        case GoAway(code)      => m"the peer closed the connection with error code $code"

    enum Reason:
      case Truncated
      case BadPreface
      case BadHuffman
      case BadInteger
      case BadIndex(index: Int)
      case BadFrameType(id: Int)
      case FrameTooLarge
      case FlowControl
      case Protocol(message: Text)
      case GoAway(code: Long)

  case class Error(reason: Http2.Error.Reason)(using Diagnostics)
  extends fulminate.Error(m"the HTTP/2 operation failed because $reason")

  // Http2Event → Http2.Event
  object Event:
    given communicable: Http2.Event is Communicable =
      case RequestSent(authority) => m"sending an HTTP/2 request to $authority"
      case GoAway(lastStream)     => m"received GOAWAY; the last processed stream was $lastStream"

  enum Event:
    case RequestSent(authority: Text) extends Http2.Event, Log.Network, Log.Protocol
    case GoAway(lastStream: Int) extends Http2.Event, Log.Network, Log.Protocol

  // Http2Connection -> Http2.Connection
  object Connection:
    // The client connection preface (RFC 7540 §3.5): a fixed octet sequence that
    // precedes the first SETTINGS frame in prior-knowledge h2c.
    private[telekinesis] val connectionPreface: Bytes = t"PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n".in[Bytes]

    // The HTTP/2 default flow-control window (RFC 7540 §6.9.2): the initial
    // send budget for the connection, and for a stream until SETTINGS say
    // otherwise.
    private[telekinesis] val defaultWindow: Int = 65535

    // The HTTP/2 default maximum frame size (RFC 7540 §6.5.2): the largest
    // DATA payload we may send until the peer's SETTINGS raise it.
    private[telekinesis] val defaultMaxFrame: Int = 16384

    // Our advertised receive budget, per stream and (via an initial
    // connection-level WINDOW_UPDATE) per connection: the peer may have at most
    // this many unconsumed bytes in flight, which is what bounds the inbound
    // body relays — they are unbounded structures bounded by protocol credit.
    // Replenishment is consumption-driven and batched: a WINDOW_UPDATE goes out
    // once half the budget is pending, so a stalled consumer stalls the peer's
    // sender rather than accumulating its output.
    private[telekinesis] val receiveWindow: Int = 1024*1024

    // Our advertised SETTINGS: disable server push; our receive-side stream
    // window.
    private[telekinesis] def initialSettings(window: Int): List[Setting] =
      List
        ( Setting(SettingId.EnablePush.id, 0),
          Setting(SettingId.InitialWindowSize.id, window) )

    // Dispatch one decoded frame. Lives on the companion — taking the connection as a
    // plain parameter — so the reader daemon's body stays free of `this` captures.
    // The tactic is a plain using-parameter: a context-function result may not hide it.
    private def dispatch(conn: Http2.Connection, frame: Frame, decoder: Hpack)
      ( using Tactic[Http2.Error] )
    :   Boolean =
      frame match
        case Frame.Settings(settings, ack) =>
          if !ack then
            // Adopt the peer's advertised initial per-stream send window and
            // maximum frame size, which bound our request-body DATA frames.
            settings.seek(_.id == SettingId.InitialWindowSize.id).let: setting =>
              conn.peerInitialWindow.set(setting.value.toInt)

            settings.seek(_.id == SettingId.MaxFrameSize.id).let: setting =>
              conn.peerMaxFrame.set(setting.value.toInt)

            conn.send(Frame.Settings(Nil, ack = true))
            conn.started.offer(())

          true

        case Frame.Ping(opaque, ack) =>
          if !ack then conn.send(Frame.Ping(opaque, ack = true))
          true

        case Frame.GoAway(lastStreamId, _, _) =>
          Log.warn(Http2.Event.GoAway(lastStreamId))
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
            // No replenishment here: the peer's window refills only as the
            // application drains the body (see `Stream.Body`), so an unread
            // body backpressures the peer at the advertised window.
            stream.acceptData(payload)

            if endStream then
              stream.end()
              conn.streams.remove(id)

          true

        case Frame.RstStream(id, _) =>
          conn.streams.get(id).foreach: stream =>
            stream.end()
            conn.streams.remove(id)

          conn.streamWindows.remove(id)
          true

        // An inbound WINDOW_UPDATE grants send credit: on stream 0 it tops up
        // the connection window, otherwise the named stream's window (RFC 7540
        // §6.9) — releasing a request-body upload parked at exhaustion.
        case Frame.WindowUpdate(id, increment) =>
          if id == 0 then conn.connWindow.release(increment)
          else conn.streamWindows.get(id).foreach(_.release(increment))

          true

        case Frame.Continuation(_, _, _) | Frame.Ignored(_, _) =>
          true

  // One outbound request stream's receive side: a promise for its response header
  // block (resolved on the first HEADERS frame), a spool feeding the response body
  // (fed by DATA frames), and a promise for trailers (resolved on a second, end-stream
  // HEADERS frame — gRPC's status). The reader daemon populates these; the caller
  // awaits `headers` and consumes `body.stream`.
  // A multiplexed HTTP/2 connection over a persistent `Duplex` (cleartext h2c, with
  // prior knowledge — no upgrade, no TLS). A single writer daemon drains an outbound
  // `Spool[Frame]` to the socket, serialising all writes (so no lock is needed); a
  // reader daemon parses inbound frames and dispatches them by stream id. Must be
  // created within a `supervise`-provided `Monitor`.
  class Connection(duplex: Duplex, window: Int = Connection.receiveWindow)(using Monitor, Probate):
    import Http2.Connection.*

    private val streams: scc.TrieMap[Int, Http2.Stream] = scc.TrieMap()
    private val nextId: juca.AtomicInteger = juca.AtomicInteger(1)
    private val outbound: Relay[Frame] = Relay()
    private val started: Promise[Unit] = Promise()

    // Consumed-but-unreplenished inbound bytes at connection level, and the
    // batching threshold: one WINDOW_UPDATE per half-window consumed, not one
    // per DATA frame.
    private val connPending: juca.AtomicInteger = juca.AtomicInteger(0)
    private val threshold: Int = (window/2).max(1)

    // Send-side flow control (RFC 7540 §6.9), as the server role: the
    // connection window, per-stream windows created lazily at the peer's
    // advertised initial size, and the peer's advertised limits from SETTINGS.
    private[telekinesis] val connWindow: FlowWindow = FlowWindow(defaultWindow)
    private[telekinesis] val streamWindows: scc.TrieMap[Int, FlowWindow] = scc.TrieMap()

    private[telekinesis] val peerInitialWindow: juca.AtomicInteger =
      juca.AtomicInteger(defaultWindow)

    private[telekinesis] val peerMaxFrame: juca.AtomicInteger =
      juca.AtomicInteger(defaultMaxFrame)

    private def send(frame: Frame): Unit = outbound.put(frame)

    // Consumption-driven replenishment: the body's accounted stream reports
    // each drained record's bytes, which accumulate per stream and per
    // connection until the threshold releases them as WINDOW_UPDATEs.
    private[telekinesis] def consumed(stream: Http2.Stream, count: Int): Unit =
      stream.unreplenished.addAndGet(count)
      connPending.addAndGet(count)
      replenish(stream.unreplenished, stream.id)
      replenish(connPending, 0)

    // CAS-drain: emit one WINDOW_UPDATE for everything pending once the
    // threshold is crossed; concurrent consumers of different streams race
    // safely on the connection counter.
    private def replenish(pending: juca.AtomicInteger, id: Int): Unit =
      var continue = true

      while continue do
        val value = pending.get
        if value < threshold then continue = false
        else if pending.compareAndSet(value, 0) then
          send(Frame.WindowUpdate(id, value))
          continue = false

    // Tear the connection down after an unrecoverable reader/writer failure: unblock a
    // pending handshake, end every open stream so awaiters of its headers/body/trailers
    // don't hang on a connection that can no longer make progress, and stop the outbound
    // spool so the writer exits.
    private def tearDown(): Unit =
      started.cancel()
      streams.values.each(_.end())
      outbound.stop()

    // The writer drains the outbound spool to the socket, serialising all writes (so no
    // lock is needed); the reader decodes inbound frames and dispatches them until the
    // socket ends, a GOAWAY arrives, or a protocol error occurs. Both run under a `trap`
    // that tears the connection down on failure, so a write, parse or HPACK error is
    // isolated to this connection — it neither escalates nor leaves a request awaiter
    // hanging — rather than being swallowed or escaping the daemon.
    private val (writer, reader): (Daemon, Daemon) =
      // The containment and its protected body share only this connection's own state; no
      // aliased writer.
      scala.caps.unsafe.unsafeAssumeSeparate:
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

            outbound0.stream.records.each: frame => duplex0.send(zephyrine.Stream(frame.serialize))

          val frameReaderRef: AnyRef = FrameReader(duplex0.source).asInstanceOf[AnyRef]

          val reader = daemon:
            // A protocol error tears down just this connection; throw it to the enclosing
            // `contain`, which runs `tearDown()` and stops the reader.
            given Tactic[Http2.Error] = AsyncTactic()

            val frameReader = frameReaderRef.asInstanceOf[FrameReader^]
            val decoder = Hpack()
            var continue = true

            while continue do (frameReader.next(): @unchecked) match
              case Unset        => continue = false
              case frame: Frame =>
                continue = dispatch(self.asInstanceOf[Http2.Connection], frame, decoder)

          (writer, reader)

    // Perform the connection handshake: emit our SETTINGS and await the peer's.
    // Plain using-parameters, de-sugared from `raises`: a context-function result may
    // not hide `this`.
    def start()(using Tactic[Async.Error]): Unit =
      send(Frame.Settings(initialSettings(window), ack = false))

      // The connection window has no SETTINGS entry: raise it from the RFC
      // default to our receive budget. It can only be raised, so a budget below
      // the default leaves the connection at the default and the stream window
      // is what gates.
      if window > defaultWindow then send(Frame.WindowUpdate(0, window - defaultWindow))
      started.await()

    // Open a new client stream, send its header block (and optional body), and return
    // the stream handle whose promises/spool the reader will populate.
    def request(headerBlock: List[Hpack.Entry], body: Optional[Bytes]): Http2.Stream =
      val id = nextId.getAndAdd(2)

      // The consumption callback reaches only JMM-safe state (atomic counters
      // and the thread-safe outbound relay), so it is laundered pure at this
      // rim rather than tracked into the stream.
      val stream = Http2.Stream(id, scala.caps.unsafe.unsafeAssumePure(consumed))
      streams(id) = stream
      val encoder = Hpack()
      val noBody = body.absent

      send(Frame.Headers(id, encoder.encode(headerBlock), endStream = noBody, endHeaders = true))

      // The body drains under the peer's flow-control windows and frame-size
      // limit, so a large upload parks this (requesting) fiber at window
      // exhaustion until the peer's WINDOW_UPDATEs grant more.
      body.let: payload =>
        val streamWindow = streamWindows.getOrElseUpdate(id, FlowWindow(peerInitialWindow.get))

        sendFlowControlled
          (id, payload, endStream = true, connWindow, streamWindow, peerMaxFrame.get, send)

      stream

    // Issue a telekinesis `Http.Request` over this connection and return the
    // `Http.Response`, blocking only until the response HEADERS arrive; the body
    // streams lazily from the stream's spool. `scheme`/`authority` supply the
    // pseudo-headers the request type doesn't carry. Trailers (e.g. gRPC status) are
    // available afterwards via `stream.trailers`.
    def fetch(request: Http.Request, scheme: Text, authority: Text)
      ( using Tactic[Http2.Error], Tactic[Async.Error] )
    :   (Http2.Stream, Http.Response) =

      Log.fine(Http2.Event.RequestSent(authority))
      val headerBlock = PseudoHeaders.request(request, scheme, authority)
      val data = request.body().memoize

      val payload: Optional[Bytes] = if data.length == 0 then Unset else data

      val stream = this.request(headerBlock, payload)
      val responseHeaders = stream.headers.await()

      (stream, PseudoHeaders.response(responseHeaders, Chain.from(stream.body.stream.records)))

    def close(): Unit =
      send(Frame.GoAway(0, ErrorCode.NoError.code, Array.empty[Byte]))
      outbound.stop()
      reader.cancel()
      writer.cancel()
      duplex.close()

  object Stream:
    // The inbound body buffer: an unbounded relay whose effective buffering is
    // bounded by protocol credit — the peer may have at most the advertised
    // receive window in flight beyond what the consumer has drained. Draining
    // through `stream` accounts each record's bytes via `onConsume` at the
    // point data actually leaves the buffer for the application, and the
    // connection turns the accumulation into deferred, batched WINDOW_UPDATEs.
    class Body(onConsume: Int -> Unit):
      private val relay: Relay[Bytes] = Relay()

      private[telekinesis] def put(data: Bytes): Unit = relay.put(data)
      private[telekinesis] def stop(): Unit = relay.stop()

      def stream
        ( using addressable: (Array[Bytes]^{}) is Addressable, buffering: Buffering )
      :   (zephyrine.Stream[Array[Bytes]^{}] over Credit)^ =

        accounted(relay.stream)

      // A pass-through wrapper counting the bytes of each record the consumer
      // skips past — the single point where records leave the relay's window.
      private def accounted
        ( consume underlying0: (zephyrine.Stream[Array[Bytes]^{}] over Credit)^ )
        ( using addressable: (Array[Bytes]^{}) is Addressable )
      :   (zephyrine.Stream[Array[Bytes]^{}] over Credit)^ =

        new zephyrine.Stream[Array[Bytes]^{}](using addressable):
          type Transport = Credit

          // The adopted stream is held through a neutral carrier: an exclusive
          // field would be read-only, so the accessor re-asserts the ownership
          // this wrapper took at construction.
          private val held: AnyRef = underlying0.asInstanceOf[AnyRef]

          private def underlying: (zephyrine.Stream[Array[Bytes]^{}] over Credit)^ =
            held.asInstanceOf[(zephyrine.Stream[Array[Bytes]^{}] over Credit)^]

          update def refill(demand: Credit): Optional[Int] = underlying.refill(demand)

          protected def storage0: AnyRef =
            val current = underlying
            current.storage(using Unsafe).asInstanceOf[AnyRef]

          def start: Int = underlying.start
          def limit: Int = underlying.limit

          update def skip(count: Int): Unit =
            val window = storage0.asInstanceOf[scala.Array[AnyRef]]
            val offset = underlying.start
            var index = 0
            var bytes = 0

            while index < count do
              bytes += window(offset + index).asInstanceOf[Bytes].length
              index += 1

            underlying.skip(count)
            if bytes > 0 then onConsume(bytes)

          override update def close(): Unit = underlying.close()

  // Http2.Stream -> Http2.Stream
  class Stream(val id: Int, onConsume: (Http2.Stream, Int) -> Unit = (_, _) => ()):
    val headers: Promise[List[Hpack.Entry]] = Promise()
    val trailers: Promise[List[Hpack.Entry]] = Promise()

    // Consumed-but-unreplenished inbound bytes, drained by the connection's
    // batched replenishment.
    private[telekinesis] val unreplenished: juca.AtomicInteger = juca.AtomicInteger(0)

    val body: Stream.Body = Stream.Body(count => onConsume(this, count))

    // Untracked: written only by the connection's single reader daemon.
    @caps.unsafe.untrackedCaptures
    private var headersSeen: Boolean = false

    // Record an incoming HEADERS block: the first becomes the response headers, a
    // subsequent one (always end-stream) becomes the trailers.
    def acceptHeaders(block: List[Hpack.Entry]): Unit =
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

  // Send `payload` as DATA frames on `streamId`, honouring the peer's
  // flow-control windows (RFC 7540 §6.9) and its maximum frame size (§6.5.2):
  // each chunk is bounded by the connection window, the stream window and
  // SETTINGS_MAX_FRAME_SIZE, and the calling fiber parks when a window is
  // exhausted until an inbound WINDOW_UPDATE tops it up. An empty payload
  // carries no data and is sent immediately (its only role is `endStream`);
  // otherwise `endStream` rides the final chunk. Shared by both roles: the
  // server's response bodies and the client's request bodies drain identically.
  private def sendFlowControlled
    ( streamId:     Int,
      payload:      Bytes,
      endStream:    Boolean,
      connWindow:   FlowWindow,
      streamWindow: FlowWindow,
      maxFrame:     Int,
      emit:         Frame => Unit )
  :   Unit =

    if payload.length == 0 then emit(Frame.Data(streamId, payload, endStream)) else
      var offset = 0

      while offset < payload.length do
        val remaining = (payload.length - offset).min(maxFrame.max(1))
        // Acquire connection credit first, then stream credit up to that, and
        // return any connection surplus the stream could not match.
        val connChunk = connWindow.acquire(remaining)
        val streamChunk = streamWindow.acquire(connChunk)
        if streamChunk < connChunk then connWindow.release(connChunk - streamChunk)

        val chunk: Bytes = payload.segment((offset).z till (offset + streamChunk).z)
        val last: Boolean = endStream && offset + streamChunk == payload.length
        emit(Frame.Data(streamId, chunk, last))
        offset += streamChunk

  // Http2ServerConnection → Http2.ServerConnection
  object ServerConnection:
    // Our advertised SETTINGS: our receive-side stream window (see
    // `Connection.receiveWindow`). `EnablePush` is a client-only setting, so
    // the server sends only the window.
    private def serverSettings(window: Int): List[Setting] =
      List(Setting(SettingId.InitialWindowSize.id, window))

    // Dispatch one decoded frame, in the server role: the peer is a client, so a
    // HEADERS frame for an unknown (client-initiated, odd) stream id CREATES the
    // stream — its header block is the request head — and announces it on the
    // `accepted` relay for the serve loop to handle; a second HEADERS block on a
    // known stream carries request trailers. Lives on the companion — taking the
    // connection as a plain parameter — so the reader daemon's body stays free of
    // `this` captures. Returns false to stop the reader.
    private def dispatch(conn: Http2.ServerConnection, frame: Frame, decoder: Hpack)
      ( using Tactic[Http2.Error] )
    :   Boolean =
      frame match
        case Frame.Settings(settings, ack) =>
          if !ack then
            // Adopt the peer's advertised initial per-stream send window; applies
            // to streams opened after this point (existing streams are not
            // retroactively adjusted — a deliberate simplification).
            settings.seek(_.id == SettingId.InitialWindowSize.id).let: setting =>
              conn.peerInitialWindow.set(setting.value.toInt)

            settings.seek(_.id == SettingId.MaxFrameSize.id).let: setting =>
              conn.peerMaxFrame.set(setting.value.toInt)

            conn.send(Frame.Settings(Nil, ack = true))
            conn.started.offer(())

          true

        case Frame.Ping(opaque, ack) =>
          if !ack then conn.send(Frame.Ping(opaque, ack = true))
          true

        case Frame.GoAway(lastStreamId, _, _) =>
          Log.warn(Http2.Event.GoAway(lastStreamId))
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
              // The consumption callback reaches only JMM-safe state (atomic
              // counters and the thread-safe outbound relay), so it is
              // laundered pure at this rim rather than tracked into the stream.
              val stream = Http2.Stream(id, scala.caps.unsafe.unsafeAssumePure(conn.consumed))
              conn.streams(id) = stream
              stream.acceptHeaders(decoder.decode(block))
              if endStream then stream.end() else ()
              conn.accepted.put(stream)

          true

        case Frame.Data(id, payload, endStream) =>
          conn.streams.get(id).foreach: stream =>
            // No replenishment here: the peer's window refills only as the
            // handler drains the request body (see `Stream.Body`), so an
            // unread body backpressures the peer at the advertised window.
            stream.acceptData(payload)

            if endStream then
              stream.end()
              conn.streams.remove(id)

          true

        case Frame.RstStream(id, _) =>
          conn.streams.get(id).foreach: stream =>
            stream.end()
            conn.streams.remove(id)

          conn.streamWindows.remove(id)
          true

        // An inbound WINDOW_UPDATE grants send credit: on stream 0 it tops up the
        // connection window, otherwise the named stream's window (RFC 7540 §6.9).
        case Frame.WindowUpdate(id, increment) =>
          if id == 0 then conn.connWindow.release(increment)
          else conn.streamWindows.get(id).foreach(_.release(increment))

          true

        case Frame.Continuation(_, _, _) | Frame.Ignored(_, _) =>
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
  class ServerConnection(duplex: Duplex^, window: Int = Connection.receiveWindow)
    ( using Monitor, Probate ):
    import Http2.ServerConnection.*
    import Http2.Connection.defaultWindow

    // A socket-backed `Duplex` captures its I/O capabilities, so it crosses into
    // the reader/writer daemons (and reaches `close`) as a neutral `AnyRef` rim.
    private val duplexRef: AnyRef = duplex.asInstanceOf[AnyRef]

    private[telekinesis] val streams: scc.TrieMap[Int, Http2.Stream] = scc.TrieMap()
    private val outbound: Relay[Frame] = Relay()
    private[telekinesis] val started: Promise[Unit] = Promise()

    // Send-side flow control (RFC 7540 §6.9): the connection window, a per-stream
    // window created lazily at the peer's advertised initial size, and that
    // advertised size (updated by the peer's SETTINGS).
    private[telekinesis] val connWindow: FlowWindow = FlowWindow(defaultWindow)
    private[telekinesis] val streamWindows: scc.TrieMap[Int, FlowWindow] = scc.TrieMap()
    private[telekinesis] val peerInitialWindow: juca.AtomicInteger = juca.AtomicInteger(defaultWindow)

    private[telekinesis] val peerMaxFrame: juca.AtomicInteger =
      juca.AtomicInteger(Connection.defaultMaxFrame)

    // Streams opened by the client, in arrival order; the serve loop takes each
    // and runs its handler. Stopped when the connection ends.
    private[telekinesis] val accepted: Relay[Http2.Stream] = Relay()

    // Receive-side replenishment state, as `Connection`'s: consumed bytes
    // accumulate per stream and per connection, released as batched
    // WINDOW_UPDATEs at the half-window threshold.
    private val connPending: juca.AtomicInteger = juca.AtomicInteger(0)
    private val threshold: Int = (window/2).max(1)

    private[telekinesis] def send(frame: Frame): Unit = outbound.put(frame)

    private[telekinesis] def consumed(stream: Http2.Stream, count: Int): Unit =
      stream.unreplenished.addAndGet(count)
      connPending.addAndGet(count)
      replenish(stream.unreplenished, stream.id)
      replenish(connPending, 0)

    private def replenish(pending: juca.AtomicInteger, id: Int): Unit =
      var continue = true

      while continue do
        val value = pending.get
        if value < threshold then continue = false
        else if pending.compareAndSet(value, 0) then
          send(Frame.WindowUpdate(id, value))
          continue = false

    // Tear the connection down after an unrecoverable reader/writer failure or a
    // bad preface: unblock a pending handshake, end every open stream, and stop
    // the spools so the writer and the serve loop exit.
    private def tearDown(): Unit =
      started.cancel()
      streams.values.foreach(_.end())
      outbound.stop()
      accepted.stop()

    private val (writer, reader) =
      // As `Http2.Connection`: no aliased writer between containment and body.
      scala.caps.unsafe.unsafeAssumeSeparate:
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

            outbound0.stream.records.each: frame => duplex0.send(zephyrine.Stream(frame.serialize))

          val frameReaderRef: AnyRef =
            FrameReader(duplexRef.asInstanceOf[Duplex^].source).asInstanceOf[AnyRef]

          val reader = daemon:
            // A protocol error tears down just this connection; throw it to the
            // enclosing `contain`, which runs `tearDown()` and stops the reader.
            given Tactic[Http2.Error] = AsyncTactic()

            val frameReader = frameReaderRef.asInstanceOf[FrameReader^]

            // The server's first read: consume and validate the client
            // connection preface before frame-parsing.
            frameReader.expectPreface(Http2.Connection.connectionPreface)

            val decoder = Hpack()
            var continue = true

            while continue do (frameReader.next(): @unchecked) match
              case Unset        => continue = false
              case frame: Frame =>
                continue = dispatch(self.asInstanceOf[Http2.ServerConnection], frame, decoder)

            self.asInstanceOf[Http2.ServerConnection].accepted.stop()

          (writer, reader)

    // Perform the server side of the connection handshake: emit our SETTINGS and
    // await the client's (which the dispatch acks). Plain using-parameters,
    // de-sugared from `raises`: a context-function result may not hide `this`.
    def start()(using Tactic[Async.Error]): Unit =
      send(Frame.Settings(serverSettings(window), ack = false))

      // Raise the connection window from the RFC default to our receive
      // budget, as `Connection.start` does.
      if window > defaultWindow then send(Frame.WindowUpdate(0, window - defaultWindow))
      started.await()

    // Run `handler` for each client-initiated stream as it arrives, on the
    // reader-driven serve loop; returns when the connection ends. The handler
    // typically spawns a per-stream fiber so requests multiplexed on the one
    // connection are served concurrently.
    def eachStream(handler: Http2.Stream => Unit): Unit =
      accepted.stream.records.each(handler)

    // Send a response header block on `streamId`, encoded with a fresh
    // (always-literal) HPACK encoder. `endStream` marks a bodiless response.
    def sendHeaders(streamId: Int, entries: List[Hpack.Entry], endStream: Boolean): Unit =
      val encoder = Hpack()
      send(Frame.Headers(streamId, encoder.encode(entries), endStream, endHeaders = true))

    // Send `payload` as DATA on `streamId`, honouring the peer's flow-control
    // windows and frame-size limit; see `sendFlowControlled`. Blocks the
    // calling (per-stream) fiber at window exhaustion.
    def sendData(streamId: Int, payload: Bytes, endStream: Boolean): Unit =
      val streamWindow = streamWindows.getOrElseUpdate(streamId, FlowWindow(peerInitialWindow.get))
      sendFlowControlled
        (streamId, payload, endStream, connWindow, streamWindow, peerMaxFrame.get, send)

    // Send a trailing HEADERS block (always end-stream) on `streamId` — the
    // response trailers, e.g. gRPC's `grpc-status`. A response with trailers must
    // leave `endStream` unset on its HEADERS and DATA, so this block closes the
    // stream. Encoded with a fresh (always-literal) HPACK encoder.
    def sendTrailers(streamId: Int, entries: List[Hpack.Entry]): Unit =
      val encoder = Hpack()
      send(Frame.Headers(streamId, encoder.encode(entries), endStream = true, endHeaders = true))

    def close(): Unit =
      send(Frame.GoAway(0, ErrorCode.NoError.code, Array.empty[Byte]))
      outbound.stop()
      accepted.stop()
      reader.cancel()
      writer.cancel()
      duplexRef.asInstanceOf[Duplex^].close()

