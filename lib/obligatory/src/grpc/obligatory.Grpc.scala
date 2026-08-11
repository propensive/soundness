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
package obligatory

import scala.caps

import anticipation.*
import coaxial.*
import contingency.*
import cordillera.*
import distillate.*
import fulminate.*
import gossamer.*
import locomotion.*
import parasite.*
import pneumatic.*
import prepositional.*
import proscenium.compat.*
import rudiments.*
import spectacular.*
import telekinesis.*
import turbulence.*
import urticose.*
import vacuous.*
import zephyrine.*

// The gRPC vocabulary. Grouped under `Grpc` so its generic names — `Status`,
// `Method`, `Metadata` — don't crowd the top-level namespace, mirroring how
// `telekinesis` groups its types under `Http` and `cordillera` under `Http2`.
object Grpc:
  object Status:
    given communicable: Status is Communicable = status => m"${status.toString.tt}"

    // The canonical status code (RFC: grpc-status), or `Unset` if unrecognised.
    def of(code: Int): Optional[Status] = values.find(_.code == code).optional

  // The seventeen canonical gRPC status codes, carried in the `grpc-status`
  // trailer of a response. `Ok` (0) signifies success; the rest are failures.
  enum Status(val code: Int):
    case Ok                 extends Status(0)
    case Cancelled          extends Status(1)
    case Unknown            extends Status(2)
    case InvalidArgument    extends Status(3)
    case DeadlineExceeded   extends Status(4)
    case NotFound           extends Status(5)
    case AlreadyExists      extends Status(6)
    case PermissionDenied   extends Status(7)
    case ResourceExhausted  extends Status(8)
    case FailedPrecondition extends Status(9)
    case Aborted            extends Status(10)
    case OutOfRange         extends Status(11)
    case Unimplemented      extends Status(12)
    case Internal           extends Status(13)
    case Unavailable        extends Status(14)
    case DataLoss           extends Status(15)
    case Unauthenticated    extends Status(16)

  // A fully-qualified gRPC method, addressed on the wire as the HTTP/2 `:path`
  // `/<package>.<Service>/<Method>`, e.g. `/grpc.health.v1.Health/Check`.
  case class Method(service: Text, rpc: Text):
    def path: Text = t"/$service/$rpc"

  object Endpoint:
    given sessional: [endpoint: {Connectable, Showable}]
    =>  ( monitor:    Monitor,
          probate:    Probate,
          asyncError: Tactic[Async.Error],
          loggable:   (SocketEvent is Loggable)^ )
    =>  ( GrpcSessional[endpoint]^{monitor, asyncError, loggable, caps.any} ) =

      GrpcSessional()

  // A gRPC endpoint: the target of a scoped channel, `Grpc.Endpoint(endpoint).session:
  // channel ?=> …`, whose underlying HTTP/2 connection lives exactly as long as the
  // block — no parked daemon holds the connection open, because the loan and the scope
  // coincide.
  case class Endpoint[endpoint: {Connectable, Showable}]
    ( endpoint: Http2.Endpoint[endpoint], defaults: Metadata = Metadata() )

  // Custom call metadata: arbitrary key/value pairs sent as HTTP/2 headers
  // alongside the gRPC pseudo-headers (e.g. containerd's `containerd-namespace`).
  case class Metadata(entries: List[(Text, Text)] = Nil)

  // Derive a client stub for a service interface of `@rpc`-annotated methods over the
  // given channel; `service` is the fully-qualified proto service name (the package
  // and `Service`), and each method's own name completes the `:path`.
  inline def remote[interface](channel: Grpc.Channel, service: Text): interface =
    ${grpcInternal.remote[interface]('channel, 'service)}

  // GrpcChannel → Grpc.Channel
  object Channel:
    // Open a channel to a cleartext-h2c endpoint, completing the HTTP/2 handshake. The
    // connection's read/write daemons capture the ambient `Monitor`/`Probate`, so this
    // must be called inside a `supervise` scope.
    def apply[endpoint]
      ( endpoint: Http2.Endpoint[endpoint], defaults: Grpc.Metadata = Grpc.Metadata() )
      ( using monitor: Monitor, probate: Probate, asyncError: Tactic[Async.Error] )
    :   Channel^{monitor, caps.any} =

      new Channel(endpoint.connect(), endpoint.authority, defaults)

  // A gRPC channel over a single, persistent HTTP/2 connection (`cordillera`). Each
  // call opens one multiplexed stream: the request is one length-prefixed protobuf
  // message (`locomotion`), and the canonical status arrives in the response's
  // `grpc-status` trailer. v1 supports unary and server-streaming calls; the request
  // is always a single message, so client-streaming and bidirectional streaming wait
  // on a `cordillera` enhancement.
  // The channel retains its connection — a capability holding the ambient `Monitor` —
  // so a channel is itself a capability.
  class Channel
    ( connection: Http2.Connection^, authority: Text, defaults: Grpc.Metadata = Grpc.Metadata() ):
    // The `:authority` pseudo-header is supplied to `fetch` separately; the request's
    // `Host` is unused by the HTTP/2 transport, so the hostname is parsed leniently.
    private val host: Host = unsafely(authority.cut(t":").prim.or(authority).as[Host])

    // Build the gRPC HTTP/2 request: POST to `/package.Service/Method` with the
    // mandatory content-type and `te: trailers`, plus any custom metadata, and a body
    // of exactly one length-prefixed message.
    private def httpRequest(method: Grpc.Method, metadata: Grpc.Metadata, message: Data)
    :   Http.Request =

      val metadataHeaders = (defaults.entries.stdlib ++ metadata.entries.stdlib).map: (key, value) =>
        Http.Header(key, value)

      val headers =
        Http.Header(t"content-type", t"application/grpc+proto") ::
          Http.Header(t"te", t"trailers") ::
          metadataHeaders

      val body: Spring[Data] = () => Stream(Framing.encode(message))
      Http.Request(Http.Post, 2.0, host, method.path, List.of(headers), body)

    // gRPC requires HTTP status 200; anything else is a transport-level failure.
    private def expectOk(response: Http.Response): Unit raises Error =
      val code = response.status.code

      if code != 200
      then abort(Error(Grpc.Status.Internal, t"the server returned HTTP status $code"))

    // Read the canonical status from the `grpc-status`/`grpc-message` fields, looking
    // in the trailers first and then the initial headers (a Trailers-Only response
    // carries the status in the headers). Raise unless the status is `Ok`.
    // Declared with explicit tactics rather than stacked `raises`: see `bintelDocument`
    // in stratiform (capture checking cannot unify cross-level tactic captures, 3.10).
    private def expectStatus(stream: Http2.Stream)
      ( using Monitor^, Tactic[Error], Tactic[Async.Error] )
    :   Unit =

      val fields = stream.trailers.await().stdlib ++ stream.headers.await().stdlib
      val codeText = fields.find(_.name == t"grpc-status").optional.let(_.value)
      val message = fields.find(_.name == t"grpc-message").optional.let(_.value).or(t"")

      val code =
        codeText.lay(Grpc.Status.Unknown.code): text =>
          safely(text.as[Int]).or(Grpc.Status.Unknown.code)

      val status = Grpc.Status.of(code).or(Grpc.Status.Unknown)
      if status != Grpc.Status.Ok then abort(Error(status, message))

    // The protobuf message codec, with evidence passed explicitly so the abstract
    // `request`/`response` types don't collide with locomotion's universal derivation
    // givens at this generic site (they resolve cleanly at the concrete call site).
    private def encodeMessage[value](value: value)(using encodable: value is Encodable in Protobuf)
    :   Data =

      summon[Protobuf is Encodable in Data].encoded(encodable.encoded(value))

    private def decodeMessage[value](bytes: Data)(using decodable: value is Decodable in Protobuf)
    :   value raises Protobuf.Error =

      decodable.decoded(Chain(bytes).read[Protobuf])

    // A unary call: send one message, read exactly one response message, then verify
    // the trailing status.
    def unary[request, response]
      ( method: Grpc.Method, value: request, metadata: Grpc.Metadata = Grpc.Metadata() )
      ( using request is Encodable in Protobuf, response is Decodable in Protobuf )
      ( using Monitor^ )
      ( using Tactic[Error], Tactic[Http2.Error], Tactic[Async.Error], Tactic[Protobuf.Error] )
    :   response =

      val (stream, response) =
        connection.fetch(httpRequest(method, metadata, encodeMessage(value)), t"http", authority)

      expectOk(response)
      val messages = stream.body.stream.records.frames[Framing]
      val first: Optional[Data] = if messages.hasNext then messages.next() else Unset

      // Verify the trailing status before demanding a body, so a Trailers-Only error
      // response surfaces its real status rather than "no message".
      expectStatus(stream)

      first.lay(abort(Error(Grpc.Status.Internal, t"the server sent no response message"))):
        message => decodeMessage[response](message)

    // A server-streaming call: send one message, then lazily decode each response
    // message. The trailing status is verified once the response stream is exhausted,
    // so the returned `Chain` must be consumed within the enclosing `supervise` scope.
    def serverStreaming[request, response]
      ( method: Grpc.Method, value: request, metadata: Grpc.Metadata = Grpc.Metadata() )
      ( using request is Encodable in Protobuf, response is Decodable in Protobuf )
      ( using Monitor^ )
      ( using Tactic[Error], Tactic[Http2.Error], Tactic[Async.Error], Tactic[Protobuf.Error] )
    :   Chain[response] =

      val (stream, response) =
        connection.fetch(httpRequest(method, metadata, encodeMessage(value)), t"http", authority)

      expectOk(response)
      val messages = stream.body.stream.records.frames[Framing]

      def recur(): Chain[response] =
        if messages.hasNext then
          // Successive pulls from the same single-owner message iterator.
          scala.caps.unsafe.unsafeAssumeSeparate(decodeMessage[response](messages.next()) #:: recur())
        else
          expectStatus(stream)
          Chain()

      recur()

  // GrpcError → Grpc.Error
  // Raised when a gRPC call completes with a non-`Ok` `grpc-status` trailer, or when
  // the response cannot be framed or decoded. The `status` mirrors the canonical
  // gRPC code; `detail` carries the `grpc-message` text (or a local diagnostic).
  case class Error(status: Grpc.Status, detail: Text)(using Diagnostics)
  extends fulminate.Error(m"the gRPC call failed with status $status: $detail")

  // GrpcFraming → Grpc.Framing
  // gRPC's length-prefixed message framing (the same wire shape for every codec):
  // each message is a 1-byte compression flag, a 4-byte big-endian length, then that
  // many payload bytes. A flag of 1 means the payload is compressed with the call's
  // `grpc-encoding` (gzip here). The `Framable` instance reassembles whole messages
  // from the arbitrarily-chunked response body, reusing `Framable.frames` exactly as
  // `LengthPrefix` does for the JSON-RPC stream framing.
  object Framing:
    private def gzip(message: Data): Data = Gzip.compression.compress(Chain(message)).read[Data]
    private def gunzip(message: Data): Data =
      Gzip.compression.decompress(Chain(message)).read[Data]

    // Prefix one message for the wire, optionally gzip-compressing the payload.
    def encode(message: Data, compress: Boolean = false): Data =
      val payload = if compress then gzip(message) else message
      val length = payload.length

      val header: Data =
        Array.of
          ( (if compress then 1 else 0).toByte,
            (length >>> 24).toByte,
            (length >>> 16).toByte,
            (length >>> 8).toByte,
            length.toByte )

      header ++ payload

    given framable: (tactic: Tactic[Error])
    =>  ((Data is Framable by Framing)^{tactic}) = input =>
      def truncated(): Nothing =
        abort(Error(Grpc.Status.Internal, t"the gRPC message frame was truncated"))

      val cursor = Cursor(input)

      // Read the 5-byte prefix: a compression flag plus a big-endian length. `Unset`
      // at a clean message boundary (the stream is exhausted) ends the iterator.
      def header: Optional[(Boolean, Int)] =
        cursor.lay(Unset): flag =>
          cursor.next()

          cursor.lay(truncated()): byte0 =>
            cursor.next()

            cursor.lay(truncated()): byte1 =>
              cursor.next()

              cursor.lay(truncated()): byte2 =>
                cursor.next()

                cursor.lay(truncated()): byte3 =>
                  cursor.next()
                  ( flag != 0,
                    byte0.asInstanceOf[Byte] << 24 | byte1.asInstanceOf[Byte] << 16
                      | byte2.asInstanceOf[Byte] << 8 | byte3.asInstanceOf[Byte] )

      Framable.frames[Data]:
        header.let: (compressed, length) =>
          // The inline `take` expansion re-infers a fresh `any.rd` on the frozen chunk;
          // the cast reasserts the frozen form, which `take` already guarantees.
          val payload = cursor.take(truncated())(length).asInstanceOf[Data]
          if compressed then gunzip(payload) else payload

  sealed trait Framing
