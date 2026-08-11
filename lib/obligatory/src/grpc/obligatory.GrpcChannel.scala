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
import gossamer.*
import locomotion.*
import parasite.*
import prepositional.*
import rudiments.*
import spectacular.*
import telekinesis.*
import turbulence.*
import urticose.*
import vacuous.*
import zephyrine.*

// A scoped gRPC channel over cordillera's `Http2.EndpointSessional` flow: the HTTP/2
// connection is opened and its handshake completed, the channel is lent to the lambda,
// and the connection (with its reader/writer daemons) is torn down when the scope ends —
// unlike `GrpcChannel.apply`, whose connection is held open by a parked daemon until the
// enclosing `supervise` scope ends. A named instance class rather than an anonymous
// given: an anonymous subclass would freshen the capability types in its inferred
// `Result` member.
class GrpcSessional[endpoint: {Connectable, Showable}]
  ( using monitor:    Monitor,
          probate:    Probate,
          asyncError: Tactic[Async.Error],
          loggable:   (SocketEvent is Loggable)^ )
extends Sessional:
  type Self = Grpc.Endpoint[endpoint]

  // A fresh capability (`^`, not `^{caps.any}`): each `session` call's handle is its own
  // existential, so returning it (or anything capturing it) from the block is a level
  // violation the capture checker rejects.
  type Result = GrpcChannel^

  def session[result](target: Self)(lambda: (session: Result) ?=> result): result =
    target.endpoint.session: connection ?=>
      lambda(using new GrpcChannel(connection, target.endpoint.authority, target.defaults))

object GrpcChannel:
  // Open a channel to a cleartext-h2c endpoint, completing the HTTP/2 handshake. The
  // connection's read/write daemons capture the ambient `Monitor`/`Probate`, so this
  // must be called inside a `supervise` scope.
  def apply[endpoint]
    ( endpoint: Http2.Endpoint[endpoint], defaults: Grpc.Metadata = Grpc.Metadata() )
    ( using monitor: Monitor, probate: Probate, asyncError: Tactic[Async.Error] )
  :   GrpcChannel^{monitor, caps.any} =

    new GrpcChannel(endpoint.connect(), endpoint.authority, defaults)

// A gRPC channel over a single, persistent HTTP/2 connection (`cordillera`). Each
// call opens one multiplexed stream: the request is one length-prefixed protobuf
// message (`locomotion`), and the canonical status arrives in the response's
// `grpc-status` trailer. v1 supports unary and server-streaming calls; the request
// is always a single message, so client-streaming and bidirectional streaming wait
// on a `cordillera` enhancement.
// The channel retains its connection — a capability holding the ambient `Monitor` —
// so a channel is itself a capability.
class GrpcChannel
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

    val body: Spring[Data] = () => Stream(GrpcFraming.encode(message))
    Http.Request(Http.Post, 2.0, host, method.path, List.of(headers), body)

  // gRPC requires HTTP status 200; anything else is a transport-level failure.
  private def expectOk(response: Http.Response): Unit raises GrpcError =
    val code = response.status.code

    if code != 200
    then abort(GrpcError(Grpc.Status.Internal, t"the server returned HTTP status $code"))

  // Read the canonical status from the `grpc-status`/`grpc-message` fields, looking
  // in the trailers first and then the initial headers (a Trailers-Only response
  // carries the status in the headers). Raise unless the status is `Ok`.
  // Declared with explicit tactics rather than stacked `raises`: see `bintelDocument`
  // in stratiform (capture checking cannot unify cross-level tactic captures, 3.10).
  private def expectStatus(stream: Http2Stream)
    ( using Monitor^, Tactic[GrpcError], Tactic[Async.Error] )
  :   Unit =

    val fields = stream.trailers.await().stdlib ++ stream.headers.await().stdlib
    val codeText = fields.find(_.name == t"grpc-status").optional.let(_.value)
    val message = fields.find(_.name == t"grpc-message").optional.let(_.value).or(t"")

    val code =
      codeText.lay(Grpc.Status.Unknown.code): text =>
        safely(text.as[Int]).or(Grpc.Status.Unknown.code)

    val status = Grpc.Status.of(code).or(Grpc.Status.Unknown)
    if status != Grpc.Status.Ok then abort(GrpcError(status, message))

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
    ( using Tactic[GrpcError], Tactic[Http2.Error], Tactic[Async.Error], Tactic[Protobuf.Error] )
  :   response =

    val (stream, response) =
      connection.fetch(httpRequest(method, metadata, encodeMessage(value)), t"http", authority)

    expectOk(response)
    val messages = stream.body.stream.records.frames[GrpcFraming]
    val first: Optional[Data] = if messages.hasNext then messages.next() else Unset

    // Verify the trailing status before demanding a body, so a Trailers-Only error
    // response surfaces its real status rather than "no message".
    expectStatus(stream)

    first.lay(abort(GrpcError(Grpc.Status.Internal, t"the server sent no response message"))):
      message => decodeMessage[response](message)

  // A server-streaming call: send one message, then lazily decode each response
  // message. The trailing status is verified once the response stream is exhausted,
  // so the returned `Chain` must be consumed within the enclosing `supervise` scope.
  def serverStreaming[request, response]
    ( method: Grpc.Method, value: request, metadata: Grpc.Metadata = Grpc.Metadata() )
    ( using request is Encodable in Protobuf, response is Decodable in Protobuf )
    ( using Monitor^ )
    ( using Tactic[GrpcError], Tactic[Http2.Error], Tactic[Async.Error], Tactic[Protobuf.Error] )
  :   Chain[response] =

    val (stream, response) =
      connection.fetch(httpRequest(method, metadata, encodeMessage(value)), t"http", authority)

    expectOk(response)
    val messages = stream.body.stream.records.frames[GrpcFraming]

    def recur(): Chain[response] =
      if messages.hasNext then
        // Successive pulls from the same single-owner message iterator.
        scala.caps.unsafe.unsafeAssumeSeparate(decodeMessage[response](messages.next()) #:: recur())
      else
        expectStatus(stream)
        Chain()

    recur()
