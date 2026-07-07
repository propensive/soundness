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
package obligatory

import anticipation.*
import contingency.*
import cordillera.*
import distillate.*
import gossamer.*
import locomotion.*
import parasite.*
import prepositional.*
import rudiments.*
import telekinesis.*
import turbulence.*
import urticose.*
import vacuous.*

object GrpcChannel:
  // Open a channel to a cleartext-h2c endpoint, completing the HTTP/2 handshake. The
  // connection's read/write daemons capture the ambient `Monitor`/`Probate`, so this
  // must be called inside a `supervise` scope.
  def apply[endpoint]
    ( endpoint: Http2.Endpoint[endpoint], defaults: Grpc.Metadata = Grpc.Metadata() )
    ( using Monitor, Probate )
  :   GrpcChannel raises AsyncError =

    new GrpcChannel(endpoint.connect(), endpoint.authority, defaults)

// A gRPC channel over a single, persistent HTTP/2 connection (`cordillera`). Each
// call opens one multiplexed stream: the request is one length-prefixed protobuf
// message (`locomotion`), and the canonical status arrives in the response's
// `grpc-status` trailer. v1 supports unary and server-streaming calls; the request
// is always a single message, so client-streaming and bidirectional streaming wait
// on a `cordillera` enhancement.
class GrpcChannel
  ( connection: Http2Connection, authority: Text, defaults: Grpc.Metadata = Grpc.Metadata() ):
  // The `:authority` pseudo-header is supplied to `fetch` separately; the request's
  // `Host` is unused by the HTTP/2 transport, so the hostname is parsed leniently.
  private val host: Host = unsafely(authority.cut(t":").prim.or(authority).decode[Host])

  // Build the gRPC HTTP/2 request: POST to `/package.Service/Method` with the
  // mandatory content-type and `te: trailers`, plus any custom metadata, and a body
  // of exactly one length-prefixed message.
  private def httpRequest(method: Grpc.Method, metadata: Grpc.Metadata, message: Data)
  :   Http.Request =

    val metadataHeaders = (defaults.entries ++ metadata.entries).map: (key, value) =>
      Http.Header(key, value)

    val headers =
      Http.Header(t"content-type", t"application/grpc+proto") ::
        Http.Header(t"te", t"trailers") ::
        metadataHeaders

    val body = () => Stream(GrpcFraming.encode(message))
    Http.Request(Http.Post, 2.0, host, method.path, headers, body)

  // gRPC requires HTTP status 200; anything else is a transport-level failure.
  private def expectOk(response: Http.Response): Unit raises GrpcError =
    val code = response.status.code

    if code != 200
    then abort(GrpcError(Grpc.Status.Internal, t"the server returned HTTP status $code"))

  // Read the canonical status from the `grpc-status`/`grpc-message` fields, looking
  // in the trailers first and then the initial headers (a Trailers-Only response
  // carries the status in the headers). Raise unless the status is `Ok`.
  private def expectStatus(stream: Http2Stream): Unit raises GrpcError raises AsyncError =
    val fields = stream.trailers.await() ++ stream.headers.await()
    val codeText = fields.find(_.name == t"grpc-status").optional.let(_.value)
    val message = fields.find(_.name == t"grpc-message").optional.let(_.value).or(t"")

    val code =
      codeText.lay(Grpc.Status.Unknown.code): text =>
        safely(text.decode[Int]).or(Grpc.Status.Unknown.code)

    val status = Grpc.Status.of(code).or(Grpc.Status.Unknown)
    if status != Grpc.Status.Ok then abort(GrpcError(status, message))

  // The protobuf message codec, with evidence passed explicitly so the abstract
  // `request`/`response` types don't collide with locomotion's universal derivation
  // givens at this generic site (they resolve cleanly at the concrete call site).
  private def encodeMessage[value](value: value)(using encodable: value is Encodable in Protobuf)
  :   Data =

    summon[Protobuf is Encodable in Data].encoded(encodable.encoded(value))

  private def decodeMessage[value](bytes: Data)(using decodable: value is Decodable in Protobuf)
  :   value raises ProtobufError =

    decodable.decoded(Stream(bytes).read[Protobuf])

  // A unary call: send one message, read exactly one response message, then verify
  // the trailing status.
  def unary[request, response]
    ( method: Grpc.Method, value: request, metadata: Grpc.Metadata = Grpc.Metadata() )
    ( using request is Encodable in Protobuf, response is Decodable in Protobuf )
  :   response raises GrpcError raises Http2Error raises AsyncError raises ProtobufError =

    val (stream, response) =
      connection.fetch(httpRequest(method, metadata, encodeMessage(value)), t"http", authority)

    expectOk(response)
    val messages = stream.body.stream.iterator.frames[GrpcFraming]
    val first: Optional[Data] = if messages.hasNext then messages.next() else Unset

    // Verify the trailing status before demanding a body, so a Trailers-Only error
    // response surfaces its real status rather than "no message".
    expectStatus(stream)

    first.lay(abort(GrpcError(Grpc.Status.Internal, t"the server sent no response message"))):
      message => decodeMessage[response](message)

  // A server-streaming call: send one message, then lazily decode each response
  // message. The trailing status is verified once the response stream is exhausted,
  // so the returned `Stream` must be consumed within the enclosing `supervise` scope.
  def serverStreaming[request, response]
    ( method: Grpc.Method, value: request, metadata: Grpc.Metadata = Grpc.Metadata() )
    ( using request is Encodable in Protobuf, response is Decodable in Protobuf )
  :   Stream[response] raises GrpcError raises Http2Error raises AsyncError raises ProtobufError =

    val (stream, response) =
      connection.fetch(httpRequest(method, metadata, encodeMessage(value)), t"http", authority)

    expectOk(response)
    val messages = stream.body.stream.iterator.frames[GrpcFraming]

    def recur(): Stream[response] =
      if messages.hasNext then decodeMessage[response](messages.next()) #:: recur()
      else
        expectStatus(stream)
        Stream()

    recur()
