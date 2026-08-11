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

import scala.annotation.*
import scala.quoted.*

import java.util.concurrent as juc

import anticipation.*
import contingency.*
import eucalyptus.*
import gesticulate.*
import gossamer.*
import hieroglyph.*
import inimitable.*
import jacinta.*
import parasite.*
import prepositional.*
import rudiments.*
import spectacular.*
import telekinesis.*
import turbulence.*
import urticose.*
import vacuous.*
import zephyrine.*

import httpBackends.virtualMachine

object JsonRpc:
  // Requests in flight, by correlation id. Concurrent and self-evicting: the requesting thread
  // registers, and a reader on another thread completes and removes — a plain `HashMap` races,
  // and entries that are never removed leak for the lifetime of the process.
  private val promises: juc.ConcurrentHashMap[Text | Int, Promise[Json]] = juc.ConcurrentHashMap()

  // Faults for requests answered with an `error` member. An error response carries no result to
  // fulfil the promise with, so the fault is recorded here and the promise cancelled; `outcome`
  // pairs the two back together for the caller awaiting the request.
  private val faults: juc.ConcurrentHashMap[Text | Int, Failure] = juc.ConcurrentHashMap()

  private def claim(id: Text | Int): Optional[Promise[Json]] = promises.remove(id) match
    case null                   => Unset
    case promise: Promise[Json] => promise

  inline def serve[interface](interface: interface): Json => Optional[Json] =
    ${obligatory.internal.dispatcher[interface]('interface)}

  // The JSON-RPC method names an interface declares (its `@rpc` members). Used to route a
  // request across several `serve` dispatchers when a single interface's dispatcher would be too
  // large to compile into one class. A covariant `List` rather than a `Set`: under capture
  // checking, the opaque `Text` in an invariant position freshens against the expansion site.
  inline def methods[interface]: List[Text] = ${obligatory.internal.methodNames[interface]}

  case class Request(jsonrpc: Text, method: Text, params: Json, id: Optional[Json])
  case class Response(jsonrpc: Text, result: Json, id: Optional[Json])
  case class Failure(code: Int, message: Text, data: Optional[Json] = Unset)

  // A specification-correct error response: the fault is carried in the `error` member (not
  // `result`), and `id` echoes the failing request's id — as an explicit JSON `null`, as the
  // specification requires, when the request was unparseable and its id unknowable.
  def failure(code: Int, message: Text, id: Optional[Json] = Unset): Json =
    Map
     ( t"jsonrpc" -> t"2.0".in[Json],
       t"error"   -> Failure(code, message).in[Json],
       t"id"      -> id.or(Json.ast(Json.Ast(Json.JsonNull))) )
    . in[Json]

  def notification(target: JsonRpc, method: Text, payload: Json): Promise[Unit] =

    target.put(Request("2.0", method, payload, Unset).in[Json])
    Promise[Unit]().tap(_.offer(()))

  // A request in flight: the promise its response will fulfil, and the correlation id it was sent
  // under. The id is retained because an error response cannot fulfil the promise, and must be
  // matched back to the caller by id instead.
  case class Pending(id: Text, promise: Promise[Json])

  def call(target: JsonRpc, method: Text, payload: Json): Pending =
    val uuid = Uuid().text
    val promise: Promise[Json] = Promise()
    promises.put(uuid, promise)

    target.put(Request("2.0", method, payload, uuid.in[Json]).in[Json])
    Pending(uuid, promise)

  def request(target: JsonRpc, method: Text, payload: Json): Promise[Json] =
    call(target, method, payload).promise

  // Awaits a request's response, raising the peer's fault as a `JsonRpcError` rather than
  // blocking forever on a promise an error response could never fulfil.
  def outcome(pending: Pending)(using Monitor, Tactic[JsonRpcError]): Json =
    import fulminate.errorDiagnostics.stackTracesDiagnostics

    try unsafely(pending.promise.await())
    catch case async: Async.Error => faults.remove(pending.id) match
      case null =>
        abort(JsonRpcError(JsonRpcError.Reason.Abandoned))

      case failure: Failure =>
        abort(JsonRpcError(JsonRpcError.Reason.Failed, failure.code, failure.message))

  def receive(id: Text, result: Json): Unit = claim(id).let(_.offer(result))

  def receiveFailure(id: Text, failure: Failure): Unit =
    faults.put(id, failure)
    claim(id).let(_.cancel())

  // Routes a response back to the caller awaiting it. A conformant peer answers with exactly one
  // of `result` and `error`, and the distinction matters: the second is a fault, not a value.
  def answer(json: Json): Unit =
    import dynamicJsonAccess.enabled
    import strategies.throwUnsafely

    // `try`/`catch` rather than `safely`: a decodable cannot thread the boundary tactic under
    // separation checking, and an unreadable member is simply absent.
    val id: Optional[Text] = try json.id.as[Text] catch case _: Exception => Unset
    val failure: Optional[Failure] = try json.error.as[Failure] catch case _: Exception => Unset
    val result: Optional[Json] = try json.result catch case _: Exception => Unset

    id.let: id =>
      failure.lay(result.let(receive(id, _)))(receiveFailure(id, _))


  def request(target: HttpUrl, method: Text, payload: Json)(using Monitor, Probate, Online)
  :   Promise[Json] =

    val uuid = Uuid().text
    val promise: Promise[Json] = Promise()
    promises.put(uuid, promise)
    import charEncoders.utf8Encoder
    import formatting.compactJsonFormatting
    import logging.silentLogging

    val request = Request("2.0", method, payload, uuid.in[Json]).in[Json]

    async:
      recover:
        case MediaTypeError(_, _)   => promise.cancel()
        case ConnectError(_)        => promise.cancel()
        case ParseError(_, _, _)    => promise.cancel()
        case HttpError(_, _)        => promise.cancel()
        case Async.Error(_)          => promise.cancel()

      . protect:
          promise.fulfill(target.submit(Http.Post)(request).receive[Json])

    promise


  def notification(target: HttpUrl, method: Text, payload: Json)
    ( using Monitor, Probate, Online )
  :   Promise[Unit] =

    import charEncoders.utf8Encoder
    import formatting.compactJsonFormatting
    import logging.silentLogging

    val request = Request("2.0", method, payload, Unset).in[Json]

    recover:
      case MediaTypeError(_, _) => ()
      case ConnectError(_)      => ()
      case HttpError(_, _)      => ()

    . protect:
        target.submit(Http.Post)(request).receive[Text]
        ()

    Promise[Unit]().tap(_.offer(()))

trait JsonRpc extends Original:
  private val channel: Relay[Json] = Relay()

  inline def client: Origin = proxy[Origin]

  // A caller-side proxy for an arbitrary interface, rather than for the fixed `Origin`. A protocol
  // whose method surface is split across several interfaces — to keep each generated dispatcher
  // within the per-class constant-pool limit — needs one proxy per interface, all sharing this
  // instance's single outgoing channel.
  inline def proxy[interface]: interface = ${obligatory.internal.client[interface]('this)}

  def put(json: Json): Unit =
    channel.put(json)

  // Each accessor drains the shared queue through a fresh single-owner view
  // (the audited bridge); use one or the other per instance, as before.
  def outgoing: Chain[Json] = Chain.from(channel.stream.records)

  def stream: Chain[Sse] =
    Chain.from(channel.stream.records).map: json =>
      Sse(data = List(json.encode))
