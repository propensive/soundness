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
import scala.collection.mutable as scm
import scala.quoted.*

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
  private val promises: scm.HashMap[Text | Int, Promise[Json]] = scm.HashMap()

  inline def serve[interface](interface: interface): Json => Optional[Json] =
    ${obligatory.internal.dispatcher[interface]('interface)}

  // The set of JSON-RPC method names an interface declares (its `@rpc` members). Used to route a
  // request across several `serve` dispatchers when a single interface's dispatcher would be too
  // large to compile into one class.
  inline def methods[interface]: Set[Text] = ${obligatory.internal.methodNames[interface]}

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

  def request(target: JsonRpc, method: Text, payload: Json): Promise[Json] =
    val uuid = Uuid().text
    val promise: Promise[Json] = Promise()
    promises(uuid) = promise

    target.put(Request("2.0", method, payload, uuid.in[Json]).in[Json])
    promise

  def receive(id: Text, result: Json): Unit = promises.at(id).let(_.offer(result))


  def request(target: HttpUrl, method: Text, payload: Json)(using Monitor, Probate, Online)
  :   Promise[Json] =

    val uuid = Uuid().text
    val promise: Promise[Json] = Promise()
    promises(uuid) = promise
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
        case AsyncError(_)          => promise.cancel()

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

  inline def client: Origin = ${obligatory.internal.client[Origin]('this)}

  def put(json: Json): Unit =
    channel.put(json)

  // Each accessor drains the shared queue through a fresh single-owner view
  // (the audited bridge); use one or the other per instance, as before.
  def outgoing: Chain[Json] = Chain.from(channel.stream.records)

  def stream: Chain[Sse] =
    Chain.from(channel.stream.records).map: json =>
      Sse(data = List(json.encode))
