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


object JsonRpc:
  private val promises: scm.HashMap[Text | Int, Promise[Json]] = scm.HashMap()

  inline def serve[interface](interface: interface): Json => Optional[Json] =
    ${obligatory.internal.dispatcher[interface]('interface)}

  case class Request(jsonrpc: Text, method: Text, params: Json, id: Optional[Json])
  case class Response(jsonrpc: Text, result: Json, id: Optional[Json])

  def error(code: Int, message: Text): Response =
    Response("2.0", Map(t"code" -> code.json, t"message" -> message.json).json, Unset)

  def notification(target: JsonRpc, method: Text, payload: Json): Promise[Unit] =

    target.put(Request("2.0", method, payload, Unset).json)
    Promise[Unit]().tap(_.offer(()))

  def request(target: JsonRpc, method: Text, payload: Json): Promise[Json] =
    val uuid = Uuid().text
    val promise: Promise[Json] = Promise()
    promises(uuid) = promise

    target.put(Request("2.0", method, payload, uuid.json).json)
    promise

  def receive(id: Text, result: Json): Unit = promises.at(id).let(_.offer(result))


  def request(target: HttpUrl, method: Text, payload: Json)(using Monitor, Probate, Online)
  :   Promise[Json] =

    val uuid = Uuid().text
    val promise: Promise[Json] = Promise()
    promises(uuid) = promise
    import charEncoders.utf8Encoder
    import printers.jsonMinimalPrinter
    import logging.silent

    val request = Request("2.0", method, payload, uuid.json).json

    async:
      whereas:
        case MediaTypeError(_, _)   => promise.cancel()
        case ConnectError(_)        => promise.cancel()
        case ParseError(_, _, _)    => promise.cancel()
        case HttpError(_, _)        => promise.cancel()
        case AsyncError(_)          => promise.cancel()

      . recover:
          promise.fulfill(target.submit(Http.Post)(request).receive[Json])

    promise


  def notification(target: HttpUrl, method: Text, payload: Json)
    ( using Monitor, Probate, Online )
  :   Promise[Unit] =

    import charEncoders.utf8Encoder
    import printers.jsonMinimalPrinter
    import logging.silent

    val request = Request("2.0", method, payload, Unset).json

    whereas:
      case MediaTypeError(_, _) => ()
      case ConnectError(_)      => ()
      case HttpError(_, _)      => ()

    . recover:
        target.submit(Http.Post)(request).receive[Text]
        ()

    Promise[Unit]().tap(_.offer(()))

trait JsonRpc extends Original:
  private val channel: Spool[Json] = Spool()

  inline def client: Origin = ${obligatory.internal.client[Origin]('this)}

  def put(json: Json): Unit =
    channel.put(json)

  def outgoing: Stream[Json] = channel.stream

  def stream: Stream[Sse] =
    channel.stream.map: json =>
      Sse(data = List(json.encode))
