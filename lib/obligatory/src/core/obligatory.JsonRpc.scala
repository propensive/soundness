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
┃    Soundness, version 0.53.0.                                                                    ┃
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

import scala.collection.mutable as scm

import anticipation.*
import contingency.*
import distillate.*
import eucalyptus.*
import fulminate.*
import gossamer.*
import hieroglyph.*
import inimitable.*
import jacinta.*
import parasite.*
import prepositional.*
import revolution.*
import rudiments.*
import telekinesis.*
import urticose.*
import vacuous.*
import zephyrine.*

import scala.annotation.*
import scala.quoted.*

import errorDiagnostics.stackTraces

object JsonRpc:
  private val promises: scm.HashMap[Text | Int, Promise[Json]] = scm.HashMap()

  inline def serve[interface](interface: interface): Json => Optional[Json] =
    ${Obligatory.dispatcher[interface]('interface)}

  case class Request(jsonrpc: Text, method: Text, params: Json, id: Optional[Json])
  case class Response(jsonrpc: Text, result: Json, id: Optional[Json])

  def error(code: Int, message: Text): Response =
    Response("2.0", Map(t"code" -> code.json, t"message" -> message.json).json, Unset)

  // def receive(json: Json): Unit raises RpcError =
  //   mitigate:
  //     case error: JsonError => RpcError()
  //     case error: UuidError => RpcError()

  //   . within:
  //       val response = json.as[Response]
  //       response.id.let(_.decode[Uuid]).let: uuid =>
  //         promises.at(uuid).let: promise =>
  //           safely(promise.fulfill(response.result))


  def request(url: HttpUrl, method: Text, payload: Json)(using Monitor, Codicil, Online): Promise[Json] =
    val uuid = Uuid().text
    val promise: Promise[Json] = Promise()
    promises(uuid) = promise
    import charEncoders.utf8
    import jsonPrinters.minimal
    import logging.silent

    unsafely:
      async:
        promise.fulfill:
          unsafely:
            url.submit(Http.Post)(Request("2.0", method, payload, uuid.json).json).receive[Json]

    promise
