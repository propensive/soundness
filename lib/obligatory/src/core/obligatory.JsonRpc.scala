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
import gesticulate.*
import gossamer.*
import hieroglyph.*
import inimitable.*
import jacinta.*
import parasite.*
import prepositional.*
import revolution.*
import rudiments.*
import spectacular.*
import telekinesis.*
import turbulence.*
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

  inline def server
    [ interface: {Associable by Http.Request onto Http.Response, Streamable by Sse as streamable} ]
    ( using request: Http.Request )
    ( using Monitor, Codicil, Online )
  : Http.Response =
      send(serve[interface](_))

  def send
    [ interface: { Associable by Http.Request onto Http.Response, Streamable by Sse as streamable }]
    ( dispatch: interface => Json => Optional[Json] )
    ( using request: Http.Request )
    ( using Monitor, Codicil, Online )
  : Http.Response =
      import jsonPrinters.minimal
      import charEncoders.utf8
      import charDecoders.utf8
      import textSanitizers.skip

      given mcpSessionId: ("mcpSessionId" is Directive of Text) = identity(_)
      val sessionId: Text = request.headers.mcpSessionId.prim.or(Uuid().encode)
      val session: interface = interface.association(request)

      recover:
        case error: ParseError => interface.associate(session):
          Http.Response(Http.Ok):
            JsonRpc.error(-32700, t"Parse error: ${error.message}".show).json

        case error: JsonError => interface.associate(session):
          Http.Response(Http.Ok):
            JsonRpc.error(-32600, t"Invalid request: ${error.message}".show).json

      . within:
          val input = request.body().read[Json]
          try
            request.method match
              case Http.Get => interface.associate(session):
                Thread.sleep(2000)
                Http.Response(Http.Ok, connection = t"keep-alive", cacheControl = t"no-cache")
                  ( streamable.stream(session) )

              case Http.Post =>
                dispatch(session)(input).let: json =>
                  interface.associate(session)(Http.Response(Http.Ok)(json))

                . or:
                    interface.associate(session)(Http.Response(Http.Accepted)())
          catch
            case error: Throwable =>
              println(error.getMessage)
              error.printStackTrace()
              Http.Response(Http.Ok):
                JsonRpc.error(-32603, t"Internal error: ${error.toString}".show).json

  def error(code: Int, message: Text): Response =
    Response("2.0", Map(t"code" -> code.json, t"message" -> message.json).json, Unset)

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
