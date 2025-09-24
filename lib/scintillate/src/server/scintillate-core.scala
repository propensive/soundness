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
┃    Soundness, version 0.41.0.                                                                    ┃
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
package scintillate

import anticipation.*
import contingency.*
import distillate.*
import fulminate.*
import gossamer.*
import parasite.*
import prepositional.*
import proscenium.*
import rudiments.*
import serpentine.*
import spectacular.*
import telekinesis.*
import urticose.*
import vacuous.*

package httpServers:
  given stdlib: [port <: (80 | 443 | 8080 | 8000)]
        => (Tactic[ServerError], Monitor, Codicil, HttpServerEvent is Loggable)
        =>  Http is Protocolic:

    type Transport = TcpPort of port
    type Self = Http
    type Server = Service
    type Request = HttpConnection
    type Response = Http.Response

    def server(port: TcpPort of port)(lambda: Request ?=> Response): Service =
      HttpServer(port.number, true).handle(lambda)

  given stdlibPublic: [port <: (80 | 443 | 8080 | 8000)]
        => (Tactic[ServerError], Monitor, Codicil, HttpServerEvent is Loggable)
        =>  Http is Protocolic:

    type Transport = TcpPort of port
    type Self = Http
    type Server = Service
    type Request = HttpConnection
    type Response = Http.Response

    def server(port: TcpPort of port)(lambda: Request ?=> Response): Service =
      HttpServer(port.number, false).handle(lambda)

def cookie(using request: Http.Request)(key: Text): Optional[Text] = request.textCookies.at(key)


def basicAuth(validate: (Text, Text) => Boolean, realm: Text)(response: => Http.Response)
   (using connection: HttpConnection)
: Http.Response raises AuthError =

    connection.headers.authorization match
      case List(Auth.Basic(username, password)) =>
        if validate(username, password) then response else Http.Response(Http.Forbidden)()

      case _ =>
        val auth = t"""Basic realm="$realm", charset="UTF-8""""

        Http.Response(Http.Unauthorized, wwwAuthenticate = auth)()


inline def request: Http.Request = infer[Http.Request]

given realm: Realm = realm"scintillate"

extension (request: Http.Request)
  def as[body: Acceptable]: body = body.accept(request)


  def path(using connection: HttpConnection)
  : HttpUrl raises PathError raises UrlError raises HostnameError =

      val scheme = if connection.tls then t"https" else t"http"
      t"$scheme://${request.host}${request.location}".decode[HttpUrl]
