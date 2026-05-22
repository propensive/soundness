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
package scintillate

import anticipation.*
import contingency.*
import digression.*
import gesticulate.*
import gossamer.*
import hellenism.*
import hieroglyph.*
import parasite.*
import prepositional.*
import rudiments.*
import serpentine.*
import spectacular.*
import telekinesis.*
import turbulence.*
import urticose.*
import vacuous.*

package httpServers:
  given stdlib: [port <: (80 | 443 | 8080 | 8000)]
  =>  ( Tactic[ServerError], Monitor, Codicil, HttpServerEvent is Loggable )
  =>  WebserverErrorPage
  =>  Http is Protocolic:

    type Transport = TcpPort of port
    type Self = Http
    type Server = Service
    type Request = HttpConnection
    type Response = Http.Response

    def server(port: TcpPort of port)(lambda: Request ?=> Response): Service =
      HttpServer(port.number, true).handle(lambda)


  given stdlibPublic: [port <: (80 | 443 | 8080 | 8000)]
  =>  ( Tactic[ServerError], Monitor, Codicil, HttpServerEvent is Loggable )
  =>  WebserverErrorPage
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
  ( using connection: HttpConnection )
:   Http.Response raises AuthError =

  connection.headers.authorization match
    case List(Auth.Basic(username, password)) =>
      if validate(username, password) then response else Http.Response(Http.Forbidden)()

    case _ =>
      val auth = t"""Basic realm="$realm", charset="UTF-8""""

      Http.Response(Http.Unauthorized, wwwAuthenticate = auth)()


inline def request: Http.Request = infer[Http.Request]

extension (request: Http.Request)
  def as[body: Acceptable]: body = body.accept(request)

package webserverErrorPages:
  given minimal: WebserverErrorPage = (request, throwable) =>
    import hieroglyph.charEncoders.utf8
    Http.Response(Unfulfilled(t"An error occurred which prevented the request from completing."))

  private def prefix(using Classloader): Data = cp"/scintillate/error.pre.html".read[Data]
  private def postfix(using Classloader): Data = cp"/scintillate/error.post.html".read[Data]

  given standard: Classloader => WebserverErrorPage = (throwable, request) =>
    Http.Response(Unfulfilled(Stream(prefix, postfix).ascribe(media"text/html")))

  given stackTraces: Classloader => WebserverErrorPage = (throwable, request) =>
    import charEncoders.utf8

    val stack = t"<pre>${throwable.stackTrace}</pre>".read[Data]
    Http.Response(Unfulfilled(Stream(prefix, stack, postfix).ascribe(media"text/html")))
