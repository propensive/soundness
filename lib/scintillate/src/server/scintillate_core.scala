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
package scintillate

import scala.caps

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
  // A named class rather than four anonymous given instances: the anonymous form freshens
  // the inferred type members with capture variables, which then fail to conform to the
  // declared refinement (the galilei `FileOpenable` finding). Honestly tracked: the instance
  // retains its tactic and monitor (rep/DECISIONS.md ruling: server givens are capabilities).
  private class HttpProtocolic[port <: (80 | 443 | 8080 | 8000)](native: Boolean, local: Boolean)
    ( using tactic:    Tactic[ServerError],
            monitor:   Monitor,
            probate:   Probate,
            loggable:  HttpServer.Event is Loggable,
            errorPage: WebserverErrorPage )
  extends Protocolic:
    type Transport = Tcp.Port of port
    type Self = Http
    type Server = Service
    // Alias with the explicit capture: a bare capability-class alias decorates the trait's
    // `Request ?=>` parameter on one side of the override only.
    type Request = HttpConnection^
    type Response = Http.Response

    def server(port: Tcp.Port of port)(lambda: (request: Request) ?=> Response^{request})
    :   Server^ =
      if native then SocketServer(port.number, local).handle(lambda)
      else HttpServer(port.number, local).handle(lambda)

  // Public: the soundness bundle's hand-written forwarder givens name this alias.
  type HttpServerFor[port] =
    Http is Protocolic
      { type Transport = Tcp.Port of port
        type Request = HttpConnection^
        type Response = Http.Response
        type Server = Service }

  given stdlibHttpServer: [port <: (80 | 443 | 8080 | 8000)]
  =>  ( tactic: Tactic[ServerError], monitor: Monitor, probate: Probate )
  =>  ( loggable: HttpServer.Event is Loggable, errorPage: WebserverErrorPage )
  =>  ((HttpServerFor[port])^{tactic, monitor, caps.any}) =
    HttpProtocolic[port](false, true)

  given stdlibPublicHttpServer: [port <: (80 | 443 | 8080 | 8000)]
  =>  ( tactic: Tactic[ServerError], monitor: Monitor, probate: Probate )
  =>  ( loggable: HttpServer.Event is Loggable, errorPage: WebserverErrorPage )
  =>  ((HttpServerFor[port])^{tactic, monitor, caps.any}) =
    HttpProtocolic[port](false, false)

  given nativeHttpServer: [port <: (80 | 443 | 8080 | 8000)]
  =>  ( tactic: Tactic[ServerError], monitor: Monitor, probate: Probate )
  =>  ( loggable: HttpServer.Event is Loggable, errorPage: WebserverErrorPage )
  =>  ((HttpServerFor[port])^{tactic, monitor, caps.any}) =
    HttpProtocolic[port](true, true)

  given nativePublicHttpServer: [port <: (80 | 443 | 8080 | 8000)]
  =>  ( tactic: Tactic[ServerError], monitor: Monitor, probate: Probate )
  =>  ( loggable: HttpServer.Event is Loggable, errorPage: WebserverErrorPage )
  =>  ((HttpServerFor[port])^{tactic, monitor, caps.any}) =
    HttpProtocolic[port](true, false)

def cookie(using request: Http.Request)(key: Text): Optional[Text] = request.textCookies(key)

def basicAuth(validate: (Text, Text) => Boolean, realm: Text)(response: => Http.Response)
  ( using connection: HttpConnection )
  ( using Tactic[AuthError] )
:   Http.Response =

  connection.headers.authorization match
    case List(Auth.Basic(username, password)) =>
      if validate(username, password) then response else Http.Response(Http.Forbidden)()

    case _ =>
      val auth = t"""Basic realm="$realm", charset="UTF-8""""

      Http.Response(Http.Unauthorized, wwwAuthenticate = auth)()


inline def request: Http.Request^ = infer[Http.Request]

extension (request: Http.Request)
  @unexported
  def as[body: Acceptable]: body = body.accept(request)

package webserverErrorPages:
  given minimalErrorPage: WebserverErrorPage = (request, throwable) =>
    import hieroglyph.charEncoders.utf8Encoder
    Http.Response(Unfulfilled(t"An error occurred which prevented the request from completing."))

  private def prefix(using Classloader): Data = cp"/scintillate/error.pre.html".read[Data]
  private def postfix(using Classloader): Data = cp"/scintillate/error.post.html".read[Data]

  given standardErrorPage: Classloader => WebserverErrorPage = (throwable, request) =>
    // Direct `Content`, not `.ascribe`: the inline re-elaboration freshens the chunk type.
    Http.Response(Unfulfilled(Content(media"text/html", Chain[Data](prefix, postfix))))

  given stackTracesErrorPage: Classloader => WebserverErrorPage = (throwable, request) =>
    import charEncoders.utf8Encoder

    val stack = t"<pre>${throwable.stackTrace}</pre>".read[Data]
    Http.Response(Unfulfilled(Content(media"text/html", Chain[Data](prefix, stack, postfix))))
