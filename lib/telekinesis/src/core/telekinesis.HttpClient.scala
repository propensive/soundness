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
┃    Soundness, version 0.32.0.                                                                    ┃
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
package telekinesis

import anticipation.*
import coaxial.*
import contingency.*
import nettlesome.*
import prepositional.*
import proscenium.*
import rudiments.*
import spectacular.*
import turbulence.*
import vacuous.*

import java.net as jn
import java.net.http as jnh
import java.io as ji
import javax.net.ssl as jns

import language.dynamics

trait HttpClient:
  type Target
  def request(request: Http.Request, target: Target): Http.Response logs HttpEvent

object HttpClient:
  private lazy val client: jnh.HttpClient = jnh.HttpClient.newHttpClient().nn

  given domainSocket: Tactic[StreamError] => HttpClient onto DomainSocket = new HttpClient:
    type Target = DomainSocket

    def request(request: Http.Request, socket: DomainSocket): Http.Response logs HttpEvent =

      unsafely(Http.Response.parse(socket.transmit(request)))

  given http: Tactic[ConnectError] => Online => HttpClient:
    type Target = Origin["http" | "https"]


    def request(httpRequest: Http.Request, origin: Origin["http" | "https"])
    : Http.Response logs HttpEvent =

        val url = httpRequest.on(origin)

        Log.info(HttpEvent.Send(httpRequest.method, url, httpRequest.textHeaders))

        val request: jnh.HttpRequest.Builder =
          jnh.HttpRequest.newBuilder().nn.uri(jn.URI.create(url.show.s)).nn

        lazy val body = httpRequest.body() match
          case Stream()      =>
            jnh.HttpRequest.BodyPublishers.noBody.nn

          case Stream(bytes) =>
            jnh.HttpRequest.BodyPublishers.ofByteArray(bytes.mutable(using Unsafe))

          case stream =>
            jnh.HttpRequest.BodyPublishers.ofInputStream { () => stream.inputStream }

        httpRequest.method match
          case Http.Delete  => request.DELETE().nn
          case Http.Get     => request.GET().nn
          case Http.Post    => request.POST(body).nn
          case Http.Put     => request.PUT(body).nn
          case Http.Connect => request.method("CONNECT", body).nn
          case Http.Head    => request.method("HEAD", body).nn
          case Http.Options => request.method("OPTIONS", body).nn
          case Http.Patch   => request.method("PATCH", body).nn
          case Http.Trace   => request.method("TRACE", body).nn

        request.header("User-Agent", "Telekinesis/1.0.0")

        httpRequest.textHeaders.each:
          case Http.Header(key, value) => request.header(key.s, value.s)

        val response: jnh.HttpResponse[ji.InputStream] =
          import ConnectError.Reason.*, Ssl.Reason.*

          val client = HttpClient.client

          try client.send(request.build(), jnh.HttpResponse.BodyHandlers.ofInputStream()).nn catch
            case error: jns.SSLHandshakeException       => abort(ConnectError(Ssl(Handshake)))
            case error: jns.SSLProtocolException        => abort(ConnectError(Ssl(Protocol)))
            case error: jns.SSLPeerUnverifiedException  => abort(ConnectError(Ssl(Peer)))
            case error: jns.SSLKeyException             => abort(ConnectError(Ssl(Key)))
            case error: jn.UnknownHostException         => abort(ConnectError(Dns))
            case error: jnh.HttpConnectTimeoutException => abort(ConnectError(Timeout))
            case error: jn.ConnectException             => error.getMessage() match
              case "Connection refused"                    => abort(ConnectError(Refused))
              case "Connection timed out"                  => abort(ConnectError(Timeout))
              case "HTTP connect timed out"                => abort(ConnectError(Timeout))
              case error                                   => abort(ConnectError(Unknown))
            case error: ji.IOException                  => abort(ConnectError(Unknown))

        val status2: Http.Status = Http.Status.unapply(response.statusCode()).getOrElse:
          abort(ConnectError(ConnectError.Reason.Unknown))

        val headers2: List[Http.Header] = response.headers.nn.map().nn.asScala.to(List).flatMap:
          (key, values) => values.asScala.map { value => Http.Header(key.tt, value.tt) }

        Http.Response.make(status2, headers2, unsafely(response.body().nn.stream[Bytes]))
