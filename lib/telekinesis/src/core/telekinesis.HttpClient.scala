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
package telekinesis

import language.dynamics

import java.io as ji
import java.net as jn
import java.net.http as jnh
import javax.net.ssl as jns

import scala.util.NotGiven

import anticipation.*
import coaxial.*
import contingency.*
import gossamer.*
import prepositional.*
import rudiments.*
import spectacular.*
import turbulence.*
import urticose.*
import vacuous.*

object HttpClient:
  // Build the underlying Java client with redirect-following disabled — the
  // redirect-following telekinesis given runs its own loop so it can honour
  // `HttpRedirection` exactly. Java's `NORMAL` policy has its own hard-coded
  // cap and would shadow the limit we summon.
  private lazy val client: jnh.HttpClient =
    jnh.HttpClient.newBuilder.nn.followRedirects(jnh.HttpClient.Redirect.NEVER).nn.build.nn

  given domainSocket: Tactic[StreamError] => HttpClient onto DomainSocket = new HttpClient:
    type Target = DomainSocket

    def request(request: Http.Request, socket: DomainSocket): Http.Response logs HttpEvent =

      unsafely(Http.Response.parse(socket.transmit(request)))

  private def buildJavaRequest
    ( uri:         jn.URI,
      method:      Http.Method,
      textHeaders: List[Http.Header],
      bodyFn:      () => Stream[Data] )
  :   jnh.HttpRequest =

    val request: jnh.HttpRequest.Builder = jnh.HttpRequest.newBuilder().nn.uri(uri).nn

    lazy val body = bodyFn() match
      case Stream() => jnh.HttpRequest.BodyPublishers.noBody.nn

      case Stream(bytes) =>
        jnh.HttpRequest.BodyPublishers.ofByteArray(bytes.mutable(using Unsafe))

      case stream =>
        jnh.HttpRequest.BodyPublishers.ofInputStream: () => stream.inputStream

    method match
      case Http.Delete  => request.DELETE().nn
      case Http.Get     => request.GET().nn
      case Http.Post    => request.POST(body).nn
      case Http.Put     => request.PUT(body).nn
      case Http.Connect => request.method("CONNECT", body).nn
      case Http.Head    => request.method("HEAD", body).nn
      case Http.Options => request.method("OPTIONS", body).nn
      case Http.Patch   => request.method("PATCH", body).nn
      case Http.Trace   => request.method("TRACE", body).nn

    request.header("User-Agent", "internal/1.0.0")

    textHeaders.each:
      case Http.Header(key, value) => request.header(key.s, value.s)

    request.build().nn

  private def send(request: jnh.HttpRequest)(using Tactic[ConnectError])
  :   jnh.HttpResponse[ji.InputStream] =

    import ConnectError.Reason.*, Ssl.Reason.*

    try client.send(request, jnh.HttpResponse.BodyHandlers.ofInputStream()).nn catch
      case error: jns.SSLHandshakeException       => abort(ConnectError(Ssl(Handshake)))
      case error: jns.SSLProtocolException        => abort(ConnectError(Ssl(Protocol)))
      case error: jns.SSLPeerUnverifiedException  => abort(ConnectError(Ssl(Peer)))
      case error: jns.SSLKeyException             => abort(ConnectError(Ssl(Key)))
      case error: jn.UnknownHostException         => abort(ConnectError(Dns))
      case error: jnh.HttpConnectTimeoutException => abort(ConnectError(Timeout))

      case error: jn.ConnectException =>
        error.getMessage() match
          case "Connection refused"                    => abort(ConnectError(Refused))
          case "Connection timed out"                  => abort(ConnectError(Timeout))
          case "HTTP connect timed out"                => abort(ConnectError(Timeout))
          case error                                   => abort(ConnectError(Unknown))

      case error: ji.IOException =>
        abort(ConnectError(Unknown))

  private def buildResponse(response: jnh.HttpResponse[ji.InputStream])(using Tactic[ConnectError])
  :   Http.Response =

    val status: Http.Status = Http.Status.unapply(response.statusCode()).getOrElse:
      abort(ConnectError(ConnectError.Reason.Unknown))

    val headers: List[Http.Header] = response.headers.nn.map().nn.asScala.to(List).flatMap:
      (key, values) => values.asScala.map: value => Http.Header(key.tt, value.tt)

    status(headers, Http.Body.Streaming(unsafely(response.body().nn.stream[Data])))

  // 301/302/303 historically downgrade the method to GET and drop the body;
  // 307/308 preserve both. Matches Java's `Redirect.NORMAL` and the WHATWG
  // fetch spec.
  private def redirectMethod(code: Int, original: Http.Method): Http.Method = code match
    case 301 | 302 | 303 => Http.Get
    case _               => original

  private def isRedirect(code: Int): Boolean = code match
    case 301 | 302 | 303 | 307 | 308 => true
    case _                           => false

  // The JVM default transport, using `java.net.http`. Other platforms (e.g.
  // Scala.js) or implementations (e.g. an HTTP/2 client) supply their own
  // `Http.Backend` given instead.
  val javaBackend: Http.Backend = new Http.Backend:
    def request
      ( url:     Text,
        method:  Http.Method,
        headers: List[Http.Header],
        body:    () => Stream[Data] )
      ( using Tactic[ConnectError] )
    :   Http.Response =

      buildResponse(send(buildJavaRequest(jn.URI.create(url.s).nn, method, headers, body)))

  given httpStrict: Tactic[ConnectError]
  =>  Online
  =>  Redirects.Disabled
  =>  ( backend: Http.Backend )
  =>  HttpClient:
    type Target = Origin["http" | "https"]

    def request(httpRequest: Http.Request, origin: Origin["http" | "https"])
    :   Http.Response logs HttpEvent =

      val url = httpRequest.on(origin)
      Log.info(HttpEvent.Send(httpRequest.method, url, httpRequest.textHeaders))

      backend.request(url.show, httpRequest.method, httpRequest.textHeaders, httpRequest.body)

  given http: Tactic[ConnectError]
  =>  Online
  =>  NotGiven[Redirects.Disabled]
  =>  ( redirection: HttpRedirection )
  =>  ( backend: Http.Backend )
  =>  HttpClient:
    type Target = Origin["http" | "https"]

    def request(httpRequest: Http.Request, origin: Origin["http" | "https"])
    :   Http.Response logs HttpEvent =

      val url = httpRequest.on(origin)
      Log.info(HttpEvent.Send(httpRequest.method, url, httpRequest.textHeaders))

      def loop(uri: jn.URI, method: Http.Method, bodyFn: () => Stream[Data], remaining: Int)
      :   Http.Response =

        val response = backend.request(uri.toString.tt, method, httpRequest.textHeaders, bodyFn)
        val code = response.status.code

        if !isRedirect(code) || remaining <= 0 then response else
          response.textHeaders.find(_.key.lower == t"location") match
            case None =>
              response

            case Some(header) =>
              // Drain the discarded intermediate body to free its connection.
              safely(response.body.stream.each { _ => () })

              val nextUri = uri.resolve(jn.URI.create(header.value.s).nn).nn
              val nextMethod = redirectMethod(code, method)

              val nextBody: () => Stream[Data] =
                if nextMethod == method then bodyFn else () => Stream()

              loop(nextUri, nextMethod, nextBody, remaining - 1)

      loop(jn.URI.create(url.show.s).nn, httpRequest.method, httpRequest.body, redirection.value)

trait HttpClient extends Targetable:
  def request(request: Http.Request, target: Target): Http.Response logs HttpEvent
