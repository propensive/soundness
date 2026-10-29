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
package telekinesis

import java.io as ji
import java.net as jn
import java.net.http as jnh
import java.util.concurrent as juc
import javax.net.ssl as jns

import anticipation.*
import coaxial.*
import coaxial.socketBackends.javaBaseSockets
import contingency.*
import gigantism.*
import prepositional.*
import rudiments.*
import telekinesis.*
import turbulence.*
import urticose.*
import zephyrine.*

// Build the underlying Java client with redirect-following disabled — the
// redirect-following telekinesis given runs its own loop so it can honour
// `Http.Redirection` exactly. Java's `NORMAL` policy has its own hard-coded
// cap and would shadow the limit we summon. Java clients are immutable per
// SSL configuration, so one is built and cached for each distinct
// `TlsAcceptance` in use.
private val javaClients: juc.ConcurrentHashMap[TlsAcceptance, jnh.HttpClient] =
  juc.ConcurrentHashMap()

private def javaClient(using acceptance: TlsAcceptance): jnh.HttpClient =
  javaClients.computeIfAbsent(acceptance, { acceptance0 =>
    val (context, parameters) = acceptance0.nn.materialize()

    jnh.HttpClient.newBuilder.nn
    . followRedirects(jnh.HttpClient.Redirect.NEVER).nn
    . sslContext(context).nn
    . sslParameters(parameters).nn
    . build.nn }).nn

private def buildJavaRequest
  ( uri:         jn.URI,
    method:      Http.Method,
    textHeaders: List[Http.Header],
    bodyFn:      Spring[Data] )
:   jnh.HttpRequest =

  val request: jnh.HttpRequest.Builder = jnh.HttpRequest.newBuilder().nn.uri(uri).nn

  // The publisher pulls a fresh pull endpoint each time it is subscribed (the
  // JDK may re-subscribe on retry), draining it lazily through an
  // `InputStream` so the body is never held whole in memory.
  lazy val body =
    jnh.HttpRequest.BodyPublishers.ofInputStream { () => bodyFn().inputStream }.nn

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

private def send(request: jnh.HttpRequest)(using Tactic[Connect.Error], TlsAcceptance)
:   jnh.HttpResponse[ji.InputStream] =

  import Connect.Error.Reason.*, Ssl.Reason.*

  try javaClient.send(request, jnh.HttpResponse.BodyHandlers.ofInputStream()).nn catch
    case error: jns.SSLHandshakeException       => abort(Connect.Error(Ssl(Handshake)))
    case error: jns.SSLProtocolException        => abort(Connect.Error(Ssl(Protocol)))
    case error: jns.SSLPeerUnverifiedException  => abort(Connect.Error(Ssl(Peer)))
    case error: jns.SSLKeyException             => abort(Connect.Error(Ssl(Key)))
    case error: jn.UnknownHostException         => abort(Connect.Error(Dns))
    case error: jnh.HttpConnectTimeoutException => abort(Connect.Error(Timeout))

    case error: jn.ConnectException =>
      error.getMessage() match
        case "Connection refused"                    => abort(Connect.Error(Refused))
        case "Connection timed out"                  => abort(Connect.Error(Timeout))
        case "HTTP connect timed out"                => abort(Connect.Error(Timeout))
        case error                                   => abort(Connect.Error(Unknown))

    case error: ji.IOException =>
      abort(Connect.Error(Unknown))

private def buildResponse(response: jnh.HttpResponse[ji.InputStream])(using Tactic[Connect.Error])
:   Http.Response =

  val status: Http.Status = response.statusCode() match
    case Http.Status(status) => status
    case _                   => abort(Connect.Error(Connect.Error.Reason.Unknown))

  val headers: List[Http.Header] = response.headers.nn.map().nn.to[List].bind:
    (key, values) => values.to[List].map: value => Http.Header(key.tt, value.tt)

  val body = Http.Body.Flowing: () =>
    unsafely:
      summon[ji.InputStream is Streamable by Data over Credit].stream(response.body().nn)

  status(headers, body)

package httpBackends:
  // The JVM transport, using `java.net.http`. Other platforms (e.g. Scala.js)
  // or implementations (e.g. an HTTP/2 client) supply their own `Http.Backend`
  // given instead.
  given javaNetHttp: TlsAcceptance => Http.Backend = new Http.Backend:
    def request
      ( url:     Text,
        method:  Http.Method,
        headers: List[Http.Header],
        body:    Spring[Data] )
      ( using Tactic[Connect.Error] )
    :   Http.Response =

      buildResponse(send(buildJavaRequest(jn.URI.create(url.s).nn, method, headers, body)))

// A request is transmitted over a raw socket as its HTTP/1.1 wire form.
given requestTransmissible: Http.Request is Transmissible = Http.Request.serialize(_)

// Fetch from a Unix domain socket, speaking HTTP/1.1 directly over the socket
// (e.g. the Docker daemon's API). The request's `Host` is `localhost`.
given domainSocketFetchable: DomainSocket.Endpoint is Fetchable onto DomainSocket =
  new Fetchable:
    type Self = DomainSocket.Endpoint
    type Target = DomainSocket

    def target(endpoint: DomainSocket.Endpoint): DomainSocket = endpoint.socket
    def text(endpoint: DomainSocket.Endpoint): Text = endpoint.path
    def hostname(endpoint: DomainSocket.Endpoint): Host = Localhost

given domainSocketHttpClient: Tactic[Truncation.Error] => Http.Client onto DomainSocket =
  new Http.Client:
    type Target = DomainSocket

    def request(request: Http.Request, socket: DomainSocket)(using Http.Event is Loggable)
    :   Http.Response =

      unsafely:
        // Typed binding: `transmit` and `parse` are both overloaded (lazy-list and
        // endpoint forms), so the expected type picks the endpoint pair.
        val input: zephyrine.Stream[Data] over zephyrine.Credit = socket.transmit(request)
        Http.Response.parse(input)
