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
┃    Soundness, version 0.63.0.                                                                    ┃
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
import coaxial.socketBackends.virtualMachine
import contingency.*
import cordillera.*
import distillate.*
import gigantism.*
import gossamer.*
import parasite.*
import prepositional.*
import rudiments.*
import spectacular.*
import turbulence.*
import urticose.*
import vacuous.*
import zephyrine.*

// Build the underlying Java client with redirect-following disabled — the
// redirect-following telekinesis given runs its own loop so it can honour
// `HttpRedirection` exactly. Java's `NORMAL` policy has its own hard-coded
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

private def send(request: jnh.HttpRequest)(using Tactic[ConnectError], TlsAcceptance)
:   jnh.HttpResponse[ji.InputStream] =

  import ConnectError.Reason.*, Ssl.Reason.*

  try javaClient.send(request, jnh.HttpResponse.BodyHandlers.ofInputStream()).nn catch
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

  val headers: List[Http.Header] = response.headers.nn.map().nn.asScala.to(List).bind:
    (key, values) => values.asScala.map: value => Http.Header(key.tt, value.tt)

  val body = Http.Body.Flowing: () =>
    unsafely:
      summon[ji.InputStream is Streamable by Data over Credit].stream(response.body().nn)

  status(headers, body)

package httpBackends:
  // The JVM transport, using `java.net.http`. Other platforms (e.g. Scala.js)
  // or implementations (e.g. an HTTP/2 client) supply their own `Http.Backend`
  // given instead.
  given virtualMachine: TlsAcceptance => Http.Backend = new Http.Backend:
    def request
      ( url:     Text,
        method:  Http.Method,
        headers: List[Http.Header],
        body:    Spring[Data] )
      ( using Tactic[ConnectError] )
    :   Http.Response =

      buildResponse(send(buildJavaRequest(jn.URI.create(url.s).nn, method, headers, body)))

  // A native transport speaking telekinesis's own wire codecs instead of
  // `java.net.http`. Plaintext `http` is HTTP/1.1 over a raw TCP socket, with
  // keep-alive: when a response's extent was explicit (a framed body, or none)
  // and neither side asked to close, its connection returns to a per-origin
  // pool for the next request. For `https`, ALPN offers `h2` then `http/1.1`
  // during the TLS handshake, and the exchange is driven by the HTTP/2 or
  // HTTP/1.1 driver the peer selected — over the same socket. Framed bodies
  // are drained eagerly before a connection is surrendered or closed, so
  // responses do not stream yet, and `101` upgrades are not supported (the
  // upgraded stream would never end).
  given native: (online: Online)
  =>  (backend: SocketBackend, options: Every[SocketOption.Tcp], buffering: Buffering, tls: Tls)
  =>  Http.Backend = new Http.Backend:

    def request
      ( url:     Text,
        method:  Http.Method,
        headers: List[Http.Header],
        body:    Spring[Data] )
      ( using Tactic[ConnectError] )
    :   Http.Response =

      import ConnectError.Reason.*

      val parsed: HttpUrl = safely(url.as[HttpUrl]).or(abort(ConnectError(Unknown)))
      val scheme: Text = parsed.scheme.name

      if scheme != t"http" && scheme != t"https" then abort(ConnectError(Unknown))

      val secure: Boolean = scheme == t"https"
      val defaultPort: Int = if secure then 443 else 80
      val host: Host = parsed.host.or(abort(ConnectError(Dns)))
      val port: Int = parsed.authority.lay(defaultPort)(_.port.or(defaultPort))
      val tcpPort: TcpPort = safely(Port[Tcp](port)).or(abort(ConnectError(Unknown)))
      val origin: Text = t"${host.show}:$port"

      // An origin-form URL has an empty path; its request target is `/`.
      val target: Text =
        if parsed.location == t"" then t"/${parsed.requestTarget}" else parsed.requestTarget

      if secure then httpsExchange(host, port, target, method, headers, body)
      else plaintextExchange(host, tcpPort, origin, target, method, headers, body)

// Idle kept-alive connections held by the `native` backend, keyed on
// `host:port`. Bounded per origin: an overflow connection is closed rather
// than pooled. A pooled socket the server has since closed is discovered on
// use and replaced by a single retry on a fresh connection.
private val idleConnections: juc.ConcurrentHashMap[Text, juc.ConcurrentLinkedQueue[Duplex]] =
  juc.ConcurrentHashMap()

private val maxIdlePerOrigin: Int = 8

// Drain a parsed response's framed body and repackage it as a pure, fixed
// response — the shape the backend returns while responses do not yet stream.
private def repackage(response: Http.Response, data: Data): Http.Response =
  val body: Http.Body = response.body match
    case Http.Body.Empty => Http.Body.Empty
    case _               => if data.isEmpty then Http.Body.Empty else Http.Body.Fixed(data)

  response.status(response.textHeaders, body)

// The pooled, kept-alive plaintext HTTP/1.1 exchange (see `httpBackends.native`).
private def plaintextExchange
  ( host:    Host,
    tcpPort: TcpPort,
    origin:  Text,
    target:  Text,
    method:  Http.Method,
    headers: List[Http.Header],
    body:    Spring[Data] )
  ( using backend: SocketBackend, options: Every[SocketOption.Tcp], buffering: Buffering )
  ( using Tactic[ConnectError] )
:   Http.Response =

  import ConnectError.Reason.*

  val httpRequest = Http.Request(method, 1.1, host, target, headers, body)

  def connect(): Duplex =
    try backend.duplexTcp(Endpoint(host.show, tcpPort), Unset, List.of(options.values)) catch
      case error: jn.UnknownHostException => abort(ConnectError(Dns))

      case error: jn.ConnectException =>
        error.getMessage() match
          case "Connection refused"   => abort(ConnectError(Refused))
          case "Connection timed out" => abort(ConnectError(Timeout))
          case _                      => abort(ConnectError(Unknown))

      case error: ji.IOException => abort(ConnectError(Unknown))

  def borrow(): Duplex | Null =
    val queue = idleConnections.get(origin)
    if queue == null then null else queue.poll()

  def surrender(duplex: Duplex): Unit =
    val queue =
      idleConnections
      . computeIfAbsent(origin, { _ => juc.ConcurrentLinkedQueue[Duplex]() }).nn

    if queue.size < maxIdlePerOrigin then queue.add(duplex) else duplex.close()

  // One request/response exchange over `duplex`. Failures are thrown (the
  // caller translates them, retrying once if the connection came from the
  // pool); on success, the connection is surrendered for reuse or closed.
  def attempt(duplex: Duplex): Http.Response =
    duplex.send(Http.Request.serialize(httpRequest))

    // Typed binding: `parse` is overloaded (lazy-list and endpoint forms),
    // so the expected type picks the endpoint pair.
    val input: zephyrine.Stream[Data] over zephyrine.Credit = duplex.source
    val response: Http.Response = unsafely(Http.Response.parse(input, method == Http.Head))

    // A `101` body is the upgraded protocol's unending stream: draining it
    // would block forever, so refuse it here (aborting rather than throwing,
    // so a pooled connection is not pointlessly retried).
    if response.status == Http.SwitchingProtocols then
      duplex.close()
      abort(ConnectError(Unknown))

    val data: Data = unsafely(response.body.stream.memoize)

    // The connection can be kept alive only when the body's extent was
    // explicit — a framed body, or none at all — and neither side asked to
    // close: an unframed body was delimited by the server closing.
    val framed: Boolean =
      response.body == Http.Body.Empty
      || response.textHeaders.exists: header =>
           val key = header.key.lower

           key == t"content-length"
           || (key == t"transfer-encoding" && header.value.lower.contains(t"chunked"))

    val serverClose: Boolean = response.textHeaders.exists: header =>
      header.key.lower == t"connection" && header.value.lower.contains(t"close")

    val reusable: Boolean =
      framed && !serverClose && response.version == 1.1
      && !headers.exists(_.key.lower == t"connection")

    if reusable then surrender(duplex) else duplex.close()

    repackage(response, data)

  def fresh(): Http.Response =
    val duplex = connect()

    try attempt(duplex) catch
      case error: HttpResponseError => duplex.close(); abort(ConnectError(Unknown))
      case error: StreamError       => duplex.close(); abort(ConnectError(Unknown))
      case error: ji.IOException    => duplex.close(); abort(ConnectError(Unknown))

  borrow() match
    case null => fresh()

    case duplex: Duplex =>
      try attempt(duplex) catch
        case error: HttpResponseError => duplex.close(); fresh()
        case error: StreamError       => duplex.close(); fresh()
        case error: ji.IOException    => duplex.close(); fresh()

// The TLS exchange: ALPN offers `h2` then `http/1.1` during the handshake, and
// the peer's selection selects the driver — HTTP/2 framing (the `cordillera`
// stack) or the HTTP/1.1 wire codec — over the same negotiated socket. One
// connection per request for now: the framed body is drained eagerly and the
// connection closed (TLS connection reuse is a later refinement).
private def httpsExchange
  ( host:    Host,
    port:    Int,
    target:  Text,
    method:  Http.Method,
    headers: List[Http.Header],
    body:    Spring[Data] )
  ( using online: Online, options: Every[SocketOption.Tcp], buffering: Buffering, tls: Tls )
  ( using Tactic[ConnectError] )
:   Http.Response =

  import ConnectError.Reason.*

  val duplex: Duplex = secureConnect(host, port)

  duplex.alpnProtocol match
    case t"h2" =>
      import threading.virtualThreading
      import probates.cancelProbate

      // The `:authority` pseudo-header omits a default port, like browsers do.
      val authority: Text = if port == 443 then host.show else t"${host.show}:$port"

      // RFC 7540 §8.1.2.2: connection-specific headers must not appear in h2.
      val headers2: List[Http.Header] = headers.filter: header =>
        header.key.lower != t"connection"

      val httpRequest = Http.Request(method, 1.1, host, target, headers2, body)

      // The connection's reader/writer daemons live under a request-scoped
      // supervisor: the eager body drain means nothing outlives this call.
      try
        unsafely:
          supervise:
            val connection = Http2Connection(duplex)

            try
              connection.start()
              val (stream, response) = connection.fetch(httpRequest, t"https", authority)
              repackage(response, response.body.stream.memoize)

            finally connection.close()

      catch
        case error: Http2Error  => abort(ConnectError(Unknown))
        case error: AsyncError  => abort(ConnectError(Unknown))
        case error: StreamError => abort(ConnectError(Unknown))

    case _ =>
      // One-shot HTTP/1.1 over the negotiated connection. `Connection: close`
      // is sent so a body without framing headers is still delimited.
      val headers2: List[Http.Header] =
        if headers.exists(_.key.lower == t"connection") then headers
        else Http.Header(t"connection", t"close") :: headers

      val httpRequest = Http.Request(method, 1.1, host, target, headers2, body)

      try sequentialFetch(duplex, httpRequest) finally duplex.close()

// One sequential HTTP/1.1 exchange over an open connection: send the request,
// parse the framed response, drain its body, and leave the connection open,
// positioned at the next response. `101` upgrades are refused (the upgraded
// stream would never end); failures map onto `ConnectError`.
private def sequentialFetch(duplex: Duplex, request: Http.Request)
  ( using Buffering )
  ( using Tactic[ConnectError] )
:   Http.Response =

  import ConnectError.Reason.*

  try
    duplex.send(Http.Request.serialize(request))

    // Typed binding: `parse` is overloaded (lazy-list and endpoint forms),
    // so the expected type picks the endpoint pair.
    val input: zephyrine.Stream[Data] over zephyrine.Credit = duplex.source

    val response: Http.Response =
      unsafely(Http.Response.parse(input, request.method == Http.Head))

    if response.status == Http.SwitchingProtocols then abort(ConnectError(Unknown))

    repackage(response, unsafely(response.body.stream.memoize))

  catch
    case error: HttpResponseError => abort(ConnectError(Unknown))
    case error: StreamError       => abort(ConnectError(Unknown))
    case error: ji.IOException    => abort(ConnectError(Unknown))

// A request is transmitted over a raw socket as its HTTP/1.1 wire form.
given requestTransmissible: Http.Request is Transmissible = Http.Request.serialize(_)

// Fetch from a Unix domain socket, speaking HTTP/1.1 directly over the socket
// (e.g. the Docker daemon's API). The request's `Host` is `localhost`.
given domainSocketFetchable: DomainSocketEndpoint is Fetchable onto DomainSocket =
  new Fetchable:
    type Self = DomainSocketEndpoint
    type Target = DomainSocket

    def target(endpoint: DomainSocketEndpoint): DomainSocket = endpoint.socket
    def text(endpoint: DomainSocketEndpoint): Text = endpoint.path
    def hostname(endpoint: DomainSocketEndpoint): Host = Localhost

given domainSocketHttpClient: Tactic[StreamError] => HttpClient onto DomainSocket =
  new HttpClient:
    type Target = DomainSocket

    def request(request: Http.Request, socket: DomainSocket)(using HttpEvent is Loggable)
    :   Http.Response =
      unsafely:
        // Typed binding: `transmit` and `parse` are both overloaded (lazy-list and
        // endpoint forms), so the expected type picks the endpoint pair.
        val input: zephyrine.Stream[Data] over zephyrine.Credit = socket.transmit(request)
        Http.Response.parse(input)
