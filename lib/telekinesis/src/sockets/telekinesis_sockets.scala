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
import java.util.concurrent as juc

import anticipation.*
import coaxial.*
import contingency.*
import distillate.*
import gigantism.*
import gossamer.*
import prepositional.*
import rudiments.*
import spectacular.*
import turbulence.*
import urticose.*
import vacuous.*
import zephyrine.*

package httpBackends:
  // The transport speaking telekinesis's own wire codecs over coaxial's sockets, on every
  // platform with a `Socket.Backend`. Plaintext `http` is HTTP/1.1 over a raw TCP socket, with
  // keep-alive: when a response's extent was explicit (a framed body, or none) and neither side
  // asked to close, its connection returns to a per-origin pool for the next request. `https`
  // is the platform's (`Secure.exchange`): on the JVM, ALPN offers `h2` then `http/1.1` during
  // the TLS handshake and the exchange is driven by the HTTP/2 or HTTP/1.1 driver the peer
  // selected, over the same socket; on Scala Native, one HTTP/1.1 exchange over coaxial's
  // OpenSSL connection. Framed bodies are drained eagerly before a connection is surrendered or
  // closed, so responses do not stream yet, and `101` upgrades are not supported (the upgraded
  // stream would never end).
  given soundnessHttp: (online: Online)
  =>  ( backend: Socket.Backend, options: Every[Socket.Option.Tcp], buffering: Buffering, tls: Tls )
  =>  Http.Backend = new Http.Backend:

    def request
      ( url:     Text,
        method:  Http.Method,
        headers: List[Http.Header],
        body:    Spring[Data] )
      ( using Tactic[Connect.Error] )
    :   Http.Response =

      import Connect.Error.Reason.*

      val parsed: HttpUrl = safely(url.as[HttpUrl]).or(abort(Connect.Error(Unknown)))
      val scheme: Text = parsed.scheme.name

      if scheme != t"http" && scheme != t"https" then abort(Connect.Error(Unknown))

      val secure: Boolean = scheme == t"https"
      val defaultPort: Int = if secure then 443 else 80
      val host: Host = parsed.host.or(abort(Connect.Error(Dns)))
      val port: Int = parsed.authority.lay(defaultPort)(_.port.or(defaultPort))
      val tcpPort: Tcp.Port = safely(Port[Tcp](port)).or(abort(Connect.Error(Unknown)))
      val origin: Text = t"${host.show}:$port"

      // An origin-form URL has an empty path; its request target is `/`.
      val target: Text =
        if parsed.location == t"" then t"/${parsed.requestTarget}" else parsed.requestTarget

      if secure then Secure.exchange(host, port, target, method, headers, body)
      else plaintextExchange(host, tcpPort, origin, target, method, headers, body)


// Idle kept-alive connections held by the `soundnessHttp` backend, keyed on
// `host:port`. Bounded per origin: an overflow connection is closed rather
// than pooled. A pooled socket the server has since closed is discovered on
// use and replaced by a single retry on a fresh connection.
private val idleConnections: juc.ConcurrentHashMap[Text, juc.ConcurrentLinkedQueue[Duplex]] =
  juc.ConcurrentHashMap()

private val maxIdlePerOrigin: Int = 8

// Drain a parsed response's framed body and repackage it as a pure, fixed
// response — the shape the backend returns while responses do not yet stream.
private[telekinesis] def repackage(response: Http.Response, data: Data): Http.Response =
  val body: Http.Body = response.body match
    case Http.Body.Empty => Http.Body.Empty
    case _               => if data.length == 0 then Http.Body.Empty else Http.Body.Fixed(data)

  response.status(response.textHeaders, body)

// The pooled, kept-alive plaintext HTTP/1.1 exchange (see `httpBackends.soundnessHttp`).
private def plaintextExchange
  ( host:    Host,
    tcpPort: Tcp.Port,
    origin:  Text,
    target:  Text,
    method:  Http.Method,
    headers: List[Http.Header],
    body:    Spring[Data] )
  ( using backend: Socket.Backend, options: Every[Socket.Option.Tcp], buffering: Buffering )
  ( using Tactic[Connect.Error] )
:   Http.Response =

  import Connect.Error.Reason.*

  val httpRequest = Http.Request(method, 1.1, host, target, headers, body)

  def connect(): Duplex =
    try backend.duplexTcp(Endpoint(host.show, tcpPort), Unset, options.values.to(List)) catch
      case error: jn.UnknownHostException => abort(Connect.Error(Dns))

      case error: jn.ConnectException =>
        error.getMessage() match
          case "Connection refused"   => abort(Connect.Error(Refused))
          case "Connection timed out" => abort(Connect.Error(Timeout))
          case _                      => abort(Connect.Error(Unknown))

      case error: ji.IOException => abort(Connect.Error(Unknown))

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
      abort(Connect.Error(Unknown))

    val data: Data = unsafely(response.body.stream.memoize)

    // The connection can be kept alive only when the body's extent was
    // explicit — a framed body, or none at all — and neither side asked to
    // close: an unframed body was delimited by the server closing.
    val framed: Boolean =
      response.body == Http.Body.Empty ||
        response.textHeaders.exists: header =>
          val key = header.key.lower

          key == t"content-length" ||
            (key == t"transfer-encoding" && header.value.lower.contains(t"chunked"))

    val serverClose: Boolean = response.textHeaders.exists: header =>
      header.key.lower == t"connection" && header.value.lower.contains(t"close")

    val reusable: Boolean =
      framed && !serverClose && response.version == 1.1 &&
        !headers.exists(_.key.lower == t"connection")

    if reusable then surrender(duplex) else duplex.close()

    repackage(response, data)

  def fresh(): Http.Response =
    val duplex = connect()

    try attempt(duplex) catch
      case error: Http.Response.Error    => duplex.close(); abort(Connect.Error(Unknown))
      case error: Truncation.Error       => duplex.close(); abort(Connect.Error(Unknown))
      case error: ji.IOException         => duplex.close(); abort(Connect.Error(Unknown))

  borrow() match
    case null => fresh()

    case duplex: Duplex =>
      try attempt(duplex) catch
        case error: Http.Response.Error    => duplex.close(); fresh()
        case error: Truncation.Error       => duplex.close(); fresh()
        case error: ji.IOException         => duplex.close(); fresh()


// One sequential HTTP/1.1 exchange over an open connection: send the request,
// parse the framed response, drain its body, and leave the connection open,
// positioned at the next response. `101` upgrades are refused (the upgraded
// stream would never end); failures map onto `Connect.Error`.
private[telekinesis] def sequentialFetch(duplex: Duplex, request: Http.Request)
  ( using Buffering )
  ( using Tactic[Connect.Error] )
:   Http.Response =

  import Connect.Error.Reason.*

  try
    duplex.send(Http.Request.serialize(request))

    // Typed binding: `parse` is overloaded (lazy-list and endpoint forms),
    // so the expected type picks the endpoint pair.
    val input: zephyrine.Stream[Data] over zephyrine.Credit = duplex.source

    val response: Http.Response =
      unsafely(Http.Response.parse(input, request.method == Http.Head))

    if response.status == Http.SwitchingProtocols then abort(Connect.Error(Unknown))

    repackage(response, unsafely(response.body.stream.memoize))

  catch
    case error: Http.Response.Error    => abort(Connect.Error(Unknown))
    case error: Truncation.Error       => abort(Connect.Error(Unknown))
    case error: ji.IOException         => abort(Connect.Error(Unknown))

