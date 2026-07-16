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
package scintillate

import java.io as ji

import com.sun.net.httpserver as csnh

import anticipation.*
import beneficence.*
import contingency.*
import distillate.*
import gossamer.*
import prepositional.*
import rudiments.*
import spectacular.*
import symbolism.*
import telekinesis.*
import turbulence.*
import urticose.*
import vacuous.*
import zephyrine.*

object HttpConnection:
  // A named SAM rather than a curried function type: the response the server writes
  // out may capture the live connection (a streamed body reading the request stream),
  // and a function type cannot take a `^` parameter (the `Spring` precedent). The
  // tactic is a using-parameter, not a curried context-function result — a value of
  // curried dependent context-function type is not yet supported — so nothing escapes
  // `apply`; `connection.respond(response)` resolves the ambient `StreamError` tactic.
  trait Respond:
    def apply(response: Http.Response^)(using Tactic[StreamError]): Unit

  // Explicit `using` evidence instead of `logs`/`raises` sugar: the `respond` closure built
  // in the body cannot cross the nested context-function results the sugar desugars to (the
  // stacked-raises convention; see rep/DECISIONS.md).
  def apply(exchange: csnh.HttpExchange)
    ( using (HttpServerEvent is Loggable)^, Tactic[HostnameError] )
  :   HttpConnection^ =

    val uri = exchange.getRequestURI.nn
    val query = Optional(uri.getQuery)
    val target = uri.getPath.nn.tt+query.let(t"?"+_.tt).or(t"")
    val method = exchange.getRequestMethod.nn.show.as[Http.Method]

    val headers: List[Http.Header] =
      exchange.getRequestHeaders.nn.asScala.view.mapValues(_.nn.asScala.to(List)).flatMap: pair =>
        pair.absolve match
          case (key, values) => values.map: value =>
            Http.Header(key, value.tt)

      . to(List)

    val version: Http.Version = Http.Version.parse(exchange.getProtocol.nn.tt)

    val host =
      Optional(uri.getHost).let(_.tt).or:
        exchange.getLocalAddress.nn.getAddress.nn.getCanonicalHostName.nn.tt

      . as[Hostname]

    lazy val in = exchange.getRequestBody.nn

    // The Source evidence closes over `unsafely`'s ThrowTactic, which is `caps.Unscoped`
    // (it throws in place, capturing nothing scoped), so it is truthfully sealed once here
    // rather than leaking out of the per-mint `unsafely` scope through the body thunk.
    val source: ji.InputStream is Streamable by Data over Credit =
      unsafely:
        caps.unsafe.unsafeAssumePure(summon[ji.InputStream is Streamable by Data over Credit])

    val request =
      Http.Request
        ( method      = method,
          version     = version,
          host        = host,
          target      = target,
          // Each mint reads on from the same live request stream — the
          // single-owner discipline (explicit `memoize` for re-reads). A read
          // failure throws, as the raw `InputStream` did before.
          body        = () => source.stream(in),
          textHeaders = headers )

    Log.fine(HttpServerEvent.Received(request))

    val port = Option(exchange.getRequestURI.nn.getPort).filter(_ > 0).getOrElse:
      exchange.getLocalAddress.nn.getPort

    val respond: Respond^ = new Respond:
      def apply(response: Http.Response^)(using Tactic[StreamError]): Unit =
        var chunked = false

        response.textHeaders.each:
          case Http.Header(key, value) =>
            if key.lower == t"transfer-encoding" && value.lower == t"chunked" then chunked = true

            exchange.getResponseHeaders.nn.add(key.s, value.s)

        val length = if chunked then 0 else response.body match
          case Http.Body.Empty        => -1
          case Http.Body.Fixed(data)  => data.length
          case Http.Body.Flowing(_)   => 0

        exchange.sendResponseHeaders(response.status.code, length)
        val responseBody = exchange.getResponseBody.nn

        var count: Int = 0

        response.body match
          case Http.Body.Fixed(data) =>
            try
              responseBody.write(data.mutable(using Unsafe))
              count += data.length
              responseBody.flush()
            catch case _: ji.IOException => abort(StreamError(count.b))

          case Http.Body.Flowing(source) =>
            val stream = source()

            def recur(): Unit = stream.refill(Credit(Long.MaxValue)) match
              case size: Int =>
                try
                  val window = stream.window(using Unsafe).asInstanceOf[Array[Byte]]
                  responseBody.write(window, stream.start, size)
                  count += size
                  responseBody.flush()
                catch case _: ji.IOException => abort(StreamError(count.b))

                stream.skip(size)
                recur()

              case _ => ()

            recur()

          case Http.Body.Empty =>
            try responseBody.flush()
            catch case _: ji.IOException => abort(StreamError(count.b))

        exchange.close()

    new HttpConnection(request, false, port, respond)

// An `HttpConnection` is a *capability*: it is one live, socket-backed exchange, holding the
// `respond` sink that writes to (and closes) the client connection. Its lifetime is the
// handler invocation that receives it. `Exclusive` because an exchange has a single owner
// and may be responded to only once.
class HttpConnection
  (     request: Http.Request^,
    val tls:     Boolean,
    val port:    Int,
    // The sink that writes the response to (and closes) the client connection. Its
    // `apply` takes `Http.Response^`: the handler's response may capture this very
    // connection — a streamed body reading the live request stream.
    val respond: HttpConnection.Respond^ )
extends Http.Request
  ( request.method,
    request.version,
    request.host,
    request.target,
    request.textHeaders,
    request.body ),
  Findable,
  caps.ExclusiveCapability
