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

import java.io as ji

import com.sun.net.httpserver as csnh

import anticipation.*
import beneficence.*
import contingency.*
import denominative.*
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

private[scintillate] object Connections:
  // A named SAM rather than a curried function type: the response the server writes
  // Explicit `using` evidence instead of `logs`/`raises` sugar: the `respond` closure built
  // in the body cannot cross the nested context-function results the sugar desugars to (the
  // stacked-raises convention; see rep/DECISIONS.md).
  def apply(exchange: csnh.HttpExchange)
    ( using (Httpd.Event is Loggable)^, Tactic[Hostname.Error] )
  :   Http.Connection^ =

    val uri = exchange.getRequestURI.nn
    val query = Optional(uri.getQuery)
    val target = uri.getPath.nn.tt+query.let(t"?"+_.tt).or(t"")
    val method = exchange.getRequestMethod.nn.show.as[Http.Method]

    val headers: List[Http.Header] =
      List.of:
        exchange.getRequestHeaders.nn.asScala.view.mapValues(_.nn.asScala.toList).flatMap: pair =>
          pair.absolve match
            case (key, values) => values.map: value =>
              Http.Header(key, value.tt)

        . toList

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

    Log.fine(Httpd.Event.Received(request))

    val port = Option(exchange.getRequestURI.nn.getPort).filter(_ > 0).getOrElse:
      exchange.getLocalAddress.nn.getPort

    val respond: Http.Connection.Respond^ = new Http.Connection.Respond:
      def apply(response: Http.Response^)(using Tactic[Truncation.Error]): Unit =
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
              responseBody.write(Array.unsafeJvm(data))
              count += data.length
              responseBody.flush()
            catch case _: ji.IOException => abort(Truncation.Error(count.b))

          case Http.Body.Flowing(source) =>
            val stream = source()

            def recur(): Unit = stream.refill(Credit(Long.MaxValue)) match
              case size: Int =>
                try
                  stream.lend: region =>
                    range =>
                      val interval: Interval = range

                      responseBody.write(unsafely(region.raw.asInstanceOf[scala.Array[Byte]]),
                          interval.start.n0, interval.size)

                  count += size
                  responseBody.flush()
                catch case _: ji.IOException => abort(Truncation.Error(count.b))

                stream.skip(size)
                // Tail re-entry over the same single-owner stream.
                scala.caps.unsafe.unsafeAssumeSeparate(recur())

              case _ => ()

            recur()

          case Http.Body.Empty =>
            try responseBody.flush()
            catch case _: ji.IOException => abort(Truncation.Error(count.b))

        exchange.close()

    new Http.Connection(request, false, port, respond)
