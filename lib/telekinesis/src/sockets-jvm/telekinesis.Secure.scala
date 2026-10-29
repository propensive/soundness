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

import anticipation.*
import coaxial.*
import contingency.*
import gigantism.*
import gossamer.*
import parasite.*
import rudiments.*
import spectacular.*
import turbulence.*
import urticose.*
import zephyrine.*

// The JVM's `https` exchange for `httpBackends.soundnessHttp`: ALPN offers `h2` then `http/1.1`
// during the handshake, and the peer's selection selects the driver — HTTP/2 framing (the `Http2`
// stack) or the HTTP/1.1 wire codec — over the same negotiated socket. One connection per request
// for now: the framed body is drained eagerly and the connection closed (TLS connection reuse is
// a later refinement).
private[telekinesis] object Secure:
  def exchange
    ( host:    Host,
      port:    Int,
      target:  Text,
      method:  Http.Method,
      headers: List[Http.Header],
      body:    Spring[Data] )
    ( using online: Online, options: Every[Socket.Option.Tcp], buffering: Buffering, tls: Tls )
    ( using Tactic[Connect.Error] )
  :   Http.Response =

    import Connect.Error.Reason.*

    val duplex: Duplex = secureConnect(host, port)

    duplex.alpnProtocol match
      case t"h2" =>
        import threads.virtualThreads
        import probates.cancelProbate

        // The `:authority` pseudo-header omits a default port, like browsers do.
        val authority: Text = if port == 443 then host.show else t"${host.show}:$port"

        // RFC 7540 §8.1.2.2: connection-specific headers must not appear in h2.
        val headers2: List[Http.Header] = headers.filter: header => header.key.lower != t"connection"

        val httpRequest = Http.Request(method, 1.1, host, target, headers2, body)

        // The connection's reader/writer daemons live under a request-scoped
        // supervisor: the eager body drain means nothing outlives this call.
        try
          unsafely:
            supervise:
              val connection = Http2.Connection(duplex)

              try
                connection.start()
                val (stream, response) = connection.fetch(httpRequest, t"https", authority)
                repackage(response, response.body.stream.memoize)

              finally connection.close()

        catch
          case error: Http2.Error      => abort(Connect.Error(Unknown))
          case error: Async.Error      => abort(Connect.Error(Unknown))
          case error: Truncation.Error => abort(Connect.Error(Unknown))

      case _ =>
        // One-shot HTTP/1.1 over the negotiated connection. `Connection: close`
        // is sent so a body without framing headers is still delimited.
        val headers2: List[Http.Header] =
          if headers.exists(_.key.lower == t"connection") then headers
          else Http.Header(t"connection", t"close") :: headers

        val httpRequest = Http.Request(method, 1.1, host, target, headers2, body)

        try sequentialFetch(duplex, httpRequest) finally duplex.close()

