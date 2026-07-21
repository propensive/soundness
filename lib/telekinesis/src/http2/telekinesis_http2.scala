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
import javax.net.ssl as jns

import anticipation.*
import coaxial.*
import contingency.*
import cordillera.*
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

// A named instance class rather than an anonymous given: an anonymous subclass
// would freshen the capability types in its inferred `Session` member.
class UrlSessionable[url <: HttpUrl]
  ( using online:       Online,
          backend:      SocketBackend,
          options:      Every[SocketOption.Tcp],
          buffering:    Buffering,
          tls:          Tls,
          connectError: Tactic[ConnectError] )
extends Sessionable:
  type Self = url
  type Session = HttpSession^{caps.any}

  def session[result](target: url)(lambda: (session: Session) ?=> result): result =
    import ConnectError.Reason.*

    val scheme: Text = target.scheme.name
    val secure: Boolean = scheme == t"https"

    if scheme != t"http" && scheme != t"https" then abort(ConnectError(Unknown))

    val defaultPort: Int = if secure then 443 else 80
    val host: Host = target.host.or(abort(ConnectError(Dns)))
    val port: Int = target.authority.lay(defaultPort)(_.port.or(defaultPort))

    if !secure then
      val tcpPort: TcpPort = safely(Port[Tcp](port)).or(abort(ConnectError(Unknown)))

      val duplex: Duplex =
        try backend.duplexTcp(Endpoint(host.show, tcpPort), Unset, options.values) catch
          case error: ji.IOException => abort(ConnectError(Unknown))

      try lambda(using HttpSession.Sequential(duplex)) finally duplex.close()

    else
      import threading.virtualThreading
      import probates.cancelProbate

      val duplex: Duplex = secureConnect(host, port)

      try
        duplex.alpnProtocol match
          case t"h2" =>
            // The `:authority` pseudo-header omits a default port, like browsers do.
            val authority: Text = if port == 443 then host.show else t"${host.show}:$port"

            // The connection's reader/writer daemons live under a session-scoped
            // supervisor: nothing outlives the lambda.
            try
              unsafely:
                supervise:
                  val connection = Http2Connection(duplex)

                  try
                    connection.start()
                    lambda(using HttpSession.Multiplexed(connection, authority))

                  finally connection.close()

            catch
              case error: Http2Error => abort(ConnectError(Unknown))
              case error: AsyncError => abort(ConnectError(Unknown))

          case _ =>
            lambda(using HttpSession.Sequential(duplex))

      finally duplex.close()

// A session on an HTTP or HTTPS URL: the connection to the URL's origin is
// opened once, lent to the lambda as an `HttpSession`, and closed when the
// scope ends. Bounded like `Fetchable.httpUrl` so `url"https://..."` literals
// (whose types are scheme-refined subtypes of `HttpUrl`) resolve the instance.
given httpUrlSessionable: [url <: HttpUrl]
=>  (online: Online)
=>  ( backend:      SocketBackend,
      options:      Every[SocketOption.Tcp],
      buffering:    Buffering,
      tls:          Tls,
      connectError: Tactic[ConnectError] )
=>  (UrlSessionable[url]^{online, connectError, caps.any}) =
  UrlSessionable[url]()

// Open the TLS connection for an `https` exchange, offering `h2` and `http/1.1`
// by ALPN, and mapping handshake and connection failures onto `ConnectError`.
private[telekinesis] def secureConnect(host: Host, port: Int)
  ( using online: Online, options: Every[SocketOption.Tcp], tls: Tls )
  ( using Tactic[ConnectError] )
:   Duplex =

  import ConnectError.Reason.*, Ssl.Reason.*

  val alpn: Tls = Tls(tls.context, tls.verify, List(t"h2", t"http/1.1"), tls.versions)

  try
    SecureEndpoint.connectable(using online)(using options, alpn)
    . connect(SecureEndpoint(host.show, port), Unset)
  catch
    case error: jns.SSLHandshakeException      => abort(ConnectError(Ssl(Handshake)))
    case error: jns.SSLProtocolException       => abort(ConnectError(Ssl(Protocol)))
    case error: jns.SSLPeerUnverifiedException => abort(ConnectError(Ssl(Peer)))
    case error: jns.SSLKeyException            => abort(ConnectError(Ssl(Key)))
    case error: jn.UnknownHostException        => abort(ConnectError(Dns))

    case error: jn.ConnectException =>
      error.getMessage() match
        case "Connection refused"   => abort(ConnectError(Refused))
        case "Connection timed out" => abort(ConnectError(Timeout))
        case _                      => abort(ConnectError(Unknown))

    case error: ji.IOException => abort(ConnectError(Unknown))
