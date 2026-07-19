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

object HttpSession:
  // The sequential HTTP/1.1 form: one exchange at a time over the pinned,
  // kept-alive connection. `fetch` is an `update` method and its response
  // BORROWS the session — the streaming body lends the connection's wire
  // position — so separation checking forbids a second fetch while a previous
  // response is still being read; consuming the body (e.g. `memoize` or
  // `receive`) ends the borrow. Each framed body ends exactly at the next
  // response, which is what permits the next fetch.
  class Sequential private[telekinesis] (duplex: Duplex)(using Buffering) extends HttpSession:
    // The previous response's framing, kept so an unconsumed remainder can be
    // drained before the next exchange (the wire must reach the response
    // boundary). The borrow is also expressed in `fetch`'s result type;
    // enforcing it statically awaits further separation-checker support, so
    // the drain keeps the session safe meanwhile. A neutral carrier: the
    // framed stream is a capability, crossing the field as an `AnyRef`.
    private var pending: Optional[AnyRef] = Unset

    update def fetch(request: Http.Request)(using Tactic[ConnectError])
    :   Http.Response^{this, caps.any} =

      import ConnectError.Reason.*

      pending.let: stream =>
        try stream.asInstanceOf[(Stream[Data] over Credit)^].sweep { (_, _, _) => () } catch
          case error: ji.IOException => abort(ConnectError(Unknown))

      pending = Unset

      try duplex.send(Http.Request.serialize(request)) catch
        case error: ji.IOException => abort(ConnectError(Unknown))

      val cursor = Cursor[Data](duplex.source)

      val head: Http.Response.Head =
        try unsafely(Http.Response.parseHead(cursor)) catch
          case error: HttpResponseError => abort(ConnectError(Unknown))
          case error: ji.IOException    => abort(ConnectError(Unknown))

      // A `101` body is the upgraded protocol's unending stream; refuse it.
      if head.status == Http.SwitchingProtocols then abort(ConnectError(Unknown))

      val code: Int = head.status.code

      val chunked: Boolean = head.headers.exists: header =>
        header.key.lower == t"transfer-encoding" && header.value.lower.contains(t"chunked")

      val length: Optional[Int] =
        head.headers.filter(_.key.lower == t"content-length").prim.let(_.value)
        . lay(Unset: Optional[Int]): text =>
            safely(Integer.parseInt(text.s.trim.nn))

      val bodiless: Boolean =
        request.method == Http.Head || code == 204 || code == 304 ||
          (code >= 100 && code < 200)

      if bodiless then head.status(head.headers, Http.Body.Empty) else
        // The framed body is lent, not drained: it streams zero-copy off the
        // connection, stopping exactly at the frame boundary. The spring mints
        // the (stateful) framing once and re-lends the same endpoint, so each
        // further mint resumes where the previous reader stopped.
        val framed: (Stream[Data] over Credit)^ =
          if chunked then Http.Request.chunkedBody(cursor)
          else length.lay(streamOf(cursor))(Http.Request.fixedBody(cursor, _))

        // Register the framing for the pre-exchange drain, then lend it: the
        // spring re-lends the same (stateful) endpoint, so each further mint
        // resumes where the previous reader stopped.
        val framedRef: AnyRef = framed.asInstanceOf[AnyRef]
        pending = framedRef

        val spring: Spring[Data]^ = () =>
          framedRef.asInstanceOf[(Stream[Data] over Credit)^]

        head.status(head.headers, Http.Body.Flowing(spring))

  // The multiplexed HTTP/2 form, over a live `Http2Connection`, presented
  // behind the sequential contract (the URL session cannot know before the
  // handshake which protocol ALPN will select, so it promises only sequential
  // use; the `Http2.Endpoint` session lends the connection itself, whose
  // fetches may run concurrently). The body is drained within the fetch, so
  // the response is pure and the borrow ends immediately.
  class Multiplexed private[telekinesis] (connection: Http2Connection^, authority: Text)
  extends HttpSession:
    update def fetch(request: Http.Request)(using Tactic[ConnectError])
    :   Http.Response^{this, caps.any} =

      import ConnectError.Reason.*

      // RFC 7540 §8.1.2.2: connection-specific headers must not appear in h2.
      val headers: List[Http.Header] = request.textHeaders.filter: header =>
        header.key.lower != t"connection"

      // The body spring comes from a pure `Request`, so the seal only
      // discharges the field's capture-polymorphic declared type.
      val request2 = Http.Request
        ( request.method, request.version, request.host, request.target, headers,
          caps.unsafe.unsafeAssumePure(request.body) )

      // Distinct throwing tactics per error type (a single shared tactic would
      // alias across `fetch`'s two using-parameters, tripping separation).
      import strategies.throwUnsafely

      try
        val (_, response) = connection.fetch(request2, t"https", authority)
        val data: Data = response.body.stream.memoize

        val body: Http.Body = response.body match
          case Http.Body.Empty => Http.Body.Empty

          case _ =>
            if data.isEmpty then Http.Body.Empty else Http.Body.Fixed(data)

        response.status(response.textHeaders, body)

      catch
        case error: Http2Error  => abort(ConnectError(Unknown))
        case error: AsyncError  => abort(ConnectError(Unknown))
        case error: StreamError => abort(ConnectError(Unknown))

// An HTTP session: a single client connection to one origin, over which several
// requests are exchanged. The protocol is fixed for the session's lifetime —
// for `https`, by ALPN during the TLS handshake (a `Multiplexed` HTTP/2 session
// or a `Sequential` HTTP/1.1 one); for plaintext `http`, always sequential
// HTTP/1.1 with keep-alive. `fetch` requires exclusive access (`update`) and
// its response borrows the session, so an unconsumed streaming body blocks the
// next fetch at compile time.
sealed abstract class HttpSession extends caps.ExclusiveCapability, caps.Stateful:
  update def fetch(request: Http.Request)(using Tactic[ConnectError])
  :   Http.Response^{this, caps.any}
