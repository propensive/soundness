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
package octogenarian

import anticipation.*
import contingency.*
import fulminate.*
import gossamer.*
import monotonous.*
import rudiments.*
import symbolism.*
import telekinesis.*
import turbulence.*
import vacuous.*
import zephyrine.*

import alphabets.base64Standard

// Remote repositories reached over HTTP or HTTPS by git's smart protocol: the capability
// advertisement from `info/refs` and each command as a `POST` to the service, with protocol
// version 2 requested in the `Git-Protocol` header. Credentials in the URL's userinfo are sent as
// Basic authentication.
class SmartHttp()(using backend: Http.Backend) extends Transport:
  def accepts(url: Text): Boolean = url.starts(t"http://") || url.starts(t"https://")
  def connect(url: Text)(using Tactic[Git.Error]): Peer^ = Protocol(SmartHttp.Channel(url, backend))

object SmartHttp:
  import Git.Error.Reason as Reason

  // The URL without its userinfo, and the userinfo if there was one.
  private def split(url: Text): (Text, Optional[Text]) =
    val schemeEnd = url.s.indexOf("://")
    if schemeEnd < 0 then (url, Unset) else
      val authorityStart = schemeEnd + 3
      val pathStart = url.s.indexOf('/', authorityStart)
      val authorityEnd = if pathStart < 0 then url.length else pathStart
      val at = url.s.lastIndexOf('@', authorityEnd)

      if at < authorityStart then (url, Unset)
      else (t"${url.keep(authorityStart)}${url.skip(at + 1)}", url.s.substring(authorityStart, at).nn.tt)

  class Channel(url: Text, backend: Http.Backend) extends octogenarian.Channel:
    private val (base, userinfo) = split(if url.ends(t"/") then url.skip(1, Rtl) else url)

    private def headers(service: Text, extra: List[Http.Header]): List[Http.Header] =
      def basic(userinfo: Text): Http.Header =
        Http.Header(t"authorization", t"Basic ${Objects.bytes(userinfo).serialize[Base64]}")

      List(Http.Header(t"git-protocol", t"version=2")) + userinfo.let(List(_)).or(Nil).map(basic(_)) + extra

    private def request(target: Text, method: Http.Method, headers: List[Http.Header], body: Spring[Data])
      ( using tactic: Tactic[Git.Error] )
    :   Pktline.Reader =

      // A connection failure is the remote being unreachable.
      given connection: Tactic[Connect.Error] = tactic.contramap: error =>
        Git.Error(Reason.Unreachable(base), error.message.text)(using Diagnostics.omit)

      val response = backend.request(target, method, headers, body)

      response.status match
        case Http.Status.Unauthorized => abort(Git.Error(Reason.Unauthorized(base)))
        case Http.Status.Forbidden    => abort(Git.Error(Reason.Unauthorized(base)))
        case Http.Status.NotFound     => abort(Git.Error(Reason.NoRepository(base)))

        case status =>
          if status.category != Http.Status.Category.Successful
          then abort(Git.Error(Reason.Unexpected, t"the server answered ${status.code}"))

      Pktline.Reader.from(response.body.stream)

    // The advertisement, with the `# service=…` line and its flush, which the HTTP form adds in
    // front, skipped.
    def advertise(service: Text)(using Tactic[Git.Error]): Pktline.Reader =
      val reader = request(t"$base/info/refs?service=$service", Http.Get, headers(service, Nil), () => Http.emptyBody())
      val input = java.io.PushbackInputStream(reader.underlying, 65540)
      val probe = Pktline.Reader(input)

      probe.next() match
        case Pktline.Packet.Line(data) if Objects.text(data).starts(t"# service=") =>
          probe.next()
          probe

        case Pktline.Packet.Line(data) =>
          // Not the service line after all: give the packet back.
          input.unread(Pktline.frame(data).asInstanceOf[scala.Array[Byte]])
          probe

        case _ => probe

    def exchange(service: Text, request: Data)(using Tactic[Git.Error]): Pktline.Reader =
      val extra =
        List
          ( Http.Header(t"content-type", t"application/x-$service-request"),
            Http.Header(t"accept", t"application/x-$service-result") )

      this.request(t"$base/$service", Http.Post, headers(service, extra), () => request.stream)

    def close(): Unit = ()
