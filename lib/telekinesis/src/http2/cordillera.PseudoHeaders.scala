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
package cordillera

import scala.collection.mutable as scm

import anticipation.*
import contingency.*
import distillate.*
import gossamer.*
import rudiments.*
import spectacular.*
import telekinesis.*
import urticose.*
import vacuous.*
import zephyrine.*

import Http2Error.Reason

// Translates between telekinesis's transport-agnostic HTTP model and HTTP/2's
// HPACK header blocks. On the request side the pseudo-headers (`:method`, `:scheme`,
// `:authority`, `:path`) are emitted first, in order, ahead of the regular headers
// (RFC 7540 §8.1.2.1); on the response side `:status` becomes an `Http.Status` and
// the remaining fields become the response headers.
object PseudoHeaders:
  // Build the HPACK header list for a request. `scheme`/`authority` come from the
  // connection context, since an `Http.Request` carries them separately.
  def request(request: Http.Request, scheme: Text, authority: Text): List[HpackEntry] =
    val pseudo =
      List
        ( HpackEntry(t":method", request.method.show),
          HpackEntry(t":scheme", scheme),
          HpackEntry(t":authority", authority),
          HpackEntry(t":path", request.target) )

    val regular = request.textHeaders.map: header =>
      HpackEntry(header.key.lower, header.value)

    pseudo ++ regular

  // Reconstruct an `Http.Response` from a decoded HEADERS block and the body stream.
  // `:status` selects the `Http.Status`; other fields become response headers.
  def response(headerBlock: List[HpackEntry], body: LazyList[Data])
  :   Http.Response raises Http2Error =

    var statusText: Optional[Text] = Unset
    val headers = scm.ListBuffer[Http.Header]()

    headerBlock.each: entry =>
      if entry.name == t":status" then statusText = entry.value
      else if !entry.name.starts(t":") then headers += Http.Header(entry.name, entry.value)

    val code: Int = statusText.let { text => safely(Integer.parseInt(text.s)).or(0) }.or(0)

    val status: Http.Status =
      Http.Status.unapply(code).optional.lest(Http2Error(Reason.Protocol(t"missing :status")))

    status(headers.to(List), Http.Body.Flowing(() => zephyrine.Stream(body.iterator)))

  // Reconstruct an `Http.Request` from a decoded request HEADERS block and the
  // body spring: `:method`/`:path` select the method and target, `:authority`
  // the host (any port stripped, as for an HTTP/1.1 `Host` header); other
  // pseudo-headers are dropped and the remaining fields become request headers.
  // The inverse of `request`, used by the server role.
  // Plain using-parameter, de-sugared from `raises`: a context-function result
  // may not hide the capturing request.
  def requestOf(headerBlock: List[HpackEntry], consume body: Spring[Data]^)
    ( using Tactic[Http2Error] )
  :   Http.Request^ =

    var methodText: Optional[Text] = Unset
    var pathText: Optional[Text] = Unset
    var authorityText: Optional[Text] = Unset
    val headers = scm.ListBuffer[Http.Header]()

    headerBlock.each: entry =>
      if entry.name == t":method" then methodText = entry.value
      else if entry.name == t":path" then pathText = entry.value
      else if entry.name == t":authority" then authorityText = entry.value
      else if !entry.name.starts(t":") then headers += Http.Header(entry.name, entry.value)

    val authority: Text =
      authorityText.lest(Http2Error(Reason.Protocol(t"missing :authority")))

    val host: Host =
      safely(authority.as[Host]).or:
        safely(authority.cut(t":").prim.or(authority).as[Host]).or:
          abort(Http2Error(Reason.Protocol(t"bad :authority")))

    val target: Text = pathText.lest(Http2Error(Reason.Protocol(t"missing :path")))
    val method: Http.Method = methodText.lest(Http2Error(Reason.Protocol(t"missing :method"))).as

    Http.Request(method, 2.0, host, target, headers.to(List), body)

  // Build the HPACK header list for a response: `:status` first, then the
  // regular headers lowercased, with connection-specific headers stripped
  // (RFC 7540 §8.1.2.2). The inverse of `response`, used by the server role.
  def entries(response: Http.Response^): List[HpackEntry] =
    val forbidden: List[Text] =
      List(t"connection", t"keep-alive", t"transfer-encoding", t"upgrade", t"proxy-connection")

    val regular = response.textHeaders.map: header =>
      HpackEntry(header.key.lower, header.value)

    . filter: entry =>
        !forbidden.contains(entry.name)

    HpackEntry(t":status", response.status.code.show) :: regular
