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
package cordillera

import scala.collection.mutable as scm

import anticipation.*
import contingency.*
import gossamer.*
import rudiments.*
import spectacular.*
import telekinesis.*
import vacuous.*

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
  def response(headerBlock: List[HpackEntry], body: Stream[Data])
  :   Http.Response raises Http2Error =

    var statusText: Optional[Text] = Unset
    val headers = scm.ListBuffer[Http.Header]()

    headerBlock.each: entry =>
      if entry.name == t":status" then statusText = entry.value
      else if !entry.name.starts(t":") then headers += Http.Header(entry.name, entry.value)

    val code: Int = statusText.let { text => safely(Integer.parseInt(text.s)).or(0) }.or(0)

    val status: Http.Status =
      Http.Status.unapply(code).optional.lest(Http2Error(Reason.Protocol(t"missing :status")))

    status(headers.to(List), Http.Body.Streaming(body))
