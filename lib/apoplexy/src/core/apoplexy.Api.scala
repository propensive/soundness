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
package apoplexy

import language.dynamics

import anticipation.*
import contingency.*
import denominative.*
import distillate.*
import gossamer.*
import hellenism.*
import hieroglyph.*
import jacinta.*
import prepositional.*
import rudiments.*
import spectacular.*
import telekinesis.*
import turbulence.*
import urticose.*
import vacuous.*
import xylophone.*
import zephyrine.*

object Api:
  // Root constructor: `Api(cp"/spec.json")` reads the spec resource's `Locus`,
  // validates the spec at compile time, and returns `Api at "/"` carrying the
  // spec source so navigation macros can re-read it.
  transparent inline def apply(inline resource: Resource): Any = ${Apoplexy.root('resource)}

  def make(apiRequest: Api.Request): Api = new Api:
    def request: Api.Request = apiRequest

  // The runtime send (invoked by the code `.call` emits): assemble the URL (base +
  // substituted path + query), serialize the request body to bytes in its wire
  // format, set the `content-type` (from the body) and `accept` (the response
  // format the caller passes) headers, and dispatch through the telekinesis
  // `HttpClient` — which uses whichever `Http.Backend` is in scope. The body
  // printers/encoders are fixed internal defaults (minimal JSON, UTF-8), resolved
  // here, so callers never supply them.
  def send(request: Api.Request, accept: Text)
    ( using Online,
            HttpEvent is Loggable,
            Tactic[ConnectError],
            Tactic[UrlError] )
    ( using client: HttpClient onto Origin["http" | "https"] )
  :   Http.Response =

    import jacinta.formatting.compactJsonFormatting
    import charEncoders.utf8Encoder

    val substituted =
      request.substitutions.foldLeft(request.path): (path, entry) =>
        path.sub(t"{${entry(0)}}", entry(1))

    val full =
      if request.query.nil then t"${request.base}$substituted"
      else
        val parameters =
          request.query.map: (key, value) =>
            t"${key.urlEncode}=${value.urlEncode}"

        t"${request.base}$substituted?${parameters.join(t"&")}"

    val url = full.as[HttpUrl]

    val empty: Spring[Data] = () => Iterator.empty[Data].stream

    val (contentType, body): (Optional[Text], Spring[Data]) = request.body match
      case Api.Body.Empty       => (Unset, empty)
      case Api.Body.Json(value) => (t"application/json", () => value.show.in[Data].stream)
      case Api.Body.Xml(value)  => (t"application/xml", () => value.show.in[Data].stream)

    val contentTypeHeader: List[Http.Header] = contentType.lay(Nil): media =>
      List(Http.Header(t"content-type", media))

    val headers: List[Http.Header] = Http.Header(t"accept", accept) :: contentTypeHeader

    val httpRequest =
      Http.Request(request.method, 1.1, url.host.vouch, url.requestTarget, headers, body)

    client.request(httpRequest, url.origin)

  // A request body already encoded to its wire-format AST. The spec's media type
  // for the operation decides which case is built (in the `invoke` macro); `.call`
  // serializes it to bytes with the matching printer.
  enum Body derives CanEqual:
    case Empty
    case Json(value: jacinta.Json)
    case Xml(value: xylophone.Xml)

  // The runtime description of a navigated/invoked call. `base` is the server
  // URL from the spec; `path` is the still-templated path; `substitutions`
  // binds path templates to concrete values; `query` is the query string;
  // `body` is the encoded request body (`Body.Empty` when there is none).
  case class Request
    ( method:        Http.Method,
      base:          Text,
      path:          Text,
      substitutions: Map[Text, Text]    = Map(),
      query:         List[(Text, Text)] = Nil,
      body:          Api.Body           = Api.Body.Empty )

  // The result of invoking an endpoint. Its refined type records `Result` (a
  // JSON-pointer to the 2xx response schema) and `Form` (the spec source),
  // which `call` reads to check a target type for conformance against the schema.
  object Response:
    def make(apiRequest: Api.Request): Api.Response = new Api.Response:
      def request: Api.Request = apiRequest

  trait Response extends Transportive:
    type Result
    type Form
    // type Transport (the wire format) inherited from Transportive
    def request: Api.Request

    // Performs the request and decodes the response as `value`. The empty
    // parentheses mark the side effect. A bare `.call()` leaves `value`
    // unconstrained, so `value is Defaulting to Unit` resolves it to `Unit`:
    // perform the request, check for a 2xx status, and discard the body — the
    // natural default for `delete` and other no-content endpoints.
    //
    // The `inline` match on `this.Transport` selects the JSON or XML arm by the
    // spec-decided wire format, so only the matching format's givens are demanded
    // at a concrete call site. The macro first checks `value` against the response
    // schema; the send + decode run in *inline* code, so `value` is concrete when
    // the `Conformant` (and hence the jacinta/xylophone `Decodable`) is summoned —
    // which is what lets `List[T]` and other collections resolve their decoders.
    transparent inline def call[value]()
      ( using erased default: value is Defaulting to Unit )
      ( using online:   Online,
              loggable: HttpEvent is Loggable,
              connect:  Tactic[ConnectError],
              urlError: Tactic[UrlError],
              client:   HttpClient onto Origin["http" | "https"] )
    :   value =

      Apoplexy.check[value](this)

      def dispatch(accept: Text): Http.Response =
        Api.send(request, accept)(using online, loggable, connect, urlError)(using client)

      inline compiletime.erasedValue[this.Transport] match
        case _: jacinta.Json =>
          val response = dispatch(t"application/json")
          compiletime.summonInline[(value is Conformant) over jacinta.Json].read(response)

        case _: xylophone.Xml =>
          val response = dispatch(t"application/xml")
          compiletime.summonInline[(value is Conformant) over xylophone.Xml].read(response)

trait Api extends Dynamic, Locative, Transportive:
  def request: Api.Request

  transparent inline def selectDynamic(field: String): Any =
    ${Apoplexy.select('this, 'field)}

  transparent inline def applyDynamic(field: String)(inline args: Any*): Any =
    ${Apoplexy.applied('this, 'field, 'args)}

  transparent inline def applyDynamicNamed(field: String)(inline args: (String, Any)*): Any =
    ${Apoplexy.appliedNamed('this, 'field, 'args)}
