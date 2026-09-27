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
package apoplexy

import scala.compiletime

import scala.language.dynamics

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
import fulminate.*
import vacuous.*
import xylophone.*
import zephyrine.*

object Api:
  // Root constructor: `Api(cp"/spec.json")` reads the spec resource's `Locus`,
  // validates the spec at compile time, and returns `Api at "/"` carrying the
  // spec source so navigation macros can re-read it.
  transparent inline def apply(inline resource: Resource): Any = ${Apoplexy.root('resource)}

  // As above, with a base URL for a spec whose servers are relative (`/api/v3`) or absent
  transparent inline def apply(inline resource: Resource, base: Text): Any =
    ${Apoplexy.rootAt('resource, 'base)}

  def make(apiRequest: Api.Request): Api = new Api:
    def request: Api.Request = apiRequest

  // The runtime send (invoked by the code `.call` emits): assemble the URL (base +
  // substituted path + query), set the `accept` header to the media type the spec
  // says the response has and the `content-type` to the body's, add the header
  // parameters, and dispatch through the telekinesis `Http.Client` — which uses
  // whichever `Http.Backend` is in scope.
  def send(request: Api.Request)
    ( using Online,
            Http.Event is Loggable,
            Tactic[Connect.Error],
            Tactic[Url.Error] )
    ( using client: Http.Client onto Origin["http" | "https"] )
  :   Http.Response =

    val substituted =
      request.substitutions.fold(request.path): (path, entry) =>
        path.sub(t"{${entry(0)}}", entry(1))

    val full =
      if request.query.nil then t"${request.base}$substituted"
      else
        val parameters =
          request.query.map: (key, value) => t"${key.urlEncode}=${value.urlEncode}"

        t"${request.base}$substituted?${parameters.join(t"&")}"

    val url = full.as[HttpUrl]

    val empty: Spring[Data] = () => Iterator.empty[Data].stream

    val (contentType, body): (Optional[Text], Spring[Data]) = request.body match
      case Api.Body.Empty                  => (Unset, empty)
      case Api.Body.Content(media, spring) => (media, spring)

    val contentTypeHeader: List[Http.Header] = contentType.lay(Nil): media =>
      List(Http.Header(t"content-type", media))

    val acceptHeader: List[Http.Header] = request.accept.lay(Nil): media =>
      List(Http.Header(t"accept", media))

    val parameterHeaders: List[Http.Header] = request.headers.map: (key, value) =>
      Http.Header(key, value)

    val headers: List[Http.Header] =
      List.concat(acceptHeader, List.concat(contentTypeHeader, parameterHeaders))

    val httpRequest =
      Http.Request
        ( request.method,
          1.1,
          url.host.or(panic(m"an http or https URL always has a host")),
          url.requestTarget,
          headers,
          body )

    client.request(httpRequest, url.origin)

  // A request body: its media type, as the spec names it, and its bytes, sprung afresh for
  // each send. The `invoke` macro builds it from the carrier the media type construes and
  // that carrier's `Postable`.
  object Body:
    // Builds the body here, outside the call site's capture checking, from the carrier value
    // and its `Postable`, which the `invoke` macro summons where the call is written
    def content[carrier](mediaType: Text, value: carrier)(using postable: carrier is Postable)
    :   Body =

      Body.Content(mediaType, () => postable.stream(value))

  enum Body derives CanEqual:
    case Empty
    case Content(mediaType: Text, spring: Spring[Data])

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
      body:          Api.Body           = Api.Body.Empty,
      headers:       List[(Text, Text)] = Nil,
      accept:        Optional[Text]     = Unset )

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

    // Performs the request and construes the response as `value`. The empty
    // parentheses mark the side effect. A bare `.call()` leaves `value`
    // unconstrained, so `value is Defaulting to Transport` resolves it to the
    // response's own carrier: the type the spec's media type construes (a `Json`, a
    // `Raster in Png`), `Unit` for a response with no body, or the raw
    // `Http.Response` when nothing in scope construes the media type.
    //
    // The macro first checks `value` against the response schema; the send and the
    // reading run in inline code, so `value` is concrete when the `Conformant` (and
    // hence the carrier's `Aggregable` and the value's `Decodable`) is summoned —
    // which is what lets `List[T]` and other collections resolve their decoders.
    transparent inline def call[value]()
      ( using erased default: value is Defaulting to Transport )
      ( using online:   Online,
              loggable: Http.Event is Loggable,
              connect:  Tactic[Connect.Error],
              urlError: Tactic[Url.Error],
              client:   Http.Client onto Origin["http" | "https"] )
    :   value =

      Apoplexy.check[value](this)
      val response = Api.send(request)(using online, loggable, connect, urlError)(using client)
      compiletime.summonInline[(value is Conformant) over Transport].read(response)

  // ApiError → Api.Error
  object Error:
    object Reason:
      given Reason is Communicable =
        case Status(code) => m"the server responded with an unsuccessful status, $code"
        case Malformed    => m"the response body was not valid JSON"

    enum Reason(val number: Int) extends Clarification:
      case Status(code: Int) extends Reason(1)
      case Malformed         extends Reason(2)

  case class Error(reason: Api.Error.Reason)(using Diagnostics)
  extends fulminate.Error(914, reason.number)(m"the API request was not successful because $reason")

trait Api extends Dynamic, Locative, Transportive:
  def request: Api.Request

  transparent inline def selectDynamic(field: String): Any =
    ${Apoplexy.select('this, 'field)}

  transparent inline def applyDynamic(field: String)(inline args: Any*): Any =
    ${Apoplexy.applied('this, 'field, 'args)}

  transparent inline def applyDynamicNamed(field: String)(inline args: (String, Any)*): Any =
    ${Apoplexy.appliedNamed('this, 'field, 'args)}
