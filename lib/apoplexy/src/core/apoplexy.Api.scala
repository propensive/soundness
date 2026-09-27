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
import gesticulate.*
import gossamer.*
import hellenism.*
import hieroglyph.*
import jacinta.*
import legerdemain.*
import polyvinyl.*
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
  transparent inline def apply(inline resource: Resource, base: HttpUrl): Any =
    ${Apoplexy.rootAt('resource, 'base)}

  // A relative server URL (`/api/v3`) appended to a caller's base
  def extend(base: HttpUrl, server: Text): HttpUrl =
    Url(base.origin, t"${base.location}$server", base.query, base.fragment)

  def make(apiRequest: Api.Request): Api = new Api:
    def request: Api.Request = apiRequest

  // The specification `record()` and `tuple()` build their values over: JSON's reading
  // primitives, with the fields supplied per response by the macro
  object Records extends Json.Provider.Primitives:
    def fields: List[(Text, Member)] = Nil

    // The records of an array response, one per element
    def list(json: Json, transform: Text => Json => Any): List[Record] =
      repeated(t"", json).map(build(_, transform))

  // A response body read as JSON, for `record()` and `tuple()`
  def jsonOf(response: Http.Response)(using Tactic[Parse.Error]): Json =
    val data: Data = response.body.stream.memoize
    summon[Json is Aggregable by Data].aggregate(Chain(data))

  // The runtime send (invoked by the code `.call` emits): the URL is the base with the
  // substituted path template appended and the query attached; the `accept` header names the
  // media type the spec says the response has, the `content-type` the body's, and the header
  // parameters follow; then the request goes through the telekinesis `Http.Client` — which
  // uses whichever `Http.Backend` is in scope.
  def send(request: Api.Request)
    ( using Online,
            Http.Event is Loggable,
            Tactic[Connect.Error] )
    ( using client: Http.Client onto Origin["http" | "https"] )
  :   Http.Response =

    // A substituted value is encoded as a path segment: percent-escaped, with a space as `%20`
    val substituted =
      request.substitutions.fold(request.path): (path, entry) =>
        path.sub(t"{${entry(0)}}", entry(1).urlEncode.sub(t"+", t"%20"))

    val query: Optional[Text] =
      if request.query.values.nil then request.base.query else request.query.queryString

    val base = request.base
    val url: HttpUrl = Url(base.origin, t"${base.location}$substituted", query, base.fragment)

    val empty: Spring[Data] = () => Iterator.empty[Data].stream

    val (contentType, body): (Optional[MediaType], Spring[Data]) = request.body match
      case Api.Body.Empty                  => (Unset, empty)
      case Api.Body.Content(media, spring) => (media, spring)

    val contentTypeHeader: List[Http.Header] = contentType.lay(Nil): media =>
      List(Http.Header(t"content-type", media.show))

    val acceptHeader: List[Http.Header] = request.accept.lay(Nil): media =>
      List(Http.Header(t"accept", media.show))

    val headers: List[Http.Header] =
      List.concat(acceptHeader, List.concat(contentTypeHeader, request.headers))

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
    def content[carrier](mediaType: MediaType, value: carrier)(using postable: carrier is Postable)
    :   Body =

      Body.Content(mediaType, () => postable.stream(value))

  enum Body derives CanEqual:
    case Empty
    case Content(mediaType: MediaType, spring: Spring[Data])

  // The runtime description of a navigated/invoked call. `base` is the server
  // URL from the spec; `path` is the still-templated path; `substitutions`
  // binds path templates to concrete values; `query` is the query string;
  // `body` is the encoded request body (`Body.Empty` when there is none).
  case class Request
    ( method:        Http.Method,
      base:          HttpUrl,
      path:          Text,
      substitutions: Map[Text, Text]     = Map(),
      query:         Query               = Query(),
      body:          Api.Body            = Api.Body.Empty,
      headers:       List[Http.Header]   = Nil,
      accept:        Optional[MediaType] = Unset )

  // The result of invoking an endpoint. Its refined type records `Result` (a
  // JSON-pointer to the 2xx response schema) and `Form` (the spec source),
  // which `call` reads to check a target type for conformance against the schema.
  object Response:
    def make(apiRequest: Api.Request): Api.Response = new Api.Response:
      def request: Api.Request = apiRequest

  // The result of invoking an endpoint. Its refined type records `Result` (a
  // JSON pointer to the success response's schema within the spec), `Form` (the
  // spec resource), `Transport` (the type the success response is construed as)
  // and `Failure` (the type a response outside the success range is construed
  // as, which `call` raises as the payload of an `Api.Error`).
  trait Response extends Transportive:
    type Result
    type Form
    type Failure
    def request: Api.Request

    // Raises `Api.Error` with the failure payload unless the status is in the 2xx range
    inline def ensure(response: Http.Response)
      ( using tactic: Tactic[Api.Error[Failure]], diagnostics: Diagnostics )
    :   Unit =

      if response.status.category != Http.Status.Category.Successful then
        val payload = compiletime.summonInline[(Failure is Conformant) over Failure].read(response)
        abort(Api.Error(response.status, payload))(using tactic)

    // Performs the request and construes the response as `value`. The empty
    // parentheses mark the side effect. A bare `.call()` leaves `value`
    // unconstrained, so `value is Defaulting to Transport` resolves it to the
    // response's own carrier: the type the spec's media type construes (a `Json`, a
    // `Raster in Png`), `Unit` for a response with no body, or the raw
    // `Http.Response` when nothing in scope construes the media type. A response
    // outside the success range raises `Api.Error[Failure]`, carrying the status and
    // the error body construed as `Failure`.
    //
    // The macro first checks `value` against the response schema; the send and the
    // reading run in inline code, so `value` is concrete when the `Conformant` (and
    // hence the carrier's `Aggregable` and the value's `Decodable`) is summoned —
    // which is what lets `List[T]` and other collections resolve their decoders.
    transparent inline def call[value]()
      ( using erased default: value is Defaulting to Transport )
      ( using online:      Online,
              loggable:    Http.Event is Loggable,
              connect:     Tactic[Connect.Error],
              failure:     Tactic[Api.Error[Failure]],
              diagnostics: Diagnostics,
              client:      Http.Client onto Origin["http" | "https"] )
    :   value =

      Apoplexy.check[value](this)
      val response = Api.send(request)(using online, loggable, connect)(using client)
      ensure(response)
      compiletime.summonInline[(value is Conformant) over Transport].read(response)

    // Performs the request and reads the JSON response as a record typed by its schema: one
    // member per property, nested objects and `$ref`s to component schemas as nested records,
    // an array of objects as a `List` of them, each read at the type its schema declares (see
    // `Json.Provider`). Only for a JSON response; the schema must describe an object or an array
    // of objects.
    transparent inline def record()
      ( using online:      Online,
              loggable:    Http.Event is Loggable,
              connect:     Tactic[Connect.Error],
              parse:       Tactic[Parse.Error],
              failure:     Tactic[Api.Error[Failure]],
              diagnostics: Diagnostics,
              client:      Http.Client onto Origin["http" | "https"] )
    :   Any =

      val response = Api.send(request)(using online, loggable, connect)(using client)
      ensure(response)
      Apoplexy.record(this, Api.jsonOf(response))

    // As `record()`, but an eagerly-read named tuple, in the schema's property order
    transparent inline def tuple()
      ( using online:      Online,
              loggable:    Http.Event is Loggable,
              connect:     Tactic[Connect.Error],
              parse:       Tactic[Parse.Error],
              failure:     Tactic[Api.Error[Failure]],
              diagnostics: Diagnostics,
              client:      Http.Client onto Origin["http" | "https"] )
    :   Any =

      val response = Api.send(request)(using online, loggable, connect)(using client)
      ensure(response)
      Apoplexy.tuple(this, Api.jsonOf(response))

  // A response outside the success range: its status, and its body construed as the type the
  // specification declares for that response (`Failure` on the `Api.Response`), or as `Text`
  // where it declares none
  case class Error[payload](status: Http.Status, payload: payload)(using Diagnostics)
  extends fulminate.Error(914, 1)(m"the server responded with an unsuccessful status, $status")

trait Api extends Dynamic, Locative, Transportive:
  def request: Api.Request

  transparent inline def selectDynamic(field: String): Any =
    ${Apoplexy.select('this, 'field)}

  transparent inline def applyDynamic(field: String)(inline args: Any*): Any =
    ${Apoplexy.applied('this, 'field, 'args)}

  transparent inline def applyDynamicNamed(field: String)(inline args: (String, Any)*): Any =
    ${Apoplexy.appliedNamed('this, 'field, 'args)}
