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
import orthodoxy.*
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

  // The presentations of credentials, as an operation's security schemes dictate (the `invoke`
  // macro summons each `Credential` and emits the call). An API key goes in the header, query
  // parameter or cookie the scheme names; HTTP authentication and a token in the `authorization`
  // header. A token is checked, before the request is sent, against the scopes the requirement
  // names: one it does not grant raises `OAuth.Error`.
  def apiKey(credential: Credential { type Result = Text }, name: Text): Http.Header =
    Http.Header(name, credential.value)

  def cookieKey(credential: Credential { type Result = Text }, name: Text): Http.Header =
    Http.Header(t"cookie", t"$name=${credential.value}")

  def httpAuth(credential: Credential { type Result <: Auth }): Http.Header =
    Http.Header(t"authorization", (credential.value: Auth).show)

  def tokenAuth(credential: Credential { type Result = Authorization }, scopes: List[Text])
    ( using Tactic[OAuth.Error], Diagnostics )
  :   Http.Header =

    scopes.seek(scope => !credential.value.grants(List(scope))).let: scope =>
      abort(OAuth.Error(OAuth.Error.Reason.InsufficientPrivileges(scope)))

    Http.Header(t"authorization", credential.value.bearer.show)

  // A response body read as JSON, for `record()` and `tuple()`
  def jsonOf(response: Http.Response)(using Tactic[Parse.Error]): Json =
    summon[Json is Aggregable by Data].aggregate(Chain(dataOf(response)))

  // A response body's bytes, for a violation's payload
  def dataOf(response: Http.Response): Data = response.body.stream.memoize

  // The decoder for a response's text: the `charset` its `content-type` names, where it names one
  // hieroglyph knows, else the decoder in scope
  def decoderFor(response: Http.Response)(using fallback: CharDecoder): CharDecoder =
    response.contentType.let(_.at(t"charset")).let(Encoding.unapply(_)) match
      case Some(encoding: Encoding) => encoding.decoder(using fallback.sanitizer)
      case _                        => fallback

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

    // One `cookie` header carries every cookie (RFC 6265 §5.4); the API keys sent as cookies
    // arrive as separate headers and are joined here
    val cookies = request.headers.filter(_.key == t"cookie").map(_.value)

    val cookieHeader: List[Http.Header] =
      if cookies.nil then Nil else List(Http.Header(t"cookie", cookies.join(t"; ")))

    val others = request.headers.filter(_.key != t"cookie")

    val headers: List[Http.Header] =
      List.concat(acceptHeader, List.concat(contentTypeHeader, List.concat(others, cookieHeader)))

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

      // The spec's media type, with the parameters the carrier's `Postable` adds (a multipart
      // boundary, a charset) where the spec names none
      val parameters = postable.mediaType(value).parameters
      val media = if mediaType.parameters.nil then mediaType.copy(parameters = parameters) else mediaType
      Body.Content(media, () => postable.stream(value))

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

  object Response:
    def make(apiRequest: Api.Request): Api.Response = new Api.Response:
      def request: Api.Request = apiRequest

  // The result of invoking an endpoint. Its refined type records `Result` (a
  // JSON pointer to the success response's schema within the spec), `Form` (the
  // spec resource), `Locus` and `Verb` (the operation), `Transport` (the type
  // the success response is construed as) and `Failure` (the union of the
  // `Api.Error` types the operation's declared error responses raise).
  trait Response extends Transportive:
    type Result
    type Form
    type Locus
    type Verb
    type Failure <: Hazard
    def request: Api.Request

    // Raises the declared error for the response's status, with its payload construed, unless
    // the status is in the 2xx range; an undeclared status raises `Api.Violation`. The macro
    // summons a `Tactic` for each member of `Failure`, and one for `Violation`, where the call
    // is written.
    inline def ensure(response: Http.Response): Unit = Apoplexy.ensure(this, response)

    // The call as an `Attempt` over the declared errors, for a `recover` which matches them
    // exhaustively: `api.pet(42L).get.attempt[Pet]().recover { case Api.NotFound(problem) => … }`
    transparent inline def attempt[value]()
      ( using erased default: value is Defaulting to Transport )
      ( using online:      Online,
              loggable:    Http.Event is Loggable,
              connect:     Tactic[Connect.Error],
              violation:   Tactic[Api.Violation],
              diagnostics: Diagnostics,
              client:      Http.Client onto Origin["http" | "https"] )
    :   Attempt[value, Failure] =

      contingency.attempt[Failure]:
        call[value]()(using default)(using online, loggable, connect, client)

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
      ( using online:   Online,
              loggable: Http.Event is Loggable,
              connect:  Tactic[Connect.Error],
              client:   Http.Client onto Origin["http" | "https"] )
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
      ( using online:   Online,
              loggable: Http.Event is Loggable,
              connect:  Tactic[Connect.Error],
              parse:    Tactic[Parse.Error],
              client:   Http.Client onto Origin["http" | "https"] )
    :   Any =

      val response = Api.send(request)(using online, loggable, connect)(using client)
      ensure(response)
      Apoplexy.record(this, Api.jsonOf(response))

    // As `record()`, but an eagerly-read named tuple, in the schema's property order
    transparent inline def tuple()
      ( using online:   Online,
              loggable: Http.Event is Loggable,
              connect:  Tactic[Connect.Error],
              parse:    Tactic[Parse.Error],
              client:   Http.Client onto Origin["http" | "https"] )
    :   Any =

      val response = Api.send(request)(using online, loggable, connect)(using client)
      ensure(response)
      Apoplexy.tuple(this, Api.jsonOf(response))

  // A response outside the success range. Each status telekinesis names has its own error class,
  // so that an operation's declared error responses are a union of distinct types — the
  // `Failure` of its `Api.Response` — which a `recover` over an `attempt` matches exhaustively,
  // and which contingency's handlers tell apart. The payload is the error body construed as the
  // type the specification declares for that response: a record typed by its schema for a JSON
  // object, else the media type's carrier, else `Text`, or `Unit` where it declares no body.
  // `Informational`, `Redirection`, `ClientError` and `ServerError` stand for the `1XX`–`5XX`
  // ranges (and for a status telekinesis does not name); `OtherError` for `default`.
  sealed abstract class Error[+payload](val status: Http.Status, val payload: payload)
    ( using Diagnostics )
  extends fulminate.Error(914, 1)(m"the server responded with an unsuccessful status, $status")

  case class Continue[+payload](override val payload: payload)(using Diagnostics)
  extends Error[payload](Http.Continue, payload)

  case class SwitchingProtocols[+payload](override val payload: payload)(using Diagnostics)
  extends Error[payload](Http.SwitchingProtocols, payload)

  case class EarlyHints[+payload](override val payload: payload)(using Diagnostics)
  extends Error[payload](Http.EarlyHints, payload)

  case class MultipleChoices[+payload](override val payload: payload)(using Diagnostics)
  extends Error[payload](Http.MultipleChoices, payload)

  case class MovedPermanently[+payload](override val payload: payload)(using Diagnostics)
  extends Error[payload](Http.MovedPermanently, payload)

  case class Found[+payload](override val payload: payload)(using Diagnostics)
  extends Error[payload](Http.Found, payload)

  case class SeeOther[+payload](override val payload: payload)(using Diagnostics)
  extends Error[payload](Http.SeeOther, payload)

  case class NotModified[+payload](override val payload: payload)(using Diagnostics)
  extends Error[payload](Http.NotModified, payload)

  case class TemporaryRedirect[+payload](override val payload: payload)(using Diagnostics)
  extends Error[payload](Http.TemporaryRedirect, payload)

  case class PermanentRedirect[+payload](override val payload: payload)(using Diagnostics)
  extends Error[payload](Http.PermanentRedirect, payload)

  case class BadRequest[+payload](override val payload: payload)(using Diagnostics)
  extends Error[payload](Http.BadRequest, payload)

  case class Unauthorized[+payload](override val payload: payload)(using Diagnostics)
  extends Error[payload](Http.Unauthorized, payload)

  case class PaymentRequired[+payload](override val payload: payload)(using Diagnostics)
  extends Error[payload](Http.PaymentRequired, payload)

  case class Forbidden[+payload](override val payload: payload)(using Diagnostics)
  extends Error[payload](Http.Forbidden, payload)

  case class NotFound[+payload](override val payload: payload)(using Diagnostics)
  extends Error[payload](Http.NotFound, payload)

  case class MethodNotAllowed[+payload](override val payload: payload)(using Diagnostics)
  extends Error[payload](Http.MethodNotAllowed, payload)

  case class NotAcceptable[+payload](override val payload: payload)(using Diagnostics)
  extends Error[payload](Http.NotAcceptable, payload)

  case class ProxyAuthenticationRequired[+payload](override val payload: payload)(using Diagnostics)
  extends Error[payload](Http.ProxyAuthenticationRequired, payload)

  case class RequestTimeout[+payload](override val payload: payload)(using Diagnostics)
  extends Error[payload](Http.RequestTimeout, payload)

  case class Conflict[+payload](override val payload: payload)(using Diagnostics)
  extends Error[payload](Http.Conflict, payload)

  case class Gone[+payload](override val payload: payload)(using Diagnostics)
  extends Error[payload](Http.Gone, payload)

  case class LengthRequired[+payload](override val payload: payload)(using Diagnostics)
  extends Error[payload](Http.LengthRequired, payload)

  case class PreconditionFailed[+payload](override val payload: payload)(using Diagnostics)
  extends Error[payload](Http.PreconditionFailed, payload)

  case class PayloadTooLarge[+payload](override val payload: payload)(using Diagnostics)
  extends Error[payload](Http.PayloadTooLarge, payload)

  case class UriTooLong[+payload](override val payload: payload)(using Diagnostics)
  extends Error[payload](Http.UriTooLong, payload)

  case class UnsupportedMediaType[+payload](override val payload: payload)(using Diagnostics)
  extends Error[payload](Http.UnsupportedMediaType, payload)

  case class RangeNotSatisfiable[+payload](override val payload: payload)(using Diagnostics)
  extends Error[payload](Http.RangeNotSatisfiable, payload)

  case class ExpectationFailed[+payload](override val payload: payload)(using Diagnostics)
  extends Error[payload](Http.ExpectationFailed, payload)

  case class UnprocessableEntity[+payload](override val payload: payload)(using Diagnostics)
  extends Error[payload](Http.UnprocessableEntity, payload)

  case class TooEarly[+payload](override val payload: payload)(using Diagnostics)
  extends Error[payload](Http.TooEarly, payload)

  case class UpgradeRequired[+payload](override val payload: payload)(using Diagnostics)
  extends Error[payload](Http.UpgradeRequired, payload)

  case class PreconditionRequired[+payload](override val payload: payload)(using Diagnostics)
  extends Error[payload](Http.PreconditionRequired, payload)

  case class TooManyRequests[+payload](override val payload: payload)(using Diagnostics)
  extends Error[payload](Http.TooManyRequests, payload)

  case class RequestHeaderFieldsTooLarge[+payload](override val payload: payload)(using Diagnostics)
  extends Error[payload](Http.RequestHeaderFieldsTooLarge, payload)

  case class UnavailableForLegalReasons[+payload](override val payload: payload)(using Diagnostics)
  extends Error[payload](Http.UnavailableForLegalReasons, payload)

  case class InternalServerError[+payload](override val payload: payload)(using Diagnostics)
  extends Error[payload](Http.InternalServerError, payload)

  case class NotImplemented[+payload](override val payload: payload)(using Diagnostics)
  extends Error[payload](Http.NotImplemented, payload)

  case class BadGateway[+payload](override val payload: payload)(using Diagnostics)
  extends Error[payload](Http.BadGateway, payload)

  case class ServiceUnavailable[+payload](override val payload: payload)(using Diagnostics)
  extends Error[payload](Http.ServiceUnavailable, payload)

  case class GatewayTimeout[+payload](override val payload: payload)(using Diagnostics)
  extends Error[payload](Http.GatewayTimeout, payload)

  case class HttpVersionNotSupported[+payload](override val payload: payload)(using Diagnostics)
  extends Error[payload](Http.HttpVersionNotSupported, payload)

  case class VariantAlsoNegotiates[+payload](override val payload: payload)(using Diagnostics)
  extends Error[payload](Http.VariantAlsoNegotiates, payload)

  case class InsufficientStorage[+payload](override val payload: payload)(using Diagnostics)
  extends Error[payload](Http.InsufficientStorage, payload)

  case class LoopDetected[+payload](override val payload: payload)(using Diagnostics)
  extends Error[payload](Http.LoopDetected, payload)

  case class NotExtended[+payload](override val payload: payload)(using Diagnostics)
  extends Error[payload](Http.NotExtended, payload)

  case class NetworkAuthenticationRequired[+payload](override val payload: payload)(using Diagnostics)
  extends Error[payload](Http.NetworkAuthenticationRequired, payload)

  case class Informational[+payload](override val status: Http.Status, override val payload: payload)
    ( using Diagnostics )
  extends Error[payload](status, payload)

  case class Redirection[+payload](override val status: Http.Status, override val payload: payload)
    ( using Diagnostics )
  extends Error[payload](status, payload)

  case class ClientError[+payload](override val status: Http.Status, override val payload: payload)
    ( using Diagnostics )
  extends Error[payload](status, payload)

  case class ServerError[+payload](override val status: Http.Status, override val payload: payload)
    ( using Diagnostics )
  extends Error[payload](status, payload)

  case class OtherError[+payload](override val status: Http.Status, override val payload: payload)
    ( using Diagnostics )
  extends Error[payload](status, payload)

  // A response with a status the specification does not declare: the server has broken its
  // contract, which every call may meet and must handle apart from the declared errors
  case class Violation(status: Http.Status, body: Data)(using Diagnostics)
  extends fulminate.Error(914, 2)(m"the server responded with an undeclared status, $status")

trait Api extends Dynamic, Locative, Transportive:
  def request: Api.Request

  transparent inline def selectDynamic(field: String): Any =
    ${Apoplexy.select('this, 'field)}

  transparent inline def applyDynamic(field: String)(inline args: Any*): Any =
    ${Apoplexy.applied('this, 'field, 'args)}

  transparent inline def applyDynamicNamed(field: String)(inline args: (String, Any)*): Any =
    ${Apoplexy.appliedNamed('this, 'field, 'args)}
