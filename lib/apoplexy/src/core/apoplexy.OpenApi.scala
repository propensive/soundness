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

import anticipation.*
import contingency.*
import distillate.*
import fulminate.*
import gossamer.*
import hypotenuse.*
import jacinta.*
import prepositional.*
import rudiments.*
import spectacular.*
import telekinesis.*
import turbulence.*
import vacuous.*
import ypsiloid.*

import errorDiagnostics.emptyDiagnostics
import zephyrine.Parse

object OpenApi:
  case class Info(title: Text, version: Text, description: Optional[Text] = Unset)

  object ServerVariable:
    given decodable: (Tactic[Json.Error], Tactic[JsonPointer.Error], Tactic[OpenApi.Error])
    =>  ServerVariable is Json.Decodable = Json.DecodableDerivation.derived

  case class ServerVariable
    ( default:     Text,
      `enum`:      Optional[List[Text]] = Unset,
      description: Optional[Text]       = Unset )

  object Server:
    given decodable: (Tactic[Json.Error], Tactic[JsonPointer.Error], Tactic[OpenApi.Error])
    =>  Server is Json.Decodable = Json.DecodableDerivation.derived

  case class Server
    ( url:         Text,
      description: Optional[Text]           = Unset,
      variables:   Map[Text, ServerVariable] = Map() ):

    // The URL with every `{variable}` replaced by its default
    def resolved: Text = variables.fold(url): (url, entry) =>
      url.sub(t"{${entry(0)}}", entry(1).default)

  // A reference to a component, by JSON pointer (`#/components/parameters/limit`), left
  // unresolved where the document uses one; `apply()` resolves it against the document.
  case class Ref(pointer: JsonPointer)

  // A component object may be written in place or referred to by a `$ref`
  type Referable[value] = value | Ref

  // The decoder of a place-or-reference position: a `$ref` key marks a `Ref`, anything else
  // is decoded as the component itself
  private def referable[value](inner: value is Json.Decodable)
    ( using Tactic[Json.Error], Tactic[JsonPointer.Error] )
  :   Referable[value] is Json.Decodable =

    Json.Decodable(Morphology.Any): json =>
      json("$ref".tt).as[Optional[Text]].lay(inner.decoded(json)): reference =>
        Ref(reference.as[JsonPointer])

  object Componental:
    given parameter: Parameter is Componental = Componental(t"parameters", _.parameters)
    given response: Response is Componental = Componental(t"responses", _.responses)
    given requestBody: RequestBody is Componental = Componental(t"requestBodies", _.requestBodies)

    def apply[value](kind0: Text, map: Components => Map[Text, Referable[value]])
    :   value is Componental =

      new Componental:
        type Self = value
        def kind: Text = kind0

        def lookup(components: Components, name: Text): Optional[value] =
          map(components).at(name) match
            case found: Ref => Unset
            case found      => found.asInstanceOf[Optional[value]]

  // Which map in `Components` holds a kind of component, and under what pointer prefix
  trait Componental extends Typeclass.Pure:
    def kind: Text
    def lookup(components: Components, name: Text): Optional[Self]

  // Resolves a place-or-reference position to the component: the value itself, or the
  // component named by the reference. Only references into the document's own components
  // are supported.
  // The bound keeps `apply` from being tried on every application of a value without one.
  extension [value <: Parameter | Response | RequestBody: Componental](referable: Referable[value])
    def apply()(using doc: OpenApi): value raises OpenApi.Error = referable match
      case Ref(pointer) =>
        val reference = pointer.encode
        val prefix = t"#/components/${value.kind}/"

        if reference.starts(prefix) then
          val name = reference.skip(prefix.length)

          doc.components.let(value.lookup(_, name)).or:
            abort(OpenApi.Error(OpenApi.Error.Reason.UnresolvableRef(reference)))
        else
          abort(OpenApi.Error(OpenApi.Error.Reason.UnsupportedRef(reference)))

      case value =>
        value.asInstanceOf[value]

  object Parameter:
    object In:
      given decodable: Tactic[OpenApi.Error] => In is Decodable in Text = _.lower match
        case t"path"   => In.Path
        case t"query"  => In.Query
        case t"header" => In.Header
        case t"cookie" => In.Cookie
        case other     => abort(OpenApi.Error(OpenApi.Error.Reason.BadParameterLocation(other)))

    // Anchored (rather than derived inline at each use) so that `PathItem` and
    // `Operation`, which both embed `Parameter`, reference one cached instance.
    // Typed as the carrier `Json.Decodable` (not the plain `Decodable in Json`):
    // the schema-carrying product derivation summons each field as `Json.Decodable`,
    // so a plain-typed anchor would be bypassed and the type re-derived inline.
    given decodableJson: (Tactic[Json.Error], Tactic[JsonPointer.Error], Tactic[OpenApi.Error])
    =>  Parameter is Json.Decodable = Json.DecodableDerivation.derived

    given referable: (Tactic[Json.Error], Tactic[JsonPointer.Error], Tactic[OpenApi.Error])
    =>  Referable[Parameter] is Json.Decodable = OpenApi.referable(decodableJson)

    enum In:
      case Path, Query, Header, Cookie

  case class Parameter
    ( name:        Text,
      `in`:        Parameter.In,
      required:    Optional[Boolean]    = Unset,
      description: Optional[Text]       = Unset,
      schema:      Optional[JsonSchema] = Unset )

  // An OpenAPI "Media Type Object": the value of a `content` entry keyed by a
  // media-type string such as `application/json`.
  object MediaTypeObject:
    given (Tactic[Json.Error], Tactic[JsonPointer.Error], Tactic[OpenApi.Error])
    =>  MediaTypeObject is Json.Decodable = Json.DecodableDerivation.derived

  case class MediaTypeObject(schema: Optional[JsonSchema] = Unset)

  object RequestBody:
    given decodable: (Tactic[Json.Error], Tactic[JsonPointer.Error], Tactic[OpenApi.Error])
    =>  RequestBody is Json.Decodable = Json.DecodableDerivation.derived

    given referable: (Tactic[Json.Error], Tactic[JsonPointer.Error], Tactic[OpenApi.Error])
    =>  Referable[RequestBody] is Json.Decodable = OpenApi.referable(decodable)

  case class RequestBody
    ( description: Optional[Text]             = Unset,
      required:    Optional[Boolean]          = Unset,
      content:     Map[Text, MediaTypeObject] = Map() )

  object Response:
    given decodable: (Tactic[Json.Error], Tactic[JsonPointer.Error], Tactic[OpenApi.Error])
    =>  Response is Json.Decodable = Json.DecodableDerivation.derived

    given referable: (Tactic[Json.Error], Tactic[JsonPointer.Error], Tactic[OpenApi.Error])
    =>  Referable[Response] is Json.Decodable = OpenApi.referable(decodable)

  case class Response
    ( description: Optional[Text]             = Unset,
      content:     Map[Text, MediaTypeObject] = Map() )

  object Operation:
    // `PathItem` carries eight `Optional[Operation]` fields, so deriving it
    // inline would expand the whole `Operation` graph eight times and overflow
    // `-Xmax-inlines` (which surfaces, misleadingly, as a missing `Decodable`
    // instance). Anchoring `Operation` derives it once and lets `PathItem`
    // simply reference it.
    given decodableJson: (Tactic[Json.Error], Tactic[JsonPointer.Error], Tactic[OpenApi.Error])
    =>  Operation is Json.Decodable = Json.DecodableDerivation.derived

  case class Operation
    ( operationId: Optional[Text]                    = Unset,
      summary:     Optional[Text]                    = Unset,
      description: Optional[Text]                    = Unset,
      parameters:  List[Referable[Parameter]]        = Nil,
      requestBody: Optional[Referable[RequestBody]]  = Unset,
      responses:   Map[Text, Referable[Response]]    = Map(),
      security:    Optional[List[Requirement]]       = Unset )

  object PathItem:
    given (Tactic[Json.Error], Tactic[JsonPointer.Error], Tactic[OpenApi.Error])
    =>  PathItem is Json.Decodable = Json.DecodableDerivation.derived

  // The path-item object mixes HTTP-method keys with non-method keys, so each
  // verb is a fixed optional field rather than a `Map[Http.Method, Operation]`;
  // `operations` rebuilds that map for consumers that want it.
  case class PathItem
    ( summary:     Optional[Text]      = Unset,
      description: Optional[Text]      = Unset,
      get:         Optional[Operation] = Unset,
      put:         Optional[Operation] = Unset,
      post:        Optional[Operation] = Unset,
      delete:      Optional[Operation] = Unset,
      options:     Optional[Operation] = Unset,
      head:        Optional[Operation] = Unset,
      patch:       Optional[Operation] = Unset,
      trace:       Optional[Operation] = Unset,
      parameters:  List[Referable[Parameter]] = Nil ):

    def operations: Map[Http.Method, Operation] =
      val verbs =
        List
          ( Http.Get -> get, Http.Put -> put, Http.Post -> post, Http.Delete -> delete,
            Http.Options -> options, Http.Head -> head, Http.Patch -> patch,
            Http.Trace -> trace )

      verbs
      . stdlib.collect { case (method, operation: Operation) => method -> operation }
      . pipe(_.to(Map))

  object Components:
    given (Tactic[Json.Error], Tactic[JsonPointer.Error], Tactic[OpenApi.Error])
    =>  Components is Json.Decodable = Json.DecodableDerivation.derived

  case class Components
    ( schemas:         Map[Text, JsonSchema]              = Map(),
      parameters:      Map[Text, Referable[Parameter]]    = Map(),
      responses:       Map[Text, Referable[Response]]     = Map(),
      requestBodies:   Map[Text, Referable[RequestBody]]  = Map(),
      securitySchemes: Map[Text, SecurityScheme]          = Map() )

  object SecurityScheme:
    given decodable: (Tactic[Json.Error], Tactic[JsonPointer.Error], Tactic[OpenApi.Error])
    =>  SecurityScheme is Json.Decodable = Json.DecodableDerivation.derived

    enum Kind:
      case ApiKey, Http, OAuth2, OpenIdConnect, MutualTls, Unknown

    object Flow:
      given decodable: (Tactic[Json.Error], Tactic[JsonPointer.Error], Tactic[OpenApi.Error])
      =>  Flow is Json.Decodable = Json.DecodableDerivation.derived

    // One OAuth 2 flow: its endpoints and the scopes it can grant, each with a description
    case class Flow
      ( authorizationUrl: Optional[Text]  = Unset,
        tokenUrl:         Optional[Text]  = Unset,
        refreshUrl:       Optional[Text]  = Unset,
        scopes:           Map[Text, Text] = Map() )

    object Flows:
      given decodable: (Tactic[Json.Error], Tactic[JsonPointer.Error], Tactic[OpenApi.Error])
      =>  Flows is Json.Decodable = Json.DecodableDerivation.derived

    case class Flows
      ( `implicit`:        Optional[Flow] = Unset,
        password:          Optional[Flow] = Unset,
        clientCredentials: Optional[Flow] = Unset,
        authorizationCode: Optional[Flow] = Unset )

  // A security scheme (OpenAPI 3 §4.8.27): an API key sent in a header, query parameter or
  // cookie; HTTP authentication by a named scheme; OAuth 2, with its flows; OpenID Connect; or
  // mutual TLS. The fields not belonging to the scheme's `type` stay absent.
  case class SecurityScheme
    ( `type`:           Text,
      description:      Optional[Text]                 = Unset,
      name:             Optional[Text]                 = Unset,
      `in`:             Optional[Text]                 = Unset,
      scheme:           Optional[Text]                 = Unset,
      bearerFormat:     Optional[Text]                 = Unset,
      flows:            Optional[SecurityScheme.Flows] = Unset,
      openIdConnectUrl: Optional[Text]                 = Unset ):

    def kind: SecurityScheme.Kind = `type` match
      case t"apiKey"        => SecurityScheme.Kind.ApiKey
      case t"http"          => SecurityScheme.Kind.Http
      case t"oauth2"        => SecurityScheme.Kind.OAuth2
      case t"openIdConnect" => SecurityScheme.Kind.OpenIdConnect
      case t"mutualTLS"     => SecurityScheme.Kind.MutualTls
      case _                => SecurityScheme.Kind.Unknown

  // A security requirement (§4.8.30): the schemes which must all be satisfied, each with the
  // scopes it needs; an operation's (or the document's) `security` lists alternatives, any one of
  // which suffices, an empty requirement meaning that no credentials are needed.
  type Requirement = Map[Text, List[Text]]

  // The `responses` map is keyed by status text (`"200"`, `"2XX"`, `"default"`),
  // not all of which are valid `Http.Status` codes; `response` interprets a
  // concrete status against those keys.
  extension (operation: Operation)
    def response(status: Http.Status): Optional[Referable[Response]] =
      operation.responses.at(status.code.show)
      . or(operation.responses.at(t"${status.code/100}XX"))
      . or(operation.responses.at(t"default"))

  // Resolve a single `$ref` hop against the document's component schemas, via
  // `ref()`. Resolving one hop at a time (never inlining recursively) keeps
  // cyclic schema graphs from looping; the consumer drives the walk. A
  // non-reference schema resolves to itself.
  extension (schema: JsonSchema)
    def apply()(using doc: OpenApi): JsonSchema raises OpenApi.Error = schema match
      case JsonSchema.Ref(pointer, _, _) =>
        val reference = pointer.encode
        val prefix = t"#/components/schemas/"

        if reference.starts(prefix) then
          val name = reference.skip(prefix.length)

          doc.components.let(_.schemas.at(name)).or:
            abort(OpenApi.Error(OpenApi.Error.Reason.UnresolvableRef(reference)))
        else
          abort(OpenApi.Error(OpenApi.Error.Reason.UnsupportedRef(reference)))

      case other =>
        other

  // A YAML document as JSON. The model has one decoder, over `Json`; a document written as
  // YAML reaches it by translating the tree, so the two forms cannot drift apart. Both ASTs are
  // flat arrays of the same shape, so the walk is shallow. A mapping key which YAML wrote as a
  // number or boolean (`200:`, `default:` is already a string) becomes the string JSON requires.
  def json(yaml: Yaml): Json = Json.ast(translate(yaml.root))

  private def translate(node: Yaml.Ast): Json.Ast =
    if node.isNull || node.isAbsent then Json.Ast(Json.JsonNull)
    else if node.isBoolean then Json.Ast(node.asInstanceOf[Boolean])
    else if node.isLong then Json.Ast(node.asInstanceOf[Long])
    else if node.isDouble then Json.Ast(node.asInstanceOf[Double])
    else if node.isBcd then Json.Ast(Bcd.adopt(node.asInstanceOf[scala.Array[Double]]))
    else if node.isString then Json.Ast(node.asInstanceOf[String])
    else if node.isArray then
      val elements = Array.tabulate[Any](node.arrayLength): index =>
        translate(node.arrayElement(index))

      Json.Ast.arr(elements)
    else
      val entries = node.asInstanceOf[Array[Any]]
      val size = node.objectSize

      def key(index: Int): String = (entries.readable(index*2): Matchable) match
        case string: String   => string
        case long: Long       => long.toString
        case double: Double   => double.toString
        case boolean: Boolean => boolean.toString
        case _                => "null"

      Json.Ast.obj
        ( Array.tabulate[String](size)(key),
          Array.tabulate[Any](size) { index => translate(node.objectValue(index)) } )

  // The document's JSON, whether it was written as JSON or YAML: JSON begins with `{`
  private[apoplexy] def sourceJson(text: Text)
    ( using Tactic[Parse.Error], Tactic[Yaml.Error], Yaml.Tracking )
  :   Json =

    if text.trim.starts(t"{") then text.as[Json] else json(text.as[Yaml])

  // Anchor the top-level model so `as[OpenApi]` (below) materialises its decoder
  // once — with each nested type resolving to its own anchor — rather than inlining
  // the entire OpenAPI graph at the call site (which, with schema-carrying codecs,
  // overflows `-Xmax-inlines` and the JVM class-size limit).
  given decodable: (Tactic[Json.Error], Tactic[JsonPointer.Error], Tactic[OpenApi.Error])
  =>  OpenApi is Json.Decodable = Json.DecodableDerivation.derived

  // `source.read[OpenApi]`: aggregate the source text, auto-detect JSON vs YAML
  // from the first non-whitespace character, decode through the shared model,
  // then check the document declares an OpenAPI 3.x version.
  given aggregable: Tactic[OpenApi.Error] => OpenApi is Aggregable by Text =
    summon[Text is Aggregable by Text].map: text =>
      val document =
        mitigate:
          case Parse.Error(_, _, _)    => OpenApi.Error(OpenApi.Error.Reason.Malformed)
          case Json.Error(_)           => OpenApi.Error(OpenApi.Error.Reason.Malformed)
          case Yaml.Error(_)           => OpenApi.Error(OpenApi.Error.Reason.Malformed)
          case JsonPointer.Error(_, _) => OpenApi.Error(OpenApi.Error.Reason.Malformed)

        . protect(sourceJson(text).as[OpenApi])

      if document.openapi.starts(t"3.") then document
      else abort(OpenApi.Error(OpenApi.Error.Reason.UnsupportedVersion(document.openapi)))

  // OpenApiError → OpenApi.Error
  object Error:
    object Reason:
      given Reason is Communicable =
        case Malformed =>
          m"the OpenAPI document could not be parsed"

        case UnsupportedVersion(version) =>
          m"the OpenAPI version $version is not supported; only 3.x is supported"

        case UnresolvableRef(pointer) =>
          m"the reference $pointer could not be resolved"

        case UnsupportedRef(pointer) =>
          m"the reference $pointer is not supported; only #/components/schemas references work"

        case BadParameterLocation(value) =>
          m"$value is not a valid parameter location; expected one of path, query, header or cookie"

    enum Reason(val number: Int) extends Clarification:
      case Malformed                         extends Reason(1)
      case UnsupportedVersion(version: Text) extends Reason(2)
      case UnresolvableRef(pointer: Text)    extends Reason(3)
      case UnsupportedRef(pointer: Text)     extends Reason(4)
      case BadParameterLocation(value: Text) extends Reason(5)

  case class Error(reason: OpenApi.Error.Reason)(using Diagnostics)
  extends fulminate.Error(846, reason.number)(m"the OpenAPI document was not valid because $reason")

case class OpenApi
  ( openapi:    Text,
    info:       OpenApi.Info,
    servers:    List[OpenApi.Server]          = Nil,
    paths:      Map[Text, OpenApi.PathItem]   = Map(),
    components: Optional[OpenApi.Components]  = Unset,
    security:   List[OpenApi.Requirement]     = Nil )
