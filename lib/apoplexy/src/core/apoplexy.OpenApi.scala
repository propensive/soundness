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

import anticipation.*
import contingency.*
import distillate.*
import fulminate.*
import gossamer.*
import jacinta.*
import prepositional.*
import rudiments.*
import spectacular.*
import telekinesis.*
import turbulence.*
import vacuous.*
import ypsiloid.*

import errorDiagnostics.emptyDiagnostics
import zephyrine.ParseError

object OpenApi:
  case class Info(title: Text, version: Text, description: Optional[Text] = Unset)
  case class Server(url: Text, description: Optional[Text] = Unset)

  object Parameter:
    object In:
      given decodable: Tactic[OpenApiError] => In is Decodable in Text = _.lower match
        case t"path"   => In.Path
        case t"query"  => In.Query
        case t"header" => In.Header
        case t"cookie" => In.Cookie
        case other     => abort(OpenApiError(OpenApiError.Reason.BadParameterLocation(other)))

    // Anchored (rather than derived inline at each use) so that `PathItem` and
    // `Operation`, which both embed `Parameter`, reference one cached instance.
    // Typed as the carrier `Json.Decodable` (not the plain `Decodable in Json`):
    // the schema-carrying product derivation summons each field as `Json.Decodable`,
    // so a plain-typed anchor would be bypassed and the type re-derived inline.
    given decodableJson: (Tactic[JsonError], Tactic[JsonPointerError], Tactic[OpenApiError])
    =>  Parameter is Json.Decodable = Json.DecodableDerivation.derived

    given decodableYaml: (Tactic[YamlError], Tactic[JsonPointerError], Tactic[OpenApiError])
    =>  Parameter is Decodable in Yaml = Yaml.DecodableDerivation.derived

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
    given (Tactic[JsonError], Tactic[JsonPointerError], Tactic[OpenApiError])
    =>  MediaTypeObject is Json.Decodable = Json.DecodableDerivation.derived

  case class MediaTypeObject(schema: Optional[JsonSchema] = Unset)

  object RequestBody:
    given (Tactic[JsonError], Tactic[JsonPointerError], Tactic[OpenApiError])
    =>  RequestBody is Json.Decodable = Json.DecodableDerivation.derived

  case class RequestBody
    ( description: Optional[Text]             = Unset,
      required:    Optional[Boolean]          = Unset,
      content:     Map[Text, MediaTypeObject] = Map() )

  object Response:
    given (Tactic[JsonError], Tactic[JsonPointerError], Tactic[OpenApiError])
    =>  Response is Json.Decodable = Json.DecodableDerivation.derived

  case class Response
    ( description: Optional[Text]             = Unset,
      content:     Map[Text, MediaTypeObject] = Map() )

  object Operation:
    // `PathItem` carries eight `Optional[Operation]` fields, so deriving it
    // inline would expand the whole `Operation` graph eight times and overflow
    // `-Xmax-inlines` (which surfaces, misleadingly, as a missing `Decodable`
    // instance). Anchoring `Operation` derives it once and lets `PathItem`
    // simply reference it.
    given decodableJson: (Tactic[JsonError], Tactic[JsonPointerError], Tactic[OpenApiError])
    =>  Operation is Json.Decodable = Json.DecodableDerivation.derived

    given decodableYaml: (Tactic[YamlError], Tactic[JsonPointerError], Tactic[OpenApiError])
    =>  Operation is Decodable in Yaml = Yaml.DecodableDerivation.derived

  case class Operation
    ( operationId: Optional[Text]        = Unset,
      summary:     Optional[Text]        = Unset,
      description: Optional[Text]        = Unset,
      parameters:  List[Parameter]       = Nil,
      requestBody: Optional[RequestBody] = Unset,
      responses:   Map[Text, Response]   = Map() )

  object PathItem:
    given (Tactic[JsonError], Tactic[JsonPointerError], Tactic[OpenApiError])
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
      parameters:  List[Parameter]     = Nil ):

    def operations: Map[Http.Method, Operation] =
      val verbs =
        List
          ( Http.Get -> get, Http.Put -> put, Http.Post -> post, Http.Delete -> delete,
            Http.Options -> options, Http.Head -> head, Http.Patch -> patch,
            Http.Trace -> trace )

      verbs
      . stdlib.collect { case (method, operation) if operation.present => method -> operation.vouch }
      . pipe(Map.from(_))

  object Components:
    given (Tactic[JsonError], Tactic[JsonPointerError], Tactic[OpenApiError])
    =>  Components is Json.Decodable = Json.DecodableDerivation.derived

  case class Components(schemas: Map[Text, JsonSchema] = Map())

  // The `responses` map is keyed by status text (`"200"`, `"2XX"`, `"default"`),
  // not all of which are valid `Http.Status` codes; `response` interprets a
  // concrete status against those keys.
  extension (operation: Operation)
    def response(status: Http.Status): Optional[Response] =
      operation.responses.at(status.code.show)
      . or(operation.responses.at(t"${status.code/100}XX"))
      . or(operation.responses.at(t"default"))

  // Resolve a single `$ref` hop against the document's component schemas, via
  // `ref()`. Resolving one hop at a time (never inlining recursively) keeps
  // cyclic schema graphs from looping; the consumer drives the walk. A
  // non-reference schema resolves to itself.
  extension (schema: JsonSchema)
    def apply()(using doc: OpenApi): JsonSchema raises OpenApiError = schema match
      case JsonSchema.Ref(pointer, _, _) =>
        val reference = pointer.encode
        val prefix = t"#/components/schemas/"

        if reference.starts(prefix) then
          val name = reference.skip(prefix.length)

          doc.components.let(_.schemas.at(name)).or:
            abort(OpenApiError(OpenApiError.Reason.UnresolvableRef(reference)))
        else
          abort(OpenApiError(OpenApiError.Reason.UnsupportedRef(reference)))

      case other =>
        other

  // Mirror of jacinta's `JsonSchema is Decodable in Json`, over the `Yaml` AST.
  // jacinta cannot depend on ypsiloid, so the YAML decoder for `JsonSchema`
  // lives here, in scope of the model's own decoders. Kept distinct from the
  // private method so that recursive summons for nested schemas (`items`,
  // `properties`, …) resolve to this fully-defined given rather than to the
  // instance currently being initialised.
  given jsonSchemaYaml: (Tactic[YamlError], Tactic[JsonPointerError])
  =>  JsonSchema is Decodable in Yaml = decodeYamlSchema(_)

  private def decodeYamlSchema(yaml: Yaml)(using Tactic[YamlError], Tactic[JsonPointerError])
  :   JsonSchema =

    def field[value: Decodable in Yaml](name: Text): Optional[value] =
      yaml(name).as[Optional[value]]

    val reference = field[Text]("$ref".tt)

    if !reference.absent
    then JsonSchema.Ref(reference.vouch.as[JsonPointer], field[Text](t"description"))
    else field[Text](t"type") match
      case t"array" =>
        JsonSchema.Array
          ( field[Text](t"description"),
            field[JsonSchema](t"items"),
            field[Int](t"minItems"),
            field[Int](t"maxItems"),
            false,
            field[Int](t"maxContains"),
            field[Int](t"minContains") )

      case t"string" =>
        JsonSchema.String
          ( field[Text](t"description"),
            field[Int](t"minLength"),
            field[Int](t"maxLength"),
            field[Text](t"pattern"),
            field[JsonSchema.Format](t"format"),
            false )

      case t"number" =>
        JsonSchema.Number
          ( field[Text](t"description"),
            field[Double](t"multipleOf"),
            field[Double](t"maximum"),
            field[Double](t"minimum"),
            field[Double](t"exclusiveMinimum"),
            field[Double](t"exclusiveMaximum"),
            false )

      case t"integer" =>
        JsonSchema.Integer
          ( field[Text](t"description"),
            field[Int](t"maximum"),
            field[Int](t"minimum"),
            field[Int](t"exclusiveMinimum"),
            field[Int](t"exclusiveMaximum"),
            false )

      case t"boolean" =>
        JsonSchema.Boolean(field[Text](t"description"), false)

      case t"null" =>
        JsonSchema.Null(field[Text](t"description"), false)

      case _ =>
        JsonSchema.Object
          ( field[Text](t"description"),
            field[Map[Text, JsonSchema]](t"properties").or(Map()),
            false,
            field[List[Text]](t"required"),
            // `enum` holds raw `Json` values, and there is no `Yaml`->`Json`
            // bridge, so enum constraint values are not carried through the YAML
            // path; they do not affect endpoint structure.
            Unset,
            yaml(t"additionalProperties").as[Optional[scala.Boolean]].or(false),
            field[List[JsonSchema]](t"oneOf") )

  // Anchor the top-level model so `as[OpenApi]` (below) materialises its decoder
  // once — with each nested type resolving to its own anchor — rather than inlining
  // the entire OpenAPI graph at the call site (which, with schema-carrying codecs,
  // overflows `-Xmax-inlines` and the JVM class-size limit).
  given decodable: (Tactic[JsonError], Tactic[JsonPointerError], Tactic[OpenApiError])
  =>  OpenApi is Json.Decodable = Json.DecodableDerivation.derived

  // `source.read[OpenApi]`: aggregate the source text, auto-detect JSON vs YAML
  // from the first non-whitespace character, decode through the shared model,
  // then check the document declares an OpenAPI 3.x version.
  given aggregable: Tactic[OpenApiError] => OpenApi is Aggregable by Text =
    summon[Text is Aggregable by Text].map: text =>
      val document =
        mitigate:
          case ParseError(_, _, _)    => OpenApiError(OpenApiError.Reason.Malformed)
          case JsonError(_)           => OpenApiError(OpenApiError.Reason.Malformed)
          case YamlError(_)           => OpenApiError(OpenApiError.Reason.Malformed)
          case JsonPointerError(_, _) => OpenApiError(OpenApiError.Reason.Malformed)

        . protect:
            if text.trim.starts(t"{") || text.trim.starts(t"[")
            then text.as[Json].as[OpenApi]
            else text.as[Yaml].as[OpenApi]

      if document.openapi.starts(t"3.") then document
      else abort(OpenApiError(OpenApiError.Reason.UnsupportedVersion(document.openapi)))

case class OpenApi
  ( openapi:    Text,
    info:       OpenApi.Info,
    servers:    List[OpenApi.Server]        = Nil,
    paths:      Map[Text, OpenApi.PathItem] = Map(),
    components: Optional[OpenApi.Components] = Unset )
