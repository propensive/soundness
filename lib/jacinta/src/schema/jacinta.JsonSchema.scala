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
package jacinta

import scala.caps

import scala.annotation.*

import adversaria.*
import anticipation.*
import contingency.*
import distillate.*
import gossamer.*
import prepositional.*
import rudiments.*
import turbulence.*
import urticose.*
import vacuous.*
import wisteria.*

// Constructors for fused `Encodable & Schematic` / `Decodable & Schematic`
// instances. The schema is taken from the codec itself (`Json.Encodable` /
// `Json.Decodable` *carry* a `schema()`), never resolved independently, so the
// fused instance is coherent by construction: a gated or `… in Text`-branch codec
// always pairs with the schema for exactly what it reads/writes.
//
// These are deliberately *methods*, not givens: a `Decodable & Schematic` (or
// `Encodable & Schematic`) given is a subtype of both its codec typeclass *and*
// `Schematic`, so as givens the two would be ambiguous for any bare `Schematic`
// summon. As methods they never enter implicit resolution; call them where a fused
// instance is wanted (e.g. `given … = jsonSchematics.decodable[T]`).
object jsonSchematics:
  def encodable[value](using encoder: value is Json.Encodable)
  :   value is Encodable & Schematic in Json over JsonSchema =

    new Encodable with Schematic:
      type Self = value
      type Form = Json
      type Transport = JsonSchema
      def encoded(value: value): Json = encoder.encoded(value)
      def schema(): JsonSchema = JsonSchema.reify(encoder.shape())

  def decodable[value](using decoder: value is Json.Decodable)
  :   value is Decodable & Schematic in Json over JsonSchema =

    new Decodable with Schematic:
      type Self = value
      type Form = Json
      type Transport = JsonSchema
      def decoded(json: Json): value = decoder.decoded(json)
      def schema(): JsonSchema = JsonSchema.reify(decoder.shape())


object JsonSchema extends Derivable[Schematic over JsonSchema]:
  // Schema (`Schematic`) instances. Primitives and collections are single-capability
  // (schema-only); products/sums auto-derive; the fused `Encodable & Schematic`
  // lives at lower priority in `JsonSchema2`.
  given byteSchematic: Byte is Schematic over JsonSchema = () => JsonSchema.Integer()
  given shortSchematic: Short is Schematic over JsonSchema = () => JsonSchema.Integer()
  given intSchematic: Int is Schematic over JsonSchema = () => JsonSchema.Integer()
  given longSchematic: Long is Schematic over JsonSchema = () => JsonSchema.Integer()
  given floatSchematic: Float is Schematic over JsonSchema = () => JsonSchema.Number()
  given doubleSchematic: Double is Schematic over JsonSchema = () => JsonSchema.Number()
  given textSchematic: Text is Schematic over JsonSchema = () => JsonSchema.String()
  given emailSchematic: EmailAddress is Schematic over JsonSchema = () => JsonSchema.String()
  given booleanSchematic: scala.Boolean is Schematic over JsonSchema = () => JsonSchema.Boolean()

  // Reifies a codec's format-neutral `Morphology` (carried by `Json.Encodable` /
  // `Json.Decodable`) into a concrete `JsonSchema`. This is the bridge that keeps
  // `jacinta.core` free of any `JsonSchema` dependency while still letting a fused
  // `Encodable & Schematic` / `Decodable & Schematic` expose a real schema that is
  // coherent with the codec (since the `Morphology` was produced by the codec itself).
  def reify(shape: Morphology): JsonSchema = shape match
    case Morphology.Str            => JsonSchema.String()
    case Morphology.Whole          => JsonSchema.Integer()
    case Morphology.Real           => JsonSchema.Number()
    case Morphology.Bool           => JsonSchema.Boolean()
    case Morphology.Empty          => JsonSchema.Null()
    case Morphology.Any            => JsonSchema.Object(additionalProperties = true)
    case Morphology.Opt(inner)     => JsonSchema.optional(reify(inner))
    case Morphology.Arr(items)     => JsonSchema.Array(items = reify(items))
    case Morphology.Dict(_, _)     => JsonSchema.Object(additionalProperties = true)

    case Morphology.OneOf(variants) =>
      JsonSchema.Object(oneOf = variants.map(reify), required = List(t"kind"))

    case Morphology.Obj(fields, required) =>
      JsonSchema.Object
        ( properties = fields.map { (label, shape) => (label, reify(shape)) }.to[Map],
          required   = required )

  // Marks a schema as optional (used both by the schema-only `Schematic` and by
  // the schema-carrying `Json.Encodable`/`Json.Decodable` for `Optional`/`Option`).
  def optional(schema: JsonSchema): JsonSchema = schema match
    case entity: JsonSchema.Object  => entity.copy(optional = true)
    case entity: JsonSchema.Integer => entity.copy(optional = true)
    case entity: JsonSchema.Number  => entity.copy(optional = true)
    case entity: JsonSchema.String  => entity.copy(optional = true)
    case entity: JsonSchema.Array   => entity.copy(optional = true)
    case entity: JsonSchema.Boolean => entity.copy(optional = true)
    case entity: JsonSchema.Null    => entity.copy(optional = true)
    case entity: JsonSchema.Ref     => entity.copy(optional = true)

  given optionalSchematic: [inner <: value, value >: Unset.type: Mandatable to inner]
  =>  ( schematic: inner is Schematic over JsonSchema )
  =>  value is Schematic over JsonSchema =
    () => JsonSchema.optional(schematic.schema())

  given listSchematic: [value: Schematic over JsonSchema]
  =>  List[value] is Schematic over JsonSchema =
    () => JsonSchema.Array(items = value.schema())

  given setSchematic: [value: Schematic over JsonSchema]
  =>  Set[value] is Schematic over JsonSchema =
    () => JsonSchema.Array(items = value.schema())

  given mapSchematic: [key: Encodable in Text, value: Schematic over JsonSchema]
  =>  Map[key, value] is Schematic over JsonSchema =
    () => JsonSchema.Object(additionalProperties = true)

  inline given schematic: [value: Reflection] => value is Schematic over JsonSchema = derived

  // A manual schema for `JsonSchema` itself (the recursive schema-of-schemas type),
  // found in preference to the generic `schematic` derivation, so deriving a schema
  // that nests `JsonSchema` terminates instead of recursing forever.
  given jsonSchemaSchematic: JsonSchema is Schematic over JsonSchema = () => JsonSchema.Object()

  // `JsonPointer` carries a string (its `… in Text` codec). Provided explicitly as a
  // `Json.Encodable` so generic derivation resolves it as a leaf rather than structurally
  // deriving `serpentine.Path`.
  given pointerEncodable: JsonPointer is Json.Encodable =
    Json.Encodable(() => Morphology.Str): pointer => Json.ast(Json.Ast(pointer.encode.s))

  // `$ref` schemas have no `type` discriminator, so they are handled here
  // explicitly; every other variant is delegated to the `type`-discriminated
  // derivation. A `Ref` encodes to a bare `{"$ref": "…"}` object rather than
  // the (meaningless) `{"type": "ref", …}` the discriminated encoder would
  // produce.
  private lazy val derivedEncodable: JsonSchema is Encodable in Json =
    Json.EncodableDerivation.derived

  // A `Json.Encodable`/`Json.Decodable` (schema-carrying) rather than a plain
  // codec, so that the `map`/collection codecs — which now require their element
  // to be a `Json.Encodable`/`Json.Decodable` — resolve `JsonSchema` to *this*
  // hand-written instance instead of recursively deriving a schema-of-schemas
  // (which would diverge). The carried `schema()` is a fixed permissive object.
  given encodable: JsonSchema is Json.Encodable =
    Json.Encodable(() => Morphology.Any):
      case JsonSchema.Ref(pointer, _, _) =>
        val ref = summon[JsonPointer is Encodable in Text].encoded(pointer).s
        // Qualified: `JsonSchema.Array` (this file's schema node) shadows the prelude `Array`.
        Json.ast(Json.Ast.obj
          ( proscenium.Array("$ref"),
            proscenium.Array[Any](Json.Ast(ref)) ))

      case other =>
        derivedEncodable.encoded(other)

  // Hand-written rather than derived: a JSON-Schema object is discriminated by
  // the *value* of its `type` field (and may carry none, for `$ref`), which the
  // `type`-as-Scala-subtype derivation cannot model. Decoding by hand also
  // keeps the recursion on nested schemas pointed back at this same given.
  given decodable: (jsonError: Tactic[Json.Error], pointerError: Tactic[JsonPointer.Error])
  =>  ((JsonSchema is Json.Decodable)^{jsonError, pointerError}) =
    // The decoder captures the two tactics it raises through.
    Json.Decodable(Morphology.Any)(decodeSchema(_))

  // The body of `decodable`, named so the nested-schema recursion below can rebuild its
  // own element codecs explicitly. The local primitive codecs re-expose the primitives, sealed
  // pure, at a higher priority so the `optional`/`array`/`map` givens' pure by-name
  // inner codecs (their thunks must remain pure to be expressible inside staged quotes)
  // resolve without consulting the enclosing tactics.
  private def decodeSchema(json: Json)
    ( using jsonError: Tactic[Json.Error], pointerError: Tactic[JsonPointer.Error] )
  :   JsonSchema =
    // Sealed pure: a capability-typed local would hide the tactic from every
    // subsequent statement (the statement rule).
    // [field-purity] local codec given would hide tactic from statements
    given textDecodable: (Text is Json.Decodable) = caps.unsafe.unsafeAssumePure(Json.text)
    // [field-purity]
    given intDecodable: (Int is Json.Decodable) = caps.unsafe.unsafeAssumePure(Json.int)

    given doubleDecodable: (Double is Json.Decodable) =
      // [field-purity]
      caps.unsafe.unsafeAssumePure(Json.double)

    given booleanDecodable: (scala.Boolean is Json.Decodable) =
      // [field-purity]
      caps.unsafe.unsafeAssumePure(Json.boolean)

    def field[value](name: Text)(using decodable: (value is Json.Decodable)^)
    :   Optional[value] =
      json(name).as[Optional[value]]

    // The collection reads take their element codecs explicitly, as sealed-pure vals: resolved
    // implicitly, the synthesized by-name codec thunks re-evaluate tactic-capturing given
    // expansions at the application, aliasing the collection givens' capture-polymorphic tactic
    // parameter — a separation failure. A reference to a pure local val carries no hidden set.
    // The seals are consistent with (and no stronger than) the enclosing whole-instance seal on
    // `decodable`.
    val self: JsonSchema is Json.Decodable =
      // [by-name-receiver] codec passed by-name aliases collection given's tactic
      caps.unsafe.unsafeAssumePure(Json.Decodable(Morphology.Any)(decodeSchema(_)))

    // A plain val, not the `textDecodable` given alias: a given alias re-evaluates its
    // (tactic-applying) right-hand side inside the synthesized thunk.
    val textDecodable0: Text is Json.Decodable = textDecodable

    val textList: List[Text] is Json.Decodable =
      // [by-name-receiver] by-name codec thunk aliases tactic argument
      caps.unsafe.unsafeAssumePure
        (Json.listDecodable[List, Text](using jsonError, summon)(using textDecodable0))

    val schemaList: List[JsonSchema] is Json.Decodable =
      // [by-name-receiver]
      caps.unsafe.unsafeAssumePure
        (Json.listDecodable[List, JsonSchema](using jsonError, summon)(using self))

    val schemaMap: Map[Text, JsonSchema] is Json.Decodable =
      // [by-name-receiver]
      caps.unsafe.unsafeAssumePure
        (Json.map[Text, JsonSchema](using self)(using summon, jsonError))

    // Every read below inspects the node's kind first, so that a document written to another
    // draft or dialect — a boolean `exclusiveMinimum`, a schema-valued `additionalProperties`,
    // a `type` array — is read for what it says rather than rejected. The decoder is total.
    def numeric(name: Text): Optional[Double] =
      val node = json(name)
      if node.root.isNumber then node.as[Double] else Unset

    def flag(name: Text): scala.Boolean =
      val node = json(name)
      node.root.isBoolean && node.as[scala.Boolean]

    def schema(name: Text): Optional[JsonSchema] =
      val node = json(name)

      if node.root.isObject || node.root.isBoolean then field[JsonSchema](name)(using self)
      else Unset

    def schemas(name: Text): Optional[List[JsonSchema]] =
      val node = json(name)
      if node.root.isArray then field[List[JsonSchema]](name)(using schemaList) else Unset

    // A boolean schema: `true` admits anything, `false` nothing
    if json.root.isBoolean then JsonSchema.Object(additionalProperties = json.as[scala.Boolean])
    else
      val reference = json("$ref".tt)

      // `type` may be one name or (since draft 2019-09, and OpenAPI 3.1) several; a `"null"`
      // among them, or OpenAPI 3.0's `nullable`, admits `null`, which the model records as
      // `optional`.
      val typeNode = json(t"type")

      val types: List[Text] =
        if typeNode.root.isArray then field[List[Text]](t"type")(using textList).or(Nil)
        else if typeNode.root.isString then List(typeNode.as[Text])
        else Nil

      val nullable: scala.Boolean = flag(t"nullable") || types.has(t"null")
      val kind: Optional[Text] = types.filter(_ != t"null").prim

      // Draft 4 (and OpenAPI 3.0) write `exclusiveMinimum: true` to qualify `minimum`; later
      // drafts give it a number of its own. Both read to the same model.
      val exclusiveMinimumFlag = flag(t"exclusiveMinimum")
      val exclusiveMaximumFlag = flag(t"exclusiveMaximum")
      val minimum = if exclusiveMinimumFlag then Unset else numeric(t"minimum")
      val maximum = if exclusiveMaximumFlag then Unset else numeric(t"maximum")

      val exclusiveMinimum =
        if exclusiveMinimumFlag then numeric(t"minimum") else numeric(t"exclusiveMinimum")

      val exclusiveMaximum =
        if exclusiveMaximumFlag then numeric(t"maximum") else numeric(t"exclusiveMaximum")

      val format: Optional[JsonSchema.Format] =
        val node = json(t"format")
        if node.root.isString then node.as[Text].as[JsonSchema.Format] else Unset

      if !reference.root.isAbsent
      then JsonSchema.Ref(reference.as[JsonPointer], field[Text](t"description"), nullable)
      else kind match
        case t"array" =>
          JsonSchema.Array
            ( field[Text](t"description"),
              schema(t"items"),
              numeric(t"minItems").let(_.toInt),
              numeric(t"maxItems").let(_.toInt),
              nullable,
              numeric(t"maxContains").let(_.toInt),
              numeric(t"minContains").let(_.toInt) )

        case t"string" =>
          JsonSchema.String
            ( field[Text](t"description"),
              numeric(t"minLength").let(_.toInt),
              numeric(t"maxLength").let(_.toInt),
              field[Text](t"pattern"),
              format,
              nullable )

        case t"number" =>
          JsonSchema.Number
            ( field[Text](t"description"),
              numeric(t"multipleOf"),
              maximum,
              minimum,
              exclusiveMinimum,
              exclusiveMaximum,
              nullable )

        case t"integer" =>
          JsonSchema.Integer
            ( field[Text](t"description"),
              maximum.let(_.toLong),
              minimum.let(_.toLong),
              exclusiveMinimum.let(_.toLong),
              exclusiveMaximum.let(_.toLong),
              nullable,
              format )

        case t"boolean" =>
          JsonSchema.Boolean(field[Text](t"description"), nullable)

        case t"null" =>
          JsonSchema.Null(field[Text](t"description"), true)

        case _ =>
          // `object`, or an untyped schema (treated as an object)
          val additional = json(t"additionalProperties")

          val additionalSchema =
            if additional.root.isObject then schema(t"additionalProperties") else Unset

          val additionalProperties =
            if additional.root.isBoolean then additional.as[scala.Boolean]
            else additionalSchema.present

          JsonSchema.Object
            ( field[Text](t"description"),
              field[Map[Text, JsonSchema]](t"properties")(using schemaMap).or(Map()),
              nullable,
              field[List[Text]](t"required")(using textList),
              field[List[Json]](t"enum"),
              additionalProperties,
              schemas(t"oneOf"),
              additionalSchema,
              schemas(t"allOf"),
              schemas(t"anyOf"),
              schema(t"not"),
              field[Json](t"const") )

  given discriminatedUnion: JsonSchema is Discriminable:
    type Form = Json
    type Self = JsonSchema

    import dynamicAccess.dynamicJson

    def rewrite(kind: Text, json: Json): Json = unsafely(json.updateDynamic("type")(kind.lower))
    def variant(json: Json): Json = unsafely(json.updateDynamic("type")(Unset))

    def discriminate(json: Json): Optional[Text] =
      // Reads the AST directly rather than via `as[Text]`: the `Text` codec is a
      // tactic-taking given whose instance would capture `safely`'s tactic, which the
      // at-focus decodable evidence cannot retain.
      safely(json.selectField("type").root.string).let(_.capitalize)


  inline def conjunction[derivation <: Product: ProductReflection]
  :   derivation is Schematic over JsonSchema =

    () =>
      val descriptions = infer[derivation is Annotated by memo] match
        case annotated: Annotated.Fields => annotated.fields
        case _                           => Map()

      val map =
        contexts[derivation]():
          [field] => schema =>
            val schema2 = descriptions(label).lay(schema.schema()): memo =>
              schema.schema().description = memo.map(_.description).join(t"\n")

            (label, schema2)

        .pipe(iarr => iarr.readable.to(Map))

      val required: List[Text] =
        contexts[derivation]():
          [field] => schema => label.unless(_ => schema.schema().optional)

        . readable.compact
        . to(proscenium.List)

      Object(properties = map, required = required)


  inline def disjunction[derivation: SumReflection]: derivation is Schematic over JsonSchema =
    () =>
      val descriptions = infer[Annotated by memo under derivation] match
        case annotated: Annotated.Subtypes => annotated.subtypes
        case _                             => Map()

      val schemas =
        choices:
          [variant <: derivation] => schema =>
            descriptions(label).lay(schema.schema()): memo =>
              schema.schema().description = memo.map(_.description).join(t"\n")

        . to[List]

      JsonSchema.Object(oneOf = schemas, required = List("kind"))

  object Format:
    given encodable: Format is Encodable in Text =
      case Other(name) => name
      case format      => format.toString.tt.uncamel.kebab

    // A schema's `format` is an open vocabulary: JSON Schema's own names, OpenAPI's additions
    // (`int64`, `binary`, `password`), and whatever a document invents (`snowflake`,
    // `unix-time`). Decoding is therefore total, keeping an unknown name as `Other`.
    given decodable: Format is Decodable in Text = value => value.s match
      case "date-time"             => DateTime
      case "date"                  => Date
      case "time"                  => Time
      case "duration"              => Duration
      case "email"                 => Email
      case "hostname"              => Hostname
      case "ipv4"                  => Ipv4
      case "ipv6"                  => Ipv6
      case "uri"                   => Uri
      case "uri-reference"         => UriReference
      case "uri-template"          => UriTemplate
      case "uuid"                  => Uuid
      case "json-pointer"          => JsonPointer
      case "relative-json-pointer" => RelativeJsonPointer
      case "regex"                 => Regex
      case "int32"                 => Int32
      case "int64"                 => Int64
      case "float"                 => Float
      case "double"                => Double
      case "byte"                  => Byte
      case "binary"                => Binary
      case "password"              => Password
      case other                   => Other(other.tt)

  enum Format:
    case DateTime, Date, Time, Duration, Email, Hostname, Ipv4, Ipv6, Uri, UriReference,
      UriTemplate, Uuid, JsonPointer, RelativeJsonPointer, Regex, Int32, Int64, Float, Double,
      Byte, Binary, Password

    case Other(name: Text)

enum JsonSchema extends Documentary:
  def optional: scala.Boolean
  def description: Optional[Text]

  def `description_=`(description: Text): JsonSchema = this match
    case entity: Object  => entity.copy(description = description)
    case entity: Array   => entity.copy(description = description)
    case entity: String  => entity.copy(description = description)
    case entity: Number  => entity.copy(description = description)
    case entity: Integer => entity.copy(description = description)
    case entity: Boolean => entity.copy(description = description)
    case entity: Null    => entity.copy(description = description)
    case entity: Ref     => entity.copy(description = description)

  // `additionalProperties` is `true` when the schema admits further properties, whether by a
  // bare `true` or by a schema for them, which `additionalSchema` then carries.
  case Object
    ( description:          Optional[Text]             = Unset,
      properties:           Map[Text, JsonSchema]      = Map(),
      optional:             scala.Boolean              = false,
      required:             Optional[List[Text]]       = Unset,
      `enum`:               Optional[List[Json]]       = Unset,
      additionalProperties: scala.Boolean              = false,
      oneOf:                Optional[List[JsonSchema]] = Unset,
      additionalSchema:     Optional[JsonSchema]       = Unset,
      allOf:                Optional[List[JsonSchema]] = Unset,
      anyOf:                Optional[List[JsonSchema]] = Unset,
      not:                  Optional[JsonSchema]       = Unset,
      const:                Optional[Json]             = Unset )

  case Array
    ( description: Optional[Text]       = Unset,
      items:       Optional[JsonSchema] = Unset,
      minItems:    Optional[Int]        = Unset,
      maxItems:    Optional[Int]        = Unset,
      optional:    scala.Boolean        = false,
      maxContains: Optional[Int]        = Unset,
      minContains: Optional[Int]        = Unset )

  case String
    ( description: Optional[Text]              = Unset,
      minLength:   Optional[Int]               = Unset,
      maxLength:   Optional[Int]               = Unset,
      pattern:     Optional[Text]              = Unset,
      format:      Optional[JsonSchema.Format] = Unset,
      optional:    scala.Boolean               = false )

  case Number
    ( description:      Optional[Text]   = Unset,
      multipleOf:       Optional[Double] = Unset,
      maximum:          Optional[Double] = Unset,
      minimum:          Optional[Double] = Unset,
      exclusiveMinimum: Optional[Double] = Unset,
      exclusiveMaximum: Optional[Double] = Unset,
      optional:         scala.Boolean    = false )

  case Integer
    ( description:      Optional[Text]              = Unset,
      maximum:          Optional[Long]              = Unset,
      minimum:          Optional[Long]              = Unset,
      exclusiveMinimum: Optional[Long]              = Unset,
      exclusiveMaximum: Optional[Long]              = Unset,
      optional:         scala.Boolean               = false,
      format:           Optional[JsonSchema.Format] = Unset )

  case Boolean(description: Optional[Text] = Unset, optional: scala.Boolean = false)
  case Null(description: Optional[Text] = Unset, optional: scala.Boolean = false)

  // A JSON Reference (`{"$ref": "#/…"}`). The pointer is retained unresolved and
  // dereferenced lazily by the consumer, so cyclic schema graphs stay finite.
  case Ref
    ( pointer:     JsonPointer,
      description: Optional[Text] = Unset,
      optional:    scala.Boolean  = false )
