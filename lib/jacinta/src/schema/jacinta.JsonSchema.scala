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
package jacinta

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
        ( properties = fields.map { (label, shape) => (label, reify(shape)) }.to(Map),
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
    Json.Encodable(Morphology.Str): pointer =>
      Json.ast(Json.Ast(pointer.encode.s))

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
    Json.Encodable(Morphology.Any):
      case JsonSchema.Ref(pointer, _, _) =>
        val ref = summon[JsonPointer is Encodable in Text].encoded(pointer).s
        Json.ast(Json.Ast.obj
          ( IArray("$ref"),
            IArray(Json.Ast(ref)) ))

      case other =>
        derivedEncodable.encoded(other)

  // Hand-written rather than derived: a JSON-Schema object is discriminated by
  // the *value* of its `type` field (and may carry none, for `$ref`), which the
  // `type`-as-Scala-subtype derivation cannot model. Decoding by hand also
  // keeps the recursion on nested schemas pointed back at this same given.
  given decodable: (jsonError: Tactic[JsonError], pointerError: Tactic[JsonPointerError])
  =>  JsonSchema is Json.Decodable =
    // The decoder closes over the two resolution-scoped tactics (and, through the nested-
    // schema recursion, over itself), which share the instance's given-resolution lifetime;
    // the whole instance is laundered pure per the codec-thunk seal pattern (see
    // rep/DECISIONS.md).
    caps.unsafe.unsafeAssumePure:
      Json.Decodable(Morphology.Any)(decodeSchema(_))

  // The body of `decodable`, named so the nested-schema recursion below can rebuild its
  // own element codecs explicitly. The local primitive codecs re-expose the (sealed-pure)
  // primitives at a higher priority so the `optional`/`array`/`map` givens' pure by-name
  // inner codecs (their thunks must remain pure to be expressible inside staged quotes)
  // resolve without consulting the enclosing tactics.
  private def decodeSchema(json: Json)
    ( using jsonError: Tactic[JsonError], pointerError: Tactic[JsonPointerError] )
  :   JsonSchema =
    // Sealed pure: a capability-typed local would hide the tactic from every
    // subsequent statement (the statement rule); this whole decoder is already
    // sealed at the `decodable` given, which documents the honesty blockage.
    given textDecodable: (Text is Json.Decodable) = caps.unsafe.unsafeAssumePure(Json.text)
    given intDecodable: (Int is Json.Decodable) = caps.unsafe.unsafeAssumePure(Json.int)

    given doubleDecodable: (Double is Json.Decodable) =
      caps.unsafe.unsafeAssumePure(Json.double)

    given booleanDecodable: (scala.Boolean is Json.Decodable) =
      caps.unsafe.unsafeAssumePure(Json.boolean)

    def field[value](name: Text)(using decodable: (value is Json.Decodable)^)
    :   Optional[value] =
      json(name).as[Optional[value]]

    val reference = json("$ref".tt)

    if !reference.root.isAbsent
    then JsonSchema.Ref(reference.as[JsonPointer], field[Text](t"description"))
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
        // `object`, or an untyped schema (treated as an object). The collection reads
        // take their element codecs explicitly, as sealed-pure vals: resolved
        // implicitly, the synthesized by-name codec thunks re-evaluate tactic-capturing
        // given expansions at the application, aliasing the collection givens'
        // capture-polymorphic tactic parameter — a separation failure. A reference to a
        // pure local val carries no hidden set. The seals are consistent with (and no
        // stronger than) the enclosing whole-instance seal on `decodable`.
        val self: JsonSchema is Json.Decodable =
          caps.unsafe.unsafeAssumePure(Json.Decodable(Morphology.Any)(decodeSchema(_)))

        // A plain val, not the `textDecodable` given alias: a given alias re-evaluates
        // its (tactic-applying) right-hand side inside the synthesized thunk.
        val textDecodable0: Text is Json.Decodable = textDecodable

        val textList: List[Text] is Json.Decodable =
          caps.unsafe.unsafeAssumePure
            (Json.array[List, Text](using summon, jsonError, summon)(using textDecodable0))

        val schemaList: List[JsonSchema] is Json.Decodable =
          caps.unsafe.unsafeAssumePure
            (Json.array[List, JsonSchema](using summon, jsonError, summon)(using self))

        val schemaMap: Map[Text, JsonSchema] is Json.Decodable =
          caps.unsafe.unsafeAssumePure
            (Json.map[Text, JsonSchema](using self)(using summon, jsonError))

        JsonSchema.Object
          ( field[Text](t"description"),
            field[Map[Text, JsonSchema]](t"properties")(using schemaMap).or(Map()),
            false,
            field[List[Text]](t"required")(using textList),
            field[List[Json]](t"enum"),
            field[scala.Boolean](t"additionalProperties").or(false),
            field[List[JsonSchema]](t"oneOf")(using schemaList) )

  given discriminatedUnion: JsonSchema is Discriminable:
    type Form = Json
    type Self = JsonSchema

    import dynamicJsonAccess.enabled

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
            val schema2 = descriptions.at(label).lay(schema.schema()): memo =>
              schema.schema().description = memo.map(_.description).join(t"\n")

            (label, schema2)

        .to(Map)

      val required: List[Text] =
        contexts[derivation]():
          [field] => schema => label.unless(_ => schema.schema().optional)

        . compact
        . to(List)

      Object(properties = map, required = required)


  inline def disjunction[derivation: SumReflection]: derivation is Schematic over JsonSchema =
    () =>
      val descriptions = infer[Annotated by memo under derivation] match
        case annotated: Annotated.Subtypes => annotated.subtypes
        case _                             => Map()

      val schemas =
        choices:
          [variant <: derivation] => schema =>
            descriptions.at(label).lay(schema.schema()): memo =>
              schema.schema().description = memo.map(_.description).join(t"\n")

        . to(List)

      JsonSchema.Object(oneOf = schemas, required = List("kind"))

  object Format:
    given encodable: Format is Encodable in Text = _.toString.tt.uncamel.kebab
    given decodable: Format is Decodable in Text = value => Format.valueOf(value.unkebab.pascal.s)

  enum Format:
    case DateTime, Date, Time, Duration, Email, Hostname, Ipv4, Ipv6, Uri, UriReference,
      UriTemplate, Uuid, JsonPointer, RelativeJsonPointer, Regex

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

  case Object
    ( description:          Optional[Text]             = Unset,
      properties:           Map[Text, JsonSchema]      = Map(),
      optional:             scala.Boolean              = false,
      required:             Optional[List[Text]]       = Unset,
      `enum`:               Optional[List[Json]]       = Unset,
      additionalProperties: scala.Boolean              = false,
      oneOf:                Optional[List[JsonSchema]] = Unset )

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
    ( description:      Optional[Text] = Unset,
      maximum:          Optional[Int]  = Unset,
      minimum:          Optional[Int]  = Unset,
      exclusiveMinimum: Optional[Int]  = Unset,
      exclusiveMaximum: Optional[Int]  = Unset,
      optional:         scala.Boolean  = false )

  case Boolean(description: Optional[Text] = Unset, optional: scala.Boolean = false)
  case Null(description: Optional[Text] = Unset, optional: scala.Boolean = false)

  // A JSON Reference (`{"$ref": "#/…"}`). The pointer is retained unresolved and
  // dereferenced lazily by the consumer, so cyclic schema graphs stay finite.
  case Ref
    ( pointer:     JsonPointer,
      description: Optional[Text] = Unset,
      optional:    scala.Boolean  = false )
