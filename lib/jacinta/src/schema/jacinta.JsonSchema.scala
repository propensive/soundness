package jacinta

import scala.collection.mutable as scm
import scala.annotation.*

import adversaria.*
import anticipation.*
import contingency.*
import distillate.*
import gossamer.*
import prepositional.*
import proscenium.*
import rudiments.*
import turbulence.*
import vacuous.*
import wisteria.*


object JsonSchema extends Derivable[Schematic in JsonSchema]:

  given encodable: JsonSchema is Encodable in Json = Json.EncodableDerivation.derived

  given discriminatedUnion: JsonSchema is Discriminable:
    type Form = Json
    type Self = JsonSchema

    import dynamicJsonAccess.enabled

    def rewrite(kind: Text, json: Json): Json = unsafely(json.updateDynamic("type")(kind.lower))
    def variant(json: Json): Json = unsafely(json.updateDynamic("type")(Unset))

    def discriminate(json: Json): Optional[Text] =
      safely(json.selectDynamic("type").as[Text]).let(_.capitalize)

  inline def join[derivation <: Product: ProductReflection]: derivation is Schematic in JsonSchema =
    () =>
      val descriptions = infer[derivation is Annotated by memo] match
        case annotated: Annotated.Fields => annotated.fields
        case _                             => Map()

      val map =
        contexts:
          [field] => schema =>
            val schema2 = descriptions.at(label).lay(schema.schema()): memo =>
              schema.schema().description = memo.map(_.description).join(t"\n")
            (label, schema2)
        .to(Map)

      val required: List[Text] =
        contexts:
          [field] => schema => label.unless(schema.schema().optional)
        . compact
        . to(List)

      Object(properties = map, required = required)

  inline def split[derivation: SumReflection]: derivation is Schematic in JsonSchema =
    () =>
      val descriptions = infer[Annotated by memo under derivation] match
        case annotated: Annotated.Subtypes => annotated.subtypes
        case _                             => Map()

      val schemas = variantLabels.map: label =>
        delegate(label):
          [variant <: derivation] => schema =>
            descriptions.at(label).lay(schema.schema()): memo =>
              schema.schema().description = memo.map(_.description).join(t"\n")

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


  case Object
        (description:          Optional[Text]             = Unset,
         properties:           Map[Text, JsonSchema]      = Map(),
         optional:             scala.Boolean              = false,
         required:             Optional[List[Text]]       = Unset,
         `enum`:               Optional[List[Json]]       = Unset,
         additionalProperties: scala.Boolean              = false,
         oneOf:                Optional[List[JsonSchema]] = Unset)

  case Array
        (description: Optional[Text]       = Unset,
         items:       Optional[JsonSchema] = Unset,
         minItems:    Optional[Int]        = Unset,
         maxItems:    Optional[Int]        = Unset,
         optional:    scala.Boolean        = false,
         maxContains: Optional[Int]        = Unset,
         minContains: Optional[Int]        = Unset)

  case String
        (description: Optional[Text]              = Unset,
         minLength:   Optional[Int]               = Unset,
         maxLength:   Optional[Int]               = Unset,
         pattern:     Optional[Text]              = Unset,
         format:      Optional[JsonSchema.Format] = Unset,
         optional:    scala.Boolean               = false)

  case Number
        (description:      Optional[Text]   = Unset,
         multipleOf:       Optional[Double] = Unset,
         maximum:          Optional[Double] = Unset,
         minimum:          Optional[Double] = Unset,
         exclusiveMinimum: Optional[Double] = Unset,
         exclusiveMaximum: Optional[Double] = Unset,
         optional:         scala.Boolean    = false)

  case Integer
        (description:      Optional[Text] = Unset,
         maximum:          Optional[Int]  = Unset,
         minimum:          Optional[Int]  = Unset,
         exclusiveMinimum: Optional[Int]  = Unset,
         exclusiveMaximum: Optional[Int]  = Unset,
         optional:         scala.Boolean  = false)

  case Boolean(description: Optional[Text] = Unset, optional: scala.Boolean = false)
  case Null(description: Optional[Text] = Unset, optional: scala.Boolean = false)
