package jacinta

import scala.collection.mutable as scm
import scala.annotation.*

import anticipation.*
import contingency.*
import denominative.*
import distillate.*
import fulminate.*
import gossamer.*
import prepositional.*
import rudiments.*
import serpentine.*
import telekinesis.*
import turbulence.*
import urticose.*
import vacuous.*
import wisteria.*
import zephyrine.*

object Schematic:
  given byte: Byte is Schematic in JsonSchema = () => JsonSchema.Integer()
  given short: Short is Schematic in JsonSchema = () => JsonSchema.Integer()
  given int: Int is Schematic in JsonSchema = () => JsonSchema.Integer()
  given long: Long is Schematic in JsonSchema = () => JsonSchema.Integer()
  given float: Float is Schematic in JsonSchema = () => JsonSchema.Number()
  given double: Double is Schematic in JsonSchema = () => JsonSchema.Number()
  given text: Text is Schematic in JsonSchema = () => JsonSchema.String()
  given email: EmailAddress is Schematic in JsonSchema = () => JsonSchema.String()
  given boolean: Boolean is Schematic in JsonSchema = () => JsonSchema.Boolean()

  given optional: [value: Schematic in JsonSchema] => Optional[value] is Schematic in JsonSchema =
    () =>
      value.schema() match
        case entity: JsonSchema.Object  => entity.copy(optional = true)
        case entity: JsonSchema.Integer => entity.copy(optional = true)
        case entity: JsonSchema.Number  => entity.copy(optional = true)
        case entity: JsonSchema.String  => entity.copy(optional = true)
        case entity: JsonSchema.Array   => entity.copy(optional = true)
        case entity: JsonSchema.Boolean => entity.copy(optional = true)
        case entity: JsonSchema.Null    => entity.copy(optional = true)

  given list: [value: Schematic in JsonSchema] => List[value] is Schematic in JsonSchema =
    () => JsonSchema.Array(items = value.schema())

  given set: [value: Schematic in JsonSchema] => Set[value] is Schematic in JsonSchema =
    () => JsonSchema.Array(items = value.schema(), uniqueItems = true)

  given map: [key: Encodable in Text, value: Schematic in JsonSchema]
        =>  Map[key, value] is Schematic in JsonSchema =
    () => JsonSchema.Object(additionalProperties = value.schema())

trait Schematic extends Typeclass, Formal:
  def schema(): Form


object JsonSchema extends ProductDerivable[Schematic in JsonSchema]:
  inline def join[derivation <: Product: ProductReflection]: derivation is Schematic in JsonSchema =
    () =>
      val map =
        contexts { [field] => schema => (label, schema.schema()) }.to(Map)

      val required: List[Text] =
        contexts:
          [field] => schema => label.unless(schema.schema().optional)
        . compact
        . to(List)

      Object(/*properties = map, */required = required)

  object Format:
    given encodable: Format is Encodable in Text = _.toString.tt.uncamel.kebab
    given decodable: Format is Decodable in Text = value => Format.valueOf(value.unkebab.pascal.s)

  enum Format:
    case DateTime, Date, Time, Duration, Email, Hostname, Ipv4, Ipv6, Uri, UriReference,
      UriTemplate, Uuid, JsonPointer, RelativeJsonPointer, Regex


case class memo() extends StaticAnnotation

enum JsonSchema extends Documentary:

  def optional: scala.Boolean
  def description: Optional[Text]

  case Object
        (description:          Optional[Text]        = Unset,
         properties:           Map[Text, JsonSchema] = Map(),
         optional:             scala.Boolean         = false,
         required:             List[Text]            = Nil,
         `enum`:               List[Json]            = Nil,
         additionalProperties: Optional[JsonSchema]  = Unset)

  case Array
        (description: Optional[Text]       = Unset,
         items:       Optional[JsonSchema] = Unset,
         minItems:    Optional[Int]        = Unset,
         maxItems:    Optional[Int]        = Unset,
         optional:    scala.Boolean        = false,
         uniqueItems: scala.Boolean        = false,
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


object JsonPointer:
  trait Registry:
    private val documents: scm.HashMap[HttpUrl, Json] = scm.HashMap()
    def update(url: HttpUrl, document: Json): Unit = documents(url) = document
    def apply(url: HttpUrl): Optional[Json] = documents.at(url).or(lookup(url))
    protected def lookup(url: HttpUrl): Optional[Json]

  given navigable: [ordinal <: Ordinal] => ordinal is Navigable on JsonPointer = _.n0.toString.tt
  given admissible: [ordinal <: Ordinal] => ordinal is Admissible on JsonPointer = _ => ()
  given admissible2: [text <: Text] => text is Admissible on JsonPointer = _ => ()

  given filesystem: JsonPointer is Filesystem:
    override def escape(text: Text): Text = text.sub("~", "~0").sub("/", "~1")
    override def unescape(text: Text): Text = text.sub("~1", "/").sub("~0", "~")

    val parent: Text = ".."
    val self: Text = "#"
    val separator: Text = "/"

  given JsonPointer is Encodable in Text = pointer =>
    t"${pointer.url.let(_.encode).or(t"")}#${pointer.path}"

case class JsonPointerError()(using Diagnostics) extends Error(m"could not resolve JSON pointer")

case class JsonPointer(url: Optional[HttpUrl], path: Path on JsonPointer):
  def apply(using registry: JsonPointer.Registry)(document: Json): Json raises JsonPointerError =
    url.let(registry(_).lest(JsonPointerError())).or(document)

  def apply(ordinal: Ordinal): JsonPointer = JsonPointer(url, path / ordinal)
  def apply(text: Text): JsonPointer = JsonPointer(url, path / text)

package jsonPointerRegistries:
  given standalone: JsonPointer.Registry:
    protected def lookup(url: HttpUrl): Optional[Json] = Unset

  given fetching: (Online, HttpEvent is Loggable, HttpClient) => JsonPointer.Registry:
    protected def lookup(url: HttpUrl): Optional[Json] =
      recover:
        case VariantError(_, _, _) => Unset
        case ConnectError(_)       => Unset
        case HttpError(_, _)       => Unset
        case ParseError(_, _, _)   => Unset
        case JsonError(_)          => Unset
      . within(url.fetch().receive[Json])
