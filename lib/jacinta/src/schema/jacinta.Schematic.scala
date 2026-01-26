package jacinta

import scala.collection.mutable as scm
import scala.annotation.*

import adversaria.*
import anticipation.*
import contingency.*
import denominative.*
import distillate.*
import fulminate.*
import gossamer.*
import prepositional.*
import proscenium.*
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
        case entity: JsonSchema.Object => entity.copy(optional = true)
        case entity: JsonSchema.Integer  => entity.copy(optional = true)
        case entity: JsonSchema.Number   => entity.copy(optional = true)
        case entity: JsonSchema.String   => entity.copy(optional = true)
        case entity: JsonSchema.Array    => entity.copy(optional = true)
        case entity: JsonSchema.Boolean  => entity.copy(optional = true)
        case entity: JsonSchema.Null   => entity.copy(optional = true)

  given list: [value: Schematic in JsonSchema] => List[value] is Schematic in JsonSchema =
    () => JsonSchema.Array(items = value.schema())

  given set: [value: Schematic in JsonSchema] => Set[value] is Schematic in JsonSchema =
    () => JsonSchema.Array(items = value.schema())

  given map: [key: Encodable in Text, value: Schematic in JsonSchema]
        =>  Map[key, value] is Schematic in JsonSchema =
    () => JsonSchema.Object(additionalProperties = true)

trait Schematic extends Typeclass, Formal:
  def schema(): Form
