/*
    Jacinta, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package jacinta

import language.dynamics
import language.experimental.pureFunctions

import scala.collection.Factory
import scala.compiletime.*
import scala.collection.mutable as scm

import anticipation.*
import contingency.*
import gossamer.*
import hieroglyph.*
import merino.*
import prepositional.*
import proscenium.*
import rudiments.*
import spectacular.*
import turbulence.*
import vacuous.*
import wisteria.*

import JsonError.Reason

trait Json2:
  given optionalEncodable: [ValueType: Encodable in Json as encodable]
  =>    Optional[ValueType] is Encodable in Json =
    new Encodable:
      type Self = Optional[ValueType]
      type Format = Json

      def encode(value: Optional[ValueType]): Json =
        value.let(encodable.encode(_)).or(Json.ast(JsonAst(Unset)))

  given optional: [ValueType: Decodable in Json] => Tactic[JsonError]
  =>    Optional[ValueType] is Decodable in Json = json =>
    if json.root.isAbsent then Unset else ValueType.decode(json)

  given memory: Tactic[JsonError] => Memory is Decodable in Json = json => json.root.long.b

  inline given decodable: [ValueType] => ValueType is Decodable in Json = summonFrom:
    case given Decoder[ValueType] =>
      summonInline[Tactic[JsonError]].give(_.root.string.decode[ValueType])

    case given Reflection[ValueType] =>
      DecodableDerivation.derived

  inline given encodable: [ValueType] => ValueType is Encodable in Json = summonFrom:
    case given (ValueType is Encodable in Text) => value => Json.ast(JsonAst(value.encode.s))
    case given Reflection[ValueType]            => EncodableDerivation.derived

  object DecodableDerivation extends Derivable[Decodable in Json]:
    inline def join[DerivationType <: Product: ProductReflection]
    :     DerivationType is Decodable in Json =
      json =>
        summonInline[Foci[JsonPointer]].give:
          summonInline[Tactic[JsonError]].give:
            val keyValues = json.root.obj
            val values = keyValues(0).zip(keyValues(1)).to(Map)

            construct: [FieldType] =>
              context =>
                focus(prior.or(JsonPointer()) / label):
                  if !values.contains(label.s)
                  then default().or(context.decode(new Json(JsonAst(Unset))))
                  else context.decode(new Json(values(label.s)))

    inline def split[DerivationType: SumReflection]: DerivationType is Decodable in Json =
      json =>
        summonInline[Tactic[JsonError]].give:
          summonInline[Tactic[VariantError]].give:
            val values = json.root.obj

            values(0).indexOf("_type") match
              case -1 =>
                focus(prior.or(JsonPointer()) / t"_type"):
                  abort(JsonError(Reason.Absent))

              case index =>
                delegate(values(1)(index).string): [VariantType <: DerivationType] =>
                  context => context.decode(json)

  object EncodableDerivation extends Derivable[Encodable in Json]:
    inline def join[DerivationType <: Product: ProductReflection]
    :     DerivationType is Encodable in Json =

      value =>
        summonInline[Foci[JsonPointer]].give:
          val labels: scm.ArrayBuffer[String] = scm.ArrayBuffer()
          val values: scm.ArrayBuffer[JsonAst] = scm.ArrayBuffer()

          fields(value): [FieldType] =>
            field => focus(prior.or(JsonPointer()) / label):
              context.encode(field).root.tap: encoded =>
                if !encoded.isAbsent then
                  labels += label.s
                  values += encoded

          Json.ast
           (JsonAst
             ((labels.toArray.immutable(using Unsafe), values.toArray.immutable(using Unsafe))))

    inline def split[DerivationType: SumReflection]: DerivationType is Encodable in Json = value =>
      variant(value): [VariantType <: DerivationType] =>
        value =>
          Json.ast:
            context.encode(value).root.asMatchable.absolve match
              case (labels, values) => labels.asMatchable.absolve match
                case labels: IArray[String] => values.asMatchable.absolve match
                  case values: IArray[JsonAst] =>
                    JsonAst((("_type" +: labels), (label.asInstanceOf[JsonAst] +: values)))

  // given integral: [IntegralType: Numeric](using Tactic[JsonError])
  // =>    IntegralType is Decodable in Json =
  //   (value, omit) => IntegralType.fromInt(value.root.long.toInt)

object Json extends Json2, Dynamic:
  def ast(value: JsonAst): Json = new Json(value)

  given boolean: Json is Decodable in Json = identity(_)

  given boolean: Tactic[JsonError] => Boolean is Decodable in Json =
    value => value.root.boolean

  given double: Tactic[JsonError] => Double is Decodable in Json =
    value => value.root.double

  given float: Tactic[JsonError] => Float is Decodable in Json =
    value => value.root.double.toFloat

  given long: Tactic[JsonError] => Long is Decodable in Json = _.root.long
  given int: Tactic[JsonError] => Int is Decodable in Json = _.root.long.toInt
  given text: Tactic[JsonError] => Text is Decodable in Json = _.root.string

  given string: Tactic[JsonError] => String is Decodable in Json =
    value => value.root.string.s

  given option: [ValueType: Decodable in Json] => Tactic[JsonError]
  =>    Option[ValueType] is Decodable in Json =

    json => if json.root.isAbsent then None else Some(ValueType.decode(json))

  given optionEncodable: [ValueType: Encodable in Json as encodable]
  =>    Option[ValueType] is Encodable in Json =
    new Encodable:
      type Self = Option[ValueType]
      type Format = Json

      def encode(value: Option[ValueType]): Json = value match
        case None        => Json.ast(JsonAst(Unset))
        case Some(value) => encodable.encode(value)

  given integralEncodable: [IntegralType: Integral] => IntegralType is Encodable in Json =
    integral => Json.ast(JsonAst(IntegralType.toLong(integral)))

  given textEncodable: Text is Encodable in Json = text => Json.ast(JsonAst(text.s))
  given stringEncodable: String is Encodable in Json = string => Json.ast(JsonAst(string))
  given doubleEncodable: Double is Encodable in Json = double => Json.ast(JsonAst(double))
  given intEncodable: Int is Encodable in Json = int => Json.ast(JsonAst(int.toLong))
  given longEncodable: Long is Encodable in Json = long => Json.ast(JsonAst(long))
  given booleanEncodable: Boolean is Encodable in Json = boolean => Json.ast(JsonAst(boolean))
  given jsonEncodable: Json is Encodable in Json = identity(_)

  given [CollectionType <: Iterable, ElementType: Encodable in Json as encodable]
  =>    CollectionType[ElementType] is Encodable in Json =
    values => Json.ast(JsonAst(IArray.from(values.map(encodable.encode(_).root))))

  given array: [CollectionType <: Iterable, ElementType: Decodable in Json]
  =>   (factory:    Factory[ElementType, CollectionType[ElementType]],
        jsonAccess: Tactic[JsonError],
        foci:       Foci[JsonPointer])
  =>    CollectionType[ElementType] is Decodable in Json =
    value =>
      val builder = factory.newBuilder
      value.root.array.each: json =>
        focus(prior.or(JsonPointer()) / ordinal)
         (builder += ElementType.decode(Json.ast(json)))

      builder.result()

  given map: [ElementType: Decodable in Json] => Tactic[JsonError]
  =>    Map[Text, ElementType] is Decodable in Json =

    value =>
      val (keys, values) = value.root.obj

      keys.indices.foldLeft(Map[Text, ElementType]()): (acc, index) =>
        focus(prior.or(JsonPointer()) / keys(index).tt):
          acc.updated(keys(index).tt, ElementType.decode(Json.ast(values(index))))

  given encodable: Json is Encodable in Text = json => JsonPrinter.print(json.root, false)

  inline def parse[SourceType](value: SourceType): Json raises JsonParseError =
    summonFrom:
      case given (SourceType is Readable by Bytes) => Json(JsonAst.parse(value))
      case given (SourceType is Readable by Text)  =>
        Json(JsonAst.parse(value.read[Bytes]))

  given JsonPrinter => Json is Showable = json =>
    try json.root.show catch case err: JsonError => t"<${err.reason.show}>"

  given (encoder: CharEncoder, printer: JsonPrinter)
  =>    Json is Abstractable across HttpStreams into HttpStreams.Content =
    new Abstractable:
      type Self = Json
      type Domain = HttpStreams
      type Result = HttpStreams.Content

      def genericize(json: Json): HttpStreams.Content =
        (t"application/json; charset=${encoder.encoding.name}", Stream(json.show.bytes))

  given Tactic[JsonParseError] => Decoder[Json] =
    text => Json.parse(Stream(text.bytes(using charEncoders.utf8)))

  given Tactic[JsonParseError] => Json is Instantiable across HttpRequests from Text =
    text => Json.parse(Stream(text.bytes(using charEncoders.utf8)))

  def applyDynamicNamed(methodName: "of")(elements: (String, Json)*): Json =
    val keys: IArray[String] = IArray.from(elements.map(_(0)))
    val values: IArray[JsonAst] = IArray.from(elements.map(_(1).root))
    Json(JsonAst((keys, values)))

class Json(rootValue: Any) extends Dynamic derives CanEqual:
  def root: JsonAst = rootValue.asInstanceOf[JsonAst]
  def apply(index: Int): Json raises JsonError = Json(root.array(index))

  def selectDynamic(field: String)(using erased DynamicJsonEnabler): Json raises JsonError =
    apply(field.tt)

  def applyDynamic(field: String)(index: Int)(using erased DynamicJsonEnabler)
  :     Json raises JsonError =
    apply(field.tt)(index)

  def apply(field: Text): Json raises JsonError =
    root.obj(0).indexWhere(_ == field.s) match
      case -1    => focus(prior.or(JsonPointer()) / field)(raise(JsonError(Reason.Absent)) yet this)
      case index => Json(root.obj(1)(index))

  override def hashCode: Int =
    def recur(value: JsonAst): Int = value.asMatchable match
      case value: Long       => value.hashCode
      case value: Double     => value.hashCode
      case value: BigDecimal => value.hashCode
      case value: String     => value.hashCode
      case value: Boolean    => value.hashCode

      case value: IArray[JsonAst] @unchecked =>
        value.foldLeft(value.length.hashCode)(_*31^recur(_))

      case (keys, values) => keys.asMatchable.absolve match
        case keys: IArray[String] @unchecked => values.asMatchable.absolve match
          case values: IArray[JsonAst] @unchecked =>
            keys.zip(values).to(Map).view.mapValues(recur(_)).hashCode

      case _ =>
        0

    recur(root)

  override def equals(right: Any): Boolean = right.asMatchable match
    case right: Json =>
      def recur(left: JsonAst, right: JsonAst): Boolean = right.asMatchable match
        case right: Long     => left.asMatchable match
          case left: Long       => left == right
          case left: Double     => left == right
          case left: BigDecimal => left == BigDecimal(right)
          case _             => false

        case right: Double => left.asMatchable match
          case left: Long       => left == right
          case left: Double     => left == right
          case left: BigDecimal => left == BigDecimal(right)
          case _             => false

        case right: BigDecimal => left.asMatchable match
          case left: Long       => BigDecimal(left) == right
          case left: Double     => BigDecimal(left) == right
          case left: BigDecimal => left == right
          case _             => false

        case right: String => left.asMatchable match
          case left: String => left == right
          case _         => false

        case right: Boolean => left.asMatchable match
          case left: Boolean => left == right
          case _         => false

        case right: IArray[JsonAst] @unchecked => left.asMatchable match
          case left: IArray[JsonAst] @unchecked =>
            right.length == left.length && right.indices.all: index =>
              recur(left(index), right(index))

          case _ =>
            false

        case (rightKeys, rightValues) => rightKeys.asMatchable.absolve match
           case rightKeys: IArray[String] @unchecked => rightValues.asMatchable.absolve match
            case rightValues: IArray[JsonAst] @unchecked => left.asMatchable.absolve match
              case (leftKeys, leftValues) => leftKeys.asMatchable.absolve match
                case leftKeys: IArray[String] @unchecked =>
                  leftValues.asMatchable.absolve match
                    case leftValues: IArray[JsonAst] @unchecked =>
                      val leftMap = leftKeys.zip(leftValues).to(Map)
                      val rightMap = rightKeys.zip(rightValues).to(Map)

                      leftMap.keySet == rightMap.keySet && leftMap.keySet.all: key =>
                        recur(leftMap(key), rightMap(key))

              case _ =>
                false

        case _ =>
          false

      recur(root, right.root)

    case _ =>
      false

  def as[ValueType: Decodable in Json]: ValueType raises JsonError tracks JsonPointer =
    ValueType.decode(this)
