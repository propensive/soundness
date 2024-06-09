/*
    Jacinta, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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

import rudiments.*
import vacuous.*
import fulminate.*
import turbulence.*
import gossamer.*
import anticipation.*
import merino.*
import hieroglyph.*
import wisteria.*
import spectacular.*
import contingency.*

import scala.collection.Factory
import scala.compiletime.*

import language.dynamics
import language.experimental.captureChecking

import JsonAccessError.Reason

erased trait DynamicJsonEnabler

object dynamicJsonAccess:
  erased given enabled: DynamicJsonEnabler = ###

given (using js: JsonPrinter): Show[JsonAst] = js.print(_)

extension (json: JsonAst)
  inline def isNumber: Boolean = isDouble || isLong || isBigDecimal
  inline def isLong: Boolean = json.isInstanceOf[Long]
  inline def isDouble: Boolean = json.isInstanceOf[Double]
  inline def isBigDecimal: Boolean = json.isInstanceOf[BigDecimal]
  inline def isObject: Boolean = json.isInstanceOf[(?, ?)]
  inline def isString: Boolean = json.isInstanceOf[String]
  inline def isBoolean: Boolean = json.isInstanceOf[Boolean]

  inline def isNull: Boolean = json.asMatchable match
    case v: Null => v == null
    case _       => false

  inline def isArray: Boolean = json.isInstanceOf[Array[?]]

  inline def array(using Errant[JsonAccessError]): IArray[JsonAst] =
    if isArray then json.asInstanceOf[IArray[JsonAst]]
    else raise(JsonAccessError(Reason.NotType(JsonPrimitive.Array)))(IArray[JsonAst]())

  inline def double(using Errant[JsonAccessError]): Double = json.asMatchable match
    case value: Double     => value
    case value: Long       => value.toDouble
    case value: BigDecimal => value.toDouble
    case _                 => raise(JsonAccessError(Reason.NotType(JsonPrimitive.Number)))(0.0)

  inline def bigDecimal(using Errant[JsonAccessError]): BigDecimal = json.asMatchable match
    case value: BigDecimal => value
    case value: Long       => BigDecimal(value)
    case value: Double     => BigDecimal(value)
    case _                 => raise(JsonAccessError(Reason.NotType(JsonPrimitive.Number)))(BigDecimal(0))

  inline def long(using Errant[JsonAccessError]): Long = json.asMatchable match
    case value: Long       => value
    case value: Double     => value.toLong
    case value: BigDecimal => value.toLong
    case _                 => raise(JsonAccessError(Reason.NotType(JsonPrimitive.Number)))(0L)

  def primitive: JsonPrimitive =
    if isNumber then JsonPrimitive.Number
    else if isBoolean then JsonPrimitive.Boolean
    else if isString then JsonPrimitive.String
    else if isObject then JsonPrimitive.Object
    else if isArray then JsonPrimitive.Array
    else JsonPrimitive.Null

  inline def string(using Errant[JsonAccessError]): Text =
    if isString then json.asInstanceOf[Text]
    else raise(JsonAccessError(Reason.NotType(JsonPrimitive.String)))("".tt)

  inline def boolean(using Errant[JsonAccessError]): Boolean =
    if isBoolean then json.asInstanceOf[Boolean]
    else raise(JsonAccessError(Reason.NotType(JsonPrimitive.Boolean)))(false)

  inline def obj(using Errant[JsonAccessError]): (IArray[String], IArray[JsonAst]) =
    if isObject then json.asInstanceOf[(IArray[String], IArray[JsonAst])]
    else raise(JsonAccessError(Reason.NotType(JsonPrimitive.Object)))(IArray[String]() -> IArray[JsonAst]())

  inline def number(using Errant[JsonAccessError]): Long | Double | BigDecimal =
    if isLong then long else if isDouble then double else if isBigDecimal then bigDecimal
    else raise(JsonAccessError(Reason.NotType(JsonPrimitive.Number)))(0L)

extension [ValueType](value: ValueType)(using encoder: JsonEncoder[ValueType])
  def json: Json = Json(encoder.encode(value))

object Json extends Dynamic:

  given encoder: Encoder[Json] = json => MinimalJsonPrinter.print(json.root)

  def parse[SourceType](value: SourceType)
      (using readable: Readable[SourceType, Bytes], jsonParse: Errant[JsonParseError])
          : Json^{readable, jsonParse} =

    Json(JsonAst.parse(value))

  given (using JsonPrinter): Show[Json] = json =>
    try json.root.show catch case err: JsonAccessError => t"<${err.reason.show}>"

  given (using encoder: CharEncoder^, printer: JsonPrinter)
      => ((Json is GenericHttpResponseStream)^{encoder}) =
    new GenericHttpResponseStream:
      type Self = Json
      def mediaType: Text = t"application/json; charset=${encoder.encoding.name}"
      def content(json: Json): LazyList[Bytes] = LazyList(json.show.bytes)

  given (using jsonParse: Errant[JsonParseError]) => Decoder[Json] =
    text => Json.parse(LazyList(text.bytes(using charEncoders.utf8)))

  given (using jsonParse: Errant[JsonParseError]) => ((Json is GenericHttpReader)^{jsonParse}) =
    text => Json.parse(LazyList(text.bytes(using charEncoders.utf8)))

  given aggregable[SourceType](using Readable[SourceType, Bytes], Errant[JsonParseError])
          : Aggregable[Bytes, Json] =

    Json.parse(_)

  def applyDynamicNamed(methodName: "of")(elements: (String, Json)*): Json =
    val keys: IArray[String] = IArray.from(elements.map(_(0)))
    val values: IArray[JsonAst] = IArray.from(elements.map(_(1).root))
    Json(JsonAst((keys, values)))

trait JsonEncoder2:
  given optional[ValueType](using encoder: JsonEncoder[ValueType])
      (using util.NotGiven[Unset.type <:< ValueType])
          : JsonEncoder[Optional[ValueType]] =

    new JsonEncoder[Optional[ValueType]]:
      override def omit(value: Optional[ValueType]): Boolean = value.absent
      def encode(value: Optional[ValueType]): JsonAst = value.let(encoder.encode(_)).or(JsonAst(null))

object JsonEncoder extends JsonEncoder2:
  given int: JsonEncoder[Int] = int => JsonAst(int.toLong)
  given text: JsonEncoder[Text] = text => JsonAst(text.s)
  given string: JsonEncoder[String] = JsonAst(_)
  given double: JsonEncoder[Double] = JsonAst(_)
  given long: JsonEncoder[Long] = JsonAst(_)
  given byte: JsonEncoder[Byte] = byte => JsonAst(byte.toLong)
  given short: JsonEncoder[Short] = short => JsonAst(short.toLong)
  given boolean: JsonEncoder[Boolean] = JsonAst(_)
  given json: JsonEncoder[Json] = _.root

  inline given derived[ValueType]: JsonEncoder[ValueType] = summonFrom:
    case given Encoder[ValueType]    => value => JsonAst(value.encode.s)
    case given Reflection[ValueType] => JsonEncoderDerivation.derived[ValueType]

  given (using jsonAccess: Errant[JsonAccessError]) => (JsonEncoder[Json]^{jsonAccess}) as json = _.root
  given nil: JsonEncoder[Nil.type] = value => JsonAst(IArray[JsonAst]())

  given collection[CollectionType[ElementType] <: Iterable[ElementType], ElementType: JsonEncoder]
          : JsonEncoder[CollectionType[ElementType]] =

    values => JsonAst(IArray.from(values.map(summon[JsonEncoder[ElementType]].encode(_))))

  given map[ValueType](using encoder: JsonEncoder[ValueType]): JsonEncoder[Map[String, ValueType]] = map =>
    val keys = new Array[String](map.size)
    val values = new Array[JsonAst](map.size)
    var index = 0

    map.each: (key, value) =>
      keys(index) = key
      values(index) = encoder.encode(value)
      index += 1

    JsonAst(keys.immutable(using Unsafe), values.immutable(using Unsafe))

  given opt[ValueType: JsonEncoder]: JsonEncoder[Option[ValueType]] with
    override def omit(value: Option[ValueType]): Boolean = value.isEmpty

    def encode(value: Option[ValueType]): JsonAst = value match
      case None        => JsonAst(null)
      case Some(value) => summon[JsonEncoder[ValueType]].encode(value)

object JsonEncoderDerivation extends Derivation[JsonEncoder]:
  inline def join[DerivationType <: Product: ProductReflection]: JsonEncoder[DerivationType] = value =>
    val labels = fields(value): [FieldType] =>
      field => label.s

    val values = fields(value): [FieldType] =>
      field => context.encode(field)

    JsonAst((labels, values))

  inline def split[DerivationType: SumReflection]: JsonEncoder[DerivationType] = value =>
    variant(value): [VariantType <: DerivationType] =>
      value => summonInline[Errant[JsonAccessError]].give:
        context.tag(label).encode(value)

trait JsonEncoder[-ValueType]:
  def omit(value: ValueType): Boolean = false
  def encode(value: ValueType): JsonAst

  def contramap[ValueType2](lambda: ValueType2 => ValueType): JsonEncoder[ValueType2]^{this, lambda} =
    encode.compose(lambda)(_)

  def tag(label: Text)(using jsonAccess: Errant[JsonAccessError]): JsonEncoder[ValueType]^{jsonAccess} =
    (value: ValueType) =>
      val (keys, values) = encode(value).obj
      JsonAst((keys :+ "_type", values :+ label.s))

trait JsonDecoder2:
  given [ValueType]
      (using decoder: JsonDecoder[ValueType]^, notGiven: util.NotGiven[Unset.type <:< ValueType])
      => (JsonDecoder[Optional[ValueType]]^{decoder}) as optional =

    new JsonDecoder[Optional[ValueType]]:
      def decode(value: JsonAst, omit: Boolean): Optional[ValueType] =
        if omit then Unset else decoder.decode(value, false)

  // given decoder
  //     [ValueType]
  //     (using jsonAccess: Errant[JsonAccessError], decoder: Decoder[ValueType])
  //     : JsonDecoder[ValueType]^{jsonAccess, decoder} =
  //   (value, omit) => decoder.decode(value.string)

object JsonDecoder extends JsonDecoder2:
  given (using jsonAccess: Errant[JsonAccessError]) => (JsonDecoder[JsonAst]^{jsonAccess}) as jsonAst =
    (value, omit) => value

  given (using jsonAccess: Errant[JsonAccessError]) => (JsonDecoder[Json]^{jsonAccess}) as json =
    (value, omit) => Json(value)

  given (using jsonAccess: Errant[JsonAccessError]) => (JsonDecoder[Int]^{jsonAccess}) as int =
    (value, omit) => value.long.toInt

  given (using jsonAccess: Errant[JsonAccessError]) => (JsonDecoder[Byte]^{jsonAccess}) as byte =
    (value, omit) => value.long.toByte

  given (using jsonAccess: Errant[JsonAccessError]) => (JsonDecoder[Short]^{jsonAccess}) as short =
    (value, omit) => value.long.toShort

  given (using jsonAccess: Errant[JsonAccessError]) => (JsonDecoder[Float]^{jsonAccess}) as float =
    (value, omit) => value.double.toFloat

  given (using jsonAccess: Errant[JsonAccessError]) => (JsonDecoder[Double]^{jsonAccess}) as double =
    (value, omit) => value.double

  given (using jsonAccess: Errant[JsonAccessError]) => (JsonDecoder[Long]^{jsonAccess}) as long =
    (value, omit) => value.long

  given (using jsonAccess: Errant[JsonAccessError]) => (JsonDecoder[Text]^{jsonAccess}) as text =
    (value, omit) => value.string

  given (using jsonAccess: Errant[JsonAccessError]) => (JsonDecoder[String]^{jsonAccess}) as string =
    (value, omit) => value.string.s

  given (using jsonAccess: Errant[JsonAccessError]) => (JsonDecoder[Boolean]^{jsonAccess}) as boolean =
    (value, omit) => value.boolean

  given [ValueType](using decoder: JsonDecoder[ValueType]^)(using Errant[JsonAccessError])
          => (JsonDecoder[Option[ValueType]]^{decoder}) as option =

    new JsonDecoder[Option[ValueType]]:
      def decode(value: JsonAst, omit: Boolean): Option[ValueType] =
        if omit then None else Some(decoder.decode(value, false))

  given [CollectionType[ElementType] <: Iterable[ElementType], ElementType]
      (using decoder:    JsonDecoder[ElementType],
             jsonAccess: Errant[JsonAccessError],
             factory:    Factory[ElementType, CollectionType[ElementType]])
      => (JsonDecoder[CollectionType[ElementType]]^{jsonAccess}) as array =

    new JsonDecoder[CollectionType[ElementType]]:
      def decode(value: JsonAst, omit: Boolean): CollectionType[ElementType] =
        val builder = factory.newBuilder
        value.array.each(builder += decoder.decode(_, false))
        builder.result()

  given [ElementType](using decoder: JsonDecoder[ElementType])(using jsonAccess: Errant[JsonAccessError])
          => (JsonDecoder[Map[String, ElementType]]^{jsonAccess}) as map =

    (value, omit) =>
      val (keys, values) = value.obj

      keys.indices.foldLeft(Map[String, ElementType]()): (acc, index) =>
        acc.updated(keys(index), decoder.decode(values(index), false))

  inline given [ValueType] => JsonDecoder[ValueType] as derived = summonFrom:
    case decoder: Decoder[ValueType] =>
      (value, omit) => decoder.decode(value.string(using summonInline[Errant[JsonAccessError]]))

    case given Reflection[ValueType] =>
      JsonDecoderDerivation.derived[ValueType]

object JsonDecoderDerivation extends Derivation[JsonDecoder]:
  inline def join[DerivationType <: Product: ProductReflection]: JsonDecoder[DerivationType] = (json, omit) =>
    summonInline[Errant[JsonAccessError]].give:
      val keyValues = json.obj
      val values = keyValues(0).zip(keyValues(1)).to(Map)

      construct: [FieldType] =>
        context =>
          val omit = !values.contains(label.s)
          val value = if omit then JsonAst(0L) else values(label.s)
          context.decode(value, omit)

  inline def split[DerivationType: SumReflection]: JsonDecoder[DerivationType] = (json, omit) =>
    summonInline[Errant[JsonAccessError]].give:
      summonInline[Errant[VariantError]].give:
        val values = json.obj

        values(0).indexOf("_type") match
          case -1 =>
            abort(JsonAccessError(Reason.Label(t"_type")))

          case index =>
            delegate(values(1)(index).string): [VariantType <: DerivationType] =>
              context => context.decode(json, omit)

trait JsonDecoder[ValueType]:
  private inline def decoder: this.type = this

  def decode(json: JsonAst, omit: Boolean): ValueType

  def map[ValueType2](lambda: ValueType => ValueType2): JsonDecoder[ValueType2]^{this, lambda} =
    (json, omit) => lambda(decoder.decode(json, omit))

class Json(rootValue: Any) extends Dynamic derives CanEqual:
  def root: JsonAst = rootValue.asInstanceOf[JsonAst]
  def apply(index: Int)(using Errant[JsonAccessError]): Json = Json(root.array(index))

  def selectDynamic(field: String)(using erased DynamicJsonEnabler)(using Errant[JsonAccessError]): Json =
    apply(field.tt)

  def applyDynamic(field: String)(index: Int)(using erased DynamicJsonEnabler, Errant[JsonAccessError]): Json =
    apply(field.tt)(index)

  def apply(field: Text)(using Errant[JsonAccessError]): Json =
    root.obj(0).indexWhere(_ == field.s) match
      case -1    => raise(JsonAccessError(Reason.Label(field)))(this)
      case index => Json(root.obj(1)(index))

  override def hashCode: Int =
    def recur(value: JsonAst): Int =
      value.asMatchable match
        case value: Long       => value.hashCode
        case value: Double     => value.hashCode
        case value: BigDecimal => value.hashCode
        case value: String     => value.hashCode
        case value: Boolean    => value.hashCode

        case value: IArray[JsonAst] @unchecked =>
          value.foldLeft(value.length.hashCode)(_*31^recur(_))

        case (keys, values) => (keys.asMatchable: @unchecked) match
          case keys: IArray[String] @unchecked => (values.asMatchable: @unchecked) match
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

        case (rightKeys, rightValues) => (rightKeys.asMatchable: @unchecked) match
          case rightKeys: IArray[String] => (rightValues.asMatchable: @unchecked) match
            case rightValues: IArray[JsonAst] @unchecked => (left.asMatchable: @unchecked) match
              case (leftKeys, leftValues) => (leftKeys.asMatchable: @unchecked) match
                case leftKeys: IArray[String] @unchecked =>
                  (leftValues.asMatchable: @unchecked) match
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

  def as[ValueType](using decoder: JsonDecoder[ValueType], jsonAccess: Errant[JsonAccessError]): ValueType =
    decoder.decode(root, false)

trait JsonPrinter:
  def print(json: JsonAst): Text

package jsonPrinters:
  given indented: JsonPrinter = IndentedJsonPrinter
  given minimal: JsonPrinter = MinimalJsonPrinter

object MinimalJsonPrinter extends JsonPrinter:
  def print(json: JsonAst): Text = Text.construct:
    def appendString(str: String): Unit =
      str.each:
        case '\t' => append("\\t")
        case '\n' => append("\\n")
        case '\r' => append("\\r")
        case '\\' => append("\\\\")
        case '\f' => append("\\f")
        case char => append(char)

    def recur(json: JsonAst): Unit = json.asMatchable match
      case (keys, values) => (keys.asMatchable: @unchecked) match
        case keys: Array[String] @unchecked => (values.asMatchable: @unchecked) match
          case values: Array[JsonAst] @unchecked =>
            append('{')
            val last = keys.length - 1
            keys.indices.each: i =>
              append('"')
              appendString(keys(i))
              append('"')
              append(':')
              recur(values(i))
              append(if i == last then '}' else ',')

      case array: Array[JsonAst] @unchecked =>
        append('[')
        val last = array.length - 1
        array.indices.each: i =>
          recur(array(i))
          append(if i == last then ']' else ',')

      case long: Long =>
       append(long.toString)

      case double: Double =>
        append(double.toString)

      case string: String =>
        append('"')
        string.tt.chars.each:
          case '\n' => append("\\n")
          case '\"' => append("\\\"")
          case '\\' => append("\\\\")
          case '\r' => append("\\r")
          case '\t' => append("\\t")
          case '\b' => append("\\b")
          case '\f' => append("\\f")
          case char => append(char)
        append('"')

      case boolean: Boolean =>
        append(boolean.toString)
      case _ =>
        append("null")

    recur(json)

// FIXME: Implement this
object IndentedJsonPrinter extends JsonPrinter:
  def print(json: JsonAst): Text = Text.construct:
    def appendString(string: String): Unit =
      string.each:
        case '\t' => append("\\t")
        case '\n' => append("\\n")
        case '\r' => append("\\r")
        case '\\' => append("\\\\")
        case '\f' => append("\\f")
        case ch   => append(ch)

    def recur(json: JsonAst, indent: Int): Unit = json.asMatchable match
      case (keys, values) => (keys.asMatchable: @unchecked) match
        case keys: Array[String] => (values.asMatchable: @unchecked) match
          case values: Array[JsonAst] @unchecked =>
            append('{')
            val last = keys.length - 1

            keys.indices.each: index =>
              append('"')
              appendString(keys(index))
              append('"')
              append(':')
              recur(values(index), indent)
              append(if index == last then '}' else ',')

      case array: Array[JsonAst] @unchecked =>
        append('[')
        val last = array.length - 1

        array.indices.each: index =>
          recur(array(index), indent)
          append(if index == last then ']' else ',')

      case long: Long =>
       append(long.toString)

      case double: Double =>
        append(double.toString)

      case string: String =>
        append('"')
        appendString(string)
        append('"')

      case boolean: Boolean =>
        append(boolean.toString)

      case _ =>
        append("null")

    recur(json, 0)

object JsonAccessError:
  enum Reason:
    case Index(value: Int)
    case Label(label: Text)
    case NotType(primitive: JsonPrimitive)

  object Reason:
    given Reason is Communicable =
      case Index(value)       => msg"the index $value out of range"
      case Label(label)       => msg"the JSON object does not contain the label $label"
      case NotType(primitive) => msg"the JSON value did not have the type $primitive"

case class JsonAccessError(reason: JsonAccessError.Reason)
extends Error(msg"could not access the value because $reason")

object JsonPrimitive:
  given JsonPrimitive is Communicable = primitive => Message(primitive.show)

enum JsonPrimitive:
  case Array, Object, Number, Null, Boolean, String
