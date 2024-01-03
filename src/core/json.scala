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
import wisteria2.*
import spectacular.*
import perforate.*

import scala.collection.Factory
import scala.compiletime.*
import scala.deriving.*

import language.dynamics
import language.experimental.captureChecking

import JsonAccessError.Reason

erased trait DynamicJsonEnabler

object dynamicJsonAccess:
  erased given enabled: DynamicJsonEnabler = ###

given (using js: JsonPrinter): Show[JsonAst] = js.serialize(_)

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
  
  inline def array(using Raises[JsonAccessError]): IArray[JsonAst] =
    if isArray then json.asInstanceOf[IArray[JsonAst]]
    else raise(JsonAccessError(Reason.NotType(JsonPrimitive.Array)))(IArray[JsonAst]())
  
  inline def double(using Raises[JsonAccessError]): Double = json.asMatchable match
    case value: Double     => value
    case value: Long       => value.toDouble
    case value: BigDecimal => value.toDouble
    case _                 => raise(JsonAccessError(Reason.NotType(JsonPrimitive.Number)))(0.0)
  
  inline def bigDecimal(using Raises[JsonAccessError]): BigDecimal = json.asMatchable match
    case value: BigDecimal => value
    case value: Long       => BigDecimal(value)
    case value: Double     => BigDecimal(value)
    case _                 => raise(JsonAccessError(Reason.NotType(JsonPrimitive.Number)))(BigDecimal(0))
  
  inline def long(using Raises[JsonAccessError]): Long = json.asMatchable match
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

  inline def string(using Raises[JsonAccessError]): Text =
    if isString then json.asInstanceOf[Text]
    else raise(JsonAccessError(Reason.NotType(JsonPrimitive.String)))("".tt)
  
  inline def boolean(using Raises[JsonAccessError]): Boolean =
    if isBoolean then json.asInstanceOf[Boolean]
    else raise(JsonAccessError(Reason.NotType(JsonPrimitive.Boolean)))(false)
  
  inline def obj(using Raises[JsonAccessError]): (IArray[String], IArray[JsonAst]) =
    if isObject then json.asInstanceOf[(IArray[String], IArray[JsonAst])]
    else raise(JsonAccessError(Reason.NotType(JsonPrimitive.Object)))(IArray[String]() -> IArray[JsonAst]())
  
  inline def number(using Raises[JsonAccessError]): Long | Double | BigDecimal =
    if isLong then long else if isDouble then double else if isBigDecimal then bigDecimal
    else raise(JsonAccessError(Reason.NotType(JsonPrimitive.Number)))(0L)
  
extension [ValueType](value: ValueType)(using encoder: JsonEncoder[ValueType])
  def json: Json = Json(encoder.encode(value))

object Json extends Dynamic:
  
  def parse
      [SourceType]
      (value: SourceType)
      (using readable: Readable[SourceType, Bytes], jsonParse: Raises[JsonParseError])
      : Json^{readable, jsonParse} =
    Json(JsonAst.parse(value))

  given (using JsonPrinter): Show[Json] = json =>
    try json.root.show catch case err: JsonAccessError => t"<${err.reason.show}>"

  given
      (using encoder: CharEncoder^, printer: JsonPrinter)
      : GenericHttpResponseStream[Json]^{encoder} =
    new GenericHttpResponseStream[Json]:
      def mediaType: Text = t"application/json; charset=${encoder.encoding.name}"
      def content(json: Json): LazyList[Bytes] = LazyList(json.show.bytes)

  given
      (using jsonParse: Raises[JsonParseError], charEncoder: CharEncoder^)
      : GenericHttpReader[Json]^{jsonParse, charEncoder} =
    text => Json.parse(LazyList(text.bytes))

  given aggregable
      [SourceType]
      (using Readable[SourceType, Bytes], Raises[JsonParseError])
      : Aggregable[Bytes, Json] =
    Json.parse(_)

  def applyDynamicNamed(methodName: "of")(elements: (String, Json)*): Json =
    val keys: IArray[String] = IArray.from(elements.map(_(0)))
    val values: IArray[JsonAst] = IArray.from(elements.map(_(1).root))
    Json(JsonAst((keys, values)))

trait FallbackJsonEncoder:
  given [ValueType](using encoder: JsonEncoder[ValueType]): JsonEncoder[Optional[ValueType]] =
    new JsonEncoder[Optional[ValueType]]:
      override def omit(value: Optional[ValueType]): Boolean = value.absent
      def encode(value: Optional[ValueType]): JsonAst = value.let(encoder.encode(_)).or(JsonAst(null))

object JsonEncoder extends FallbackJsonEncoder:
  given int: JsonEncoder[Int] = int => JsonAst(int.toLong)
  given text: JsonEncoder[Text] = text => JsonAst(text.s)
  given string: JsonEncoder[String] = JsonAst(_)
  given double: JsonEncoder[Double] = JsonAst(_)
  given long: JsonEncoder[Long] = JsonAst(_)
  given byte: JsonEncoder[Byte] = byte => JsonAst(byte.toLong)
  given short: JsonEncoder[Short] = short => JsonAst(short.toLong)
  given boolean: JsonEncoder[Boolean] = JsonAst(_)

  given encoder[ValueType](using encoder: Encoder[ValueType]): JsonEncoder[ValueType]^{encoder} = value =>
    JsonAst(value.encode.s)
  
  given json(using jsonAccess: Raises[JsonAccessError]): JsonEncoder[Json]^{jsonAccess} = _.root
  
  given nil: JsonEncoder[Nil.type] = value => JsonAst(IArray[JsonAst]())

  given collection[CollectionType[ElementType] <: Iterable[ElementType], ElementType: JsonEncoder]
      : JsonEncoder[CollectionType[ElementType]] = values =>
    JsonAst(IArray.from(values.map(summon[JsonEncoder[ElementType]].encode(_))))

  given map[ValueType](using encoder: JsonEncoder[ValueType]): JsonEncoder[Map[String, ValueType]] = map =>
    val keys = new Array[String](map.size)
    val values = new Array[JsonAst](map.size)
    var index = 0
    
    map.foreach: (key, value) =>
      keys(index) = key
      values(index) = encoder.encode(value)
      index += 1
    
    JsonAst(keys.immutable(using Unsafe), values.immutable(using Unsafe))

  given opt[ValueType: JsonEncoder]: JsonEncoder[Option[ValueType]] with
    override def omit(value: Option[ValueType]): Boolean = value.isEmpty
    
    def encode(value: Option[ValueType]): JsonAst = value match
      case None        => JsonAst(null)
      case Some(value) => summon[JsonEncoder[ValueType]].encode(value)

  inline given derived
      [DerivationType](using mirror: Mirror.Of[DerivationType])(using Raises[JsonAccessError])
      : JsonEncoder[DerivationType] =
    inline mirror match
      case given Mirror.ProductOf[DerivationType & Product] => (value: DerivationType) =>
        (value.asMatchable: @unchecked) match
          case value: Product =>
            val labels: IArray[String] =
              Derivation.productOf[DerivationType & Product, JsonEncoder](value)(label.s)
            
            val values: IArray[JsonAst] = Derivation.productOf[DerivationType & Product, JsonEncoder](value):
              typeclass.encode(param)

            JsonAst((labels, values))
    
      case sumMirror: Mirror.SumOf[DerivationType] =>
        (value: DerivationType) =>
          Derivation.sumOf[DerivationType, JsonEncoder](value):
            typeclass.tag(label).encode(value)

trait JsonEncoder[-ValueType]:
  def omit(value: ValueType): Boolean = false
  def encode(value: ValueType): JsonAst
  
  def contraMap[ValueType2](fn: ValueType2 => ValueType): JsonEncoder[ValueType2]^{this, fn} =
    (value: ValueType2) => fn.andThen(encode)(value)

  def tag(label: Text)(using jsonAccess: Raises[JsonAccessError]): JsonEncoder[ValueType]^{jsonAccess} = (value: ValueType) =>
    val (keys, values) = encode(value).obj
    JsonAst((keys :+ "_type", values :+ label.s))

trait FallbackJsonDecoder:
  given maybe[ValueType](using decoder: JsonDecoder[ValueType]^): JsonDecoder[Optional[ValueType]]^{decoder} =
    new JsonDecoder[Optional[ValueType]]:
      def decode(value: JsonAst, missing: Boolean): Optional[ValueType] =
        if missing then Unset else decoder.decode(value, false)
  
  given decoder
      [ValueType]
      (using jsonAccess: Raises[JsonAccessError], decoder: Decoder[ValueType])
      : JsonDecoder[ValueType]^{jsonAccess, decoder} =
    (value, missing) => decoder.decode(value.string)

object JsonDecoder extends FallbackJsonDecoder:
  given jsonAst(using jsonAccess: Raises[JsonAccessError]): JsonDecoder[JsonAst]^{jsonAccess} =
    (value, missing) => value
  
  given json(using jsonAccess: Raises[JsonAccessError]): JsonDecoder[Json]^{jsonAccess} =
    (value, missing) => Json(value)
  
  given int(using jsonAccess: Raises[JsonAccessError]): JsonDecoder[Int]^{jsonAccess} =
    (value, missing) => value.long.toInt
  
  given byte(using jsonAccess: Raises[JsonAccessError]): JsonDecoder[Byte]^{jsonAccess} =
    (value, missing) => value.long.toByte
  
  given short(using jsonAccess: Raises[JsonAccessError]): JsonDecoder[Short]^{jsonAccess} =
    (value, missing) => value.long.toShort
  
  given float(using jsonAccess: Raises[JsonAccessError]): JsonDecoder[Float]^{jsonAccess} =
    (value, missing) => value.double.toFloat
  
  given double(using jsonAccess: Raises[JsonAccessError]): JsonDecoder[Double]^{jsonAccess} =
    (value, missing) => value.double
  
  given long(using jsonAccess: Raises[JsonAccessError]): JsonDecoder[Long]^{jsonAccess} =
    (value, missing) => value.long

  given text(using jsonAccess: Raises[JsonAccessError]): JsonDecoder[Text]^{jsonAccess} =
    (value, missing) => value.string

  given string(using jsonAccess: Raises[JsonAccessError]): JsonDecoder[String]^{jsonAccess} =
    (value, missing) => value.string.s
  
  given boolean(using jsonAccess: Raises[JsonAccessError]): JsonDecoder[Boolean]^{jsonAccess} =
    (value, missing) => value.boolean
  
  given option[ValueType](using decoder: JsonDecoder[ValueType]^)(using Raises[JsonAccessError])
      : JsonDecoder[Option[ValueType]]^{decoder} =
    new JsonDecoder[Option[ValueType]]:
      def decode(value: JsonAst, missing: Boolean): Option[ValueType] =
        if missing then None else Some(decoder.decode(value, false))

  given array
      [CollectionType[ElementType] <: Iterable[ElementType], ElementType]
      (using decoder: JsonDecoder[ElementType], jsonAccess: Raises[JsonAccessError],
          factory: Factory[ElementType, CollectionType[ElementType]])
      : JsonDecoder[CollectionType[ElementType]]^{jsonAccess} =
    new JsonDecoder[CollectionType[ElementType]]:
      def decode(value: JsonAst, missing: Boolean): CollectionType[ElementType] =
        val bld = factory.newBuilder
        value.array.foreach(bld += decoder.decode(_, false))
        bld.result()

  given map[ElementType]
      (using decoder: JsonDecoder[ElementType])
      (using jsonAccess: Raises[JsonAccessError]): JsonDecoder[Map[String, ElementType]]^{jsonAccess} =
    (value, missing) =>
      val (keys, values) = value.obj
        
      keys.indices.foldLeft(Map[String, ElementType]()): (acc, index) =>
        acc.updated(keys(index), decoder.decode(values(index), false))

  inline given derived
      [DerivationType](using mirror: Mirror.Of[DerivationType])(using jsonAccess: Raises[JsonAccessError])
      : JsonDecoder[DerivationType] =
    inline mirror match
      case given Mirror.ProductOf[DerivationType] => (value, missing) =>
        val keyValues = value.obj
        val values = keyValues(0).zip(keyValues(1)).to(Map)
        
        Derivation.productOf[DerivationType, JsonDecoder]:
          val missing = !values.contains(label.s)
          val value = if missing then JsonAst(0L) else values(label.s)
          typeclass.decode(value, missing)
    
      case mirror: Mirror.SumOf[DerivationType] => (value, missing) =>
        val values = value.obj
        
        values(0).indexOf("_type") match
          case -1 =>
            abort(JsonAccessError(Reason.Label(t"_type")))
          
          case index =>
            val discriminant = values(1)(index).string
            Derivation.sumOf[DerivationType, JsonDecoder](discriminant)(typeclass.decode(value, missing))

trait JsonDecoder[ValueType]:
  private inline def decoder: this.type = this
  
  def decode(json: JsonAst, missing: Boolean): ValueType
  def map[ValueType2](fn: ValueType => ValueType2): JsonDecoder[ValueType2]^{this, fn} =
    (json, missing) => fn(decoder.decode(json, missing))

class Json(rootValue: Any) extends Dynamic derives CanEqual:
  def root: JsonAst = rootValue.asInstanceOf[JsonAst]
  def apply(index: Int)(using Raises[JsonAccessError]): Json = Json(root.array(index))
  
  def selectDynamic(field: String)(using erased DynamicJsonEnabler)(using Raises[JsonAccessError]): Json =
    apply(field.tt)

  def applyDynamic(field: String)(index: Int)(using erased DynamicJsonEnabler, Raises[JsonAccessError]): Json =
    apply(field.tt)(index)
  
  def apply(field: Text)(using Raises[JsonAccessError]): Json =
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
            right.length == left.length && right.indices.forall: index =>
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
                    
                      leftMap.keySet == rightMap.keySet && leftMap.keySet.forall: key =>
                        recur(leftMap(key), rightMap(key))
              
              case _ =>
                false
        
        case _ =>
          false

      recur(root, right.root)
    
    case _ =>
      false

  def as[ValueType](using decoder: JsonDecoder[ValueType], jsonAccess: Raises[JsonAccessError]): ValueType =
    decoder.decode(root, false)

trait JsonPrinter:
  def serialize(json: JsonAst): Text

package jsonPrinters:
  given humanReadable: JsonPrinter = HumanReadableSerializer
  given minimal: JsonPrinter = MinimalSerializer

object MinimalSerializer extends JsonPrinter:
  def serialize(json: JsonAst): Text =
    val builder: StringBuilder = StringBuilder()
    def appendString(str: String): Unit =
      str.foreach:
        case '\t' => builder.append("\\t")
        case '\n' => builder.append("\\n")
        case '\r' => builder.append("\\r")
        case '\\' => builder.append("\\\\")
        case '\f' => builder.append("\\f")
        case char => builder.append(char)

    def recur(json: JsonAst): Unit = json.asMatchable match
      case (keys, values) => (keys.asMatchable: @unchecked) match
        case keys: Array[String] @unchecked => (values.asMatchable: @unchecked) match
          case values: Array[JsonAst] @unchecked =>
            builder.append('{')
            val last = keys.length - 1
            keys.indices.foreach: i =>
              builder.append('"')
              appendString(keys(i))
              builder.append('"')
              builder.append(':')
              recur(values(i))
              builder.append(if i == last then '}' else ',')
      
      case array: Array[JsonAst] @unchecked =>
        builder.append('[')
        val last = array.length - 1
        array.indices.foreach: i =>
          recur(array(i))
          builder.append(if i == last then ']' else ',')
      
      case long: Long =>
       builder.append(long.toString)
      
      case double: Double =>
        builder.append(double.toString)
      
      case string: String =>
        builder.append('"')
        appendString(string)
        builder.append('"')
      case boolean: Boolean => builder.append(boolean.toString)
      case _ => builder.append("null")

    recur(json)
    builder.toString.show

// FIXME: Implement this
object HumanReadableSerializer extends JsonPrinter:
  def serialize(json: JsonAst): Text =
    val builder: StringBuilder = StringBuilder()
    
    def appendString(string: String): Unit =
      string.foreach:
        case '\t' => builder.append("\\t")
        case '\n' => builder.append("\\n")
        case '\r' => builder.append("\\r")
        case '\\' => builder.append("\\\\")
        case '\f' => builder.append("\\f")
        case ch   => builder.append(ch)

    def recur(json: JsonAst, indent: Int): Unit = json.asMatchable match
      case (keys, values) => (keys.asMatchable: @unchecked) match
        case keys: Array[String] => (values.asMatchable: @unchecked) match
          case values: Array[JsonAst] @unchecked =>
            builder.append('{')
            val last = keys.length - 1
            
            keys.indices.foreach: index =>
              builder.append('"')
              appendString(keys(index))
              builder.append('"')
              builder.append(':')
              recur(values(index), indent)
              builder.append(if index == last then '}' else ',')
      
      case array: Array[JsonAst] @unchecked =>
        builder.append('[')
        val last = array.length - 1
        
        array.indices.foreach: index =>
          recur(array(index), indent)
          builder.append(if index == last then ']' else ',')
      
      case long: Long =>
       builder.append(long.toString)
      
      case double: Double =>
        builder.append(double.toString)
      
      case string: String =>
        builder.append('"')
        appendString(string)
        builder.append('"')
      
      case boolean: Boolean =>
        builder.append(boolean.toString)
      
      case _ =>
        builder.append("null")

    recur(json, 0)
    builder.text


object JsonAccessError:
  enum Reason:
    case Index(value: Int)
    case Label(label: Text)
    case NotType(primitive: JsonPrimitive)
  
  object Reason:
    given Communicable[Reason] =
      case Index(value)       => msg"the index $value out of range"
      case Label(label)       => msg"the JSON object does not contain the label $label"
      case NotType(primitive) => msg"the JSON value did not have the type $primitive"

case class JsonAccessError(reason: JsonAccessError.Reason)
extends Error(msg"could not access the value because $reason")

object JsonPrimitive:
  given Communicable[JsonPrimitive] = primitive => Message(primitive.show)

enum JsonPrimitive:
  case Array, Object, Number, Null, Boolean, String
