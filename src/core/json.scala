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
  
extension [ValueType](value: ValueType)(using writer: JsonSerializer[ValueType])
  def json: Json = Json(writer.write(value))

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

  def applyDynamicNamed[T <: String](methodName: "of")(elements: (String, Json)*): Json =
    val keys: IArray[String] = IArray.from(elements.map(_(0)))
    val values: IArray[JsonAst] = IArray.from(elements.map(_(1).root))
    Json(JsonAst((keys, values)))

trait FallbackJsonSerializer:
  given [ValueType](using writer: JsonSerializer[ValueType]): JsonSerializer[Optional[ValueType]] =
    new JsonSerializer[Optional[ValueType]]:
      override def omit(value: Optional[ValueType]): Boolean = value.absent
      def write(value: Optional[ValueType]): JsonAst = value.let(writer.write(_)).or(JsonAst(null))

object JsonSerializer extends FallbackJsonSerializer:
  given int: JsonSerializer[Int] = int => JsonAst(int.toLong)
  given text: JsonSerializer[Text] = text => JsonAst(text.s)
  given string: JsonSerializer[String] = JsonAst(_)
  given double: JsonSerializer[Double] = JsonAst(_)
  given long: JsonSerializer[Long] = JsonAst(_)
  given byte: JsonSerializer[Byte] = byte => JsonAst(byte.toLong)
  given short: JsonSerializer[Short] = short => JsonAst(short.toLong)
  given boolean: JsonSerializer[Boolean] = JsonAst(_)

  given encoder[ValueType](using encoder: Encoder[ValueType]): JsonSerializer[ValueType]^{encoder} = value =>
    JsonAst(value.encode.s)
  
  given (using jsonAccess: Raises[JsonAccessError]): JsonSerializer[Json]^{jsonAccess} = _.root
  
  given nil: JsonSerializer[Nil.type] = value => JsonAst(IArray[JsonAst]())

  given collection[CollectionType[ElementType] <: Iterable[ElementType], ElementType: JsonSerializer]
      : JsonSerializer[CollectionType[ElementType]] = values =>
    JsonAst(IArray.from(values.map(summon[JsonSerializer[ElementType]].write(_))))

  given map[ValueType](using serializer: JsonSerializer[ValueType]): JsonSerializer[Map[String, ValueType]] =
    map =>
      val keys = new Array[String](map.size)
      val values = new Array[JsonAst](map.size)
      var index = 0
      
      map.foreach: (key, value) =>
        keys(index) = key
        values(index) = serializer.write(value)
        index += 1
      
      JsonAst(keys.immutable(using Unsafe), values.immutable(using Unsafe))

  given opt[ValueType: JsonSerializer]: JsonSerializer[Option[ValueType]] with
    override def omit(value: Option[ValueType]): Boolean = value.isEmpty
    
    def write(value: Option[ValueType]): JsonAst = value match
      case None        => JsonAst(null)
      case Some(value) => summon[JsonSerializer[ValueType]].write(value)

  inline given derived
      [DerivationType](using mirror: Mirror.Of[DerivationType])(using Raises[JsonAccessError])
      : JsonSerializer[DerivationType] =
    inline mirror match
      case given Mirror.ProductOf[DerivationType & Product] => (value: DerivationType) =>
        (value.asMatchable: @unchecked) match
          case value: Product =>
            val labels: IArray[Text] = Derivation.productOf[DerivationType & Product, JsonSerializer](value)(label)
            
            val values: IArray[JsonAst] = Derivation.productOf[DerivationType & Product, JsonSerializer](value):
              typeclass.write(param)

            (labels, values).asInstanceOf[JsonAst]
    
      case sumMirror: Mirror.SumOf[DerivationType] =>
        (value: DerivationType) =>
          Derivation.sumOf[DerivationType, JsonSerializer](value):
            typeclass.tag(label).write(value)

trait JsonSerializer[-ValueType]:
  def omit(value: ValueType): Boolean = false
  def write(value: ValueType): JsonAst
  
  def contraMap[ValueType2](fn: ValueType2 => ValueType): JsonSerializer[ValueType2]^{this, fn} =
    (value: ValueType2) => fn.andThen(write)(value)

  def tag(label: Text)(using jsonAccess: Raises[JsonAccessError]): JsonSerializer[ValueType]^{jsonAccess} = (value: ValueType) =>
    val (keys, values) = write(value).obj
    (keys :+ "_type", values :+ label.s).asInstanceOf[JsonAst]

trait FallbackJsonDeserializer:
  given maybe[ValueType](using reader: JsonDeserializer[ValueType]^): JsonDeserializer[Optional[ValueType]]^{reader} =
    new JsonDeserializer[Optional[ValueType]]:
      def read(value: JsonAst, missing: Boolean): Optional[ValueType] =
        if missing then Unset else reader.read(value, false)
  
  given decoder
      [ValueType]
      (using jsonAccess: Raises[JsonAccessError], decoder: Decoder[ValueType])
      : JsonDeserializer[ValueType]^{jsonAccess, decoder} =
    (value, missing) => decoder.decode(value.string)

object JsonDeserializer extends FallbackJsonDeserializer:
  given jsonAst(using jsonAccess: Raises[JsonAccessError]): JsonDeserializer[JsonAst]^{jsonAccess} =
    (value, missing) => value
  
  given json(using jsonAccess: Raises[JsonAccessError]): JsonDeserializer[Json]^{jsonAccess} =
    (value, missing) => Json(value)
  
  given int(using jsonAccess: Raises[JsonAccessError]): JsonDeserializer[Int]^{jsonAccess} =
    (value, missing) => value.long.toInt
  
  given byte(using jsonAccess: Raises[JsonAccessError]): JsonDeserializer[Byte]^{jsonAccess} =
    (value, missing) => value.long.toByte
  
  given short(using jsonAccess: Raises[JsonAccessError]): JsonDeserializer[Short]^{jsonAccess} =
    (value, missing) => value.long.toShort
  
  given float(using jsonAccess: Raises[JsonAccessError]): JsonDeserializer[Float]^{jsonAccess} =
    (value, missing) => value.double.toFloat
  
  given double(using jsonAccess: Raises[JsonAccessError]): JsonDeserializer[Double]^{jsonAccess} =
    (value, missing) => value.double
  
  given long(using jsonAccess: Raises[JsonAccessError]): JsonDeserializer[Long]^{jsonAccess} =
    (value, missing) => value.long

  given text(using jsonAccess: Raises[JsonAccessError]): JsonDeserializer[Text]^{jsonAccess} =
    (value, missing) => value.string

  given string(using jsonAccess: Raises[JsonAccessError]): JsonDeserializer[String]^{jsonAccess} =
    (value, missing) => value.string.s
  
  given boolean(using jsonAccess: Raises[JsonAccessError]): JsonDeserializer[Boolean]^{jsonAccess} =
    (value, missing) => value.boolean
  
  given option[ValueType](using reader: JsonDeserializer[ValueType]^)(using Raises[JsonAccessError])
      : JsonDeserializer[Option[ValueType]]^{reader} =
    new JsonDeserializer[Option[ValueType]]:
      def read(value: JsonAst, missing: Boolean): Option[ValueType] =
        if missing then None else Some(reader.read(value, false))

  given array
      [CollectionType[ElementType] <: Iterable[ElementType], ElementType]
      (using reader: JsonDeserializer[ElementType], jsonAccess: Raises[JsonAccessError],
          factory: Factory[ElementType, CollectionType[ElementType]])
      : JsonDeserializer[CollectionType[ElementType]]^{jsonAccess} =
    new JsonDeserializer[CollectionType[ElementType]]:
      def read(value: JsonAst, missing: Boolean): CollectionType[ElementType] =
        val bld = factory.newBuilder
        value.array.foreach(bld += reader.read(_, false))
        bld.result()

  given map[ElementType]
      (using reader: JsonDeserializer[ElementType])
      (using jsonAccess: Raises[JsonAccessError]): JsonDeserializer[Map[String, ElementType]]^{jsonAccess} =
    (value, missing) =>
      val (keys, values) = value.obj
        
      keys.indices.foldLeft(Map[String, ElementType]()): (acc, index) =>
        acc.updated(keys(index), reader.read(values(index), false))

  inline given derived
      [DerivationType](using mirror: Mirror.Of[DerivationType])(using jsonAccess: Raises[JsonAccessError])
      : JsonDeserializer[DerivationType] =
    inline mirror match
      case given Mirror.ProductOf[DerivationType] => (value, missing) =>
        val keyValues = value.obj
        val values = keyValues(0).zip(keyValues(1)).to(Map)
        
        Derivation.productOf[DerivationType, JsonDeserializer]:
          val missing = !values.contains(label.s)
          val value = if missing then 0.asInstanceOf[JsonAst] else values(label.s)
          typeclass.read(value, missing)
    
      case mirror: Mirror.SumOf[DerivationType] => (value, missing) =>
        val values = value.obj
        
        values(0).indexOf("_type") match
          case -1 =>
            abort(JsonAccessError(Reason.Label(t"_type")))
          
          case index =>
            val discriminant = values(1)(index).string
            Derivation.sumOf[DerivationType, JsonDeserializer](discriminant)(typeclass.read(value, missing))

trait JsonDeserializer[ValueType]:
  private inline def reader: this.type = this
  
  def read(json: JsonAst, missing: Boolean): ValueType
  def map[ValueType2](fn: ValueType => ValueType2): JsonDeserializer[ValueType2]^{this, fn} =
    (json, missing) => fn(reader.read(json, missing))

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

  def as[ValueType](using reader: JsonDeserializer[ValueType], jsonAccess: Raises[JsonAccessError]): ValueType =
    reader.read(root, false)

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
