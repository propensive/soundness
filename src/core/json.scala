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
  
extension [T](value: T)(using writer: JsonSerializer[T])
  def json: Json = Json(writer.write(value))

object Json extends Dynamic:
  
  given transport: Transport[Json] with
    type Writer[-DataType] = JsonSerializer[DataType]
    type Reader[DataType] = JsonDeserializer[DataType]

    def write[DataType: Writer](value: DataType): LazyList[Bytes] = ???
    def read[DataType: Reader](value: LazyList[Bytes]): DataType = ???


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
  given [T](using writer: JsonSerializer[T]): JsonSerializer[Optional[T]] = new JsonSerializer[Optional[T]]:
    override def omit(t: Optional[T]): Boolean = t.absent
    def write(value: Optional[T]): JsonAst = value.let(writer.write(_)).or(JsonAst(null))

object JsonSerializer extends FallbackJsonSerializer:
  given JsonSerializer[Int] = int => JsonAst(int.toLong)
  given JsonSerializer[Text] = text => JsonAst(text.s)
  given JsonSerializer[String] = JsonAst(_)
  given JsonSerializer[Double] = JsonAst(_)
  given JsonSerializer[Long] = JsonAst(_)
  given JsonSerializer[Byte] = byte => JsonAst(byte.toLong)
  given JsonSerializer[Short] = short => JsonAst(short.toLong)
  given JsonSerializer[Boolean] = JsonAst(_)

  //given [T](using canon: Canonical[T]): JsonSerializer[T] = v => JsonAst(canon.serialize(v).s)
  
  given (using jsonAccess: Raises[JsonAccessError]): JsonSerializer[Json]^{jsonAccess} = _.root
  
  given JsonSerializer[Nil.type] = value => JsonAst(IArray[JsonAst]())

  given [Coll[T1] <: Iterable[T1], T: JsonSerializer]: JsonSerializer[Coll[T]] = values =>
    JsonAst(IArray.from(values.map(summon[JsonSerializer[T]].write(_))))

  //given [T: JsonSerializer]: JsonSerializer[Map[String, T]] = values =>
  //  JObject(mutable.Map(values.view.mapValues(summon[JsonSerializer[T]].write(_)).to(Seq)*))

  given opt[ValueType: JsonSerializer]: JsonSerializer[Option[ValueType]] with
    override def omit(value: Option[ValueType]): Boolean = value.isEmpty
    
    def write(value: Option[ValueType]): JsonAst = value match
      case None        => JsonAst(null)
      case Some(value) => summon[JsonSerializer[ValueType]].write(value)

  private transparent inline def deriveProduct
      [LabelsType <: Tuple]
      (tuple: Tuple, labels: Array[String], values: Array[JsonAst], index: Int)
      : (Array[String], Array[JsonAst]) =
    inline tuple match
      case EmptyTuple => (labels, values)
      case cons: (? *: ?) => cons match
        case head *: tail => inline erasedValue[LabelsType] match
          case _: (headLabel *: tailLabels) => inline valueOf[headLabel].asMatchable match
            case label: String =>
              labels(index) = label
              values(index) = summonInline[JsonSerializer[head.type]].write(head)
              deriveProduct[tailLabels](tail, labels, values, index + 1)

  private transparent inline def deriveSum
      [TupleType <: Tuple, DerivedType, LabelsType <: Tuple]
      (ordinal: Int)(using Raises[JsonAccessError])
      : JsonSerializer[DerivedType] =
    inline erasedValue[TupleType] match
      case _: (head *: tail) => inline erasedValue[LabelsType] match
        case _: (headLabel *: tailLabel) =>
          if ordinal == 0 then inline valueOf[headLabel].asMatchable match
            case label: String =>
              summonInline[JsonSerializer[head]].tag(label).asInstanceOf[JsonSerializer[DerivedType]]
          else deriveSum[tail, DerivedType, tailLabel](ordinal - 1)
      
      case _ =>
        throw Mistake(msg"could not match subtype in its apparent coproduct type")

  inline given derived
      [DerivationType](using mirror: Mirror.Of[DerivationType])(using Raises[JsonAccessError])
      : JsonSerializer[DerivationType] =
    inline mirror match
      case given Mirror.ProductOf[DerivationType & Product] => (value: DerivationType) =>
        (value.asMatchable: @unchecked) match
          case value: Product =>
            val labels: Array[String] = new Array(value.productArity)
            val values: Array[JsonAst] = new Array(value.productArity)
            
            deriveProduct[mirror.MirroredElemLabels](Tuple.fromProductTyped(value), labels, values,
                0).asInstanceOf[JsonAst]
    
      case sumMirror: Mirror.SumOf[DerivationType] =>
        (value: DerivationType) =>
          deriveSum[sumMirror.MirroredElemTypes, DerivationType,
              sumMirror.MirroredElemLabels](sumMirror.ordinal(value)).write(value)

trait JsonSerializer[-ValueType]:
  def omit(value: ValueType): Boolean = false
  def write(value: ValueType): JsonAst
  
  def contraMap[ValueType2](fn: ValueType2 => ValueType): JsonSerializer[ValueType2]^{this, fn} =
    (value: ValueType2) => fn.andThen(write)(value)

  def tag(label: String)(using jsonAccess: Raises[JsonAccessError]): JsonSerializer[ValueType]^{jsonAccess} = (value: ValueType) =>
    val (keys, values) = write(value).obj
    (keys :+ "_type", values :+ label).asInstanceOf[JsonAst]

trait FallbackJsonDeserializer:
  given maybe[T](using reader: JsonDeserializer[T]^): JsonDeserializer[Optional[T]]^{reader} =
    new JsonDeserializer[Optional[T]]:
      def read(value: JsonAst, missing: Boolean): Optional[T] =
        if missing then Unset else reader.read(value, false)

object JsonDeserializer extends FallbackJsonDeserializer:
  given jsonAst(using jsonAccess: Raises[JsonAccessError]): JsonDeserializer[JsonAst]^{jsonAccess} = (value, missing) => value
  given json(using jsonAccess: Raises[JsonAccessError]): JsonDeserializer[Json]^{jsonAccess} = (value, missing) => Json(value)
  given int(using jsonAccess: Raises[JsonAccessError]): JsonDeserializer[Int]^{jsonAccess} = (value, missing) => value.long.toInt
  given byte(using jsonAccess: Raises[JsonAccessError]): JsonDeserializer[Byte]^{jsonAccess} = (value, missing) => value.long.toByte
  given short(using jsonAccess: Raises[JsonAccessError]): JsonDeserializer[Short]^{jsonAccess} = (value, missing) => value.long.toShort
  given float(using jsonAccess: Raises[JsonAccessError]): JsonDeserializer[Float]^{jsonAccess} = (value, missing) => value.double.toFloat
  given double(using jsonAccess: Raises[JsonAccessError]): JsonDeserializer[Double]^{jsonAccess} = (value, missing) => value.double
  given long(using jsonAccess: Raises[JsonAccessError]): JsonDeserializer[Long]^{jsonAccess} = (value, missing) => value.long
  given text(using jsonAccess: Raises[JsonAccessError]): JsonDeserializer[Text]^{jsonAccess} = (value, missing) => value.string
  given string(using jsonAccess: Raises[JsonAccessError]): JsonDeserializer[String]^{jsonAccess} = (value, missing) => value.string.s
  given boolean(using jsonAccess: Raises[JsonAccessError]): JsonDeserializer[Boolean]^{jsonAccess} = (value, missing) => value.boolean
  
  //given [T](using canon: Canonical[T]): JsonDeserializer[T] = v => canon.deserialize(v.string)

  given option[T](using reader: JsonDeserializer[T]^)(using Raises[JsonAccessError]): JsonDeserializer[Option[T]]^{reader} =
    new JsonDeserializer[Option[T]]:
      def read(value: JsonAst, missing: Boolean): Option[T] =
        if missing then None else Some(reader.read(value, false))

  given array[Coll[T1] <: Iterable[T1], T]
              (using reader: JsonDeserializer[T], jsonAccess: Raises[JsonAccessError], factory: Factory[T, Coll[T]]): JsonDeserializer[Coll[T]]^{jsonAccess} =
    new JsonDeserializer[Coll[T]]:
      def read(value: JsonAst, missing: Boolean): Coll[T] =
        val bld = factory.newBuilder
        value.array.foreach(bld += reader.read(_, false))
        bld.result()

  given map[T](using reader: JsonDeserializer[T])(using jsonAccess: Raises[JsonAccessError]): JsonDeserializer[Map[String, T]]^{jsonAccess} =
    new JsonDeserializer[Map[String, T]]:
      def read(value: JsonAst, missing: Boolean): Map[String, T] =
        val (keys, values) = value.obj
        
        keys.indices.foldLeft(Map[String, T]()): (acc, index) =>
          acc.updated(keys(index), reader.read(values(index), false))

  private transparent inline def deriveProduct
      [LabelsType <: Tuple, ParamsType <: Tuple]
      (values: Map[String, JsonAst])
      : Tuple =
    inline erasedValue[ParamsType] match
      case _: (paramHead *: paramsTail) => inline erasedValue[LabelsType] match
        case _: (labelHead *: labelsTail) => inline valueOf[labelHead].asMatchable match
          case label: String =>
            val missing = !values.contains(label)
            val value = if missing then 0.asInstanceOf[JsonAst] else values(label)
            val paramValue = summonInline[JsonDeserializer[paramHead]].read(value, missing)
            
            paramValue *: deriveProduct[labelsTail, paramsTail](values)
      
      case _ =>
        EmptyTuple
      
  inline given derived
      [DerivationType](using mirror: Mirror.Of[DerivationType])(using Raises[JsonAccessError])
      : JsonDeserializer[DerivationType] =
    inline mirror match
      case mirror: Mirror.ProductOf[DerivationType & Product] => (value, missing) =>
        val keyValues = value.obj
        val values = keyValues(0).zip(keyValues(1)).to(Map)
        
        val product: Product = mirror.fromProduct(deriveProduct[mirror.MirroredElemLabels,
            mirror.MirroredElemTypes](values))
        
        mirror.fromProduct(product)
    
      case mirror: Mirror.SumOf[DerivationType] => (value: JsonAst, missing: Boolean) =>
        val values = value.obj
        
        values(0).indexOf("_type") match
          case -1    =>
            ???
          
          case index =>
            deriveSum[mirror.MirroredElemTypes, mirror.MirroredElemLabels, DerivationType](values(1)
                (index).string(using summonInline[Raises[JsonAccessError]]).s).read(value, missing)
  
  private transparent inline def deriveSum
      [SubtypesType <: Tuple, LabelsType <: Tuple, DerivationType]
      (subtype: String)
      : JsonDeserializer[DerivationType] =
    inline erasedValue[SubtypesType] match
      case _: (head *: tail) => inline erasedValue[LabelsType] match
        case _: (headLabel *: tailLabels) => inline valueOf[headLabel].asMatchable match
          case label: String =>
            if label == subtype
            then summonInline[JsonDeserializer[head]].asInstanceOf[JsonDeserializer[DerivationType]]
            else deriveSum[tail, tailLabels, DerivationType](subtype)
      
      case _ =>
        throw Mistake(msg"could not match subtype in its apparent coproduct type")

trait JsonDeserializer[ValueType]:
  private inline def reader: this.type = this
  
  def read(json: JsonAst, missing: Boolean): ValueType
  def map[ValueType2](fn: ValueType => ValueType2): JsonDeserializer[ValueType2]^{this, fn} =
    (json, missing) => fn(reader.read(json, missing))

class Json(rootValue: Any) extends Dynamic derives CanEqual:
  def root: JsonAst = rootValue.asInstanceOf[JsonAst]
  def apply(idx: Int)(using Raises[JsonAccessError]): Json = Json(root.array(idx))
  def selectDynamic(field: String)(using erased DynamicJsonEnabler)(using Raises[JsonAccessError]): Json = apply(Text(field))

  def applyDynamic(field: String)(idx: Int)(using erased DynamicJsonEnabler)(using Raises[JsonAccessError]): Json =
    apply(Text(field))(idx)
  
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
    val sb: StringBuilder = StringBuilder()
    def appendString(str: String): Unit =
      str.foreach:
        case '\t' => sb.append("\\t")
        case '\n' => sb.append("\\n")
        case '\r' => sb.append("\\r")
        case '\\' => sb.append("\\\\")
        case '\f' => sb.append("\\f")
        case ch   => sb.append(ch)

    def recur(json: JsonAst): Unit = json.asMatchable match
      case (keys, values) => (keys.asMatchable: @unchecked) match
        case keys: Array[String] @unchecked => (values.asMatchable: @unchecked) match
          case values: Array[JsonAst] @unchecked =>
            sb.append('{')
            val last = keys.length - 1
            keys.indices.foreach: i =>
              sb.append('"')
              appendString(keys(i))
              sb.append('"')
              sb.append(':')
              recur(values(i))
              sb.append(if i == last then '}' else ',')
      
      case array: Array[JsonAst] @unchecked =>
        sb.append('[')
        val last = array.length - 1
        array.indices.foreach: i =>
          recur(array(i))
          sb.append(if i == last then ']' else ',')
      
      case long: Long =>
       sb.append(long.toString)
      
      case double: Double =>
        sb.append(double.toString)
      
      case string: String =>
        sb.append('"')
        appendString(string)
        sb.append('"')
      case boolean: Boolean => sb.append(boolean.toString)
      case _ => sb.append("null")

    recur(json)
    sb.toString.show

// FIXME: Implement this
object HumanReadableSerializer extends JsonPrinter:
  def serialize(json: JsonAst): Text =
    val sb: StringBuilder = StringBuilder()
    def appendString(str: String): Unit =
      str.foreach:
        case '\t' => sb.append("\\t")
        case '\n' => sb.append("\\n")
        case '\r' => sb.append("\\r")
        case '\\' => sb.append("\\\\")
        case '\f' => sb.append("\\f")
        case ch   => sb.append(ch)

    def recur(json: JsonAst, indent: Int): Unit = json.asMatchable match
      case (keys, values) => (keys.asMatchable: @unchecked) match
        case keys: Array[String] => (values.asMatchable: @unchecked) match
          case values: Array[JsonAst] @unchecked =>
            sb.append('{')
            val last = keys.length - 1
            keys.indices.foreach: i =>
              sb.append('"')
              appendString(keys(i))
              sb.append('"')
              sb.append(':')
              recur(values(i), indent)
              sb.append(if i == last then '}' else ',')
      
      case array: Array[JsonAst] @unchecked =>
        sb.append('[')
        val last = array.length - 1
        array.indices.foreach: i =>
          recur(array(i), indent)
          sb.append(if i == last then ']' else ',')
      
      case long: Long =>
       sb.append(long.toString)
      
      case double: Double =>
        sb.append(double.toString)
      
      case string: String =>
        sb.append('"')
        appendString(string)
        sb.append('"')
      
      case boolean: Boolean => sb.append(boolean.toString)
      case _ => sb.append("null")

    recur(json, 0)
    sb.toString.show


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
