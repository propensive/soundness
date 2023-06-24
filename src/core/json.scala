/*
    Jacinta, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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
import digression.*
import turbulence.*
import gossamer.*
import anticipation.*
import merino.*
import hieroglyph.*
import spectacular.*

import scala.collection.Factory
import scala.compiletime.*
import scala.deriving.*

import language.dynamics
import language.experimental.captureChecking

import unsafeExceptions.canThrowAny
import JsonAccessError.Issue

erased trait DynamicJsonAccess

object jsonAccess:
  erased given DynamicJsonAccess = ###

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
  
  inline def array: IArray[JsonAst] =
    if isArray then json.asInstanceOf[IArray[JsonAst]]
    else throw JsonAccessError(Issue.NotType(JsonPrimitive.Array))
  
  inline def double: Double throws JsonAccessError = json.asMatchable match
    case value: Double     => value
    case value: Long       => value.toDouble
    case value: BigDecimal => value.toDouble
    case _                 => throw JsonAccessError(Issue.NotType(JsonPrimitive.Number))
  
  inline def bigDecimal: BigDecimal = json.asMatchable match
    case value: BigDecimal => value
    case value: Long       => BigDecimal(value)
    case value: Double     => BigDecimal(value)
    case _                 => throw JsonAccessError(Issue.NotType(JsonPrimitive.Number))
  
  inline def long: Long throws JsonAccessError = json.asMatchable match
    case value: Long       => value
    case value: Double     => value.toLong
    case value: BigDecimal => value.toLong
    case _                 => throw JsonAccessError(Issue.NotType(JsonPrimitive.Number))
  
  inline def string: Text throws JsonAccessError =
    if isString then json.asInstanceOf[Text]
    else throw JsonAccessError(Issue.NotType(JsonPrimitive.String))
  
  inline def boolean: Boolean throws JsonAccessError =
    if isBoolean then json.asInstanceOf[Boolean]
    else throw JsonAccessError(Issue.NotType(JsonPrimitive.Boolean))
  
  inline def obj: (IArray[String], IArray[JsonAst]) =
    if isObject then json.asInstanceOf[(IArray[String], IArray[JsonAst])]
    else throw JsonAccessError(Issue.NotType(JsonPrimitive.Object))
  
  inline def number: Long | Double | BigDecimal =
    if isLong then long else if isDouble then double else if isBigDecimal then bigDecimal
    else throw JsonAccessError(Issue.NotType(JsonPrimitive.Number))
  
extension [T: JsonWriter](value: T)
  def json: Json = Json(summon[JsonWriter[T]].write(value))

object Json extends Dynamic:
  def parse
      [SourceType]
      (value: SourceType)
      (using readable: Readable[SourceType, Bytes], jsonParse: CanThrow[JsonParseError])
      : Json^{readable, jsonParse} =
    Json(JsonAst.parse(value))

  given (using JsonPrinter): Show[Json] = json =>
    try json.root.show catch case err: JsonAccessError => t"<${err.reason.show}>"

  given
      (using encoder: CharEncoder^, printer: JsonPrinter)
      : GenericHttpResponseStream[Json]^{encoder} =
    new GenericHttpResponseStream[Json]:
      def mediaType: String = t"application/json; charset=${encoder.encoding.name}".s
      def content(json: Json): LazyList[Bytes] = LazyList(json.show.bytes)

  given (using jsonParse: CanThrow[JsonParseError], charEncoder: CharEncoder^): GenericHttpReader[Json]^{jsonParse, charEncoder} =
    string => Json.parse(LazyList(Text(string).bytes))

  given aggregable
      [SourceType]
      (using Readable[SourceType, Bytes], CanThrow[JsonParseError])
      : Aggregable[Bytes, Json] =
    Json.parse(_)

  def applyDynamicNamed[T <: String](methodName: "of")(elements: (String, Json)*): Json =
    val keys: IArray[String] = IArray.from(elements.map(_(0)))
    val values: IArray[JsonAst] = IArray.from(elements.map(_(1).root))
    Json(JsonAst((keys, values)))

object JsonWriter:
  given JsonWriter[Int] = v => JsonAst(v.toLong)
  given JsonWriter[Text] = v => JsonAst(v.s)
  given JsonWriter[Double] = JsonAst(_)
  given JsonWriter[Long] = JsonAst(_)
  given JsonWriter[Byte] = v => JsonAst(v.toLong)
  given JsonWriter[Short] = v => JsonAst(v.toLong)
  given JsonWriter[Boolean] = JsonAst(_)

  //given [T](using canon: Canonical[T]): JsonWriter[T] = v => JsonAst(canon.serialize(v).s)
  
  given (using jsonAccess: CanThrow[JsonAccessError]): JsonWriter[Json]^{jsonAccess} = _.root
  
  given JsonWriter[Nil.type] = value => JsonAst(IArray[JsonAst]())

  given [Coll[T1] <: Iterable[T1], T: JsonWriter]: JsonWriter[Coll[T]] = values =>
    JsonAst(IArray.from(values.map(summon[JsonWriter[T]].write(_))))

  // given [T: JsonWriter]: JsonWriter[Map[String, T]] = values =>
  //   JObject(mutable.Map(values.view.mapValues(summon[JsonWriter[T]].write(_)).to(Seq)*))

  // given [T: JsonWriter]: JsonWriter[Maybe[T]] = new JsonWriter[Maybe[T]]:
  //   override def omit(t: Maybe[T]): Boolean = t.unset
  //   def write(value: Maybe[T]): JsonAst = value match
  //     case Unset               => JsonAst(null)
  //     case value: T @unchecked => summon[JsonWriter[T]].write(value)

  given opt[T: JsonWriter]: JsonWriter[Option[T]] = new JsonWriter[Option[T]]:
    override def omit(t: Option[T]): Boolean = t.isEmpty
    
    def write(value: Option[T]): JsonAst = value match
      case None        => JsonAst(null)
      case Some(value) => summon[JsonWriter[T]].write(value)

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
              values(index) = summonInline[JsonWriter[head.type]].write(head)
              deriveProduct[tailLabels](tail, labels, values, index + 1)

  private transparent inline def deriveSum
      [TupleType <: Tuple, DerivedType, LabelsType <: Tuple]
      (ordinal: Int)
      : JsonWriter[DerivedType] =
    inline erasedValue[TupleType] match
      case _: (head *: tail) => inline erasedValue[LabelsType] match
        case _: (headLabel *: tailLabel) =>
          if ordinal == 0 then inline valueOf[headLabel].asMatchable match
            case label: String => summonInline[JsonWriter[head]].tag(label).asInstanceOf[JsonWriter[DerivedType]]
          else deriveSum[tail, DerivedType, tailLabel](ordinal - 1)
      
      case _ => ???

  inline given derived
      [DerivationType](using mirror: Mirror.Of[DerivationType])
      : JsonWriter[DerivationType] =
    inline mirror match
      case given Mirror.ProductOf[DerivationType & Product] => (value: DerivationType) =>
        (value.asMatchable: @unchecked) match
          case value: Product =>
            val labels: Array[String] = new Array(value.productArity)
            val values: Array[JsonAst] = new Array(value.productArity)
            deriveProduct[mirror.MirroredElemLabels](Tuple.fromProductTyped(value), labels, values, 0).asInstanceOf[JsonAst]
    
      case sumMirror: Mirror.SumOf[DerivationType] =>
        (value: DerivationType) =>
          deriveSum[sumMirror.MirroredElemTypes, DerivationType, sumMirror.MirroredElemLabels](sumMirror.ordinal(value)).write(value)

  // def join[T](caseClass: CaseClass[JsonWriter, T]): JsonWriter[T] = value =>
  //   val labels: IArray[String] = caseClass.params.collect:
  //     case p if !p.typeclass.omit(p.deref(value)) => p.label

  //   val values: IArray[JsonAst] = caseClass.params.collect:
  //     case p if !p.typeclass.omit(p.deref(value)) => p.typeclass.write(p.deref(value))
    
  //   JsonAst((labels, values))
    
  // def split[T](sealedTrait: SealedTrait[JsonWriter, T]): JsonWriter[T] = value =>
  //   sealedTrait.choose(value): subtype =>
  //     val obj = subtype.typeclass.write(subtype.cast(value)).obj
  //       JsonAst((obj(0) :+ "_type", obj(1) :+ subtype.typeInfo.short))

trait JsonWriter[-ValueType]:
  def omit(value: ValueType): Boolean = false
  def write(value: ValueType): JsonAst
  
  def contraMap[ValueType2](fn: ValueType2 => ValueType): JsonWriter[ValueType2]^{this, fn} =
    (value: ValueType2) => fn.andThen(write)(value)

  def tag(label: String): JsonWriter[ValueType] = (value: ValueType) =>
    val (keys, values) = write(value).obj
    (keys :+ "_type", values :+ label).asInstanceOf[JsonAst]

object JsonReader:
  given jsonAst: JsonReader[JsonAst] = (value, missing) => value
  given json: JsonReader[Json] = (value, missing) => Json(value)
  given int: JsonReader[Int] = (value, missing) => value.long.toInt
  given byte: JsonReader[Byte] = (value, missing) => value.long.toByte
  given short: JsonReader[Short] = (value, missing) => value.long.toShort
  given float: JsonReader[Float] = (value, missing) => value.double.toFloat
  given double: JsonReader[Double] = (value, missing) => value.double
  given long: JsonReader[Long] = (value, missing) => value.long
  given text: JsonReader[Text] = (value, missing) => value.string
  given boolean: JsonReader[Boolean] = (value, missing) => value.boolean
  
  //given [T](using canon: Canonical[T]): JsonReader[T] = v => canon.deserialize(v.string)

  given option[T](using reader: JsonReader[T]^): JsonReader[Option[T]]^{reader} = new JsonReader[Option[T]]:
    def read(value: JsonAst, missing: Boolean): Option[T] =
      if missing then None else Some(reader.read(value, false))

  given array[Coll[T1] <: Iterable[T1], T]
              (using reader: JsonReader[T], factory: Factory[T, Coll[T]]): JsonReader[Coll[T]] =
    new JsonReader[Coll[T]]:
      def read(value: JsonAst, missing: Boolean): Coll[T] =
        val bld = factory.newBuilder
        value.array.foreach(bld += reader.read(_, false))
        bld.result()

  given map[T](using reader: JsonReader[T]): JsonReader[Map[String, T]] = new JsonReader[Map[String, T]]:
    def read(value: JsonAst, missing: Boolean): Map[String, T] =
      val (keys, values) = value.obj
      
      keys.indices.foldLeft(Map[String, T]()): (acc, index) =>
        acc.updated(keys(index), reader.read(values(index), false))

  // def join[T](caseClass: CaseClass[JsonReader, T]): JsonReader[T] = new JsonReader[T]:
  //   def read(value: JsonAst, missing: Boolean): T =
  //     caseClass.construct: param =>
  //       val (keys, values) = value.obj
  //       keys.indexOf(param.label) match
  //         case -1  => param.typeclass.read(0.asInstanceOf[JsonAst], true)
  //         case idx => param.typeclass.read(values(idx), false)

  // def split[T](sealedTrait: SealedTrait[JsonReader, T]): JsonReader[T] = new JsonReader[T]:
  //   def read(value: JsonAst, missing: Boolean) =
  //     val _type = Json(value)("_type").as[Text]
  //     val subtype = sealedTrait.subtypes.find { t => Text(t.typeInfo.short) == _type }
  //       .getOrElse(throw JsonAccessError(Issue.NotType(JsonPrimitive.Object))) // FIXME
      
  //     subtype.typeclass.read(value, missing)


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
            val paramValue = summonInline[JsonReader[paramHead]].read(value, missing)
            
            paramValue *: deriveProduct[labelsTail, paramsTail](values)
      
      case _ =>
        EmptyTuple
      
  inline given derived
      [DerivationType](using mirror: Mirror.Of[DerivationType])
      : JsonReader[DerivationType] =
    inline mirror match
      case mirror: Mirror.ProductOf[DerivationType & Product] => (value: JsonAst, missing: Boolean) =>
        
        val keyValues = value.obj
        val values = keyValues(0).zip(keyValues(1)).to(Map)
        
        val product: Product = mirror.fromProduct(deriveProduct[mirror.MirroredElemLabels, mirror.MirroredElemTypes](values))
        mirror.fromProduct(product)
    
      case mirror: Mirror.SumOf[DerivationType] => (value: JsonAst, missing: Boolean) =>
        val values = value.obj
        
        values(0).indexOf("_type") match
          case -1    => ???
          case index => deriveSum[mirror.MirroredElemTypes, mirror.MirroredElemLabels, DerivationType](values(1)(index).string.s).read(value, missing)
  
  private transparent inline def deriveSum
      [SubtypesType <: Tuple, LabelsType <: Tuple, DerivationType]
      (subtype: String)
      : JsonReader[DerivationType] =
    inline erasedValue[SubtypesType] match
      case _: (head *: tail) => inline erasedValue[LabelsType] match
        case _: (headLabel *: tailLabels) => inline valueOf[headLabel].asMatchable match
          case label: String =>
            if label == subtype then summonInline[JsonReader[head]].asInstanceOf[JsonReader[DerivationType]]
            else deriveSum[tail, tailLabels, DerivationType](subtype)
      
      case _ => ???
        

    
trait JsonReader[T]:
  private inline def reader: this.type = this
  
  def read(json: JsonAst, missing: Boolean): T
  def map[S](fn: T => S): JsonReader[S]^{this, fn} = (json, missing) => fn(reader.read(json, missing))

class Json(rootValue: Any) extends Dynamic derives CanEqual:
  def root: JsonAst = rootValue.asInstanceOf[JsonAst]
  def apply(idx: Int): Json throws JsonAccessError = Json(root.array(idx))
  def selectDynamic(field: String)(using erased DynamicJsonAccess): Json = apply(Text(field))

  def applyDynamic(field: String)(idx: Int)(using erased DynamicJsonAccess): Json =
    apply(Text(field))(idx)
  
  def apply(field: Text): Json throws JsonAccessError =
    root.obj(0).indexWhere(_ == field.s) match
      case -1    => throw JsonAccessError(Issue.Label(field))
      case index => Json(root.obj(1)(index))
  

  override def hashCode: Int =
    def recur(value: JsonAst): Int =
      value.asMatchable match
        case value: Long                       => value.hashCode
        case value: Double                     => value.hashCode
        case value: BigDecimal                 => value.hashCode
        case value: String                     => value.hashCode
        case value: Boolean                    => value.hashCode
        case value: IArray[JsonAst] @unchecked => value.foldLeft(value.length.hashCode)(_*31^recur(_))
        
        case (keys, values) => (keys.asMatchable: @unchecked) match
          case keys: IArray[String] @unchecked => (values.asMatchable: @unchecked) match
            case values: IArray[JsonAst] @unchecked =>
              keys.zip(values).to(Map).view.mapValues(recur(_)).hashCode
        
        case _                             => 0
    
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

  def as[ValueType](using reader: JsonReader[ValueType]): ValueType throws JsonAccessError =
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
  enum Issue:
    case Index(value: Int)
    case Label(label: Text)
    case NotType(primitive: JsonPrimitive)
  
  object Issue:
    given Show[Issue] =
      case Index(value)       => t"the index $value out of range"
      case Label(label)       => t"the JSON object does not contain the label $label"
      case NotType(primitive) => t"the JSON value did not have the type $primitive"

case class JsonAccessError(reason: JsonAccessError.Issue)
extends Error(err"could not access the value because $reason")

object JsonPrimitive:
  given Show[JsonPrimitive] = _.toString.show

enum JsonPrimitive:
  case Array, Object, Number, Null, Boolean, String
