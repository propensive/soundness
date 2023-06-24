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

import wisteria.*
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

object JsonWriter extends Derivation[JsonWriter]:
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

  given [T: JsonWriter]: JsonWriter[Maybe[T]] = new JsonWriter[Maybe[T]]:
    override def omit(t: Maybe[T]): Boolean = t.unset
    def write(value: Maybe[T]): JsonAst = value match
      case Unset               => JsonAst(null)
      case value: T @unchecked => summon[JsonWriter[T]].write(value)

  given opt[T: JsonWriter]: JsonWriter[Option[T]] = new JsonWriter[Option[T]]:
    override def omit(t: Option[T]): Boolean = t.isEmpty
    
    def write(value: Option[T]): JsonAst = value match
      case None        => JsonAst(null)
      case Some(value) => summon[JsonWriter[T]].write(value)

  def join[T](caseClass: CaseClass[JsonWriter, T]): JsonWriter[T] = value =>
    val labels: IArray[String] = caseClass.params.collect:
      case p if !p.typeclass.omit(p.deref(value)) => p.label

    val values: IArray[JsonAst] = caseClass.params.collect:
      case p if !p.typeclass.omit(p.deref(value)) => p.typeclass.write(p.deref(value))
    
    JsonAst((labels, values))
    
  def split[T](sealedTrait: SealedTrait[JsonWriter, T]): JsonWriter[T] = value =>
    sealedTrait.choose(value): subtype =>
      val obj = subtype.typeclass.write(subtype.cast(value)).obj
        JsonAst((obj(0) :+ "_type", obj(1) :+ subtype.typeInfo.short))

trait JsonWriter[T]:
  def omit(t: T): Boolean = false
  def write(t: T): JsonAst
  def contraMap[S](fn: S => T): JsonWriter[S]^{this, fn} = (v: S) => fn.andThen(write)(v)

object JsonReader extends Derivation[JsonReader]:
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

  def join[T](caseClass: CaseClass[JsonReader, T]): JsonReader[T] = new JsonReader[T]:
    def read(value: JsonAst, missing: Boolean): T =
      caseClass.construct: param =>
        val (keys, values) = value.obj
        keys.indexOf(param.label) match
          case -1  => param.typeclass.read(0.asInstanceOf[JsonAst], true)
          case idx => param.typeclass.read(values(idx), false)

  def split[T](sealedTrait: SealedTrait[JsonReader, T]): JsonReader[T] = new JsonReader[T]:
    def read(value: JsonAst, missing: Boolean) =
      val _type = Json(value)("_type").as[Text]
      val subtype = sealedTrait.subtypes.find { t => Text(t.typeInfo.short) == _type }
        .getOrElse(throw JsonAccessError(Issue.NotType(JsonPrimitive.Object))) // FIXME
      
      subtype.typeclass.read(value, missing)

    
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
    def recur(i: JsonAst): Int =
      i match
        case i: Long                       => i.hashCode
        case i: Double                     => i.hashCode
        case i: BigDecimal                 => i.hashCode
        case i: String                     => i.hashCode
        case i: Boolean                    => i.hashCode
        case i: IArray[JsonAst] @unchecked => i.foldLeft(i.length.hashCode)(_*31^recur(_))
        
        case (ik: IArray[String] @unchecked, iv: IArray[JsonAst] @unchecked) =>
          ik.zip(iv).to(Map).view.mapValues(recur(_)).hashCode
        
        case _                             => 0
    
    recur(root)

  override def equals(that: Any): Boolean = that match
    case json: Json =>
      val i = this
      val j = json
      
      def recur(j: JsonAst, i: JsonAst): Boolean = j.asMatchable match
        case j: Long     => i.asMatchable match
          case i: Long       => i == j
          case i: Double     => i == j
          case i: BigDecimal => i == BigDecimal(j)
          case _             => false
        case j: Double => i.asMatchable match
          case i: Long       => i == j
          case i: Double     => i == j
          case i: BigDecimal => i == BigDecimal(j)
          case _             => false
        case j: BigDecimal => i.asMatchable match
          case i: Long       => BigDecimal(i) == j
          case i: Double     => BigDecimal(i) == j
          case i: BigDecimal => i == j
          case _             => false
        case j: String => i.asMatchable match
          case i: String => i == j
          case _         => false
        case j: Boolean => i.asMatchable match
          case i: Boolean => i == j
          case _         => false
        case j: IArray[JsonAst] @unchecked => i.asMatchable match
          case i: IArray[JsonAst] @unchecked =>
            j.length == i.length && j.indices.forall { idx => recur(i(idx), j(idx)) }
          case _ =>
            false
        case (jk: IArray[String] @unchecked, jv: IArray[JsonAst] @unchecked) => i.asMatchable match
          case (ik: IArray[String] @unchecked, iv: IArray[JsonAst] @unchecked) =>
            val im = ik.zip(iv).to(Map)
            val jm = jk.zip(jv).to(Map)
            im.keySet == jm.keySet && im.keySet.forall { k => recur(im(k), jm(k)) }
          case _                  => false
        
        case _ =>
          false

      recur(root, json.root)
    
    case _ =>
      false

  def as[T](using reader: JsonReader[T]): T throws JsonAccessError = reader.read(root, false)

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
      case (keys: Array[String], values: Array[JsonAst] @unchecked) =>
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
      case (keys: Array[String], values: Array[JsonAst] @unchecked) =>
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
