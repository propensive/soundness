/*
    Euphemism, version 0.4.0. Copyright 2019-23 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package euphemism

import wisteria.*
import rudiments.*
import turbulence.*
import gossamer.*
import anticipation.*
import merino.*

import scala.collection.mutable, collection.Factory
import scala.util.*

import scala.quoted.*
import scala.deriving.*

import language.dynamics

import unsafeExceptions.canThrowAny
import JsonAccessError.Issue

given (using js: JsonSerializer): Show[JsonAst] = js.serialize(_)

extension (json: JsonAst)
  inline def isNumber: Boolean = isDouble || isLong || isBigDecimal
  inline def isLong: Boolean = json.isInstanceOf[Long]
  inline def isDouble: Boolean = json.isInstanceOf[Double]
  inline def isBigDecimal: Boolean = json.isInstanceOf[BigDecimal]
  inline def isObject: Boolean = json.isInstanceOf[(?, ?)]
  inline def isString: Boolean = json.isInstanceOf[String]
  inline def isBoolean: Boolean = json.isInstanceOf[Boolean]
  
  inline def isNull: Boolean = json match
    case v: Null => v == null
    case _       => false

  inline def isArray: Boolean = json.isInstanceOf[Array[?]]
  
  inline def array: IArray[JsonAst] =
    if isArray then json.asInstanceOf[IArray[JsonAst]]
    else throw JsonAccessError(Issue.Type(JsonPrimitive.Array))
  
  inline def double: Double throws JsonAccessError = json match
    case value: Double     => value
    case value: Long       => value.toDouble
    case value: BigDecimal => value.toDouble
    case _                 => throw JsonAccessError(Issue.Type(JsonPrimitive.Number))
  
  inline def bigDecimal: BigDecimal = json match
    case value: BigDecimal => value
    case value: Long       => BigDecimal(value)
    case value: Double     => BigDecimal(value)
    case _                 => throw JsonAccessError(Issue.Type(JsonPrimitive.Number))
  
  inline def long: Long throws JsonAccessError = json match
    case value: Long       => value
    case value: Double     => value.toLong
    case value: BigDecimal => value.toLong
    case _                 => throw JsonAccessError(Issue.Type(JsonPrimitive.Number))
  
  inline def string: Text throws JsonAccessError =
    if isString then json.asInstanceOf[Text]
    else throw JsonAccessError(Issue.Type(JsonPrimitive.String))
  
  inline def boolean: Boolean throws JsonAccessError =
    if isBoolean then json.asInstanceOf[Boolean]
    else throw JsonAccessError(Issue.Type(JsonPrimitive.Boolean))
  
  inline def obj: (IArray[String], IArray[JsonAst]) =
    if isObject then json.asInstanceOf[(IArray[String], IArray[JsonAst])]
    else throw JsonAccessError(Issue.Type(JsonPrimitive.Object))
  
  inline def number: Long | Double | BigDecimal =
    if isLong then long else if isDouble then double else if isBigDecimal then bigDecimal
    else throw JsonAccessError(Issue.Type(JsonPrimitive.Number))
  
extension [T: Json.Writer](value: T)
  def json: Json = Json(summon[Json.Writer[T]].write(value))

object Json extends Dynamic:
  def parse[T: Streamable](value: T): Json = Json(JsonAst.parse(summon[Streamable[T]].stream(value)), Nil)

  given (using JsonSerializer): Show[Json] = json =>
    try json.normalize.root.show catch case err: JsonAccessError => t"<${err.reason}>"

  given (using JsonSerializer): GenericHttpResponseStream[Json] with
    def mediaType: String = "application/json"
    def content(json: Json): LazyList[IArray[Byte]] = LazyList(json.show.bytes)

  given GenericHttpReader[Json, JsonParseError] with
    def read(value: String): Json throws JsonParseError =
      Json(JsonAst.parse(LazyList(value.getBytes("UTF-8").nn.immutable(using Unsafe))), Nil)

  given (using CanThrow[StreamCutError], CanThrow[JsonAccessError]): Readable[Json] = new Readable[Json]:
    def read(value: DataStream) = Json(JsonAst.parse(value), Nil)
  
  object Writer extends Derivation[Writer]:
    given Writer[Int] = v => JsonAst(v.toLong)
    given Writer[Text] = v => JsonAst(v.s)
    given Writer[Double] = JsonAst(_)
    given Writer[Long] = JsonAst(_)
    given Writer[Byte] = v => JsonAst(v.toLong)
    given Writer[Short] = v => JsonAst(v.toLong)
    given Writer[Boolean] = JsonAst(_)
    
    given (using CanThrow[JsonAccessError]): Writer[Json] = _.normalize.toOption.get.root
    
    given Writer[Nil.type] = value => JsonAst(IArray[JsonAst]())

    given [Coll[T1] <: Traversable[T1], T: Writer]: Writer[Coll[T]] = values =>
      JsonAst(IArray.from(values.map(summon[Writer[T]].write(_))))

    // given [T: Writer]: Writer[Map[String, T]] = values =>
    //   JObject(mutable.Map(values.view.mapValues(summon[Writer[T]].write(_)).to(Seq)*))

    given [T: Writer]: Writer[Maybe[T]] = new Writer[Maybe[T]]:
      override def omit(t: Maybe[T]): Boolean = t.unset
      def write(value: Maybe[T]): JsonAst = value match
        case Unset               => JsonAst(null)
        case value: T @unchecked => summon[Writer[T]].write(value)

    given opt[T: Writer]: Writer[Option[T]] = new Writer[Option[T]]:
      override def omit(t: Option[T]): Boolean = t.isEmpty
      
      def write(value: Option[T]): JsonAst = value match
        case None        => JsonAst(null)
        case Some(value) => summon[Writer[T]].write(value)

    def join[T](caseClass: CaseClass[Writer, T]): Writer[T] = value =>
      val labels: IArray[String] = caseClass.params.collect:
        case p if !p.typeclass.omit(p.deref(value)) => p.label

      val values: IArray[JsonAst] = caseClass.params.collect:
        case p if !p.typeclass.omit(p.deref(value)) => p.typeclass.write(p.deref(value))
      
      JsonAst((labels, values))
      
    def split[T](sealedTrait: SealedTrait[Writer, T]): Writer[T] = value =>
      sealedTrait.choose(value): subtype =>
        val obj = subtype.typeclass.write(subtype.cast(value)).obj
          JsonAst((obj(0) :+ "_type", obj(1) :+ subtype.typeInfo.short))

  trait Writer[T]:
    def omit(t: T): Boolean = false
    def write(t: T): JsonAst
    def contramap[S](fn: S => T): Writer[S] = (v: S) => fn.andThen(write)(v)

  object Reader extends Derivation[Reader]:
    given Reader[JsonAst] = identity(_)
    given Reader[Json] = Json(_)
    given Reader[Int] = _.long.toInt
    given Reader[Byte] = _.long.toByte
    given Reader[Short] = _.long.toShort
    given Reader[Float] = _.double.toFloat
    given Reader[Double] = _.double
    given Reader[Long] = _.long
    given Reader[Text] = _.string
    given Reader[Boolean] = _.boolean

    given opt[T](using Reader[T]): Reader[Option[T]] with
      def read(value: => JsonAst): Option[T] =
        try Some(summon[Reader[T]].read(value)) catch case e: Throwable => None

    given array[Coll[T1] <: Traversable[T1], T]
               (using reader: Reader[T], factory: Factory[T, Coll[T]]): Reader[Coll[T]] =
      new Reader[Coll[T]]:
        def read(value: => JsonAst): Coll[T] =
          val bld = factory.newBuilder
          value.array.foreach(bld += reader.read(_))
          bld.result()

    given map[T](using reader: Reader[T]): Reader[Map[String, T]] = new Reader[Map[String, T]]:
      def read(value: => JsonAst): Map[String, T] =
        val (keys, values) = value.obj
        
        keys.indices.foldLeft(Map[String, T]()): (acc, i) =>
          acc.updated(keys(i), reader.read(values(i)))

    def join[T](caseClass: CaseClass[Reader, T]): Reader[T] = new Reader[T]:
      def read(value: => JsonAst) =
        caseClass.construct: param =>
          val (keys, values) = value.obj
            param.typeclass.read:
              keys.indexOf(param.label) match
                case -1  => throw JsonAccessError(Issue.Label(Text(param.label)))
                case idx => values(idx)

    def split[T](sealedTrait: SealedTrait[Reader, T]): Reader[T] = new Reader[T]:
      def read(value: => JsonAst) =
        val _type = Json(value, Nil)._type.as[Text]
        val subtype = sealedTrait.subtypes.find { t => Text(t.typeInfo.short) == _type }
          .getOrElse(throw JsonAccessError(Issue.Type(JsonPrimitive.Object))) // FIXME
        
        subtype.typeclass.read(value)

  trait Reader[T]:
    private inline def self: this.type = this
    
    def read(json: => JsonAst): T
    def map[S](fn: T => S): Reader[S] = new Reader[S]:
      def read(json: => JsonAst): S = fn(self.read(json))

  def applyDynamicNamed[T <: String](methodName: "of")(elements: (String, Json)*): Json =
    val keys: IArray[String] = IArray.from(elements.map(_(0)))
    val values: IArray[JsonAst] = IArray.from(elements.map(_(1).normalize.root))
    Json(JsonAst((keys, values)), Nil)

case class Json(root: JsonAst, path: List[Int | Text] = Nil) extends Dynamic derives CanEqual:
  def apply(idx: Int): Json = Json(root, idx :: path)
  def apply(field: Text): Json = Json(root, field :: path)
  def selectDynamic(field: String): Json = this(Text(field))
  def applyDynamic(field: String)(idx: Int): Json = this(Text(field))(idx)

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
    
    recur(normalize.root)

  override def equals(that: Any): Boolean = that match
    case json: Json =>
      val i = normalize
      val j = json.normalize
      
      def recur(j: JsonAst, i: JsonAst): Boolean = j match
        case j: Long     => i match
          case i: Long       => i == j
          case i: Double     => i == j
          case i: BigDecimal => i == BigDecimal(j)
          case _             => false
        case j: Double => i match
          case i: Long       => i == j
          case i: Double     => i == j
          case i: BigDecimal => i == BigDecimal(j)
          case _             => false
        case j: BigDecimal => i match
          case i: Long       => BigDecimal(i) == j
          case i: Double     => BigDecimal(i) == j
          case i: BigDecimal => i == j
          case _             => false
        case j: String => i match
          case i: String => i == j
          case _         => false
        case j: Boolean => i match
          case i: Boolean => i == j
          case _         => false
        case j: IArray[JsonAst] @unchecked => i match
          case i: IArray[JsonAst] @unchecked =>
            j.length == i.length && j.indices.forall { idx => recur(i(idx), j(idx)) }
          case _ =>
            false
        case (jk: IArray[String] @unchecked, jv: IArray[JsonAst] @unchecked) => i match
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

  def normalize: Json throws JsonAccessError =
    def deref(value: JsonAst, path: List[Int | Text]): JsonAst throws JsonAccessError = path match
      case Nil =>
        value
      case (idx: Int) :: tail => value match
        case vs: IArray[JsonAst] @unchecked =>
          deref((vs.lift(idx) match
            case None        => throw JsonAccessError(Issue.Index(idx))
            case Some(value) => value
          ), tail)
        case _ =>
          throw JsonAccessError(Issue.Type(JsonPrimitive.Array))
      case (field: Text) :: tail => value match
        case vs: (IArray[String] @unchecked, IArray[JsonAst] @unchecked) =>
          val v = vs(0).indexOf(field.s) match
            case -1  => throw JsonAccessError(Issue.Label(field))
            case idx => vs(1)(idx)
          deref(v, tail)
        case _ =>
          throw JsonAccessError(Issue.Type(JsonPrimitive.Object))
      
      case _ => throw Mistake("should never match")
      
    Json(deref(root, path.reverse), Nil)

  def as[T](using reader: Json.Reader[T]): T throws JsonAccessError =
    reader.read(normalize.root)

trait JsonSerializer:
  def serialize(json: JsonAst): Text

package jsonSerializers:
  given humanReadable: JsonSerializer = HumanReadableSerializer
  given minimal: JsonSerializer = MinimalSerializer

object MinimalSerializer extends JsonSerializer:
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

    def recur(json: JsonAst): Unit = json match
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
object HumanReadableSerializer extends JsonSerializer:
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

    def recur(json: JsonAst, indent: Int): Unit = json match
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
    case Type(primitive: JsonPrimitive)
  
  object Issue:
    given Show[Issue] =
      case Index(value)    => t"index $value out of range"
      case Label(label)    => t"the object does not contain the label $label"
      case Type(primitive) => t"the value had the type $primitive"

case class JsonAccessError(reason: JsonAccessError.Issue)
extends Error(err"could not access the value because $reason")

object JsonPrimitive:
  given Show[JsonPrimitive] = Showable(_).show

enum JsonPrimitive:
  case Array, Object, Number, Null, Boolean, String
