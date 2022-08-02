/*
    Euphemism, version 0.4.0. Copyright 2019-22 Jon Pretty, Propensive OÜ.

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
import tetromino.*
import gossamer.*
import anticipation.*

import org.typelevel.jawn.{ParseException as JawnParseException, *}, ast.*

import scala.collection.mutable, collection.Factory
import scala.util.*

import scala.quoted.*
import scala.deriving.*

import language.dynamics

case class JsonParseError(line: Int, column: Int, detail: Text)
extends Error(err"could not parse JSON at line $line, column $column: $detail")

object JsonAccessError:
  enum Reason:
    case Index(value: Int)
    case Label(label: Text)
    case Type(primitive: JsonPrimitive)
  
  object Reason:
    given Show[Reason] =
      case Index(value)    => t"index $value out of range"
      case Label(label)    => t"the object does not contain the label $label"
      case Type(primitive) => t"the value had the type $primitive"

import JsonAccessError.Reason

case class JsonAccessError(reason: JsonAccessError.Reason)
extends Error(err"could not access the value because $reason")

object JsonPrimitive:
  given Show[JsonPrimitive] = Showable(_).show

enum JsonPrimitive:
  case Array, Object, Number, Null, Boolean, String

extension [T: Json.Writer](value: T)
  def json: Json = Json(summon[Json.Writer[T]].write(value))

object Json extends Dynamic:
  given Show[Json] = json =>
    try Text(json.normalize.root.render())
    catch case err: JsonAccessError => t"<${err.reason}>"

  given HttpResponseStream[Json] with
    def mediaType: String = "application/json"
    def content(json: Json): LazyList[IArray[Byte]] = LazyList(json.show.bytes)

  given anticipation.HttpReader[Json, JsonParseError] with
    def read(value: String): Json throws JsonParseError = Json.parse(Text(value))

  given (using readable: Readable[Text]): Readable[Json] with
    type E = JsonParseError | readable.E
    def read(stream: DataStream, rubrics: Rubric*): Json throws StreamCutError | E =
      Json.parse(readable.read(stream))

  object Writer extends Derivation[Writer]:
    given Writer[Int] = JNum(_)
    given Writer[Text] = value => JString(value.s)
    given Writer[Double] = JNum(_)
    given Writer[Long] = JNum(_)
    given Writer[Byte] = JNum(_)
    given Writer[Short] = JNum(_)
    given Writer[Boolean] = if _ then JTrue else JFalse
    
    given (using CanThrow[JsonAccessError]): Writer[Json] =
      _.normalize.toOption.get.root
    
    given Writer[Nil.type] = value => JArray(Array())

    given [Coll[T1] <: Traversable[T1], T: Writer]: Writer[Coll[T]] = values =>
      JArray(values.map(summon[Writer[T]].write(_)).to(Array))

    given [T: Writer]: Writer[Map[String, T]] = values =>
      JObject(mutable.Map(values.view.mapValues(summon[Writer[T]].write(_)).to(Seq)*))

    given [T: Writer]: Writer[Option[T]] = new Writer[Option[T]]:
      override def omit(t: Option[T]): Boolean = t.isEmpty
      
      def write(value: Option[T]): JValue = value match
        case None        => JNull
        case Some(value) => summon[Writer[T]].write(value)

    def join[T](caseClass: CaseClass[Writer, T]): Writer[T] = value =>
      JObject(mutable.Map(caseClass.params.filter:
        param => !param.typeclass.omit(param.deref(value))
      .map { param => (param.label, param.typeclass.write(param.deref(value))) }*))
      
    def split[T](sealedTrait: SealedTrait[Writer, T]): Writer[T] = value =>
      sealedTrait.choose(value) { subtype =>
        val obj = subtype.typeclass.write(subtype.cast(value))
        obj match
          case JObject(vs) => vs("_type") = JString(subtype.typeInfo.short)
          case _           => ()
        
        obj
      }

  trait Writer[T]:
    def omit(t: T): Boolean = false
    def write(t: T): JValue
    def contramap[S](fn: S => T): Writer[S] = (v: S) => fn.andThen(write)(v)

  object Reader extends Derivation[Reader]:
    given Reader[Json] with
      def read(value: => JValue): Json throws JsonAccessError = Json(value, Nil)

    given int: Reader[Int] with
      def read(value: => JValue): Int throws JsonAccessError =
        value.getLong.getOrElse(throw JsonAccessError(Reason.Type(JsonPrimitive.Number))).toInt
    
    given Reader[Byte] = long.map(_.toByte)
    given Reader[Short] = long.map(_.toShort)
    
    given float: Reader[Float] with
      def read(value: => JValue): Float throws JsonAccessError =
        value.getDouble.getOrElse(throw JsonAccessError(Reason.Type(JsonPrimitive.Number))).toFloat

    given double: Reader[Double] with
      def read(value: => JValue): Double throws JsonAccessError =
        value.getDouble.getOrElse(throw JsonAccessError(Reason.Type(JsonPrimitive.Number)))

    given long: Reader[Long] with
      def read(value: => JValue): Long throws JsonAccessError =
        value.getLong.getOrElse(throw JsonAccessError(Reason.Type(JsonPrimitive.Number)))

    given string: Reader[Text] with
      def read(value: => JValue): Text throws JsonAccessError =
        Text(value.getString.getOrElse(throw JsonAccessError(Reason.Type(JsonPrimitive.String))))
    
    
    given boolean: Reader[Boolean] with
      def read(value: => JValue): Boolean throws JsonAccessError =
        value.getBoolean.getOrElse(throw JsonAccessError(Reason.Type(JsonPrimitive.Number)))

    given opt[T](using Reader[T]): Reader[Option[T]] with
      def read(value: => JValue): Option[T] throws JsonAccessError =
        try Some(summon[Reader[T]].read(value)) catch case e: Throwable => None

    given array[Coll[T1] <: Traversable[T1], T]
               (using reader: Reader[T], factory: Factory[T, Coll[T]]): Reader[Coll[T]] =
      new Reader[Coll[T]]:
        
        def read(value: => JValue): Coll[T] throws JsonAccessError = value match
          case JArray(vs) =>
            val bld = factory.newBuilder
                             
            vs.foreach:
              v => bld += reader.read(v)

            bld.result()
          
          case _ =>
            throw JsonAccessError(Reason.Type(JsonPrimitive.Array))

    given map[T](using reader: Reader[T]): Reader[Map[String, T]] = new Reader[Map[String, T]]:
      
      def read(value: => JValue): Map[String, T] throws JsonAccessError = value match
        case JObject(vs) => vs.toMap.foldLeft(Map[String, T]()):
                              case (acc, (k, v)) => acc.updated(k, reader.read(v))
        
        case _           => throw JsonAccessError(Reason.Type(JsonPrimitive.Object))

    def join[T](caseClass: CaseClass[Reader, T]): Reader[T] = new Reader[T]:
      type E = JsonAccessError
      def read(value: => JValue) =
        caseClass.construct { param =>
          value match
            case JObject(vs) =>
              param.typeclass.read:
                vs.get(param.label).getOrElse(throw JsonAccessError(Reason.Label(Text(param.label))))
            
            case _ =>
              throw JsonAccessError(Reason.Type(JsonPrimitive.Object))
        }

    def split[T](sealedTrait: SealedTrait[Reader, T]): Reader[T] = new Reader[T]:
      type E = JsonAccessError
      def read(value: => JValue) =
        val _type = Json(value, Nil)._type.as[Text]
        val subtype = sealedTrait.subtypes.find { t => Text(t.typeInfo.short) == _type }
          .getOrElse(throw JsonAccessError(Reason.Type(JsonPrimitive.Object))) // FIXME
        
        try subtype.typeclass.read(value)
        catch case e: Exception => throw JsonAccessError(Reason.Label(Text(subtype.typeInfo.short)))

  abstract class MapReader[T](fn: collection.mutable.Map[String, JValue] => T) extends Reader[T]:
    
    def read(json: => JValue): T throws JsonAccessError = json match
      case JObject(vs) => fn(vs)
      case _           => throw JsonAccessError(Reason.Type(JsonPrimitive.Object))

  trait Reader[T]:
    private inline def self: this.type = this
    
    def read(json: => JValue): T throws JsonAccessError
    def map[S](fn: T => S): Reader[S] = new Reader[S]:
      def read(json: => JValue): S throws JsonAccessError = fn(self.read(json))

  def parse(str: Text): Json throws JsonParseError = JParser.parseFromString(str.s) match
    case Success(value)                   => Json(value, Nil)
    case Failure(err: JawnParseException) => throw JsonParseError(err.line, err.col, Text(err.msg))
    case Failure(err)                     => throw err

  def applyDynamicNamed[T <: String](methodName: "of")(elements: (String, Json)*): Json =
    Json(JObject(mutable.Map(elements.map(_ -> _.root)*)), Nil)

case class Json(root: JValue, path: List[Int | Text] = Nil)
extends Dynamic, Shown[Json] derives CanEqual:
  def apply(idx: Int): Json = Json(root, idx :: path)
  def apply(field: Text): Json = Json(root, field :: path)
  def selectDynamic(field: String): Json = this(Text(field))
  def applyDynamic(field: String)(idx: Int): Json = this(Text(field))(idx)

  def normalize: Json throws JsonAccessError =
    def deref(value: JValue, path: List[Int | Text]): JValue throws JsonAccessError = path match
      case Nil =>
        value
      case (idx: Int) :: tail => value match
        case JArray(vs) =>
          deref((vs.immutable(using Unsafe).lift(idx) match
            case None        => throw JsonAccessError(Reason.Index(idx))
            case Some(value) => value
          ), tail)
        case _ =>
          throw JsonAccessError(Reason.Type(JsonPrimitive.Array))
      case (field: Text) :: tail => value match
        case JObject(vs) =>
          deref((vs.get(field.s) match
            case None        => throw JsonAccessError(Reason.Label(field))
            case Some(value) => value
          ), tail)
        case _ =>
          throw JsonAccessError(Reason.Type(JsonPrimitive.Object))
      
      case _ => throw Mistake("should never match")
      
    Json(deref(root, path.reverse), Nil)

  def as[T](using reader: Json.Reader[T]): T throws JsonAccessError =
    reader.read(normalize.root)
  
