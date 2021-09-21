/*
    Euphemism, version 0.8.0. Copyright 2018-21 Jon Pretty, Propensive OÜ.

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
import gossamer.*

import org.typelevel.jawn.{ParseException as JawnParseException, *}, ast.*

import scala.collection.mutable, collection.Factory
import scala.util.*

import language.dynamics

case class ParseException(line: Int, column: Int, message: String) extends Exception

sealed trait AccessException extends Exception
case class DeserializationException() extends AccessException
case class IndexNotFound(index: Int) extends AccessException
case class LabelNotFound(label: String) extends AccessException
case class UnexpectedType(expectedType: JsonPrimitive) extends AccessException

enum JsonPrimitive:
  case Array, Object, Number, Null, Boolean, String

extension [T: Json.Serializer](value: T)
  def json: Json = Json(summon[Json.Serializer[T]].serialize(value))

object Json extends Dynamic:

  given clairvoyant.HttpResponse[Json] with
    def mimeType: String = "application/json"
    def content(json: Json): String = json.toString

  given clairvoyant.HttpReader[Json] = Json.parse(_)

  object Serializer extends Derivation[Serializer]:
    given Serializer[Int] = JNum(_)
    given Serializer[String] = JString(_)
    given Serializer[Double] = JNum(_)
    given Serializer[Long] = JNum(_)
    given Serializer[Byte] = JNum(_)
    given Serializer[Short] = JNum(_)
    given Serializer[Boolean] = if _ then JTrue else JFalse
    
    // given (using CanThrow[UnexpectedType], CanThrow[LabelNotFound], CanThrow[IndexNotFound])
    //     : Serializer[Json] = _.normalize.toOption.get.root
  
    given Serializer[Json] = _.normalize.toOption.get.root
    
    given Serializer[Nil.type] = value => JArray(Array())

    given [Coll[T1] <: Traversable[T1], T: Serializer]: Serializer[Coll[T]] = values =>
      JArray(values.map(summon[Serializer[T]].serialize(_)).to(Array))

    given [T: Serializer]: Serializer[Map[String, T]] = values =>
      JObject(mutable.Map(values.view.mapValues(summon[Serializer[T]].serialize(_)).to(Seq)*))

    given [T: Serializer]: Serializer[Option[T]] = new Serializer[Option[T]]:
      override def omit(t: Option[T]): Boolean = t.isEmpty
      
      def serialize(value: Option[T]): JValue = value match
        case None        => JNull
        case Some(value) => summon[Serializer[T]].serialize(value)

    def join[T](caseClass: CaseClass[Serializer, T]): Serializer[T] = value =>
      JObject(mutable.Map(caseClass.params.filter { param =>
        !param.typeclass.omit(param.deref(value))
      }.map { param =>
        (param.label, param.typeclass.serialize(param.deref(value)))
      }*))
      
    def split[T](sealedTrait: SealedTrait[Serializer, T]): Serializer[T] = value =>
      sealedTrait.choose(value) { subtype =>
        val obj = subtype.typeclass.serialize(subtype.cast(value))
        obj match
          case JObject(vs) => vs("_type") = JString(subtype.typeInfo.short)
          case _           => ()
        
        obj
      }

  trait Serializer[T]:
    def omit(t: T): Boolean = false
    def serialize(t: T): JValue

  object Deserializer extends Derivation[Deserializer]:
    given Deserializer[Int] = _.flatMap(_.getInt)
    given Deserializer[Double] = _.flatMap(_.getDouble)
    given Deserializer[Float] = _.flatMap(_.getDouble.map(_.toFloat))
    given Deserializer[Long] = _.flatMap(_.getLong)
    given Deserializer[String] = _.flatMap(_.getString)
    given Deserializer[Short] = _.flatMap(_.getInt.map(_.toShort))
    given Deserializer[Byte] = _.flatMap(_.getInt.map(_.toByte))
    given Deserializer[Boolean] = _.flatMap(_.getBoolean)
    given Deserializer[Json] = _.map(Json(_, Nil))

    given opt[T: Deserializer]: Deserializer[Option[T]] =
      v => Some(summon[Deserializer[T]].deserialize(v))

    given array[Coll[T1] <: Traversable[T1], T: Deserializer]
               (using factory: Factory[T, Coll[T]]): Deserializer[Coll[T]] = _.flatMap {
      case JArray(vs) =>
        vs.foldLeft(Option(factory.newBuilder)) { (builder, next) =>
          summon[Deserializer[T]].deserialize(Some(next)).flatMap { elem => builder.map(_ += elem) }
        }.map(_.result())
      
      case _ =>
        None
    }

    given map[T: Deserializer]: Deserializer[Map[String, T]] = _.flatMap {
      case JObject(vs) =>
        vs.toMap.foldLeft(Option(Map[String, T]())) {
          case (Some(acc), (k, v)) =>
            summon[Deserializer[T]].deserialize(Some(v)).map { v2 => acc.updated(k, v2) }
          
          case _ =>
            None
        }
      
      case _ =>
        None
    }

    def join[T](caseClass: CaseClass[Deserializer, T]): Deserializer[T] = _.flatMap { json =>
      caseClass.constructMonadic { param =>
        json match
          case JObject(vs) => param.typeclass.deserialize(vs.get(param.label))
          case _ => None
      }
    }

    def split[T](sealedTrait: SealedTrait[Deserializer, T]): Deserializer[T] = _.flatMap { json =>
      try for
        str     <- Some(Json(json, Nil)._type.as[String])
        subtype <- sealedTrait.subtypes.find(_.typeInfo.short == str)
        value   <- subtype.typeclass.deserialize(Some(json))
      yield value
      catch
        case DeserializationException() | IndexNotFound(_) | LabelNotFound(_) | UnexpectedType(_) => None
    }

  trait Deserializer[T]:
    def deserialize(json: Option[JValue]): Option[T]

  def parse(str: String): Json exposes ParseException = JParser.parseFromString(str) match
    case Success(value)                   => Json(value, Nil)
    case Failure(err: JawnParseException) => throw ParseException(err.line, err.col, err.msg)
    case Failure(err)                     => throw err

  // def applyDynamicNamed[T <: String](methodName: "of")(elements: (String, Json)*): Json =
  //   Json(JObject(mutable.Map(elements.map(_ -> _.json.root)*)), Nil)

case class Json(root: JValue, path: List[Int | String] = Nil) extends Dynamic derives CanEqual:
  def apply(idx: Int): Json = Json(root, idx :: path)
  def apply(field: String): Json = Json(root, field :: path)
  def selectDynamic(field: String): Json = this(field)
  def applyDynamic(field: String)(idx: Int): Json = this(field)(idx)

  def normalize: Json exposes IndexNotFound | LabelNotFound | UnexpectedType =
    def deref(value: JValue, path: List[Int | String]): JValue exposes IndexNotFound |
        UnexpectedType | LabelNotFound = path match
      case Nil =>
        value
      case (idx: Int) :: tail => value match
        case JArray(vs) =>
          deref((vs.lift(idx) match
            case None        => throw IndexNotFound(idx)
            case Some(value) => value
          ), tail)
        case _ =>
          throw UnexpectedType(JsonPrimitive.Array)
      case (field: String) :: tail => value match
        case JObject(vs) =>
          deref((vs.get(field) match
            case None        => throw LabelNotFound(field)
            case Some(value) => value
          ), tail)
        case _ =>
          throw UnexpectedType(JsonPrimitive.Object)
      
    Json(deref(root, path.reverse), Nil)

  def as[T: Json.Deserializer]: T exposes DeserializationException | UnexpectedType | LabelNotFound | IndexNotFound =
    summon[Json.Deserializer[T]]
      .deserialize(Some(normalize.root))
      .getOrElse(throw DeserializationException())
  
  override def toString(): String =
    try normalize.root.render()
    catch
      case UnexpectedType(t) => s"<type mismatch: expected $t>"
      case LabelNotFound(s)  => str"<missing label: $s>"
      case IndexNotFound(i)  => str"<missing index: $i>"
    
