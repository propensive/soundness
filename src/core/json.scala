package euphemism

import magnolia._
import mercator._
import quarantine._

import org.typelevel.jawn._, ast._

import collection.mutable, collection.generic.CanBuildFrom
import language.experimental.macros, language.dynamics, language.higherKinds, language.implicitConversions
import scala.util._

object ParseException extends Domain[ParseException]
case class ParseException(line: Int, column: Int, message: String) extends Exception


object Access extends Domain[AccessException]

sealed trait AccessException extends Exception with Product with Serializable
case class DeserializationException() extends AccessException
case class IndexNotFound(index: Int) extends AccessException
case class LabelNotFound(label: String) extends AccessException
case class UnexpectedType(expectedType: String) extends AccessException

object Json extends Dynamic {

  object Serializer extends Serializer_1 {
    implicit val int: Serializer[Int] = JNum(_)
    implicit val string: Serializer[String] = JString(_)
    implicit val double: Serializer[Double] = JNum(_)
    implicit val long: Serializer[Long] = JNum(_)
    implicit val byte: Serializer[Byte] = JNum(_)
    implicit val short: Serializer[Short] = JNum(_)
    implicit val boolean: Serializer[Boolean] = if(_) JTrue else JFalse
    implicit val json: Serializer[Json] = _.normalize.to[Option].get.root
    implicit val nil: Serializer[Nil.type] = value => JArray(Array())

    implicit def collection[Coll[T1] <: Traversable[T1], T: Serializer]
                           (implicit cbf: CanBuildFrom[Nothing, T, Array[T]]): Serializer[Coll[T]] =
      coll => JArray(coll.map(implicitly[Serializer[T]].serialize(_)).to[Array])
    
    implicit def map[T: Serializer]: Serializer[Map[String, T]] = values =>
      JObject(mutable.Map(values.mapValues(implicitly[Serializer[T]].serialize(_)).to[Seq]: _*))

    implicit def option[T: Serializer]: Serializer[Option[T]] = new Serializer[Option[T]] {
      override def omit(t: Option[T]): Boolean = t.isEmpty
      def serialize(value: Option[T]): JValue = value match {
        case None        => JNull
        case Some(value) => implicitly[Serializer[T]].serialize(value)
      }
    }
  }

  trait Serializer_1 {
    type Typeclass[T] = Serializer[T]

    def combine[T](caseClass: CaseClass[Serializer, T]): Serializer[T] = value =>
      JObject(mutable.Map(caseClass.parameters.filter { param =>
        !param.typeclass.omit(param.dereference(value))
      }.map { param =>
        (param.label, param.typeclass.serialize(param.dereference(value)))
      }: _*))
      
    def dispatch[T](sealedTrait: SealedTrait[Serializer, T]): Serializer[T] = value =>
      sealedTrait.dispatch(value) { subtype =>
        val obj = subtype.typeclass.serialize(subtype.cast(value))
        obj match {
          case JObject(vs) => vs("_type") = JString(subtype.typeName.short)
          case _ => ()
        }
        obj
      }

    implicit def gen[T]: Serializer[T] = macro Magnolia.gen[T]
  }

  trait Serializer[T] {
    def omit(t: T): Boolean = false
    def serialize(t: T): JValue
  }

  object Deserializer extends Deserializer_1 {
    implicit val int: Deserializer[Int] = _.getInt
    implicit val double: Deserializer[Double] = _.getDouble
    implicit val long: Deserializer[Long] = _.getLong
    implicit val string: Deserializer[String] = _.getString
    implicit val short: Deserializer[Short] = _.getInt.map(_.toShort)
    implicit val byte: Deserializer[Byte] = _.getInt.map(_.toByte)
    implicit val boolean: Deserializer[Boolean] = _.getBoolean
    implicit val json: Deserializer[Json] = value => Some(Json(value, Nil))

    implicit def collection[Coll[T1] <: Traversable[T1], T: Deserializer]
                           (implicit cbf: CanBuildFrom[Nothing, T, Coll[T]]): Deserializer[Coll[T]] = {
      case JArray(vs) =>
        val builder = cbf()
        vs.foldLeft(Option(())) { case (acc, next) =>
          implicitly[Deserializer[T]].deserialize(next).map(builder += _)
        }.map { _ => builder.result() }
      case _ =>
        None
    }

    def map[T: Deserializer]: Deserializer[Map[String, T]] = {
      case JObject(vs) =>
        vs.toMap.foldLeft(Option(Map[String, T]())) {
          case (Some(acc), (k, v)) =>
            implicitly[Deserializer[T]].deserialize(v).map { v2 => acc.updated(k, v2) }
          case _ =>
            None
        }
      case _ =>
        None
    }
  }

  trait Deserializer_1 {
    type Typeclass[T] = Deserializer[T]

    def combine[T](caseClass: CaseClass[Deserializer, T]): Deserializer[T] = json =>
      caseClass.constructMonadic { param => json match {
        case JObject(vs) => vs.get(param.label).flatMap(param.typeclass.deserialize(_))
        case _ => None
      } }

    def dispatch[T](sealedTrait: SealedTrait[Deserializer, T]): Deserializer[T] = { (json: JValue) =>
      import Access._
      { for {
        str     <- Json(json, Nil)._type.as[String]
        subtype <- Result.from(sealedTrait.subtypes.find(_.typeName.short == str))
        value   <- Result.from(subtype.typeclass.deserialize(json))
      } yield value }.to[Option]
    }

    implicit def gen[T]: Deserializer[T] = macro Magnolia.gen[T]
  }

  trait Deserializer[T] { def deserialize(json: JValue): Option[T] }

  def apply[T: Serializer](value: T): Json = Json(implicitly[Serializer[T]].serialize(value), Nil)
  
  def parse(str: String): ParseException.Result[Json] = JParser.parseFromString(str) match {
    case Success(value) =>
      ParseException.Answer(Json(value, Nil))
    case Failure(error: org.typelevel.jawn.ParseException) =>
      ParseException.Error(ParseException(error.line, error.col, error.msg))
    case Failure(error) =>
      ParseException.Surprise(error)
  }

  def applyDynamicNamed[T <: String](methodName: T)(elements: (String, Context)*): Json =
    Json(JObject(mutable.Map(elements.map { case (k, v) => k -> v.json.root }: _*)), Nil)

  object Context {
    implicit def toContext[T: Serializer](value: T): Context =
      Context(Json(implicitly[Serializer[T]].serialize(value), Nil))
  }

  case class Context(json: Json) extends AnyVal
}

case class Json(root: JValue, path: List[Either[Int, String]] = Nil) extends Dynamic {
  def apply(idx: Int): Json = Json(root, Left(idx) :: path)
  def apply(field: String): Json = Json(root, Right(field) :: path)

  def selectDynamic(field: String): Json = this(field)
  def applyDynamic(field: String)(idx: Int): Json = this(field)(idx)

  def normalize: Access.Result[Json] = {
    import Access._
    def dereference(value: JValue, path: List[Either[Int, String]]): Result[JValue] = path match {
      case Nil => Answer(value)
      case Left(idx) :: tail => value match {
        case JArray(vs) =>
          (vs.lift(idx) match {
            case None        => Error(IndexNotFound(idx))
            case Some(value) => Answer(value)
          }).flatMap(dereference(_, tail))
        case _ =>
          Error(UnexpectedType("array"))
      }
      case Right(field) :: tail => value match {
        case JObject(vs) =>
          (vs.get(field) match {
            case None        => Error(LabelNotFound(field))
            case Some(value) => Answer(value)
          }).flatMap(dereference(_, tail))
        case _ =>
          Error(UnexpectedType("object"))
      }
    }
      
    dereference(root, path.reverse).map(Json(_, Nil))
  }

  def as[T: Json.Deserializer] = normalize.flatMap { json =>
    Access.Result.from(implicitly[Json.Deserializer[T]].deserialize(json.root)).extenuate {
      _ => DeserializationException()
    }
  }

  override def toString(): String = normalize.map(_.root.render()).pacify("undefined")
}
