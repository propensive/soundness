package euphemism

import magnolia._
import mercator._
import org.typelevel.jawn._, ast._

import collection.mutable, collection.generic.CanBuildFrom
import language.experimental.macros, language.dynamics, language.higherKinds, language.implicitConversions

object Serializer {
  type Typeclass[T] = Serializer[T]

  def combine[T](caseClass: CaseClass[Serializer, T]): Serializer[T] = value =>
    JObject(mutable.Map(caseClass.parameters.map { param =>
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

  implicit val int: Serializer[Int] = JNum(_)
  implicit val string: Serializer[String] = JString(_)
  implicit val double: Serializer[Double] = JNum(_)
  implicit val long: Serializer[Long] = JNum(_)
  implicit val byte: Serializer[Byte] = JNum(_)
  implicit val short: Serializer[Short] = JNum(_)
  implicit val boolean: Serializer[Boolean] = if(_) JTrue else JFalse
  implicit val json: Serializer[Json] = _.normalize.get.root
  implicit val nil: Serializer[Nil.type] = value => JArray(Array())

  implicit def gen[T]: Serializer[T] = macro Magnolia.gen[T]

  implicit def collection[Coll[T1] <: Traversable[T1], T: Serializer]
                         (implicit cbf: CanBuildFrom[Nothing, T, Array[T]]): Serializer[Coll[T]] =
    coll => JArray(coll.map(implicitly[Serializer[T]].serialize(_)).to[Array])
  
  implicit def map[T: Serializer]: Serializer[Map[String, T]] = values =>
    JObject(mutable.Map(values.mapValues(implicitly[Serializer[T]].serialize(_)).to[Seq]: _*))
}

trait Serializer[T] { def serialize(t: T): JValue }

object Deserializer {
  type Typeclass[T] = Deserializer[T]

  def combine[T](caseClass: CaseClass[Deserializer, T]): Deserializer[T] = json =>
    caseClass.constructMonadic { param => json match {
      case JObject(vs) => vs.get(param.label).flatMap(param.typeclass.deserialize(_))
      case _ => None
    } }

  def dispatch[T](sealedTrait: SealedTrait[Deserializer, T]): Deserializer[T] = { json =>
    for {
      str     <- Json(json, Nil)._type.as[String]
      subtype <- sealedTrait.subtypes.find(_.typeName.short == str)
      value   <- subtype.typeclass.deserialize(json)
    } yield value
  }

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

  implicit def gen[T]: Deserializer[T] = macro Magnolia.gen[T]
}

trait Deserializer[T] { def deserialize(json: JValue): Option[T] }

object Json extends Dynamic {
  def apply[T: Serializer](value: T): Json = Json(implicitly[Serializer[T]].serialize(value), Nil)
  def parse(str: String): Option[Json] = JParser.parseFromString(str).toOption.map(Json(_, Nil))

  def applyDynamicNamed[T <: String](methodName: T)(elements: (String, JsonContext)*): Json =
    Json(JObject(mutable.Map(elements.map { case (k, v) => k -> v.json.root }: _*)), Nil)
}

object JsonContext {
  implicit def toJsonContext[T: Serializer](value: T): JsonContext =
    JsonContext(Json(implicitly[Serializer[T]].serialize(value), Nil))
}
case class JsonContext(json: Json) extends AnyVal

case class Json(root: JValue, path: List[Either[Int, String]] = Nil) extends Dynamic {
  def apply(idx: Int): Json = Json(root, Left(idx) :: path)
  def apply(field: String): Json = Json(root, Right(field) :: path)

  def selectDynamic(field: String): Json = this(field)
  def applyDynamic(field: String)(idx: Int): Json = this(field)(idx)

  def normalize: Option[Json] = {
    def dereference(value: JValue, path: List[Either[Int, String]]): Option[JValue] = path match {
      case Nil => Some(value)
      case Left(idx) :: tail => value match {
        case JArray(vs) => vs.lift(idx).flatMap(dereference(_, tail))
        case _ => None
      }
      case Right(field) :: tail => value match {
        case JObject(vs) => vs.get(field).flatMap(dereference(_, tail))
        case _ => None
      }
    }
    
    dereference(root, path.reverse).map(Json(_, Nil))
  }

  def as[T: Deserializer]: Option[T] =
    normalize.flatMap { json => implicitly[Deserializer[T]].deserialize(json.root) }

  override def toString(): String = normalize.map(_.root.render()).getOrElse("undefined")
}
