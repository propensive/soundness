package euphemism

import magnolia._
import mercator._

import org.xml.sax._
import org.w3c.dom.{Node => XValue, NodeList => XList}
import javax.xml.parsers._, javax.xml.transform._, javax.xml.transform.dom._, javax.xml.transform.stream._
import java.io._

import collection.mutable, collection.generic.CanBuildFrom
import language.experimental.macros, language.dynamics, language.higherKinds, language.implicitConversions

object Xml extends Dynamic {

  /*object Serializer extends Serializer_1 {
    implicit val int: Serializer[Int] = JNum(_)
    implicit val string: Serializer[String] = JString(_)
    implicit val double: Serializer[Double] = JNum(_)
    implicit val long: Serializer[Long] = JNum(_)
    implicit val byte: Serializer[Byte] = JNum(_)
    implicit val short: Serializer[Short] = JNum(_)
    implicit val boolean: Serializer[Boolean] = if(_) JTrue else JFalse
    implicit val xml: Serializer[Xml] = _.normalize.get.root
    implicit val nil: Serializer[Nil.type] = value => JArray(Array())

    implicit def collection[Coll[T1] <: Traversable[T1], T: Serializer]
                           (implicit cbf: CanBuildFrom[Nothing, T, Array[T]]): Serializer[Coll[T]] =
      coll => JArray(coll.map(implicitly[Serializer[T]].serialize(_)).to[Array])
    
    implicit def map[T: Serializer]: Serializer[Map[String, T]] = values =>
      JObject(mutable.Map(values.mapValues(implicitly[Serializer[T]].serialize(_)).to[Seq]: _*))

    implicit def option[T: Serializer]: Serializer[Option[T]] = new Serializer[Option[T]] {
      override def omit(t: Option[T]): Boolean = t.isEmpty
      def serialize(value: Option[T]): IndexedSeq[XValue] = value match {
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
  }*/

  trait Serializer[T] {
    def omit(t: T): Boolean = false
    def serialize(t: T): XValue
  }

  /*object Deserializer extends Deserializer_1 {
    implicit val int: Deserializer[Int] = _.getInt
    implicit val double: Deserializer[Double] = _.getDouble
    implicit val long: Deserializer[Long] = _.getLong
    implicit val string: Deserializer[String] = _.getString
    implicit val short: Deserializer[Short] = _.getInt.map(_.toShort)
    implicit val byte: Deserializer[Byte] = _.getInt.map(_.toByte)
    implicit val boolean: Deserializer[Boolean] = _.getBoolean
    implicit val xml: Deserializer[Xml] = value => Some(Xml(value, Nil))

    implicit def collection[Coll[T1] <: Traversable[T1], T: Deserializer]
                           (implicit cbf: CanBuildFrom[Nothing, T, Coll[T]]): Deserializer[Coll[T]] = { vs =>
      val builder = cbf()
      vs.foldLeft(Option(())) { case (acc, next) =>
        implicitly[Deserializer[T]].deserialize(next).map(builder += _)
      }.map { _ => builder.result() }
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

    def combine[T](caseClass: CaseClass[Deserializer, T]): Deserializer[T] = xml =>
      caseClass.constructMonadic { param => xml match {
        case JObject(vs) => vs.get(param.label).flatMap(param.typeclass.deserialize(_))
        case _ => None
      } }

    def dispatch[T](sealedTrait: SealedTrait[Deserializer, T]): Deserializer[T] = { xml =>
      for {
        str     <- Xml(xml, Nil)._type.as[String]
        subtype <- sealedTrait.subtypes.find(_.typeName.short == str)
        value   <- subtype.typeclass.deserialize(xml)
      } yield value
    }

    implicit def gen[T]: Deserializer[T] = macro Magnolia.gen[T]
  }*/

  trait Deserializer[T] { def deserialize(xml: IndexedSeq[XValue]): Option[T] }

  def apply[T: Serializer](value: T): Xml = Xml(IndexedSeq(implicitly[Serializer[T]].serialize(value)), Nil)
  def parse(str: String): Option[Xml] = {
    val builder = DocumentBuilderFactory.newInstance().newDocumentBuilder()
    try Some(Xml(IndexedSeq(builder.parse(new ByteArrayInputStream(str.getBytes)).getFirstChild), Nil)) catch { case e: Exception => None }
  }

  /*def applyDynamicNamed[T <: String](methodName: T)(elements: (String, Context)*): Xml =
    Xml(JObject(mutable.Map(elements.map { case (k, v) => k -> v.xml.root }: _*)), Nil)*/

  object Context {
    implicit def toContext[T: Serializer](value: T): Context =
      Context(Xml(IndexedSeq(implicitly[Serializer[T]].serialize(value)), Nil))
  }

  case class Context(xml: Xml) extends AnyVal

  private[euphemism] object Utils {

    private[euphemism] def dereference(value: XValue, path: List[Either[Int, String]]): Option[IndexedSeq[XValue]] = path match {
      case Nil => Some(IndexedSeq(value))
      case Left(idx) :: tail => ???
      case Right(field) :: tail => Option(children(value).filter(_.getNodeName == field)).flatMap(dereferenceList(_, tail))
    }
    
    private[euphemism] def dereferenceList(value: IndexedSeq[XValue], path: List[Either[Int, String]]): Option[IndexedSeq[XValue]] = path match {
      case Nil => Some(value)
      case Left(idx) :: tail => value.lift(idx).flatMap(dereference(_, tail))
      case Right(field) :: tail => ???
    }
    
    private[euphemism] def render(node: XValue): String = {
      val transformer = TransformerFactory.newInstance().newTransformer()
      transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes")
      transformer.setOutputProperty(OutputKeys.INDENT, "yes")
      transformer.setOutputProperty(OutputKeys.ENCODING, "UTF-8")
      transformer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "2")
      val result = new StreamResult(new StringWriter())
      val source = new DOMSource(node)
      transformer.transform(source, result)
      result.getWriter().toString
    }

    private[euphemism] def children(xml: XValue): IndexedSeq[XValue] = {
      val xs = xml.getChildNodes
      (0 until xs.getLength).map(xs.item)
    }

  }
}

case class Xml(root: IndexedSeq[XValue], path: List[Either[Int, String]] = Nil) extends Dynamic {
  def apply(idx: Int = 0): Xml = Xml(root, Left(idx) :: path)
  def apply(field: String): XmlList = XmlList(root, Right(field) :: path)

  def * : XmlList = XmlList(root, Right("") :: path)

  def selectDynamic(field: String): XmlList = this(field)
  def applyDynamic(field: String)(idx: Int = 0): Xml = this(field)(idx)

  def normalize: Option[Xml] = Xml.Utils.dereference(root.head, path.reverse).map { n => Xml(IndexedSeq(n.head), Nil) }

  def as[T: Xml.Deserializer]: Option[T] =
    normalize.flatMap { xml => implicitly[Xml.Deserializer[T]].deserialize(xml.root) }

  override def toString(): String = normalize.map { x => Xml.Utils.render(x.root.head) }.getOrElse("<undefined/>")
}

object XmlList {
  trait Serializer[T] { def serialize(t: T): IndexedSeq[XValue] }
  trait Deserializer[T] { def deserialize(xml: IndexedSeq[XValue]): Option[T] }
}

case class XmlList(root: IndexedSeq[XValue], path: List[Either[Int, String]]) extends Dynamic {
  def apply(idx: Int = 0): Xml = Xml(root, Left(idx) :: path)

  def selectDynamic(field: String): XmlList = this()(field)
  def normalize: Option[XmlList] = Xml.Utils.dereference(root.head, path.reverse).map(XmlList(_, Nil))

  def as[T: XmlList.Deserializer]: Option[T] =
    normalize.flatMap { xml => implicitly[XmlList.Deserializer[T]].deserialize(xml.root) }
  
  override def toString(): String = normalize.map { x => x.root.map(Xml.Utils.render).mkString("\n") }.getOrElse("<undefined/>")
}

object XmlAttribute {

  trait Serializer[T] {
    def omit(t: T): Boolean = false
    def serialize(t: T): String
  }

  trait Deserializer[T] { def deserialize(xml: String): Option[T] }
}

case class XmlAttribute(root: XValue, path: List[Either[Int, String]], attribute: String) {
  def as[T: XmlAttribute.Deserializer]: Option[T] = None
}

