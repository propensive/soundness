package xylophone

import wisteria.*
import rudiments.*
import contextual.*

import scala.annotation.StaticAnnotation

import language.dynamics

case class Namespace(uri: String)
case class Alias(id: String)

case class TagName(namespace: Namespace | DefaultNamespace.type, name: String)

object DefaultNamespace

enum Ast:
  case Element(name: TagName, children: Seq[Ast], attributes: Map[String, String],
                   namespaces: Map[Alias, Namespace])
  case Comment(content: String)
  case ProcessingInstruction(target: String, content: String)
  case Text(content: String)
  case CData(content: String)
  case Root(content: Ast*)

  override def toString(): String = this match
    case Element(name, children, attributes, namespaces) =>
      val inside = children.map(_.toString).join
      str"<${name.name}${attributes.map(_+"="+_).join(" ", " ", "")}>$inside</${name.name}>"

    case Comment(content) =>
      str"<!--$content-->"

    case ProcessingInstruction(target, content) =>
      str"<?$target $content?>"

    case Text(content) =>
      content

    case CData(content) =>
      str"<![CDATA[${content.toString}]]>"

    case Root(content*) =>
      str"""<?xml version = "1.0"?>${content.map(_.toString).join}"""

sealed trait Xml:
  def pointer: List[Int | String | Unit]
  def root: Ast.Root
  def +(other: Xml): Doc

  def string(using XmlPrinter[String]): String =
    summon[XmlPrinter[String]].write(Doc(Ast.Root(Xml.normalize(this)*)))

  override def toString(): String =
    printers.compact.write(Doc(Ast.Root(Xml.normalize(this)*)))

case class Fragment(head: String | Unit, path: List[Int | String | Unit], root: Ast.Root)
extends Xml, Dynamic:
  def apply(idx: Int = 0): Node = Node(idx, head :: path, root)
  def attribute(key: String): Attribute = apply(0).attribute(key)
  def pointer: List[Int | String | Unit] = (head :: path).reverse
  def selectDynamic(tagName: String): Fragment = Fragment(tagName, head :: path, root)
  def applyDynamic(tagName: String)(idx: Int = 0): Node = selectDynamic(tagName).apply(idx)
  def * : Fragment = Fragment((), head :: path, root)
  def +(other: Xml): Doc = Doc(Ast.Root(Xml.normalize(this) ++ Xml.normalize(other)*))
  def as[T: Xml.Reader]: T exposes AccessError = apply().as[T]

case class Node(head: Int, path: List[Int | String | Unit], root: Ast.Root) extends Xml, Dynamic:
  def selectDynamic(tagName: String): Fragment = Fragment(tagName, head :: path, root)
  def applyDynamic(tagName: String)(idx: Int = 0): Node = selectDynamic(tagName).apply(idx)
  def attribute(attribute: String): Attribute = Attribute(this, attribute)
  def pointer: List[Int | String | Unit] = (head :: path).reverse
  def * : Fragment = Fragment((), head :: path, root)
  def +(other: Xml): Doc = Doc(Ast.Root(Xml.normalize(this) ++ Xml.normalize(other)*))

  def as[T: Xml.Reader]: T =
    summon[Xml.Reader[T]].read(Xml.normalize(this)).getOrElse(throw ReadError())

case class Doc(root: Ast.Root) extends Xml, Dynamic:
  def pointer: List[Int | String | Unit] = Nil
  def selectDynamic(tagName: String): Fragment = Fragment(tagName, Nil, root)
  def applyDynamic(tagName: String)(idx: Int = 0): Node = selectDynamic(tagName).apply(idx)
  def * : Fragment = Fragment((), Nil, root)
  def +(other: Xml): Doc = Doc(Ast.Root(Xml.normalize(this) ++ Xml.normalize(other)*))

  def as[T: Xml.Reader]: T =
    summon[Xml.Reader[T]].read(Xml.normalize(this)).getOrElse(throw ReadError())

case class Attribute(node: Node, attribute: String):
  def as[T: Xml.Reader]: T =
    val attributes = Xml.normalize(node).headOption match
      case Some(Ast.Element(_, _, attributes, _)) => attributes
      case _                                      => throw ReadError()

    summon[Xml.Reader[T]]
      .read(Seq(Ast.Element(TagName(DefaultNamespace, "empty"),
          Seq(Ast.Text(attributes(attribute))), Map(), Map())))
      .getOrElse(throw ReadError())

class XylophoneException(msg: String) extends Exception(str"xylophone: $msg")
case class ParseError() extends Exception("could not parse XML")
case class ReadError() extends XylophoneException("could not read value")
case class AccessError() extends XylophoneException("could not access value")

case class xmlAttribute() extends StaticAnnotation

case class PartialXml(text: String)
case class XmlInput(string: String)

object XmlInterpolator extends Interpolator[XmlInput, PartialXml, Xml]:
  def complete(state: PartialXml): Xml exposes ParseError = ???
  def initial: PartialXml = ???
  def insert(state: PartialXml, value: Option[XmlInput]): PartialXml exposes ParseError = ???
  def parse(state: PartialXml, next: String): PartialXml = ???

object Xml:

  def text(value: Ast.Element): String = value.children.collect { case Ast.Text(txt) => txt }.join

  object Writer extends Derivation[Writer]:
    given Writer[String] = str =>
      Ast.Element(TagName(DefaultNamespace, "String"), List(Ast.Text(str)), Map(), Map())

    given Writer[Int] = int =>
      Ast.Element(TagName(DefaultNamespace, "Int"), List(Ast.Text(int.toString)), Map(), Map())

    private val attributeAttribute = xmlAttribute()

    def join[T](caseClass: CaseClass[Writer, T]): Writer[T] = value =>
      val elements =
        caseClass.params
          .filter(!_.annotations.contains(attributeAttribute))
          .map { p => p.typeclass.write(p.deref(value)).copy(name = TagName(DefaultNamespace,
              p.label))

      val attributes =
        caseClass.params
          .filter(_.annotations.contains(attributeAttribute))
          .map { p => p.label -> text(p.typeclass.write(p.deref(value))) }
          .to(Map)

      Ast.Element(TagName(DefaultNamespace, caseClass.typeInfo.short), elements, attributes, Map())

    def split[T](sealedTrait: SealedTrait[Writer, T]): Writer[T] = value =>
      sealedTrait.choose(value) { subtype =>
        val xml = subtype.typeclass.write(subtype.cast(value))
        Ast.Element(
          TagName(DefaultNamespace, sealedTrait.typeInfo.short),
          xml.children,
          xml.attributes.updated("type", xml.name.name),
          xml.namespaces
        )
      }

  trait Writer[T]:
    def write(value: T): Ast.Element
    def contraMap[S](fn: S => T): Writer[S] = value => write(fn(value))

  private def childElements(seq: Seq[Ast]): Seq[Ast] =
    seq.collect { case e@Ast.Element(_, children, _, _) => children }.flatten

  object Reader extends Derivation[Reader]:
    given string: Reader[String] =
      childElements(_).collect { case Ast.Text(txt) => txt }.headOption

    given Reader[Int] = string.map(Int.unapply(_))

    def join[T](caseClass: CaseClass[Reader, T]): Reader[T] = seq =>
      val elems = childElements(seq)
      caseClass.constructMonadic { param =>
        elems
          .collect { case e: Ast.Element => e }
          .find(_.name.name == param.label)
          .flatMap { e => param.typeclass.read(Seq(e)) }
      }

    def split[T](sealedTrait: SealedTrait[Reader, T]): Reader[T] = seq =>
      seq.headOption match
        case Some(Ast.Element(_, children, attributes, _)) =>
          attributes
            .get("type")
            .flatMap { t => sealedTrait.subtypes.find(_.typeInfo.short == t) }
            .flatMap(_.typeclass.read(seq))
        case _ =>
          None


  trait Reader[T]:
    def read(xml: Seq[Ast]): Option[T]
    def map[S](fn: T => Option[S]): Reader[S] = read(_).flatMap(fn(_))

  def normalize(xml: Xml): Seq[Ast] =
    def recur(path: List[Int | String | Unit], current: Seq[Ast]): Seq[Ast] = path match
      case Nil =>
        current

      case (idx: Int) :: tail =>
        recur(tail, Seq(current(idx)))

      case (unit: Unit) :: tail =>
        val next = current
          .collect { case e@Ast.Element(_, children, _, _) => children }
          .flatten
          .collect { case e: Ast.Element => e })

        recur(tail, next)

      case (label: String) :: tail =>
        val next = current
          .collect { case e@Ast.Element(_, children, _, _) => children }
          .flatten.collect { case e: Ast.Element if e.name.name == label => e }

        recur(tail, next)

    recur(xml.pointer, xml.root.content)

  def parse(content: String): Doc =
    import org.w3c.dom as owd, owd.Node.*
    import org.xml.sax as oxs
    import javax.xml.parsers.*
    import java.io.*

    val factory = DocumentBuilderFactory.newInstance()
    val builder = factory.newDocumentBuilder()

    val root = 
      try builder.parse(ByteArrayInputStream(content.bytes.unsafeMutable))
      catch case _: oxs.SAXParseException => throw ParseError()

    def readNode(node: owd.Node): Ast = node.getNodeType match
      case CDATA_SECTION_NODE =>
        Ast.CData(node.getTextContent)

      case COMMENT_NODE =>
        Ast.Comment(node.getTextContent)

      case ELEMENT_NODE =>
        val tagName = TagName(DefaultNamespace, node.getNodeName)
        val childNodes = node.getChildNodes
        val children = (0 until childNodes.getLength).map(childNodes.item(_)).map(readNode(_))
        val atts = (0 until node.getAttributes.getLength).map(node.getAttributes.item(_))
        val attributes = atts.map { att => att.getNodeName -> att.getTextContent }.to(Map)
        Ast.Element(tagName, children, attributes, Map())

      case PROCESSING_INSTRUCTION_NODE =>
        val name = node.getNodeName
        val content = node.getTextContent
        Ast.ProcessingInstruction(name, content)

      case TEXT_NODE =>
        Ast.Text(node.getTextContent)

    readNode(root.getDocumentElement) match
      case elem@Ast.Element(_, _, _, _) => Doc(Ast.Root(elem))
      case _                            => throw Impossible("xylophone: malformed XML")

extension [T](value: T)
  def xml(using write: Xml.Writer[T]): Doc = Doc(Ast.Root(write.write(value)))

trait XmlPrinter[T]:
  def write(doc: Xml): T

object XmlPrinter:
  given XmlPrinter[String] = StandardXmlPrinter(false)

object printers:
  given compact: XmlPrinter[String] = StandardXmlPrinter(true)

class StandardXmlPrinter(compact: Boolean = false) extends XmlPrinter[String]:
  def write(doc: Xml): String =
    var indent: Int = 0
    var linebreak: Boolean = false
    val buf: StringBuilder = StringBuilder()
    var pos: Int = 0

    def newline(n: Int = 0): Unit =
      if !compact then
        indent += n
        linebreak = true

    def append(strings: String*): Unit =
      for str <- strings do
        buf.append(str)
        pos += str.length

    def whitespace(): Unit =
      if !compact && linebreak then
        buf.append("\n")
        for i <- 1 to indent do buf.append("  ")
        pos = indent*2
      linebreak = false

    def inline(element: Ast.Element): Boolean = element.children.forall {
      case Ast.Text(_) => true
      case _       => false
    }

    def next(node: Ast): Unit = node match
      case element@Ast.Element(tagName, children, attributes, namespaces) =>
        whitespace()
        append("<", tagName.name)

        for attribute <- attributes do attribute match
          case (key, value) => append(" ", key, "=\"", value, "\"")

        append(">")
        if !inline(element) then newline(1)

        for child <- element.children do
          val splitLine = child match
            case Ast.Text(_) => false
            case _           => true
          if splitLine then newline()
          next(child)
          if splitLine then newline()

        if !inline(element) then newline(-1)

        whitespace()
        append("</", tagName.name, ">")
        if !inline(element) then newline(0)

      case Ast.Text(text) =>
        whitespace()
        append(text)

      case Ast.ProcessingInstruction(target, content) =>
        whitespace()
        append("<?", target, " ", content, "?>")
        newline()

      case Ast.Comment(content) =>
        whitespace()
        append("<!--", content, "-->")
        newline()

      case e => println("Skipping "+e)

    doc.root.content.foreach(next(_))

    buf.toString
