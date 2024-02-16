/*
    Xylophone, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package xylophone

import rudiments.*
import vacuous.*
import gossamer.*
import hieroglyph.*
import contingency.*
import spectacular.*
import anticipation.*

import annotation.targetName
import language.dynamics
import scala.util.NotGiven

case class Namespace(id: Text, uri: Text)

object XmlName:
  given Show[XmlName] = name => name.namespace match
    case Unset                => name.name
    case Namespace(prefix, _) => t"$prefix:${name.name}"

case class XmlName(name: Text, namespace: Optional[Namespace] = Unset)

sealed trait Xml:
  def pointer: List[Int | Text | Unit]
  def root: XmlAst.Root
  
  @targetName("add")
  infix def + (other: Xml): XmlDoc raises XmlAccessError

  def string(using XmlPrinter[Text]): Text raises XmlAccessError =
    summon[XmlPrinter[Text]].print(XmlDoc(XmlAst.Root(Xml.normalize(this)*)))

type XmlPath = List[Text | Int | Unit]

object Xml:
  given show: Show[Xml] = xml =>
    safely(printers.compact.print(XmlDoc(XmlAst.Root(Xml.normalize(xml)*)))).or(t"undefined")

  given (using enc: Encoding, printer: XmlPrinter[Text]): GenericHttpResponseStream[Xml] with
    def mediaType: Text = t"application/xml; charset=${enc.name}"
    def content(xml: Xml): LazyList[IArray[Byte]] =
      LazyList(summon[XmlPrinter[Text]].print(xml).bytes(using charEncoders.utf8))

  def print(xml: Xml)(using XmlPrinter[Text]): Text = summon[XmlPrinter[Text]].print(xml)

  def pathString(path: XmlPath): Text = if path.isEmpty then t"/" else path.map: value =>
    (value: @unchecked) match
      case idx: Int    => t"[$idx]"
      case label: Text => t"/$label"
      case unit: Unit  => t"/*"
  .join

  def parse(content: Text): XmlDoc raises XmlParseError =
    import org.w3c.dom as owd, owd.Node.*
    import org.xml.sax as oxs
    import javax.xml.parsers.*
    import java.io.*

    val factory = DocumentBuilderFactory.newInstance().nn
    factory.setNamespaceAware(true)
    val builder = factory.newDocumentBuilder().nn

    val root = 
      try
        val array = content.bytes(using charEncoders.utf8).mutable(using Unsafe)
        builder.parse(ByteArrayInputStream(array)).nn
      catch case e: oxs.SAXParseException =>
        abort(XmlParseError(e.getLineNumber - 1, e.getColumnNumber - 1))

    def getNamespace(node: owd.Node): Optional[Namespace] =
      Option(node.getPrefix) match
        case None         => Unset
        case Some(prefix) => Option(node.getNamespaceURI) match
                               case None      => Unset
                               case Some(uri) => Namespace(prefix.nn.tt, uri.nn.tt)

    def readNode(node: owd.Node): XmlAst = node.getNodeType match
      case CDATA_SECTION_NODE =>
        XmlAst.CData(node.getTextContent.nn.tt)

      case COMMENT_NODE =>
        XmlAst.Comment(node.getTextContent.nn.tt)

      case ELEMENT_NODE =>
        val xmlName = XmlName(node.getLocalName.nn.tt, getNamespace(node))
        val childNodes = node.getChildNodes
        val children = List.range(0, childNodes.nn.getLength).map(childNodes.nn.item(_).nn).map(readNode(_))
        val atts = (0 until node.getAttributes.nn.getLength).map(node.getAttributes.nn.item(_).nn)
        val attributes = atts.map { att =>
          val alias: Optional[Namespace] = getNamespace(att)
          XmlName(att.getLocalName.nn.tt, alias) -> att.getTextContent.nn.tt
        }.to(Map)
        
        XmlAst.Element(xmlName, children, attributes)

      case PROCESSING_INSTRUCTION_NODE =>
        XmlAst.ProcessingInstruction(node.getNodeName.nn.tt, node.getTextContent.nn.tt)

      case TEXT_NODE =>
        XmlAst.Textual(node.getTextContent.nn.tt)
      
      case id =>
        XmlAst.Comment(t"unrecognized node $id")

    (readNode(root.getDocumentElement.nn): @unchecked) match
      case elem@XmlAst.Element(_, _, _, _) => XmlDoc(XmlAst.Root(elem))

  def normalize(xml: Xml): List[XmlAst] raises XmlAccessError =
    def recur(path: XmlPath, current: List[XmlAst]): List[XmlAst] = (path: @unchecked) match
      case Nil =>
        current

      case (idx: Int) :: tail =>
        if current.length <= idx then abort(XmlAccessError(idx, path))
        recur(tail, List(current(idx)))

      case (unit: Unit) :: tail =>
        val next = current
          .collect { case e@XmlAst.Element(_, children, _, _) => children }
          .flatten
          .collect { case e: XmlAst.Element => e }

        recur(tail, next)

      case (label: Text) :: tail =>
        val next = current
          .collect { case e@XmlAst.Element(_, children, _, _) => children }
          .flatten.collect { case e: XmlAst.Element if e.name.name == label => e }

        recur(tail, next)

    try recur(xml.pointer, xml.root.content.to(List))
    catch case err: XmlAccessError =>
      abort(XmlAccessError(err.index, xml.pointer.dropRight(err.path.length)))


case class XmlFragment(head: Text | Unit, path: XmlPath, root: XmlAst.Root)
extends Xml, Dynamic:
  def apply(idx: Int = 0): XmlNode = XmlNode(idx, head :: path, root)
  def attribute(key: Text): Attribute = apply(0).attribute(key)
  def pointer: XmlPath = (head :: path).reverse
  def selectDynamic(tagName: String): XmlFragment = XmlFragment(tagName.tt, head :: path, root)
  def applyDynamic(tagName: String)(idx: Int = 0): XmlNode = selectDynamic(tagName).apply(idx)
  
  @targetName("all")
  def `*`: XmlFragment = XmlFragment((), head :: path, root)
  
  @targetName("add")
  infix def + (other: Xml): XmlDoc raises XmlAccessError =
    XmlDoc(XmlAst.Root(Xml.normalize(this) ++ Xml.normalize(other)*))
  
  def as
      [ValueType]
      (using decoder: XmlDecoder[ValueType])
      : ValueType raises XmlAccessError raises XmlReadError =
    apply().as[ValueType]

case class XmlNode(head: Int, path: XmlPath, root: XmlAst.Root) extends Xml, Dynamic:
  def selectDynamic(tagName: String): XmlFragment = XmlFragment(tagName.tt, head :: path, root)
  def applyDynamic(tagName: String)(idx: Int = 0): XmlNode = selectDynamic(tagName).apply(idx)
  def attribute(attribute: Text): Attribute = Attribute(this, attribute)
  def pointer: XmlPath = (head :: path).reverse
  
  @targetName("all")
  def `*`: XmlFragment = XmlFragment((), head :: path, root)
  
  @targetName("add")
  infix def + (other: Xml): XmlDoc raises XmlAccessError =
    XmlDoc(XmlAst.Root(Xml.normalize(this) ++ Xml.normalize(other)*))

  def as[ValueType: XmlDecoder](using Raises[XmlReadError], Raises[XmlAccessError]): ValueType =
    summon[XmlDecoder[ValueType]].read(Xml.normalize(this))

case class XmlDoc(root: XmlAst.Root) extends Xml, Dynamic:
  def pointer: XmlPath = Nil
  def selectDynamic(tagName: String): XmlFragment = XmlFragment(tagName.tt, Nil, root)
  def applyDynamic(tagName: String)(idx: Int = 0): XmlNode = selectDynamic(tagName).apply(idx)
  
  @targetName("all")
  def `*`: XmlFragment = XmlFragment((), Nil, root)
  
  @targetName("add")
  infix def + (other: Xml): XmlDoc raises XmlAccessError =
    XmlDoc(XmlAst.Root(Xml.normalize(this) ++ Xml.normalize(other)*))

  def as[ValueType: XmlDecoder](using Raises[XmlAccessError], Raises[XmlReadError]): ValueType =
    summon[XmlDecoder[ValueType]].read(Xml.normalize(this))

case class Attribute(node: XmlNode, attribute: Text):
  def as
      [ValueType]
      (using decoder: XmlDecoder[ValueType])
      (using Raises[XmlReadError], Raises[XmlAccessError]): ValueType =

    val attributes = Xml.normalize(node).headOption match
      case Some(XmlAst.Element(_, _, attributes, _)) => attributes
      case _                                         => abort(XmlReadError())

    decoder.read(List(XmlAst.Element(XmlName(t"empty"), List(XmlAst.Textual(attributes(XmlName(attribute)))))))

case class xmlAttribute() extends StaticAnnotation
case class xmlLabel(name: String) extends StaticAnnotation

extension [T](value: T)(using NotGiven[T =:= StringContext])
  def xml(using writer: XmlEncoder[T]): XmlDoc = XmlDoc(XmlAst.Root(writer.write(value)))
