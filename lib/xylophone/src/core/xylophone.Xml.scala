                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.27.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package xylophone

import anticipation.*
import contingency.*
import gossamer.*
import hieroglyph.*
import prepositional.*
import proscenium.*
import rudiments.*
import spectacular.*
import vacuous.*

import language.dynamics

sealed trait Xml:
  def pointer: List[Int | Text | Unit]
  def root: XmlAst.Root

  @targetName("add")
  infix def + (other: Xml): XmlDoc raises XmlAccessError

  def string(using XmlPrinter[Text]): Text raises XmlAccessError =
    summon[XmlPrinter[Text]].print(XmlDoc(XmlAst.Root(Xml.normalize(this)*)))

object Xml:
  given showable: Xml is Showable = xml =>
    safely(xmlPrinters.compact.print(XmlDoc(XmlAst.Root(Xml.normalize(xml)*)))).or(t"undefined")

  given abstractable: (encoding: Encoding { type CanEncode = true }, printer: XmlPrinter[Text])
        =>  Xml is Abstractable across HttpStreams into HttpStreams.Content =
    new Abstractable:
      type Self = Xml
      type Domain = HttpStreams
      type Result = HttpStreams.Content

      def genericize(xml: Xml): HttpStreams.Content =
        (t"application/xml; charset=${encoding.name}",
         Stream(printer.print(xml).bytes(using encoding.encoder)))

  def print(xml: Xml)(using XmlPrinter[Text]): Text = summon[XmlPrinter[Text]].print(xml)

  def pathString(path: XmlPath): Text = if path.isEmpty then t"/" else path.map: value =>
    value.absolve match
      case idx: Int    => t"[$idx]"
      case label: Text => t"/$label"
      case unit: Unit  => t"/*"

  . join

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

        val children =
          List.range(0, childNodes.nn.getLength).map(childNodes.nn.item(_).nn).map(readNode(_))

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

    readNode(root.getDocumentElement.nn).absolve match
      case elem@XmlAst.Element(_, _, _, _) => XmlDoc(XmlAst.Root(elem))

  def normalize(xml: Xml): List[XmlAst] raises XmlAccessError =
    def recur(path: XmlPath, current: List[XmlAst]): List[XmlAst] = path.absolve match
      case Nil =>
        current

      case (idx: Int) :: tail =>
        if current.length <= idx then abort(XmlAccessError(idx, path))
        recur(tail, List(current(idx)))

      case (unit: Unit) :: tail =>
        val next =
          current
          . collect { case e@XmlAst.Element(_, children, _, _) => children }
          . flatten
          . collect { case e: XmlAst.Element => e }

        recur(tail, next)

      case (label: Text) :: tail =>
        val next =
          current
          . collect { case e@XmlAst.Element(_, children, _, _) => children }
          . flatten.collect { case e: XmlAst.Element if e.name.name == label => e }

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

  def as[value](using decoder: XmlDecoder[value]): value raises XmlAccessError raises XmlReadError =
    apply().as[value]

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

  def as[value: XmlDecoder](using Tactic[XmlReadError], Tactic[XmlAccessError]): value =
    summon[XmlDecoder[value]].read(Xml.normalize(this))

case class XmlDoc(root: XmlAst.Root) extends Xml, Dynamic:
  def pointer: XmlPath = Nil
  def selectDynamic(tagName: String): XmlFragment = XmlFragment(tagName.tt, Nil, root)
  def applyDynamic(tagName: String)(idx: Int = 0): XmlNode = selectDynamic(tagName).apply(idx)

  @targetName("all")
  def `*`: XmlFragment = XmlFragment((), Nil, root)

  @targetName("add")
  infix def + (other: Xml): XmlDoc raises XmlAccessError =
    XmlDoc(XmlAst.Root(Xml.normalize(this) ++ Xml.normalize(other)*))

  def as[value: XmlDecoder](using Tactic[XmlAccessError], Tactic[XmlReadError]): value =
    summon[XmlDecoder[value]].read(Xml.normalize(this))
