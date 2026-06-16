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
┃    Soundness, version 0.54.0.                                                                    ┃
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
package savagery

import scala.collection.immutable.SeqMap

import anticipation.*
import contingency.*
import denominative.*
import gossamer.*
import hieroglyph.*
import prepositional.*
import spectacular.*
import turbulence.*
import vacuous.*
import xylophone.*
import zephyrine.*

object Svg:
  given aggregable: (XmlSchema)
  =>  Tactic[ParseError]
  =>  Tactic[XmlError]
  =>  Tactic[SvgError]
  =>  Svg is Aggregable by Text =

    source =>
      val xml: Xml = summon[Xml is Aggregable by Text].aggregate(source)
      SvgParser.decodeSvg(SvgParser.rootElement(xml))


  given loadable: (XmlSchema)
  =>  Tactic[ParseError]
  =>  Tactic[XmlError]
  =>  Tactic[SvgError]
  =>  Svg is Loadable by Text =

    source =>
      val xmlDoc: Document[Xml] = summon[Xml is Loadable by Text].load(source)
      val svgElement = SvgParser.rootElement(xmlDoc.root)
      val parsedSvg: Svg = SvgParser.decodeSvg(svgElement)

      val encoding: Encoding =
        xmlDoc.metadata.encoding.let: name => Encoding.unapply(name).getOrElse(enc"UTF-8")
        . or(enc"UTF-8")

      Document[Svg](parsedSvg, encoding)


  given showable: [doc <: Document[Svg]] => doc is Showable =
    document =>
      val header = Header(t"1.0", document.metadata.name, Unset)

      val full: Xml = document.root.xml.absolve match
        case node: Node       => Fragment(header, node)
        case Fragment(nodes*) => Fragment((header +: nodes)*)

      full.show


case class Svg
  ( width:      Float,
    height:     Float,
    defs:       List[SvgDef]    = Nil,
    figures:    List[Figure]    = Nil,
    transforms: List[Transform] = Nil )
extends Documentary:

  type Self = Svg
  type Metadata = Encoding

  def xml: Xml =
    given showable: Float is Showable = _.toString.tt

    val attrs: SeqMap[Text, Text] =
      SeqMap
        ( t"xmlns"   -> t"http://www.w3.org/2000/svg",
          t"viewBox" -> t"0 0 ${width.show} ${height.show}",
          t"width"   -> width.show,
          t"height"  -> height.show )

    val defsElement: Seq[Xml] =
      if defs.nil then Nil
      else Seq(Element(t"defs", Attributes.empty, defs.map(_.xml).toSeq.nodes))

    val figureNodes: Seq[Xml] =
      if transforms.nil then figures.map(_.xml)
      else
        val groupAttrs = SeqMap(t"transform" -> transforms.map(_.encode).join(t" "))
        Seq(Element(t"g", Attributes.from(groupAttrs), figures.map(_.xml).toSeq.nodes))

    val children: IArray[Node] = (defsElement ++ figureNodes).nodes
    Element(t"svg", Attributes.from(attrs), children)
