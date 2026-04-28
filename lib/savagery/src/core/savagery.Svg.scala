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
import gossamer.*
import hieroglyph.*
import prepositional.*
import proscenium.*
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
      val text: Text = summon[Text is Aggregable by Text].aggregate(source)
      val s = text.s.trim.nn

      val (encoding, body): (Encoding, Text) =
        if s.startsWith("<?xml") then
          val endIndex = s.indexOf("?>")

          if endIndex < 0 then (enc"UTF-8", s.tt)
          else
            val header = s.substring(0, endIndex).nn
            val encStart = header.indexOf("encoding")

            val encoding: Encoding =
              if encStart < 0 then enc"UTF-8"
              else
                val afterEq = header.indexOf("=", encStart)

                if afterEq < 0 then enc"UTF-8"
                else
                  val rest = header.substring(afterEq + 1).nn.trim.nn
                  val quote = if rest.length > 0 then rest.charAt(0) else '"'

                  if quote != '"' && quote != '\'' then enc"UTF-8"
                  else
                    val close = rest.indexOf(quote.toInt, 1)
                    if close < 0 then enc"UTF-8"
                    else
                      val name = rest.substring(1, close).nn
                      Encoding.unapply(name.tt).getOrElse(enc"UTF-8")

            (encoding, s.substring(endIndex + 2).nn.trim.nn.tt)
        else (enc"UTF-8", text)

      val xml: Xml = body.read[Xml]
      val svgElement = SvgParser.rootElement(xml)
      val parsedSvg: Svg = SvgParser.decodeSvg(svgElement)
      Document[Svg](parsedSvg, encoding)


  given showable: [doc <: Document[Svg]] => doc is Showable =
    document =>
      val header = Header(t"1.0", document.metadata.name, Unset)

      val full: Xml = document.root.xml.absolve match
        case node: Node       => Fragment(header, node)
        case Fragment(nodes*) => Fragment((header +: nodes)*)

      full.show


case class Svg
    (width: Float, height: Float, defs: List[SvgDef] = Nil, figures: List[Figure] = Nil)
extends Documentary:
  type Self = Svg
  type Metadata = Encoding

  def xml: Xml =
    given showable: Float is Showable = _.toString.tt

    val attrs: SeqMap[Text, Text] = SeqMap
     (t"xmlns"   -> t"http://www.w3.org/2000/svg",
      t"viewBox" -> t"0 0 ${width.show} ${height.show}",
      t"width"   -> width.show,
      t"height"  -> height.show)

    val defsElement: Seq[Xml] =
      if defs.isEmpty then Nil
      else Seq(Element(t"defs", SeqMap[Text, Text](), defs.map(_.xml).toSeq.nodes))

    val children: IArray[Node] = (defsElement ++ figures.map(_.xml)).nodes
    Element(t"svg", attrs, children)
