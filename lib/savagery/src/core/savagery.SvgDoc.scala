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

import anticipation.*
import contingency.*
import gossamer.*
import hieroglyph.*
import prepositional.*
import proscenium.*
import turbulence.*
import vacuous.*
import xylophone.*
import zephyrine.*

object SvgDoc:
  given aggregable: (XmlSchema)
        =>  Tactic[ParseError]
        =>  Tactic[XmlError]
        =>  Tactic[SvgError]
        =>  SvgDoc is Aggregable by Text =

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
      val root = SvgParser.rootElement(xml)
      SvgDoc(SvgParser.decodeSvg(root), encoding)


case class SvgDoc(svg: Svg, encoding: Encoding):
  def xml: Xml =
    val header = Header(t"1.0", encoding.name, Unset)

    svg.xml match
      case node: Node       => Fragment(header, node)
      case Fragment(nodes*) => Fragment((header +: nodes)*)
