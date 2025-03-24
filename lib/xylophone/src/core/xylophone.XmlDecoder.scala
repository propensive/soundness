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
import distillate.*
import prepositional.*
import proscenium.*
import rudiments.*
import wisteria.*

trait XmlDecoder[value]:
  def read(xml: List[XmlAst]): value

  def map[value2](lambda: value => value2): XmlDecoder[value2] =
    list => lambda(read(list))

object XmlDecoder extends Derivation[XmlDecoder]:
  given text(using Tactic[XmlReadError]): XmlDecoder[Text] = list =>
    val elements = childElements(list).collect { case XmlAst.Textual(text) => text }
    if elements.length == 0 then raise(XmlReadError()) yet "".tt else elements.head

  given [value: Decodable in Text]: XmlDecoder[value] = value =>
    value.absolve match
      case XmlAst.Element(_, XmlAst.Textual(text) :: _, _, _) +: _ => text.decode[value]

  inline def join[DerivationType <: Product: ProductReflection]: XmlDecoder[DerivationType] =
    list =>
      val elements = childElements(list)

      construct:
        [FieldType] => context =>
          val element =
            elements.find:
              case element: XmlAst.Element if element.name.name == label => true
              case _                                                     => false

            . get

          context.read(List(element))

  inline def split[DerivationType: SumReflection]: XmlDecoder[DerivationType] = list =>
    list.head.absolve match
      case XmlAst.Element(_, children, attributes, _) =>
        delegate(attributes.get(XmlName("type".tt)).get):
          [VariantType <: DerivationType] => decoder =>
            decoder.read(list)

  private def childElements(seq: List[XmlAst]): Seq[XmlAst] =
    seq.collect { case e@XmlAst.Element(_, children, _, _) => children }.flatten
