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
import gossamer.*
import spectacular.*
import wisteria.*

object XmlEncoder extends Derivation[XmlEncoder]:
  given text: XmlEncoder[Text] =
    text => XmlAst.Element(XmlName(t"Text"), List(XmlAst.Textual(text)))

  given string: XmlEncoder[String] =
    string => XmlAst.Element(XmlName(t"String"), List(XmlAst.Textual(string.tt)))

  given seq: [value: XmlEncoder, collection[element] <: Seq[element]]
        =>  XmlEncoder[collection[value]] = elements =>
      XmlAst.Element(XmlName(t"Seq"), elements.to(List).map(summon[XmlEncoder[value]].write(_)))

  given int: XmlEncoder[Int] = int =>
    XmlAst.Element(XmlName(t"Int"), List(XmlAst.Textual(int.show)))

  private val attributeAttribute = xmlAttribute()

  inline def join[derivation <: Product: ProductReflection]: XmlEncoder[derivation] =
    value =>
      val elements = fields(value):
        [field] => field => context.write(field).copy(name = XmlName(label))

      XmlAst.Element(XmlName(typeName), elements.to(List))

  inline def split[derivation: SumReflection]: XmlEncoder[derivation] = value =>
    variant(value):
      [variant <: derivation] => variant =>
        val xml = context.write(variant)

        XmlAst.Element
          (XmlName(typeName),
           xml.children,
           xml.attributes.updated(XmlName("type".tt), xml.name.name),
           xml.namespaces)

  private def textElements(value: XmlAst.Element): Text =
    value.children.collect { case XmlAst.Textual(txt) => txt }.join

trait XmlEncoder[-value]:
  def write(value: value): XmlAst.Element

  def contramap[value2](lambda: value2 => value): XmlEncoder[value2] =
    value => write(lambda(value))
