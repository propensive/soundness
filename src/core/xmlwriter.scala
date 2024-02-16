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

import anticipation.*
import rudiments.*
import spectacular.*
import gossamer.*

object XmlEncoder:
  given XmlEncoder[Text] = text => XmlAst.Element(XmlName(t"Text"), List(XmlAst.Textual(text)))
  given XmlEncoder[String] = string => XmlAst.Element(XmlName(t"String"), List(XmlAst.Textual(string.tt)))

  given [ValueType: XmlEncoder, CollectionType[ElementType] <: Seq[ElementType]]
      : XmlEncoder[CollectionType[ValueType]] =
    elements => XmlAst.Element(XmlName(t"Seq"), elements.to(List).map(summon[XmlEncoder[ValueType]].write(_)))

  given XmlEncoder[Int] = int =>
    XmlAst.Element(XmlName(t"Int"), List(XmlAst.Textual(int.show)))

  private val attributeAttribute = xmlAttribute()

  // def join[DerivationType](caseClass: CaseClass[XmlEncoder, DerivationType]): XmlEncoder[DerivationType] = value =>
  //   val elements = caseClass.params
  //     .filter(!_.annotations.contains(attributeAttribute))
  //     .map { p => p.typeclass.write(p.deref(value)).copy(name = XmlName(Text(p.label))) }

  //   val attributes = caseClass.params
  //     .filter(_.annotations.contains(attributeAttribute))
  //     .map { p => XmlName(Text(p.label)) -> textElements(p.typeclass.write(p.deref(value))) }
  //     .to(Map)

  //   val tag = caseClass.annotations.collect:
  //     case `xmlLabel`(name) => name.show
  //   .headOption.getOrElse(caseClass.typeInfo.short.show)

  //   XmlAst.Element(XmlName(tag), elements, attributes)

  // def split[DerivationType](sealedTrait: SealedTrait[XmlEncoder, DerivationType]): XmlEncoder[DerivationType] =
  //   value =>
  //     sealedTrait.choose(value): subtype =>
  //       val xml = subtype.typeclass.write(subtype.cast(value))
  //       XmlAst.Element(
  //         XmlName(Text(sealedTrait.typeInfo.short)),
  //         xml.children,
  //         xml.attributes.updated(XmlName(t"type"), xml.name.name),
  //         xml.namespaces
  //       )
  
  private def textElements(value: XmlAst.Element): Text =
    value.children.collect { case XmlAst.Textual(txt) => txt }.join

trait XmlEncoder[-ValueType]:
  def write(value: ValueType): XmlAst.Element
  def contramap[ValueType2](lambda: ValueType2 => ValueType): XmlEncoder[ValueType2] = value => write(lambda(value))

