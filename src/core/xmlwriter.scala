/*
    Xylophone, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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

import wisteria.*
import rudiments.*
import gossamer.*

object XmlWriter extends Derivation[XmlWriter]:
  given XmlWriter[Text] = str => Ast.Element(XmlName(t"Text"), List(Ast.Textual(str)))
  given XmlWriter[String] = str => Ast.Element(XmlName(t"String"), List(Ast.Textual(str.show)))

  given [T](using canon: Canonical[T]): XmlWriter[T] = value =>
    Ast.Element(XmlName(t"value"), List(Ast.Textual(canon.serialize(value))))

  given [T: XmlWriter, Coll[E] <: Seq[E]]: XmlWriter[Coll[T]] = xs =>
    Ast.Element(XmlName(t"Seq"), xs.map(summon[XmlWriter[T]].write(_)))

  given XmlWriter[Int] = int =>
    Ast.Element(XmlName(t"Int"), List(Ast.Textual(int.show)))

  private val attributeAttribute = xmlAttribute()

  def join[T](caseClass: CaseClass[XmlWriter, T]): XmlWriter[T] = value =>
    val elements = caseClass.params
      .filter(!_.annotations.contains(attributeAttribute))
      .map { p => p.typeclass.write(p.deref(value)).copy(name = XmlName(Text(p.label))) }

    val attributes = caseClass.params
      .filter(_.annotations.contains(attributeAttribute))
      .map { p => XmlName(Text(p.label)) -> textElements(p.typeclass.write(p.deref(value))) }
      .to(Map)

    val tag = caseClass.annotations.collect:
      case `xmlLabel`(name) => name.show
    .headOption.getOrElse(caseClass.typeInfo.short.show)

    Ast.Element(XmlName(tag), elements, attributes)

  def split[T](sealedTrait: SealedTrait[XmlWriter, T]): XmlWriter[T] = value =>
    sealedTrait.choose(value) { subtype =>
      val xml = subtype.typeclass.write(subtype.cast(value))
      Ast.Element(
        XmlName(Text(sealedTrait.typeInfo.short)),
        xml.children,
        xml.attributes.updated(XmlName(t"type"), xml.name.name),
        xml.namespaces
      )
    }
  
  private def textElements(value: Ast.Element): Text =
    value.children.collect { case Ast.Textual(txt) => txt }.join

trait XmlWriter[T]:
  def write(value: T): Ast.Element
  def contramap[S](fn: S => T): XmlWriter[S] = value => write(fn(value))

