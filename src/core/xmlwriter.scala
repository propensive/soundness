/*
    Xylophone, version 0.1.0. Copyright 2018-21 Jon Pretty, Propensive OÜ.

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
  given XmlWriter[Txt] = str =>
    Ast.Element(XmlName(str"Text"), List(Ast.Textual(str)))

  given [T: XmlWriter]: XmlWriter[List[T]] = xs =>
    Ast.Element(XmlName(str"List"), xs.map(summon[XmlWriter[T]].write(_)))

  given XmlWriter[Int] = int =>
    Ast.Element(XmlName(str"Int"), List(Ast.Textual(int.show)))

  private val attributeAttribute = xmlAttribute()

  def join[T](caseClass: CaseClass[XmlWriter, T]): XmlWriter[T] = value =>
    val elements =
      caseClass.params
        .filter(!_.annotations.contains(attributeAttribute))
        .map { p => p.typeclass.write(p.deref(value)).copy(name = XmlName(Txt(p.label))) }

    val attributes =
      caseClass.params
        .filter(_.annotations.contains(attributeAttribute))
        .map { p => XmlName(Txt(p.label)) -> textElements(p.typeclass.write(p.deref(value))) }
        .to(Map)

    Ast.Element(XmlName(Txt(caseClass.typeInfo.short)), elements, attributes)

  def split[T](sealedTrait: SealedTrait[XmlWriter, T]): XmlWriter[T] = value =>
    sealedTrait.choose(value) { subtype =>
      val xml = subtype.typeclass.write(subtype.cast(value))
      Ast.Element(
        XmlName(Txt(sealedTrait.typeInfo.short)),
        xml.children,
        xml.attributes.updated(XmlName(str"type"), xml.name.name),
        xml.namespaces
      )
    }
  
  private def textElements(value: Ast.Element): Txt =
    value.children.collect { case Ast.Textual(txt) => txt }.join

trait XmlWriter[T]:
  def write(value: T): Ast.Element
  def contramap[S](fn: S => T): XmlWriter[S] = value => write(fn(value))

