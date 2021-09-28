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

trait XmlReader[T]:
  def read(xml: Seq[Ast]): Option[T]
  def map[S](fn: T => Option[S]): XmlReader[S] = read(_).flatMap(fn(_))

object XmlReader extends Derivation[XmlReader]:
  given string: XmlReader[String] =
    childElements(_).collect { case Ast.Text(txt) => txt }.headOption
  
  given XmlReader[Int] = string.map(Int.unapply(_))
  
  def join[T](caseClass: CaseClass[XmlReader, T]): XmlReader[T] = seq =>
    val elems = childElements(seq)
    caseClass.constructMonadic { param =>
      elems
        .collect { case e: Ast.Element => e }
        .find(_.name.name == param.label)
        .flatMap { e => param.typeclass.read(Seq(e)) }
    }
  
  def split[T](sealedTrait: SealedTrait[XmlReader, T]): XmlReader[T] = seq =>
    seq.headOption match
      case Some(Ast.Element(_, children, attributes, _)) =>
        attributes
          .get(XmlName("type"))
          .flatMap { t => sealedTrait.subtypes.find(_.typeInfo.short == t) }
          .flatMap(_.typeclass.read(seq))
      case _ =>
        None
  
  private def childElements(seq: Seq[Ast]): Seq[Ast] =
    seq.collect { case e@Ast.Element(_, children, _, _) => children }.flatten
