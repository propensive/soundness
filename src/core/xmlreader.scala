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

import rudiments.*
import wisteria.*
import anticipation.*
import spectacular.*
import contingency.*

import compiletime.*

trait XmlDecoder[ValueType]:
  def read(xml: List[XmlAst]): ValueType
  def map[ValueType2](lambda: ValueType => ValueType2): XmlDecoder[ValueType2] = list => lambda(read(list))

object XmlDecoder extends Derivation[XmlDecoder]:
  given text(using Raises[XmlReadError]): XmlDecoder[Text] = list =>
    val elements = childElements(list).collect { case XmlAst.Textual(text) => text }
    if elements.length == 0 then raise(XmlReadError())("".tt) else elements.head
  
  given [ValueType](using decoder: Decoder[ValueType]): XmlDecoder[ValueType] = value =>
    (value: @unchecked) match
      case XmlAst.Element(_, XmlAst.Textual(text) :: _, _, _) +: _ => text.decodeAs[ValueType]

  inline def join[DerivationType <: Product: ProductReflection]: XmlDecoder[DerivationType] = list =>
    val elements = childElements(list)

    construct:
      [FieldType] => context =>
        val element =
          elements.collect { case element: XmlAst.Element => element }
            .find(_.name.name == label)
            .get
          
        context.read(List(element))
  
  inline def split[DerivationType: SumReflection]: XmlDecoder[DerivationType] = list =>
    list.head match
      case XmlAst.Element(_, children, attributes, _) =>
        delegate(attributes.get(XmlName("type".tt)).get):
          [VariantType <: DerivationType] => decoder =>
            decoder.read(list)

  // def join[DerivationType](caseClass: CaseClass[XmlDecoder, DerivationType]): XmlDecoder[DerivationType] = seq =>
  //   val elems = childElements(seq)
    
  //   Some:
  //     caseClass.construct: param =>
  //       elems
  //         .collect { case e: XmlAst.Element => e }
  //         .find(_.name.name.s == param.label)
  //         .flatMap { e => param.typeclass.read(List(e)) }.get
  
  // def split[DerivationType](sealedTrait: SealedTrait[XmlDecoder, DerivationType]): XmlDecoder[DerivationType] = seq =>
  //   seq.headOption match
  //     case Some(XmlAst.Element(_, children, attributes, _)) =>
  //       attributes
  //         .get(XmlName(t"type"))
  //         .flatMap { t => sealedTrait.subtypes.find(_.typeInfo.short == t.s) }
  //         .flatMap(_.typeclass.read(seq))
  //     case _ =>
  //       None
  
  private def childElements(seq: List[XmlAst]): Seq[XmlAst] =
    seq.collect { case e@XmlAst.Element(_, children, _, _) => children }.flatten
