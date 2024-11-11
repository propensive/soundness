/*
    Honeycomb, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package honeycomb

import rudiments.*
import vacuous.*
import fulminate.*
import gossamer.*
import anticipation.*
import spectacular.*
import xylophone.*

import scala.quoted.*

type Attributes = Map[String, Unset.type | Text]
type Html[+ChildType <: Label] = Node[ChildType] | Text | Int | HtmlXml

case class HtmlXml(xml: XmlDoc)

object Node:
  given [HtmlType <: Html[?]] => HtmlType is Showable as html = html => (html: @unchecked) match
    case text: Text    => text
    case int: Int      => int.show
    case node: Node[?] => node.show

  given Seq[Html[?]] is Showable as seq = _.map(_.show).join

  given [NodeType <: Node[?]] => NodeType is Showable as node = item =>
    val filling =
      item.attributes.map: keyValue =>
        (keyValue: @unchecked) match
          case (key, Unset)       => t" $key"
          case (key, value: Text) => t""" $key="$value""""
      .join

    if item.children.isEmpty && !item.verbatim
    then t"<${item.label}$filling${if item.unclosed then t"" else t"/"}>"
    else t"<${item.label}$filling>${item.children.map(_.show).join}</${item.label}>"

trait Node[+NameType <: Label]:
  node =>
  def label: Text
  def attributes: Attributes
  def children: Seq[Html[?]]
  def block: Boolean
  def unclosed: Boolean
  def verbatim: Boolean

object StartTag:
  given StartTag[?, ?] is GenericCssSelection = elem =>
    val tail = elem.attributes.map: (key, value) =>
      ((key, value): @unchecked) match
        case (key, value: Text) => t"[$key=$value]"
        case (key, Unset)       => t"[$key]"
    .join

    t"${elem.label}$tail"

case class StartTag[+NameType <: Label, ChildType <: Label]
   (labelString: NameType,
    unclosed:    Boolean,
    block:       Boolean,
    verbatim:    Boolean,
    attributes:  Attributes)
extends Node[NameType]:
  def children = Nil
  def label: Text = labelString.tt

  def apply(children: (Html[ChildType] | Seq[Html[ChildType]])*): Element[NameType] =
    Element(labelString, unclosed, block, verbatim, attributes, children)

object Honeycomb:
  given Realm = realm"honeycomb"

  def read[NameType <: Label: Type, ChildType <: Label: Type, ReturnType <: Label: Type]
     (className:  Expr[String],
      name:       Expr[NameType],
      unclosed:   Expr[Boolean],
      block:      Expr[Boolean],
      verbatim:   Expr[Boolean],
      attributes: Expr[Seq[(Label, Any)]])
     (using Quotes)
          : Expr[StartTag[NameType, ReturnType]] =

    import quotes.reflect.*

    def recur(exprs: Seq[Expr[(Label, Any)]]): List[Expr[(String, Optional[Text])]] = exprs match
      case '{type keyType <: Label; ($key: keyType, $value: valueType)} +: tail =>
        val att: String = key.value.get
        val expr: Expr[HtmlAttribute[keyType, valueType, NameType]] =
          Expr.summon[HtmlAttribute[keyType, valueType, NameType]].getOrElse:
            val typeName = TypeRepr.of[valueType].show
            abandon(m"""the attribute $att cannot take a value of type $typeName""")

        '{($expr.rename.getOrElse(Text($key)).s, $expr.convert($value))} :: recur(tail)

      case _ =>
        if className.value == Some("apply") then Nil
        else List('{("class", $className.tt)})

    (attributes: @unchecked) match
      case Varargs(exprs) =>
        '{StartTag($name, $unclosed, $block, $verbatim, ${Expr.ofSeq(recur(exprs))}.to(Map))}
