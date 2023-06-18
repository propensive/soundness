/*
    Honeycomb, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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
import gossamer.*
import anticipation.*
import spectacular.*

import scala.quoted.*

type Attributes = Map[String, Unset.type | Text]
type Html[ChildType <: Label] = Node[ChildType] | Text | Int

object Node:
  given Show[Html[?]] = html => (html: @unchecked) match
    case text: Text    => text
    case int: Int      => int.show
    case node: Node[?] => node.show
  
  given Show[Seq[Html[?]]] = _.map(_.show).join
  
  given Show[Node[?]] = item =>
    val filling = item.attributes.map: keyValue =>
      (keyValue: @unchecked) match
        case (key, Unset)       => t" $key"
        case (key, value: Text) => t""" $key="$value""""
    .join
    
    if item.children.isEmpty && !item.verbatim
    then t"<${item.label}$filling${if item.unclosed then t"" else t"/"}>"
    else t"<${item.label}$filling>${item.children.map(_.show).join}</${item.label}>"

trait Node[+NameType <: Label] extends Shown[Node[?]]:
  node =>
  def label: Text
  def attributes: Attributes
  def children: Seq[Html[?]]
  def inline: Boolean
  def unclosed: Boolean
  def verbatim: Boolean

  inline def refine[NameType <: Label]: Option[Node[NameType]] = label.s match
    case labelValue: NameType => Some:
      new Node[NameType]:
        def label: Text = labelValue.show
        export node.{attributes, children, inline, unclosed, verbatim}
    case _ => None

object StartTag:
  given GenericCssSelection[StartTag[?, ?]] = elem =>
    val tail = elem.attributes.map:
      case (key, value: Text) => t"[$key=$value]"
      case (key, Unset)       => t"[$key]"
      case _                  => throw Mistake("should never match")
    .join
    
    t"${elem.label}$tail".s


case class StartTag[+NameType <: Label, ChildType <: Label]
                   (labelString: NameType, unclosed: Boolean, inline: Boolean, verbatim: Boolean,
                        attributes: Attributes)
extends Node[NameType]:
  def children = Nil
  def label: Text = labelString.show
  def apply(children: (Html[ChildType] | Seq[Html[ChildType]])*): Element[NameType] =
    Element(labelString, unclosed, inline, verbatim, attributes, children)

object HoneycombMacros:
  def read[NameType <: Label: Type, ChildType <: Label: Type, ReturnType <: Label: Type]
          (name: Expr[NameType], unclosed: Expr[Boolean], inline: Expr[Boolean],
               verbatim: Expr[Boolean], attributes: Expr[Seq[(Label, Any)]])
          (using Quotes)
          : Expr[StartTag[NameType, ReturnType]] =
    import quotes.reflect.{Singleton as _, *}

    def recur(exprs: Seq[Expr[(Label, Any)]]): List[Expr[(String, Maybe[Text])]] = exprs match
      case '{($key: keyType & Label, $value: valueType)} +: tail =>
        val att = key.value.get
        val expr: Expr[HtmlAttribute[keyType & Label, valueType, NameType]] =
          Expr.summon[HtmlAttribute[keyType & Label, valueType, NameType]].getOrElse:
            val typeName = TypeRepr.of[valueType].show
            fail(t"""the attribute $att cannot take a value of type $typeName""".s)
        
        '{($expr.rename.getOrElse(Text($key)).s, $expr.convert($value))} :: recur(tail)
      
      case _ =>
        Nil

    attributes match
      case Varargs(exprs) =>
        '{StartTag($name, $unclosed, $inline, $verbatim, ${Expr.ofSeq(recur(exprs))}.to(Map))}
      
      case _ =>
        throw Mistake("expected varargs")
