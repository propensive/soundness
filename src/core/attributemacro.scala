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

import scala.quoted.*

type Label = String & Singleton
type Attributes = Map[String, Unset.type | Text]
type Html[Children <: Label] = Node[Children] | Text | Int

object Node:
  given Show[Html[?]] =
    case text: Text    => text
    case int: Int      => int.show
    case item: Node[?] => item.show
    case _             => throw Mistake("this should never match")
  
  given Show[Seq[Html[?]]] = _.map(_.show).join
  
  given Show[Node[?]] = item =>
    val filling = item.attributes.map:
      case (key, Unset)       => t" $key"
      case (key, value: Text) => t""" $key="${value}""""
      case _                  => throw Mistake("should never match")
    .join
    
    if item.children.isEmpty && !item.verbatim
    then t"<${item.label}$filling${if item.unclosed then t"" else t"/"}>"
    else t"<${item.label}$filling>${item.children.map(_.show).join}</${item.label}>"

trait Node[+Name <: Label] extends Shown[Node[?]]:
  node =>
    def label: Text
    def attributes: Attributes
    def children: Seq[Html[?]]
    def inline: Boolean
    def unclosed: Boolean
    def verbatim: Boolean
  
    inline def refine[N <: Label]: Option[Node[N]] = label.s match
      case lbl: N => Some:
        new Node[N]:
          def label: Text = lbl.show
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


case class StartTag[+Name <: Label, Children <: Label]
                   (labelString: Name, unclosed: Boolean, inline: Boolean, verbatim: Boolean,
                        attributes: Attributes)
extends Node[Name]:
  def children = Nil
  def label: Text = labelString.show
  def apply(children: (Html[Children] | Seq[Html[Children]])*): Element[Name] =
    Element(labelString, unclosed, inline, verbatim, attributes, children)

object HoneycombMacros:
  def read[Name <: Label: Type, Children <: Label: Type, Return <: Label: Type]
          (name: Expr[Name], unclosed: Expr[Boolean], inline: Expr[Boolean],
               verbatim: Expr[Boolean], attributes: Expr[Seq[(Label, Any)]])
          (using Quotes)
          : Expr[StartTag[Name, Return]] =
    import quotes.reflect.{Singleton as _, *}

    def recur(exprs: Seq[Expr[(Label, Any)]]): List[Expr[(String, Maybe[Text])]] = exprs match
      case '{($key: k & Label, $value: v)} +: tail =>
        val att = key.value.get
        val exp: Expr[HtmlAttribute[k & Label, v, Name]] =
          Expr.summon[HtmlAttribute[k & Label, v, Name]].getOrElse:
            val typeName = TypeRepr.of[v].show
            fail(t"""the attribute $att cannot take a value of type $typeName""".s)
        
        '{($exp.rename.getOrElse(Text($key)).s, $exp.convert($value))} :: recur(tail)
      
      case _ =>
        Nil

    attributes match
      case Varargs(exprs) =>
        '{StartTag($name, $unclosed, $inline, $verbatim, ${Expr.ofSeq(recur(exprs))}.to(Map))}
      
      case _ =>
        throw Mistake("expected varargs")
