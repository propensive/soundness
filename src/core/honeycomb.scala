/*
    Honeycomb, version 0.2.0. Copyright 2018-21 Jon Pretty, Propensive OÜ.

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

import scala.annotation.*
import scala.quoted.*

import language.dynamics

type Label = String & Singleton
type Content[Children <: Label] = Item[Children] | String | Int
type Attributes = Map[String, String | Boolean]

trait Item[+Name <: Label]:
  def label: String
  def attributes: Attributes
  def children: Seq[Content[?]]
  def inline: Boolean
  def unclosed: Boolean
  def verbatim: Boolean

object Node:
  @targetName("make")
  def apply[T <: Label, C <: Label]
           (label: String, unclosed: Boolean, inline: Boolean,
                verbatim: Boolean, attributes: Attributes,
                children: Seq[Content[C] | Seq[Content[C]]] = Nil): Node[T] =
    new Node(label, unclosed, inline, verbatim, attributes, flatten(children))

  private def flatten[C <: Label](nodes: Seq[Content[C] | Seq[Content[C]]]): Seq[Content[C]] =
    nodes.flatMap {
      case seq: Seq[?]      => seq
      case node: Content[?] => Seq(node)
    }.asInstanceOf[Seq[Content[C]]]

extension [T](value: T)
  def html[R <: Label](using ToHtml[T, R]): Seq[Content[R]] = summon[ToHtml[T, R]].convert(value)

object Html extends Item["html"]:
  def label = "html"
  def attributes: Attributes = Map()
  def children: Seq[Content[?]] = Nil
  def inline = false
  def unclosed = false
  def verbatim = false

  def apply(head: Item["head"], body: Item["body"]): Node["html"] =
    Node(label, unclosed, inline, verbatim, Map(), Seq(head, body))

case class Tag[+Name <: Label, Children <: Label, Atts <: Label]
              (label: Name, unclosed: Boolean = false, inline: Boolean = false,
                   verbatim: Boolean = false)
extends Item[Name], Dynamic:
  def attributes: Attributes = Map()
  def children: Seq[Content[?]] = Nil

  type ChildNodes = Children

  inline def applyDynamicNamed(method: "apply")
                              (inline attributes: (Atts, Any)*): Element[Name, Children] =
    ${Macro.read[Name, Children, Children]('label, 'unclosed, 'inline, 'verbatim, 'attributes)}

  def applyDynamic(method: "apply")
                  (children: (Content[Children] | Seq[Content[Children]])*): Node[Name] =
    Node(label, unclosed, inline, verbatim, Map(), children)

case class TransTag[+Name <: Label, Children <: Label, Atts <: Label]
                   (label: Name, unclosed: Boolean = false, inline: Boolean = false,
                        verbatim: Boolean = false)
extends Item[Name], Dynamic:
  def attributes: Attributes = Map()
  def children: Seq[Content[?]] = Nil

  inline def applyDynamicNamed(method: "apply")
                              (inline attributes: (Atts, Any)*): Element[Name, Children] =
    ${Macro.read[Name, Children, Children]('label, 'unclosed, 'inline, 'verbatim, 'attributes)}

  def applyDynamic[Return <: Label]
                  (method: "apply")
                  (children: (Content[Return] | Seq[Content[Return]])*): Node[Return] =
    Node(label, unclosed, inline, verbatim, Map(), children)

case class Element[+Name <: Label, Children <: Label]
                  (label: Name, unclosed: Boolean, inline: Boolean, verbatim: Boolean,
                       attributes: Attributes)
extends Item[Name]:
  def children = Nil

  def apply(children: (Content[Children] | Seq[Content[Children]])*): Node[Name] =
    Node(label, unclosed, inline, verbatim, attributes, children)

case class Node[+Name <: Label](label: String, unclosed: Boolean, tagInline: Boolean,
                                    verbatim: Boolean, attributes: Attributes,
                                    children: Seq[Content[?]]) extends Item[Name]:

  lazy val inline: Boolean = tagInline && children.forall {
    case item: Item[?] => item.inline
    case text: String  => true
    case int: Int      => true
  }

case class HtmlDoc(root: Item["html"])

object HtmlDoc:
  def serialize[T](doc: HtmlDoc, maxWidth: Int = -1)(using HtmlSerializer[T]): T =
    summon[HtmlSerializer[T]].serialize(doc, maxWidth)
  
  def simple[Stylesheet](title: String, stylesheet: Stylesheet = false)
                        (content: (Content[Flow] | Seq[Content[Flow]])*)
                        (using att: Attribute["href", Stylesheet, ?]): HtmlDoc =
    import attributes.strings.given

    val link = att.convert(stylesheet) match
      case boolean: Boolean => Nil
      case string: String   => Seq(Link(rel = "stylesheet", href = string))

    HtmlDoc(Html(Head(Title(title), link), Body(content*)))
