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
import gossamer.*
import anticipation.*
import spectacular.*

import scala.quoted.*

import language.dynamics

object Element:
  @targetName("make")
  def apply[NodeType <: Label, ChildType <: Label]
     (labelString: String,
      unclosed:    Boolean,
      block:       Boolean,
      verbatim:    Boolean,
      attributes:  Attributes,
      children:    Seq[Html[ChildType] | Seq[Html[ChildType]]] = Nil)
          : Element[NodeType] =

    new Element(labelString, unclosed, block, verbatim, attributes, flatten(children))

  private def flatten[ChildType <: Label](nodes: Seq[Html[ChildType] | Seq[Html[ChildType]]])
          : Seq[Html[ChildType]] =

    nodes.flatMap:
      case seq: Seq[Html[ChildType] @unchecked] => seq
      case node: Html[ChildType] @unchecked     => Seq(node)

object Html extends Node["html"]:
  def label: Text = t"html"
  def attributes: Attributes = Map()
  def children: Seq[Html[?]] = Nil
  def block: Boolean = true
  def unclosed: Boolean = false
  def verbatim: Boolean = false

  def apply(head: Node["head"], body: Node["body"]): Element["html"] =
    Element(label.s, unclosed, block, verbatim, Map(), Seq(head, body))

object TagType:
  given TagType[?, ?, ?] is GenericCssSelection = _.labelString.tt

open case class TagType[+NameType <: Label, ChildType <: Label, AttributeType <: Label]
   (labelString: NameType, unclosed: Boolean = false, block: Boolean = true, verbatim: Boolean = false)
extends Node[NameType], Dynamic:
  def attributes: Attributes = Map()
  def children: Seq[Html[?]] = Nil
  def label: Text = labelString.tt

  type Content = ChildType

  inline def applyDynamicNamed(method: String)(inline attributes: (AttributeType, Any)*)
          : StartTag[NameType, ChildType] =

    ${Honeycomb.read[NameType, ChildType, ChildType]('method, 'labelString, 'unclosed, 'block, 'verbatim, 'attributes)}

  def applyDynamic(method: String)(children: (Html[ChildType] | Seq[Html[ChildType]])*): Element[NameType] =
    method match
      case "apply" =>
        Element(labelString, unclosed, block, verbatim, Map(), children)

      case className =>
        Element(labelString, unclosed, block, verbatim, Map("class" -> className.tt), children)

object ClearTagType:
  given ClearTagType[?, ?, ?] is GenericCssSelection = _.labelString.tt

case class ClearTagType[+NameType <: Label, ChildType <: Label, AttributeType <: Label]
   (labelString: NameType, unclosed: Boolean = false, block: Boolean = true, verbatim: Boolean = false)
extends Node[NameType], Dynamic:

  def attributes: Attributes = Map()
  def children: Seq[Html[?]] = Nil
  def label: Text = labelString.tt

  inline def applyDynamicNamed(method: String)(inline attributes: (AttributeType, Any)*)
          : StartTag[NameType, ChildType] =

    ${Honeycomb.read[NameType, ChildType, ChildType]('method, 'labelString, 'unclosed, 'block, 'verbatim, 'attributes)}

  def applyDynamic[Return <: Label](method: "apply")(children: (Html[Return] | Seq[Html[Return]])*)
          : Element[Return] =

    Element(labelString, unclosed, block, verbatim, Map(), children)

case class Element[+NameType <: Label]
   (labelString: String,
    unclosed:    Boolean,
    tagBlock:    Boolean,
    verbatim:    Boolean,
    attributes:  Map[String, Optional[Text]],
    children:    Seq[Html[?]])
extends Node[NameType]:

  def label: Text = labelString.show

  val block: Boolean = tagBlock || children.exists: child =>
    (child: @unchecked) match
      case node: Node[?] => node.block
      case _: Text       => false
      case _: Int        => false
