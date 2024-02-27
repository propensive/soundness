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
import hieroglyph.*

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
  given GenericCssSelection[TagType[?, ?, ?]] = _.labelString.tt

case class TagType[+NameType <: Label, ChildType <: Label, AttributeType <: Label]
    (labelString: NameType, unclosed: Boolean = false, block: Boolean = true, verbatim: Boolean = false)
extends Node[NameType], Dynamic:
  def attributes: Attributes = Map()
  def children: Seq[Html[?]] = Nil
  def label: Text = labelString.show

  type ChildElements = ChildType

  inline def applyDynamicNamed(method: "apply")(inline attributes: (AttributeType, Any)*)
          : StartTag[NameType, ChildType] =

    ${Honeycomb.read[NameType, ChildType, ChildType]('labelString, 'unclosed, 'block, 'verbatim, 'attributes)}

  def applyDynamic(method: "apply")(children: (Html[ChildType] | Seq[Html[ChildType]])*): Element[NameType] =
    Element(labelString, unclosed, block, verbatim, Map(), children)

object ClearTagType:
  given GenericCssSelection[ClearTagType[?, ?, ?]] = _.labelString.tt

case class ClearTagType[+NameType <: Label, ChildType <: Label, AttributeType <: Label]
    (labelString: NameType, unclosed: Boolean = false, block: Boolean = true, verbatim: Boolean = false)
extends Node[NameType], Dynamic:

  def attributes: Attributes = Map()
  def children: Seq[Html[?]] = Nil
  def label: Text = labelString.show

  inline def applyDynamicNamed(method: "apply")(inline attributes: (AttributeType, Any)*)
          : StartTag[NameType, ChildType] =
    
    ${Honeycomb.read[NameType, ChildType, ChildType]('labelString, 'unclosed, 'block, 'verbatim, 'attributes)}

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

  lazy val block: Boolean = tagBlock || children.exists: child =>
    (child: @unchecked) match
      case node: Node[?] => node.block
      case _: Text       => false
      case _: Int        => false

case class HtmlDoc(root: Node["html"])

object HtmlDoc:
  given generic(using encoder: CharEncoder): GenericHttpResponseStream[HtmlDoc] =
    new GenericHttpResponseStream[HtmlDoc]:
      def mediaType: Text = t"text/html; charset=${encoder.encoding.name}"
      def content(value: HtmlDoc): LazyList[IArray[Byte]] = LazyList(HtmlDoc.serialize(value).bytes)

  def serialize[OutputType](doc: HtmlDoc, maxWidth: Int = -1)(using HtmlSerializer[OutputType]): OutputType =
    summon[HtmlSerializer[OutputType]].serialize(doc, maxWidth)
  
  def simple[Stylesheet](title: Text, stylesheet: Stylesheet = false)(content: (Html[Flow] | Seq[Html[Flow]])*)
      (using att: HtmlAttribute["href", Stylesheet, ?])
          : HtmlDoc =
    
    val link = (att.convert(stylesheet): @unchecked) match
      case Unset      => Nil
      case text: Text => Seq(Link(rel = Text("stylesheet"), href = text))

    HtmlDoc(Html(Head(Title(title), link), Body(content*)))
