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
import hieroglyph.*

import scala.quoted.*

import language.dynamics

object Element:
  @targetName("make")
  def apply
      [NodeType <: Label, ChildType <: Label]
      (labelString: String, unclosed: Boolean, block: Boolean, verbatim: Boolean,
          attributes: Attributes, children: Seq[Html[ChildType] | Seq[Html[ChildType]]] = Nil)
      : Element[NodeType] =
    new Element(labelString, unclosed, block, verbatim, attributes, flatten(children))

  private def flatten
      [ChildType <: Label]
      (nodes: Seq[Html[ChildType] | Seq[Html[ChildType]]])
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
  given GenericCssSelection[TagType[?, ?, ?]] = _.labelString

case class TagType[+Name <: Label, Children <: Label, Atts <: Label]
              (labelString: Name, unclosed: Boolean = false, block: Boolean = true,
                   verbatim: Boolean = false)
extends Node[Name], Dynamic:
  def attributes: Attributes = Map()
  def children: Seq[Html[?]] = Nil
  def label: Text = labelString.show

  type ChildElements = Children

  inline def applyDynamicNamed(method: "apply")
                              (inline attributes: (Atts, Any)*): StartTag[Name, Children] =
    ${Honeycomb.read[Name, Children, Children]('labelString, 'unclosed, 'block, 'verbatim,
        'attributes)}

  def applyDynamic(method: "apply")
                  (children: (Html[Children] | Seq[Html[Children]])*): Element[Name] =
    Element(labelString, unclosed, block, verbatim, Map(), children)

object TransTagType:
  given GenericCssSelection[TransTagType[?, ?, ?]] = _.labelString

case class TransTagType
    [+Name <: Label, Children <: Label, Atts <: Label]
    (labelString: Name, unclosed: Boolean = false, block: Boolean = true,
        verbatim: Boolean = false)
extends Node[Name], Dynamic:

  def attributes: Attributes = Map()
  def children: Seq[Html[?]] = Nil
  def label: Text = labelString.show

  inline def applyDynamicNamed
      (method: "apply")
      (inline attributes: (Atts, Any)*)
      : StartTag[Name, Children] =
    
    ${Honeycomb.read[Name, Children, Children]('labelString, 'unclosed, 'block, 'verbatim,
        'attributes)}

  def applyDynamic[Return <: Label]
                  (method: "apply")
                  (children: (Html[Return] | Seq[Html[Return]])*)
                  : Element[Return] =
    Element(labelString, unclosed, block, verbatim, Map(), children)

case class Element
    [+Name <: Label]
    (labelString: String, unclosed: Boolean, tagBlock: Boolean, verbatim: Boolean,
        attributes: Map[String, Maybe[Text]], children: Seq[Html[?]]) extends Node[Name]:

  def label: Text = labelString.show

  lazy val block: Boolean = tagBlock || children.exists:
    case item: Node[?] => item.block
    case _: Text       => false
    case _: Int        => false
    case _             => throw Mistake("should never match")

case class HtmlDoc(root: Node["html"])

object HtmlDoc:
  given generic(using encoder: CharEncoder): GenericHttpResponseStream[HtmlDoc] =
    new GenericHttpResponseStream[HtmlDoc]:
      def mediaType: String = t"text/html; charset=${encoder.encoding.name}".s
      
      def content(value: HtmlDoc): LazyList[IArray[Byte]] =
        LazyList(HtmlDoc.serialize(value).bytes)

  def serialize[T](doc: HtmlDoc, maxWidth: Int = -1)(using HtmlSerializer[T]): T =
    summon[HtmlSerializer[T]].serialize(doc, maxWidth)
  
  def simple
      [Stylesheet]
      (title: Text, stylesheet: Stylesheet = false)
      (content: (Html[Flow] | Seq[Html[Flow]])*)
      (using att: HtmlAttribute["href", Stylesheet, ?]): HtmlDoc =
    
    val link = att.convert(stylesheet) match
      case Unset      => Nil
      case text: Text => Seq(Link(rel = Text("stylesheet"), href = text))
      case _          => throw Mistake("should never match")

    HtmlDoc(Html(Head(Title(title), link), Body(content*)))
