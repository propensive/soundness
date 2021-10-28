/*
    Honeycomb, version 0.9.0. Copyright 2018-21 Jon Pretty, Propensive OÜ.

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

import scala.annotation.*
import scala.quoted.*

import language.dynamics

type Label = String & Singleton
type Content[Children <: Label] = Item[Children] | Txt | Int
type Attributes = Map[String, Maybe[Txt]]

extension (content: Content[?])
  def text: Txt = content match
    case item: Item[?] => item.text
    case txt: Txt      => txt
    case int: Int      => int.show
    case _             => throw Impossible("should never match")

trait Item[+Name <: Label]:
  def label: Txt
  def attributes: Attributes
  def children: Seq[Content[?]]
  def inline: Boolean
  def unclosed: Boolean
  def verbatim: Boolean
  
  def text: Txt =
    val attributeString = attributes.map {
      case (key, Unset)      => str" $key"
      case (key, value: Txt) => str""" $key="${value}""""
      case _                 => throw Impossible("should never match")
    }.join
    
    if children.isEmpty then str"<$label$attributeString/>"
    else str"<$label$attributeString>${children.map(_.text).join}</$label>"

  override def toString: String = text.s

object Node:
  @targetName("make")
  def apply[T <: Label, C <: Label]
           (labelString: String, unclosed: Boolean, inline: Boolean,
                verbatim: Boolean, attributes: Attributes,
                children: Seq[Content[C] | Seq[Content[C]]] = Nil): Node[T] =
    new Node(labelString, unclosed, inline, verbatim, attributes, flatten(children))

  private def flatten[C <: Label](nodes: Seq[Content[C] | Seq[Content[C]]]): Seq[Content[C]] =
    nodes.flatMap {
      case seq: Seq[?] => seq
      case node        => Seq(node)
    }.asInstanceOf[Seq[Content[C]]]

object Html extends Item["html"]:
  def label: Txt = str"html"
  def attributes: Attributes = Map()
  def children: Seq[Content[?]] = Nil
  def inline = false
  def unclosed = false
  def verbatim = false

  def apply(head: Item["head"], body: Item["body"]): Node["html"] =
    Node(label.s, unclosed, inline, verbatim, Map(), Seq(head, body))

object Tag:
  given clairvoyant.CssSelection[Tag[?, ?, ?]] = _.labelString

case class Tag[+Name <: Label, Children <: Label, Atts <: Label]
              (labelString: Name, unclosed: Boolean = false, inline: Boolean = false,
                   verbatim: Boolean = false)
extends Item[Name], Dynamic:
  def attributes: Attributes = Map()
  def children: Seq[Content[?]] = Nil
  def label: Txt = Txt(labelString)

  type ChildNodes = Children

  inline def applyDynamicNamed(method: "apply")
                              (inline attributes: (Atts, Any)*): Element[Name, Children] =
    ${Macro.read[Name, Children, Children]('labelString, 'unclosed, 'inline, 'verbatim, 'attributes)}

  def applyDynamic(method: "apply")
                  (children: (Content[Children] | Seq[Content[Children]])*): Node[Name] =
    Node(labelString, unclosed, inline, verbatim, Map(), children)

object TransTag:
  given clairvoyant.CssSelection[TransTag[?, ?, ?]] = _.labelString

case class TransTag[+Name <: Label, Children <: Label, Atts <: Label]
                   (labelString: Name, unclosed: Boolean = false, inline: Boolean = false,
                        verbatim: Boolean = false)
extends Item[Name], Dynamic:
  def attributes: Attributes = Map()
  def children: Seq[Content[?]] = Nil
  def label: Txt = Txt(labelString)

  inline def applyDynamicNamed(method: "apply")
                              (inline attributes: (Atts, Any)*): Element[Name, Children] =
    ${Macro.read[Name, Children, Children]('labelString, 'unclosed, 'inline, 'verbatim, 'attributes)}

  def applyDynamic[Return <: Label]
                  (method: "apply")
                  (children: (Content[Return] | Seq[Content[Return]])*): Node[Return] =
    Node(labelString, unclosed, inline, verbatim, Map(), children)

object Element:
  given clairvoyant.CssSelection[Element[?, ?]] = elem => elem.label+elem.attributes.map {
    case (key, value: Txt) => str"[$key=$value]"
    case (key, Unset)      => str"[$key]"
    case _                 => throw Impossible("should never match")
  }.join(str"")

case class Element[+Name <: Label, Children <: Label]
                  (labelString: Name, unclosed: Boolean, inline: Boolean, verbatim: Boolean,
                       attributes: Attributes)
extends Item[Name]:
  def children = Nil
  def label: Txt = Txt(labelString)
  def apply(children: (Content[Children] | Seq[Content[Children]])*): Node[Name] =
    Node(labelString, unclosed, inline, verbatim, attributes, children)

case class Node[+Name <: Label](labelString: String, unclosed: Boolean, tagInline: Boolean,
                                    verbatim: Boolean, attributes: Map[String, Maybe[Txt]],
                                    children: Seq[Content[?]]) extends Item[Name]:

  def label: Txt = Txt(labelString)

  lazy val inline: Boolean = tagInline && children.forall {
    case item: Item[?] => item.inline
    case text: Txt     => true
    case int: Int      => true
    case _             => throw Impossible("should never match")
  }

case class HtmlDoc(root: Item["html"])

object HtmlDoc:
  given clairvoyant.HttpResponse[HtmlDoc, String] with
    def mimeType: String = "text/html; charset=utf-8"
    def content(value: HtmlDoc): String = HtmlDoc.serialize(value).s

  def serialize[T](doc: HtmlDoc, maxWidth: Int = -1)(using HtmlSerializer[T]): T =
    summon[HtmlSerializer[T]].serialize(doc, maxWidth)
  
  def simple[Stylesheet](title: Txt, stylesheet: Stylesheet = false)
                        (content: (Content[Flow] | Seq[Content[Flow]])*)
                        (using att: Attribute["href", Stylesheet, ?]): HtmlDoc =
    val link = att.convert(stylesheet) match
      case Unset     => Nil
      case text: Txt => Seq(Link(rel = str"stylesheet", href = text))
      case _         => throw Impossible("should never match")

    HtmlDoc(Html(Head(Title(title), link), Body(content*)))
