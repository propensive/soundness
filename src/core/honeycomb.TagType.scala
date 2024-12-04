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
import anticipation.*

import scala.quoted.*

import language.dynamics

object TagType:
  given TagType[?, ?, ?] is GenericCssSelection = _.labelString.tt

open case class TagType[+NameType <: Label, ChildType <: Label, AttributeType <: Label]
   (labelString: NameType,
    unclosed:    Boolean  = false,
    block:       Boolean  = true,
    verbatim:    Boolean  = false)
extends Node[NameType], Dynamic:

  def attributes: Attributes = Map()
  def children: Seq[Html[?]] = Nil
  def label: Text = labelString.tt

  def preset(presetAttributes: (String, Text)*): TagType[NameType, ChildType, AttributeType] =
    new TagType[NameType, ChildType, AttributeType](labelString, unclosed, block, verbatim):
      override def attributes: Attributes = presetAttributes.to(Map)

  type Content = ChildType

  inline def applyDynamicNamed(method: String)(inline attributes: (AttributeType, Any)*)
          : StartTag[NameType, ChildType] =

    ${  Honeycomb.read[NameType, ChildType, ChildType]('this, 'method, 'labelString, 'attributes)  }

  def applyDynamic(method: String)(children: (Optional[Html[ChildType]] | Seq[Html[ChildType]])*)
          : Element[NameType] =
    method match
      case "apply" =>
        Element(labelString, unclosed, block, verbatim, attributes, children)

      case className =>
        Element
         (labelString,
          unclosed,
          block,
          verbatim,
          attributes.updated("class", className.tt),
          children)
