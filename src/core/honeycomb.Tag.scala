/*
    Honeycomb, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

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

import anticipation.*
import rudiments.*
import vacuous.*

import scala.quoted.*

import language.dynamics

object Tag:
  given Tag[?, ?, ?] is GenericCssSelection = _.labelString.tt

open case class Tag[+NameType <: Label, ChildType <: Label, AttributeType <: Label]
   (labelString: NameType)
extends Node[NameType], Dynamic:

  def attributes: Attributes = Map()
  def children: Seq[Html[?]] = Nil
  def label: Text = labelString.tt

  def preset(presetAttributes: (String, Text)*): Tag[NameType, ChildType, AttributeType] =
    new Tag[NameType, ChildType, AttributeType](labelString):
      override def attributes: Attributes = presetAttributes.to(Map)

  type Content = ChildType

  inline def applyDynamicNamed(method: String)(inline attributes: (AttributeType, Any)*)
          : StartTag[NameType, ChildType] =

    ${  Honeycomb.read[NameType, ChildType, ChildType]('this, 'method, 'labelString, 'attributes)  }

  def applyDynamic(method: String)(children: (Optional[Html[ChildType]] | Seq[Html[ChildType]])*)
          : Element[NameType] =
    method match
      case "apply"   => Element(labelString, attributes, children)
      case className => Element(labelString, attributes.updated("class", className.tt), children)
