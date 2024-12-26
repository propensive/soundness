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

import anticipation.*
import rudiments.*
import vacuous.*

import scala.quoted.*

import language.dynamics

object ClearTag:
  given ClearTag[?, ?, ?] is GenericCssSelection = _.labelString.tt

case class ClearTag[+NameType <: Label, ChildType <: Label, AttributeType <: Label]
   (labelString: NameType,
    unclosed:    Boolean = false,
    block:       Boolean = true,
    verbatim:    Boolean = false)
extends Node[NameType], Dynamic:

  def attributes: Attributes = Map()
  def children: Seq[Html[?]] = Nil
  def label: Text = labelString.tt

  inline def applyDynamicNamed(method: String)(inline attributes: (AttributeType, Any)*)
          : StartTag[NameType, ChildType] =

    ${  Honeycomb.read[NameType, ChildType, ChildType]('this, 'method, 'labelString, 'attributes)  }

  def applyDynamic[Return <: Label](method: "apply")
     (children: (Optional[Html[Return]] | Seq[Html[Return]])*)
          : Element[Return] =

    Element(labelString, unclosed, block, verbatim, Map(), children)
