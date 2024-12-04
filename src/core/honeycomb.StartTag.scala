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

object StartTag:
  given StartTag[?, ?] is GenericCssSelection = elem =>
    val tail = elem.attributes.map: (key, value) =>
      ((key, value): @unchecked) match
        case (key, value: Text) => t"[$key=$value]"
        case (key, Unset)       => t"[$key]"
    .join

    t"${elem.label}$tail"

case class StartTag[+NameType <: Label, ChildType <: Label]
   (labelString: NameType,
    unclosed:    Boolean,
    block:       Boolean,
    verbatim:    Boolean,
    attributes:  Attributes)
extends Node[NameType]:
  def children = Nil
  def label: Text = labelString.tt

  def apply(children: (Optional[Html[ChildType]] | Seq[Html[ChildType]])*): Element[NameType] =
    Element(labelString, unclosed, block, verbatim, attributes, children)
