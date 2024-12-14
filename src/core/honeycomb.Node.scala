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

object Node:
  given [HtmlType <: Html[?]] => HtmlType is Showable as html = html => (html: @unchecked) match
    case text: Text    => text
    case int: Int      => int.show
    case node: Node[?] => node.show

  given Seq[Html[?]] is Showable as seq = _.map(_.show).join

  given [NodeType <: Node[?]] => NodeType is Showable as node = item =>
    val filling =
      item.attributes.map: keyValue =>
        (keyValue: @unchecked) match
          case (key, Unset)       => t" $key"
          case (key, value: Text) => t""" $key="$value""""

      . join

    if item.children.isEmpty && !item.verbatim
    then t"<${item.label}$filling${if item.unclosed then t"" else t"/"}>"
    else t"<${item.label}$filling>${item.children.map(_.show).join}</${item.label}>"

trait Node[+NameType <: Label]:
  node =>
  def label: Text
  def attributes: Attributes
  def children: Seq[Html[?]]
  def block: Boolean
  def unclosed: Boolean
  def verbatim: Boolean
