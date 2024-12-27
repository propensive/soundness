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

object Renderable:
  given [ValueType: GenericHtmlRenderable] => ValueType is Renderable = new Renderable:
    type Self = ValueType
    type Result = Html[?]

    def html(value: ValueType): List[Html[?]] = ValueType.html(value).map(convert)

    private def convert(html: GenericHtml): Html[?] = html match
      case GenericHtml.Textual(text) =>
        text

      case GenericHtml.Node(label, attributes, children) =>
        val attributes2 = attributes.map(_.s -> _).to(Map)
        val children2 = children.map(convert(_))

        Node(label, attributes2, children2)

trait Renderable:
  type Self
  type Result <: Html[?]
  def html(value: Self): List[Result]
