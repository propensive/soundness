/*
    Harlequin, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package harlequin

import anticipation.*
import gossamer.*
import harlequin.*
import honeycomb.*, html5.*
import spectacular.*
import vacuous.*

trait CommonRenderer:
  def className(accent: Accent): List[CssClass] = List(CssClass(accent.show.lower))

  def element(accent: Accent, text: Text): Element["code"] =
    html5.Code(`class` = className(accent))(text)

  protected def postprocess(source: SourceCode): Seq[Html[Flow]] =
    val code = source.lines.map: line =>
      Span.line:
        line.map { case SourceToken(text, accent) => element(accent, text) }

    List(Div.amok(Pre(code)))
