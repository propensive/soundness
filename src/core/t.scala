/*
    Gossamer, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package gossamer

import anticipation.*
import spectacular.*

import language.experimental.captureChecking

case class SimpleTExtractor(text: Text):
  def unapply(scrutinee: Text): Boolean = text == scrutinee

extension (inline ctx: StringContext)
  transparent inline def txt(inline parts: Any*): Text = ${Interpolation.Text.expand('ctx, 'parts)}
  transparent inline def t(inline parts: Any*): Text = ${Interpolation.T.expand('ctx, 'parts)}

extension (ctx: StringContext)
  def t = SimpleTExtractor(ctx.parts.head.show)
