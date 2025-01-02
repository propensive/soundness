/*
    Punctuation, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package punctuation

import anticipation.*

import scala.quoted.*

object Punctuation:
  def md(context: Expr[StringContext], parts: Expr[Seq[Any]])(using Quotes): Expr[Markdown[Markdown.Ast.Node]] =
    Md.Interpolator.expansion(context, parts) match
      case (Md.Input.Inline(_), result) => '{$result.asInstanceOf[InlineMd]}
      case (Md.Input.Block(_), result)  => '{$result.asInstanceOf[Md]}
