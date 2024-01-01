/*
    Digression, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package digression

import rudiments.*
import anticipation.*

import scala.quoted.*

import language.experimental.captureChecking

object Codepoint:
  inline given Codepoint = ${Digression.location}

case class Codepoint(source: Text, line: Int):
  def text: Text = Text(s"${source.s.split("/").nn.last.nn}:$line")

object Digression:
  def location(using Quotes): Expr[Codepoint] =
    import quotes.*, reflect.*
    val path = Expr(Position.ofMacroExpansion.sourceFile.path)
    val line = Expr(Position.ofMacroExpansion.startLine + 1)

    '{Codepoint(Text($path), $line)}
