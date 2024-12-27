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

import anticipation.*
import contingency.*
import rudiments.*

import scala.quoted.*

import language.experimental.pureFunctions

object Digression:
  def location(using Quotes): Expr[Codepoint] =
    import quotes.*, reflect.*
    val path = Expr(Position.ofMacroExpansion.sourceFile.path)
    val line = Expr(Position.ofMacroExpansion.startLine + 1)

    '{Codepoint(Text($path), $line)}

  val javaKeywords: Set[String] =
    Set
     ("abstract", "continue", "for", "new", "switch", "assert", "default", "if", "package", "synchronized",
      "boolean", "do", "goto", "private", "this", "break", "double", "implements", "protected", "throw",
      "byte", "else", "import", "public", "throws", "case", "enum", "instanceof", "return", "transient",
      "catch", "extends", "int", "short", "try", "char", "final", "interface", "static", "void", "class",
      "finally", "long", "strictfp", "volatile", "const", "float", "native", "super", "while")

  def fqcn(context: Expr[StringContext])(using Quotes): Expr[Fqcn] =
    haltingly('{new Fqcn(${Expr(Fqcn(context.valueOrAbort.parts.head.tt).parts)})})
