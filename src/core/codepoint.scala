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
import fulminate.*

import scala.quoted.*

import language.experimental.captureChecking

object Codepoint:
  inline given Codepoint = ${Digression.location}

case class Codepoint(source: Text, line: Int):
  def text: Text = Text(s"${source.s.split("/").nn.last.nn}:$line")

given Realm = realm"digression"

object Digression:
  def location(using Quotes): Expr[Codepoint] =
    import quotes.*, reflect.*
    val path = Expr(Position.ofMacroExpansion.sourceFile.path)
    val line = Expr(Position.ofMacroExpansion.startLine + 1)

    '{Codepoint(Text($path), $line)}

  private val javaKeywords: Set[String] =
    Set
     ("abstract", "continue", "for", "new", "switch", "assert", "default", "if", "package", "synchronized",
      "boolean", "do", "goto", "private", "this", "break", "double", "implements", "protected", "throw",
      "byte", "else", "import", "public", "throws", "case", "enum", "instanceof", "return", "transient",
      "catch", "extends", "int", "short", "try", "char", "final", "interface", "static", "void", "class",
      "finally", "long", "strictfp", "volatile", "const", "float", "native", "super", "while")

  def fqcn(context: Expr[StringContext])(using Quotes): Expr[Fqcn] =
    import quotes.reflect.*
    val parts = IArray.from(context.valueOrAbort.parts.head.split("\\.").nn.map(_.nn))
    
    parts.foreach: part =>
      if part.length == 0 then fail(msg"a package name cannot be the empty string")
      if javaKeywords.has(part) then fail(msg"a package cannot be named $part, because it is a Java keyword")
      
      def valid(char: Char): Boolean =
        char >= 'A' && char <= 'Z' || char >= 'a' && char <= 'z' || char >= '0' && char <= '9' || char == '_'

      if !part.all(valid) then fail(msg"a package name may only contain the characters A-Z, a-z, 0-9 and _")

      if part.head >= '0' && part.head <= '9' then fail(msg"a package name cannot start with a digit")
      

    '{Fqcn(${Expr(parts.map(_.tt))})}

extension (inline context: StringContext)
  inline def fqcn(): Fqcn = ${Digression.fqcn('context)}
