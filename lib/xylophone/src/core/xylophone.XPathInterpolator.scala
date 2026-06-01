                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.54.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package xylophone

import scala.quoted.*

import anticipation.*
import contextual.*
import contingency.*
import distillate.*
import fulminate.*
import gigantism.*
import rudiments.*
import vacuous.*

object XPathInterpolator:
  // Reuses `XPath`'s own `Decodable` for validation: the literal is decoded at
  // macro-expansion time and, if it fails, the `XPathError`'s offset is mapped
  // back to a source position so the error points exactly at the offending
  // character. Mirrors `jacinta.JsonPointerInterpolator`.
  def expand[parts <: Tuple: Type, origins <: Tuple: Type](insertions: Expr[Seq[Any]])
  :   Macro[XPath] =

    import quotes.reflect.*

    def recur[tuple: Type](strings: List[String]): List[String] = Type.of[tuple] match
      case '[head *: tail] => recur[tail](TypeRepr.of[head].literal[String].vouch :: strings)
      case _               => strings

    def firstOrigin[tuple: Type]: Int = Type.of[tuple] match
      case '[head *: tail] => TypeRepr.of[head].dealias match
        case AppliedType(_, ConstantType(IntConstant(start)) :: _) => start
        case _                                                     => 0

      case _ => 0

    val parts = recur[parts](Nil)
    if parts.length != 1 then halt(m"an XPath literal cannot have substitutions")
    val raw: String = parts.head
    val start: Int = firstOrigin[origins]

    try unsafely(raw.tt.decode[XPath]) catch
      case error: XPathError =>
        val sourceFile = Position.ofMacroExpansion.sourceFile

        val position = sourceFile.content match
          case Some(content: String) if start > 0 && start < content.length =>
            val upper = (start + raw.length*6 + 16).min(content.length)
            val mapping = Interpolation.buildMapping(content.substring(start, upper).nn, raw)
            val at = (start + mapping(error.offset.min(raw.length))).min(content.length - 1)
            Position(sourceFile, at, (at + 1).min(content.length))

          case _ =>
            Position.ofMacroExpansion

        halt(error.message, position)

    '{unsafely(${Expr(raw)}.tt.decode[XPath])}
