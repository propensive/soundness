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
┃    Soundness, version 0.64.0.                                                                    ┃
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
package inimitable

import scala.quoted.*

import anticipation.*
import contingency.*
import fulminate.*
import gigantism.*

object internal:
  // Where a UUID's text first departs from the canonical form, and how: eight, four, four, four
  // and twelve hexadecimal digits, with a hyphen between each group.
  private def fault(text: String): (Int, Message) =
    def hex(char: Char): Boolean =
      (char >= '0' && char <= '9') || (char >= 'a' && char <= 'f') || (char >= 'A' && char <= 'F')

    var index = 0
    var found: (Int, Message) | Null = null

    while found == null && index < text.length && index < 36 do
      val char = text.charAt(index)
      val hyphen = index == 8 || index == 13 || index == 18 || index == 23

      if hyphen && char != '-' then found = (index, m"a hyphen was expected here")
      else if !hyphen && !hex(char) then
        found = (index, m"${char.toString.tt} is not a hexadecimal digit")

      index += 1

    if found != null then found.nn
    else if text.length < 36 then
      ((text.length - 1).max(0), m"it has ${text.length} characters, and needs 36")
    else
      (36, m"it has ${text.length} characters, and needs 36")

  def uuid(expr: Expr[StringContext]): Macro[Uuid] =
    import quotes.reflect.*

    val text: String = expr.valueOrAbort.parts.head

    // The place of the literal's text in the source, found from the `StringContext` it is a
    // part of; the offset of a fault is counted from there.
    def literal(term: Term): Option[Position] = term match
      case Literal(StringConstant(_)) => Some(term.pos)
      case Inlined(_, _, body)        => literal(body)
      case Typed(body, _)             => literal(body)
      case Block(_, body)             => literal(body)
      case Apply(function, arguments) => (function :: arguments).iterator.flatMap(literal(_)).nextOption
      case Repeated(elements, _)      => elements.iterator.flatMap(literal(_)).nextOption
      case _                          => None

    given Diagnostics = Diagnostics.omit

    val uuid: Uuid =
      recover:
        case error: Uuid.Error =>
          val (offset, reason) = fault(text)

          val position: Position | Null = literal(expr.asTerm).map: origin =>
            val start = (origin.start + offset).min((origin.end - 1).max(origin.start))
            Position(origin.sourceFile, start, start + 1)

          . orNull

          halt(m"${error.labelled}: $reason", position)

      . protect(Uuid.parse(text.tt))

    '{Uuid(${Expr(uuid.msb)}, ${Expr(uuid.lsb)})}
