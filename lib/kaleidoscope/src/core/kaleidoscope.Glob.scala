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
package kaleidoscope

import anticipation.*
import distillate.*
import prepositional.*
import rudiments.*
import vacuous.*

object Glob:
  // `parse` is total — any character outside the wildcard syntax is an `Exact` token — so
  // glob-valued flags and configuration values decode without an explicit step.
  given decodable: Glob is Decodable in Text = parse(_)

  import Glob.Token.*

  def parse(text: Text): Glob =
    def range(text: String): Glob.Token =
      val inverse = text.startsWith("!")
      val text2 = if inverse then text.drop(1) else text

      if text2.length == 3 && text2(1) == '-' then Glob.Token.Range(text2(0), text2(2), inverse)
      else Glob.Token.Specific(text2, inverse)

    def recur(index: Int, tokens: List[Glob.Token]): Glob =
      if index >= text.s.length then Glob(tokens.reverse*) else text.s(index) match
        case '*' =>
          tokens match
            case Star :: tail => recur(index + 1, Globstar :: (tail: List[Glob.Token]))
            case _            => recur(index + 1, Star :: tokens)

        case '?' =>
          recur(index + 1, OneChar :: tokens)

        case '[' =>
          val end = text.s.indexOf(']', index + 1)
          recur(end + 1, range(text.s.substring(index + 1, end).nn) :: tokens)

        case char =>
          recur(index + 1, Exact(char) :: tokens)

    recur(0, Nil)

  // GlobToken → Glob.Token
  object Token:
    private val needsEscaping: Set[Char] = ("\\.[]{}()<>*+-=!?^$|".iterator).to(Set)

  enum Token:
    case Star, Globstar, OneChar
    case Exact(char: Char)
    case Range(start: Char, end: Char, inverse: Boolean)
    case Specific(chars: String, inverse: Boolean)

    def regex: String = this match
      case Exact(char) =>
        (if Glob.Token.needsEscaping.has(char) then "\\" else "")+char

      case Star =>
        "[^/\\\\]*"

      case OneChar =>
        "[^/\\\\]"

      case Globstar =>
        ".*"

      case Range(start, end, inverse) =>
        s"[${if inverse then "^" else ""}${Exact(start).regex}-${Exact(end).regex}]"

      case Specific(chars, inverse) =>
        chars.flatMap(Exact(_).regex).mkString(s"[${if inverse then "^" else ""}", "", "]")

case class Glob(tokens: Glob.Token*):
  lazy val regex: Text = Text(tokens.map(_.regex).mkString)

  // Matches the whole of `text`, on the backend selected by an imported `RegexBackend`
  // (`import regexBackends.re2`), defaulting to `java.util.regex` — the same selection
  // mechanism as the `r"…"` and `g"…"` interpolators. The engines cache compilation by
  // rendered pattern, so matching many candidates against one `Glob` compiles it only once.
  def matches[form](text: Text)
    ( using backend: RegexBackend[form] = regexBackends.jur )
    ( using engine:  form is Regex.Engine )
  :   Boolean =

    Regex(regex, Nil).to[form].matches(text)(using Scanner(Unset))
