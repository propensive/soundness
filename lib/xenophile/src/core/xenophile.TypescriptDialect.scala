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
package xenophile

import anticipation.*
import vacuous.*

// A minimal grammar for TypeScript declaration files: enough to read `interface` blocks of fields
// (`name: Type;`) and methods (`name(p: T, …): Type;`). It does not interpret the rest of the
// language — unrecognised tokens between members are skipped.
object TypescriptDialect extends Dialect:
  def parse(source: Text): Map[Text, Map[Text, Signature]] = interfaces(tokenize(source.s), Map())

  // Splits the source into identifier and single-character punctuation tokens, discarding
  // whitespace; this is all the lexical structure the interface grammar needs.
  private def punctuation(char: Char): Boolean =
    char == '{' || char == '}' || char == '(' || char == ')' || char == ':' || char == ',' ||
      char == ';' || char == '[' || char == ']' || char == '?' || char == '|'

  private def tokenize(source: String): List[String] =
    def recur(index: Int, current: String, tokens: List[String]): List[String] =
      if index >= source.length
      then (if current.isEmpty then tokens else current :: tokens).reverse
      else
        val char = source.charAt(index)

        if char.isLetterOrDigit || char == '_' then recur(index + 1, current + char, tokens)
        else
          val flushed = if current.isEmpty then tokens else current :: tokens

          if punctuation(char) then recur(index + 1, "", char.toString :: flushed)
          else recur(index + 1, "", flushed)

    recur(0, "", Nil)

  private def interfaces(tokens: List[String], acc: Map[Text, Map[Text, Signature]])
  :   Map[Text, Map[Text, Signature]] =

    tokens match
      case "interface" :: name :: "{" :: rest =>
        val (members, rest2) = membersOf(rest, Map())
        interfaces(rest2, acc.updated(name.tt, members))

      case _ :: rest =>
        interfaces(rest, acc)

      case Nil =>
        acc

  // Parses a type expression into a foreign type name encoding its structure: `T[]` becomes "T[]";
  // `T | null` and `T | undefined` become "T?"; any other union "A | B" becomes "A|B". These names
  // are what `Interoperable` instances are keyed on.
  private def typeOf(tokens: List[String]): (String, List[String]) = tokens match
    case base :: rest =>
      val (array, rest2) = rest match
        case "[" :: "]" :: more => (base+"[]", more)
        case _                  => (base, rest)

      rest2 match
        case "|" :: "null" :: more      => (array+"?", more)
        case "|" :: "undefined" :: more => (array+"?", more)
        case "|" :: other :: more       => (array+"|"+other, more)
        case _                          => (array, rest2)

    case Nil =>
      ("", Nil)

  private def membersOf(tokens: List[String], acc: Map[Text, Signature])
  :   (Map[Text, Signature], List[String]) =

    tokens match
      case "}" :: rest =>
        (acc, rest)

      case name :: "(" :: rest =>
        val (parameters, rest2) = params(rest, Nil)

        rest2 match
          case ":" :: rest3 =>
            val (result, rest4) = typeOf(rest3)
            val signature = Signature(parameters.map(_.tt), result.tt)
            membersOf(semicolon(rest4), acc.updated(name.tt, signature))

          case _ =>
            (acc, rest2)

      case name :: "?" :: ":" :: rest =>
        val (result, rest2) = typeOf(rest)
        membersOf(semicolon(rest2), acc.updated(name.tt, Signature(Unset, (result+"?").tt)))

      case name :: ":" :: rest =>
        val (result, rest2) = typeOf(rest)
        membersOf(semicolon(rest2), acc.updated(name.tt, Signature(Unset, result.tt)))

      case _ :: rest =>
        membersOf(rest, acc)

      case Nil =>
        (acc, Nil)

  // Reads parameter declarations up to the closing `)`, keeping only each parameter's type.
  private def params(tokens: List[String], acc: List[String]): (List[String], List[String]) =
    tokens match
      case ")" :: rest =>
        (acc.reverse, rest)

      case name :: ":" :: rest =>
        val (kind, rest2) = typeOf(rest)

        rest2 match
          case "," :: more => params(more, kind :: acc)
          case _           => params(rest2, kind :: acc)

      case _ :: rest =>
        params(rest, acc)

      case Nil =>
        (acc.reverse, Nil)

  private def semicolon(tokens: List[String]): List[String] = tokens match
    case ";" :: rest => rest
    case _           => tokens
