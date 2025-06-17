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
┃    Soundness, version 0.35.0.                                                                    ┃
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
package exoskeleton

import anticipation.*
import denominative.*
import fulminate.*
import gossamer.*
import proscenium.*
import rudiments.*
import spectacular.*
import vacuous.*

import language.experimental.pureFunctions

object Flag:
  def serialize(name: Text | Char): Text = name.absolve match
    case char: Char => t"-$char"
    case text: Text => t"--$text"

  given communicable: Flag is Showable = _.name.absolve match
    case name: Text => t"--$name"
    case name: Char => t"-$name"

case class Flag
   (name: Text | Char,
    repeatable: Boolean         = false,
    aliases: List[Text | Char]  = Nil,
    description: Optional[Text] = Unset,
    secret: Boolean             = false):

  def suggest[operand: FlagInterpreter](suggestions: Suggestions[operand])(using cli: Cli): Unit =
    cli.register(this, suggestions)

  def matches(key: Argument): Boolean =
    val flag =
      if key().starts(t"--") then key().skip(2) else if key().starts(t"-")
      then key().at(Sec) else Unset

    flag == name || aliases.contains(flag)

  def apply[operand]()
       (using cli:             Cli,
              interpreter:     CliInterpreter,
              flagInterpreter: FlagInterpreter[operand],
              suggestions:     Suggestions[operand] = Suggestions.noSuggestions)
  : Optional[operand] =

      cli.register(this, suggestions)
      cli.readParameter(this)


  def select[operand](options: Iterable[operand])
       (using cli: Cli, interpreter: CliInterpreter, suggestible: operand is Suggestible)
  : Optional[operand] =

      val mapping: Map[Text, operand] =
        options.map { option => (suggestible.suggest(option).text, option) }.to(Map)

      given flagInterpreter: FlagInterpreter[operand] =
        case List(value) => mapping.at(value())
        case _           => Unset

      given suggestions: Suggestions[operand] = () => options.map(suggestible.suggest(_))

      this()
