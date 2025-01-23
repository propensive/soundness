/*
    Exoskeleton, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package exoskeleton

import anticipation.*
import denominative.*
import fulminate.*
import gossamer.*
import rudiments.*
import spectacular.*
import vacuous.*

import language.experimental.pureFunctions

object Flag:
  def serialize(name: Text | Char): Text = name.absolve match
    case char: Char => t"-$char"
    case text: Text => t"--$text"

  given Flag is Communicable = flag => flag.name.absolve match
    case name: Text => Message(t"--$name")
    case name: Char => Message(t"-$name")

case class Flag
   (name: Text | Char,
    repeatable: Boolean         = false,
    aliases: List[Text | Char]  = Nil,
    description: Optional[Text] = Unset,
    secret: Boolean             = false):

  def suggest[OperandType: FlagInterpreter](suggestions: Suggestions[OperandType])(using cli: Cli): Unit =
    cli.register(this, suggestions)

  def matches(key: Argument): Boolean =
    val flag = if key().starts(t"--") then key().skip(2) else if key().starts(t"-") then key().at(Sec) else Unset
    flag == name || aliases.contains(flag)

  def apply[OperandType]()
     (using cli:             Cli,
            interpreter:     CliInterpreter,
            flagInterpreter: FlagInterpreter[OperandType],
            suggestions:     Suggestions[OperandType] = Suggestions.noSuggestions)
          : Optional[OperandType] =

    cli.register(this, suggestions)
    cli.readParameter(this)

  def select[OperandType](options: Iterable[OperandType])
     (using cli: Cli, interpreter: CliInterpreter, suggestible: OperandType is Suggestible)
          : Optional[OperandType] =

    val mapping: Map[Text, OperandType] =
      options.map { option => (suggestible.suggest(option).text, option) }.to(Map)

    given FlagInterpreter[OperandType] =
      case List(value) => mapping.at(value())
      case _           => Unset

    given Suggestions[OperandType] = () => options.map(suggestible.suggest(_))

    this()
