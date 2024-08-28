/*
    Exoskeleton, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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

import rudiments.*
import vacuous.*
import gossamer.*
import spectacular.*
import denominative.*
import anticipation.*

import language.experimental.pureFunctions

object Flag:
  def serialize(name: Text | Char): Text = (name: @unchecked) match
    case char: Char => t"-$char"
    case text: Text => t"--$text"

case class Flag[OperandType]
    (name: Text | Char,
     repeatable: Boolean         = false,
     aliases: List[Text | Char]  = Nil,
     description: Optional[Text] = Unset,
     secret: Boolean             = false)
    (using FlagInterpreter[OperandType]):

  def suggest(suggestions: Suggestions[OperandType])(using cli: Cli): Unit =
    cli.register(this, suggestions)

  def matches(key: Argument): Boolean =
    val flag = if key().starts(t"--") then key().skip(2) else if key().starts(t"-") then key().at(Sec) else Unset
    flag == name || aliases.contains(flag)

  def apply()
      (using cli:             Cli,
             interpreter:     CliInterpreter,
             flagInterpreter: FlagInterpreter[OperandType],
             suggestions:     Suggestions[OperandType] = Suggestions.noSuggestions)
          : Optional[OperandType] =

    cli.register(this, suggestions)
    cli.readParameter(this)
