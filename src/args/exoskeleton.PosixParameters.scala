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
import contingency.*

import language.experimental.pureFunctions

case class PosixParameters
    (positional:     List[Argument]                = Nil,
     parameters:     Map[Argument, List[Argument]] = Map(),
     postpositional: List[Argument]                = Nil,
     focusFlag:      Optional[Argument]            = Unset)
extends FlagParameters:

  def read[OperandType](flag: Flag)
      (using cli: Cli, interpreter: FlagInterpreter[OperandType], suggestions: Suggestions[OperandType])
          : Optional[OperandType] =

    cli.register(flag, suggestions)

    parameters.where { (key, _) => flag.matches(key) }.let: (_, operands) =>
      cli.present(flag)
      safely(interpreter.interpret(operands))
