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

import vacuous.*
import anticipation.*
import gossamer.*
import rudiments.*

import language.experimental.captureChecking

case class Argument(position: Int, value: Text, cursor: Optional[Int]):
  def apply(): Text = value
  def prefix: Optional[Text] = cursor.let(value.keep(_))
  def suffix: Optional[Text] = cursor.let(value.skip(_))

  def suggest(using cli: Cli)(update: (prior: List[Suggestion]) ?=> List[Suggestion]) =
    cli.suggest(this, update)

  def select[OperandType](options: Seq[OperandType])
     (using cli: Cli, interpreter: CliInterpreter, suggestible: OperandType is Suggestible)
          : Optional[OperandType] =

    val mapping: Map[Text, OperandType] =
      options.map { option => (suggestible.suggest(option).text, option) }.to(Map)

    suggest(options.to(List).map(suggestible.suggest(_)))
    mapping.at(this())
