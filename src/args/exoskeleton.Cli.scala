/*
    Exoskeleton, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

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

import ambience.*
import anticipation.*
import gossamer.*
import profanity.*
import rudiments.*
import vacuous.*

import language.experimental.captureChecking

object Cli:
  def arguments
     (textArguments: Iterable[Text], focus: Optional[Int] = Unset, position: Optional[Int] = Unset)
          : List[Argument] =

    textArguments.to(List).padTo(focus.or(0) + 1, t"").zipWithIndex.map: (text, index) =>
      Argument(index, text, if focus == index then position else Unset)

trait Cli extends ProcessContext:
  def arguments: List[Argument]
  def environment: Environment
  def workingDirectory: WorkingDirectory

  def readParameter[OperandType](flag: Flag)
     (using FlagInterpreter[OperandType], Suggestions[OperandType])
          : Optional[OperandType]

  def register(flag: Flag, suggestions: Suggestions[?]): Unit = ()
  def present(flag: Flag): Unit = ()
  def explain(update: (prior: Optional[Text]) ?=> Optional[Text]): Unit = ()
  def suggest(argument: Argument, update: (prior: List[Suggestion]) ?=> List[Suggestion]) = ()
