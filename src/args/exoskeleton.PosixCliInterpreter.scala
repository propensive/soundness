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
import gossamer.*
import rudiments.*
import vacuous.*

import language.experimental.pureFunctions

object PosixCliInterpreter extends CliInterpreter:
  type Parameters = PosixParameters
  def interpret(arguments: List[Argument]): PosixParameters =
    def recur
       (todo:      List[Argument],
        arguments:  List[Argument],
        current:    Optional[Argument],
        parameters: PosixParameters)
    :     PosixParameters =

      def push(): PosixParameters = current match
        case Unset =>
          PosixParameters(arguments.reverse)

        case current: Argument =>
          parameters.copy(parameters = parameters.parameters.updated(current, arguments.reverse))

      todo match
        case head :: tail =>
          if head() == t"--" then push().copy(postpositional = tail)
          else if head().starts(t"-") then recur(tail, Nil, head, push())
          else
            val parameters2 = if head.cursor.present then parameters.copy(focusFlag = current) else parameters
            recur(tail, head :: arguments, current, parameters2)

        case Nil =>
          push()

    recur(arguments, Nil, Unset, PosixParameters())
