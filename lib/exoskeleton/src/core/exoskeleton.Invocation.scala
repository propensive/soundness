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
package exoskeleton

import ambience.*
import anticipation.*
import rudiments.*
import turbulence.*
import vacuous.*
import rudiments.sortingAlgorithms.timsort

case class Invocation
  ( arguments:        List[Argument],
    environment:      Environment,
    workingDirectory: WorkingDirectory,
    stdio:            Stdio,
    proceed:          Boolean,
    login:            Login )
  ( using interpreter: Interpreter )
extends Cli, Stdio:

  export stdio.{termcap, out, err, in}

  @scala.caps.unsafe.untrackedCaptures
  private var matchedArguments: List[Argument] = Nil

  @scala.caps.unsafe.untrackedCaptures
  private var missingFlags: List[Flag] = Nil

  override def demand(flag: Flag, present: Boolean): Unit =
    if !present then missingFlags = flag :: missingFlags

  // The required flags which were not specified, in the order they were demanded.
  def missingRequisites: List[Flag] = missingFlags.reverse.distinct

  @scala.caps.unsafe.untrackedCaptures
  private var faultedFlags: List[(Flag, Text)] = Nil

  override def fault(flag: Flag, message: Text): Unit =
    faultedFlags = (flag, message) :: faultedFlags

  // The validated flags whose operands failed to decode, with the reasons, in the order they
  // were validated.
  def faults: List[(Flag, Text)] = faultedFlags.reverse.distinct

  override def locate(flag: Flag): Optional[List[Argument]] =
    interpreter.locate(parameters, flag)

  override def matched(argument: Argument): Unit = matchedArguments = argument :: matchedArguments

  // The recorded matches, deduplicated (re-matches of one position record the identical
  // argument) and restricted to the contiguous run from the first argument, so that a match
  // against a later argument alone cannot fabricate a false prefix.
  override def matches: List[Text] =
    def recur(arguments: List[Argument], position: Int): List[Text] = arguments match
      case argument :: rest if argument.position == position =>
        argument() :: recur(rest, position + 1)

      case _ =>
        Nil

    recur(matchedArguments.distinct.order(_.position), 0)

  private lazy val parameters: interpreter.Topic = interpreter.interpret(arguments)

  def parameter[operand: Interpretable](flag: Flag)(using (? <: operand) is Discoverable)
  :   Optional[operand] =

    // An alias of `this` with its precise capture, not a fresh capability.
    given cli: (Cli^{this}) = this
    interpreter.read[operand](parameters, flag)
