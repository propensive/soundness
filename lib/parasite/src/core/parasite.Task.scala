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
┃    Soundness, version 0.27.0.                                                                    ┃
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
package parasite

import language.experimental.pureFunctions
import language.experimental.into

import anticipation.*
import contingency.*
import digression.*
import mercator.*
import vacuous.*

object Task:
  def apply[ResultType]
     (evaluate: Worker => ResultType, daemon: Boolean, name: Optional[Text])
     (using monitor: Monitor, codepoint: Codepoint, codicil: Codicil)
  :     Task[ResultType] =
    inline def evaluate0: Worker => ResultType = evaluate
    inline def name0: Optional[Text] = name

    new Worker(codepoint, monitor, codicil, Unset) with Task[ResultType]:
      type Result = ResultType
      def name: Optional[Text] = name0
      def daemon: Boolean = false
      def evaluate(worker: Worker): Result = evaluate0(worker)

  given (Monitor, Codicil, Tactic[AsyncError]) => Monad[Task]:
    def bind[ValueType, ValueType2](value: Task[ValueType])(lambda: ValueType => Task[ValueType2])
    :     Task[ValueType2] =
      value.bind(lambda)

    def point[ValueType](value: ValueType): Task[ValueType] = async(value)

    def apply[ValueType, ValueType2](value: Task[ValueType])(lambda: ValueType -> ValueType2)
    :     Task[ValueType2] =
      value.map(lambda)

trait Task[+ResultType]:
  def ready: Boolean
  def await(): ResultType raises AsyncError
  def attend(): Unit
  def cancel(): Unit

  def await[DurationType: GenericDuration](duration: DurationType): ResultType raises AsyncError

  def bind[ResultType2](lambda: ResultType => Task[ResultType2])(using Monitor, Codicil)
  :     Task[ResultType2] raises AsyncError

  def map[ResultType2](lambda: ResultType => ResultType2)(using Monitor, Codicil)
  :     Task[ResultType2] raises AsyncError
