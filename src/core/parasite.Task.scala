/*
    Parasite, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package parasite

import language.experimental.pureFunctions
import language.experimental.into

import anticipation.*
import contingency.*
import digression.*
import vacuous.*

object Task:
  def apply[ResultType]
     (evaluate: Subordinate => ResultType, daemon: Boolean, name: Optional[Text])
     (using monitor: Monitor, codepoint: Codepoint, codicil: Codicil)
          : Task[ResultType] =
    inline def evaluate0: Subordinate => ResultType = evaluate
    inline def name0: Optional[Text] = name

    new Subordinate(codepoint, monitor, codicil, Unset) with Task[ResultType]:
      type Result = ResultType
      def name: Optional[Text] = name0
      def daemon: Boolean = false
      def evaluate(subordinate: Subordinate): Result = evaluate0(subordinate)

trait Task[+ResultType]:
  def ready: Boolean
  def await(): ResultType raises ConcurrencyError
  def attend(): Unit
  def suspend(): Unit
  def resume(force: Boolean = false): Unit
  def cancel(): Unit

  def await[DurationType: GenericDuration](duration: DurationType)
          : ResultType raises ConcurrencyError

  def flatMap[ResultType2](lambda: ResultType => Task[ResultType2])(using Monitor, Codicil)
          : Task[ResultType2] raises ConcurrencyError

  def map[ResultType2](lambda: ResultType => ResultType2)(using Monitor, Codicil)
          : Task[ResultType2] raises ConcurrencyError
