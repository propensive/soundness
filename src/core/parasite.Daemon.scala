/*
    Parasite, version 0.24.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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

import language.experimental.into
import language.experimental.pureFunctions

import anticipation.*
import digression.*
import vacuous.*

object Daemon:
  def apply(evaluate: Worker => Unit)
     (using monitor: Monitor, codepoint: Codepoint, codicil: Codicil)
          : Daemon =
    inline def evaluate0: Worker => Unit = evaluate

    new Worker(codepoint, monitor, codicil, Unset) with Daemon:
      type Result = Unit
      def name: Optional[Text] = Unset
      def daemon: Boolean = true
      def evaluate(worker: Worker): Result = evaluate0(worker)

trait Daemon:
  def attend(): Unit
  def cancel(): Unit
