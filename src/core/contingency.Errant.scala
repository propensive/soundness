/*
    Contingency, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package contingency

import language.experimental.pureFunctions

import symbolism.*
import fulminate.*
import rudiments.*

object Errant:
  given [ErrorType <: Error: Errant, ErrorType2 <: Error: Mitigable into ErrorType]
      => Errant[ErrorType2] =
    ErrorType.contramap(ErrorType2.mitigate(_))

@capability
trait Errant[-ErrorType <: Error]:
  private inline def errant: this.type = this

  def record(error: ErrorType): Unit
  def abort(error: ErrorType): Nothing

  def contramap[ErrorType2 <: Error](lambda: ErrorType2 => ErrorType): Errant[ErrorType2] =
    new Errant[ErrorType2]:
      def record(error: ErrorType2): Unit = errant.record(lambda(error))
      def abort(error: ErrorType2): Nothing = errant.abort(lambda(error))
