/*
    Contingency, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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

import fulminate.*
import proscenium.*

class TrackTactic
   [ErrorType <: Exception, AccrualType, ResultType, SupplementType]
   (label: boundary.Label[Option[ResultType]],
    initial: AccrualType,
    foci: Foci[SupplementType])
   (using Diagnostics)
extends Tactic[ErrorType]:
  def diagnostics: Diagnostics = summon[Diagnostics]
  def record(error: Diagnostics ?=> ErrorType): Unit = foci.register(error)
  def finish(): Unit = ()

  def abort(error: Diagnostics ?=> ErrorType): Nothing =
    foci.register(error)
    boundary.break(None)(using label)
