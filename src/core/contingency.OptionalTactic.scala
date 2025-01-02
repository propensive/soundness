/*
    Contingency, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

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
import rudiments.*
import vacuous.*

class OptionalTactic[ErrorType <: Exception, SuccessType]
   (label: boundary.Label[Optional[SuccessType]])
extends Tactic[ErrorType]:
  type Result = Optional[SuccessType]
  type Return = Optional[SuccessType]

  def diagnostics: Diagnostics = Diagnostics.omit
  def record(error: Diagnostics ?=> ErrorType): Unit = boundary.break(Unset)(using label)
  def abort(error: Diagnostics ?=> ErrorType): Nothing = boundary.break(Unset)(using label)
