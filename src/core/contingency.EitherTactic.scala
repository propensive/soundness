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
import rudiments.*

class EitherTactic[ErrorType <: Exception, SuccessType]
   (label: boundary.Label[Either[ErrorType, SuccessType]])
   (using Diagnostics)
extends Tactic[ErrorType]:
  def diagnostics: Diagnostics = summon[Diagnostics]
  def record(error: Diagnostics ?=> ErrorType): Unit = boundary.break(Left(error))(using label)
  def abort(error: Diagnostics ?=> ErrorType): Nothing = boundary.break(Left(error))(using label)
