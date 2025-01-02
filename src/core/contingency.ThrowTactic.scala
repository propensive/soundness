/*
    Contingency, version 0.24.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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

class ThrowTactic[ErrorType <: Exception, SuccessType]()
   (using @annotation.constructorOnly error: CanThrow[ErrorType])
extends Tactic[ErrorType]:

  def diagnostics: Diagnostics = Diagnostics.capture
  def record(error: Diagnostics ?=> ErrorType): Unit = throw error(using diagnostics)
  def abort(error: Diagnostics ?=> ErrorType): Nothing = throw error(using diagnostics)
