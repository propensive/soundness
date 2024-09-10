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

import anticipation.*
import contingency.*
import denominative.*
import vacuous.*

trait Tenacity:
  def delay(attempt: Ordinal): Optional[Long]

  def limit(n: Int): Tenacity raises RetryError = attempt =>
    if attempt.n0 > n then abort(RetryError(attempt.n0)) else delay(attempt)

object Tenacity:
  def exponential[DurationType: GenericDuration](initial: DurationType, base: Double): Tenacity =
    attempt => (initial.milliseconds*math.pow(base, attempt.n0)).toLong

  def fixed[DurationType: GenericDuration](duration: DurationType): Tenacity =
    attempt => duration.milliseconds
