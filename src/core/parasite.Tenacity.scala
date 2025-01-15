/*
    Parasite, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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
  private inline def tenacity: this.type = this
  def delay(attempt: Ordinal): Optional[Long] raises RetryError

  def limit(n: Int): Tenacity = new:
    def delay(attempt: Ordinal): Optional[Long] raises RetryError =
      if attempt.n1 > n then abort(RetryError(attempt.n1 - 1)) else tenacity.delay(attempt)

object Tenacity:
  def exponential[DurationType: GenericDuration](initial: DurationType, base: Double): Tenacity =
    new:
      def delay(attempt: Ordinal): Optional[Long] raises RetryError =
        if attempt == Prim then 0L else (initial.milliseconds*math.pow(base, attempt.n1)).toLong

  def fixed[DurationType: GenericDuration](duration: DurationType): Tenacity = new:
    def delay(attempt: Ordinal): Optional[Long] raises RetryError =
      if attempt == Prim then 0L else duration.milliseconds
