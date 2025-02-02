/*
    Aviation, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package anticipation

import aviation.*
import prepositional.*

package instantApi:
  given aviationInstant: Timing.Instant is (Abstractable & Concretizable) across Instants into
                          Long from Long =
    new Abstractable with Concretizable:
      type Self = Timing.Instant
      type Source = Long
      type Result = Long
      type Domain = Instants
      export Timing.Instant.generic.{generalization, apply}

package durationApi:
  given aviationDuration: Timing.Duration is GenericDuration & SpecificDuration =
    new GenericDuration with SpecificDuration:
      type Self = Timing.Duration
      export Timing.Duration.generic.{duration, milliseconds}
