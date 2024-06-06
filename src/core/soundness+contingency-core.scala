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

package soundness

export contingency.{Errant, ThrowStrategy, FailStrategy, AggregateStrategy, EitherStrategy, OptionalStrategy, AttemptStrategy, Recovery, raise, abort, safely, unsafely, throwErrors, validate, capture, attempt, failCompilation, AggregateError, ExpectationError, raises, Attempt, Tended, remedy, mitigate, tend}

package errorHandlers:
  export contingency.errorHandlers.{throwUnsafely, throwSafely}
