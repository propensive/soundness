/*
    Metamorphose, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package metamorphose

import fulminate.*

object PermutationError:
  enum Reason:
    case BaseRange(value: Int, base: Int)
    case DuplicateIndex(index: Int, element: Int)
    case InvalidIndex(last: Int, max: Int)
    case TooShort(length: Int, min: Int)

  import Reason.*

  given Reason is Communicable =
    case BaseRange(value, base) =>
      m"the value $value is too large for its positional base $base"

    case DuplicateIndex(element, index) =>
      m"the index $element was duplicated at $index"

    case InvalidIndex(index, max) =>
      m"the index $index appears, but every index should be in the range 0-$max"

    case TooShort(size, min) =>
      m"the input, of size $size, is too short for the permutation of size $min"

case class PermutationError(reason: PermutationError.Reason)(using Diagnostics)
extends Error(m"could not construct permutation because $reason")
