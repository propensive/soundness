/*
    Jacinta, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package jacinta

import language.dynamics
import language.experimental.pureFunctions

import anticipation.*
import fulminate.*

object JsonError:
  enum Reason:
    case OutOfRange
    case NotType(found: JsonPrimitive, primitive: JsonPrimitive)
    case Absent

  object Reason:
    given Reason is Communicable =
      case OutOfRange                => m"the array index is out of range"
      case NotType(found, primitive) => m"the JSON value had type $found instead of $primitive"
      case Absent                    => m"the JSON value was not present"

case class JsonError(reason: JsonError.Reason)(using Diagnostics)
extends Error(m"could not access the value because $reason")
