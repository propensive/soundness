/*
    Serpentine, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package serpentine

import fulminate.*
import anticipation.*

import scala.compiletime.*
import scala.reflect.*

object PathError:
  enum Reason:
    case InvalidChar(char: Char)
    case InvalidPrefix(prefix: Text)
    case InvalidSuffix(suffix: Text)
    case InvalidName(name: Text)
    case ParentOfRoot
    case NotRooted

  given Reason is Communicable =
    case Reason.InvalidChar(char)     => m"the character $char may not appear in its name"
    case Reason.InvalidPrefix(prefix) => m"its name cannot begin with $prefix"
    case Reason.InvalidSuffix(suffix) => m"its name cannot end with $suffix"
    case Reason.InvalidName(name)     => m"the name $name is not valid"
    case Reason.ParentOfRoot          => m"it has no parent"
    case Reason.NotRooted             => m"it is not rooted"

case class PathError(path: Text, reason: PathError.Reason)
extends Error(m"the path $path is invalid because $reason")
