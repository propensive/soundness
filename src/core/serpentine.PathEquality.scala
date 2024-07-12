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

import scala.compiletime.*
import scala.reflect.*

trait PathEquality[PathType <: Matchable](using directional: PathType is Directional[?, ?])
    (using TypeTest[Any, PathType]):
  this: PathType & Matchable =>

  override def equals(other: Any): Boolean = other.asMatchable match
    case `%` =>
      directional.descent(this) == Nil

    case other: PathType =>
      directional.descent(other) == directional.descent(this) && directional.ascent(other) == directional.ascent(this)

    case other =>
      false

  override def hashCode: Int = if directional.descent(this) == Nil then 0 else super.hashCode
