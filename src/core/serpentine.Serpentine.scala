/*
    Serpentine, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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

import contingency.*
import prepositional.*
import rudiments.*
import vacuous.*

object Serpentine:
  @targetName("Parent")
  object ^

  @targetName("RelativeRoot")
  val `?` = PathAscent(0)

  @targetName("Slash")
  object `/`:
    def unapply[PlatformType <: AnyRef & Matchable: {Navigable, Radical}, ElementType]
       (path: Path on PlatformType)
            : Option[(Path on PlatformType, PlatformType.Operand)] =
      path.textDescent match
        case Nil          => None
        case head :: Nil  => Some((PlatformType.root(path.textRoot), PlatformType.element(head)))
        case head :: tail => Some((unsafely(path.parent.vouch), PlatformType.element(head)))
