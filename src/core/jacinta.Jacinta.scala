/*
    Jacinta, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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

import scala.compiletime.*

import anticipation.*
import denominative.*
import gossamer.*
import rudiments.*
import spectacular.*
import vacuous.*

object Jacinta:
  opaque type JsonPointer = List[Text | Ordinal]

  object JsonPointer:
    given JsonPointer is Showable = pointer =>
      def recur(elements: List[Ordinal | Text], result: Text): Text =
        elements.asMatchable.runtimeChecked match
          case Nil         => if result.empty then t"." else result
          case key :: tail => key.asMatchable.runtimeChecked match
            case index: Ordinal => recur(tail, t"[${index.n0}]$result")
            case key: Text      => recur(tail, t".$key$result")

      recur(pointer.reverse, t"")

    def apply(elements: List[Text | Ordinal] = Nil): JsonPointer = elements

  extension (path: JsonPointer)
    @targetName("child")
    infix def / (child: Text | Ordinal): JsonPointer = child :: path

    def parent: Optional[JsonPointer] = if path.isEmpty then Unset else JsonPointer(path.tail)
