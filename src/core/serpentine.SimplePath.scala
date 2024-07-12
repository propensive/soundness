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

import rudiments.*
import vacuous.*
import anticipation.*
import spectacular.*
import contingency.*
import symbolism.*
import gossamer.*

object SimplePath:
  inline given decoder(using Tactic[PathError]): Decoder[SimplePath] = new Decoder[SimplePath]:
    def decode(text: Text): SimplePath = Navigable.decode[SimplePath](text)

  inline given (using path: Tactic[PathError]) => SimplePath is Addable as addable:
    type Result = SimplePath
    type Operand = SimpleLink
    def add(left: SimplePath, right: SimpleLink): SimplePath = left.append(right)

  inline def parse(text: Text)(using path: Tactic[PathError]): SimplePath/*^{path}*/ =
    text.decodeAs[SimplePath]

  given SimplePath is Showable as show = _.render
  given SimplePath is Radical as radical = () => SimplePath(Nil)

  given rootParser: RootParser[SimplePath, Root.type] with
    def parse(text: Text): Optional[(Root.type, Text)] =
      if text.starts(t"/") then (Root, text.drop(1)) else Unset

  given SimplePath is Navigable[".*\\/.*", Root.type] as navigable:
    def separator(path: SimplePath): Text = t"/"
    def root(path: SimplePath): Root.type = serpentine.Root
    def prefix(root: Root.type): Text = t"/"
    def descent(path: SimplePath): List[Name[".*\\/.*"]] = path.descent

  val nav: SimplePath is Directional[".*\\/.*", Root.type] = navigable

  given pathCreator: PathCreator[SimplePath, ".*\\/.*", Root.type] with
    def path(root: Root.type, descent: List[Name[".*\\/.*"]]): SimplePath = SimplePath(descent)

case class SimplePath(descent: List[Name[".*\\/.*"]])
extends PathEquality[SimplePath](using SimplePath.navigable)
