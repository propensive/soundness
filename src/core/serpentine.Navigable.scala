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
import spectacular.*
import anticipation.*
import gossamer.*
import contingency.*

import scala.compiletime.*

object Navigable:
  inline def decode[PathType <: Matchable](text: Text)[NameType <: Label, RootType]
      (using navigable:  PathType is Navigable[NameType, RootType],
             rootParser: RootParser[PathType, RootType],
             creator:    PathCreator[PathType, NameType, RootType])
      (using path: Tactic[PathError])
      : PathType =
    val rootRest: Optional[(RootType, Text)] = rootParser.parse(text)
    if rootRest.absent
    then raise
     (PathError(text, PathError.Reason.NotRooted),
      creator.path(summonInline[Default[RootType]](), Nil))
    else
      // FIXME: The casts below avoid an error in the compiler which just prints an AST without explanation
      val root: RootType = rootRest.asInstanceOf[(RootType, Text)](0)
      val rest: Text = rootRest.asInstanceOf[(RootType, Text)](1)

      val names = rest.cut(navigable.separator(creator.path(root, Nil))).to(List).reverse match
        case t"" :: tail => tail
        case names       => names

      creator.path(root, names.map(Name(_)))


@capability
trait Navigable[NameType <: Label, RootType]
extends Directional[NameType, RootType]:
  type Self <: Matchable
  def separator(path: Self): Text
  def prefix(root: RootType): Text
  def root(path: Self): RootType
  def depth(path: Self): Int = descent(path).length
  def ascent(path: Self): RootType = root(path)

  def render(path: Self): Text =
    t"${prefix(root(path))}${descent(path).reverse.map(_.render).join(separator(path))}"

  def ancestor[Self2 <: Self](path: Self, n: Int)
      (using creator: PathCreator[Self2, NameType, RootType])
          : Optional[Self2] =
    if descent(path).length < n then Unset else creator.path(root(path), descent(path).drop(n))
