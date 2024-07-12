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

trait Directional[NameType <: Label, AscentType]:
  type Self <: Matchable
  def separator(path: Self): Text
  def descent(path: Self): List[Name[NameType]]
  def render(path: Self): Text
  def ascent(path: Self): AscentType

  def ancestor[Self2 <: Self](path: Self, n: Int)
      (using creator: PathCreator[Self2, NameType, AscentType])
          : Optional[Self2]

  def parent[Self2 <: Self](path: Self)
      (using creator: PathCreator[Self2, NameType, AscentType])
          : Optional[Self2] =
    ancestor(path, 1)

  def child[Self2 <: Self](path: Self, name: Name[NameType])
      (using creator: PathCreator[Self2, NameType, AscentType])
          : Self2 =
    creator.path(ascent(path), name :: descent(path))
