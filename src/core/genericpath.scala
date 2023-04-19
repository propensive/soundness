/*
    Serpentine, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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
import gossamer.*

import language.experimental.captureChecking

object GenericPath:
  @targetName("GeneralPathRoot")
  object `^` extends GenericPath(Nil)

  given Show[GenericPath] = Hierarchy.show[GenericPath]

  given hierarchy: Hierarchy[GenericPath] with
    type ForbiddenType = '/' | ".." | ""

    def separator: Text = t"/"
    def prefix(root: GenericPath): Text = t"/"
    def root(path: GenericPath): `^`.type = ^
    def elements(path: GenericPath): List[PathElement['/' | ".." | ""]] = path.elements
    def child(base: GenericPath, child: PathElement['/' | ".." | ""]): GenericPath =
      base.copy(elements = child :: base.elements)
    
    def parent(path: GenericPath): GenericPath = path.copy(elements = path.elements.tail)

case class GenericPath(elements: List[SerpentineInternals.PathElement['/' | ".." | ""]])

export GenericPath.`^`