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
  object `^` extends PathRoot:
    def prefix: Text = t"/"

  given Show[GenericPath] = Hierarchy.show[GenericPath]

  def parse(text: Text): GenericPath throws PathError =
    if text.starts(t"/") then GenericPath(text.drop(1).cut(t"/").filter(_ != t"").map(PathElement(_)))
    else throw PathError(PathError.Reason.NotRooted)

  given hierarchy: Hierarchy[GenericPath] with
    type RootType = `^`.type
    type Forbidden = ".*/.*" | "\\.\\." | ""

    def separator: Text = t"/"
    def selfName: Text = t"."
    def parentName: Text = t".."
    
    def parseElement(text: Text): PathElement[Forbidden] throws PathError = text match
      case t".." => throw PathError(PathError.Reason.InvalidName(t".."))
      case t""   => throw PathError(PathError.Reason.InvalidName(t""))
      case text  => if text.contains('/') then throw PathError(PathError.Reason.InvalidName(text))
                    else PathElement(text)

    def root(path: GenericPath): `^`.type = ^
    def elements(path: GenericPath): List[PathElement[Forbidden]] = path.elements
    def remake(path: {*} GenericPath, elements: List[PathElement[Forbidden]]): {path} GenericPath =
      GenericPath(elements)
    
    def parent(path: GenericPath): GenericPath = path.copy(elements = path.elements.tail)

case class GenericPath(elements: List[SerpentineInternals.PathElement[".*/.*" | "\\.\\." | ""]])

export GenericPath.`^`