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

type ForbiddenSet = Singleton & (Char | String)

object Hierarchy:
  given show[PathType](using hierarchy: Hierarchy[PathType]): Show[PathType] = path =>
    t"${hierarchy.prefix(path)}${path.elements.map(_.show).reverse.join(hierarchy.separator)}"

trait Hierarchy[PathType]:

  type ForbiddenType <: Singleton & (Char | String)
  def separator: Text
  def prefix(root: PathType): Text
  def root(path: PathType): PathType
  def elements(path: PathType): List[PathElement[ForbiddenType]]
  def child(base: PathType, element: PathElement[ForbiddenType]): PathType
  def parent(path: PathType): PathType

object PathBounds:
  given [PathType](using pathError: CanThrow[PathError]): PathBounds[PathType] = isRoot =>
    if isRoot then throw PathError(PathError.Reason.ParentOfRoot)

@capability
trait PathBounds[PathType]:
  def hasParent(isRoot: Boolean): Unit

extension [PathType, PathType2 >: PathType](path: PathType)(using hierarchy: Hierarchy[PathType2])
  def elements: List[PathElement[hierarchy.ForbiddenType]] = hierarchy.elements(path)
  def root: PathType2 = hierarchy.root(path)
  
  def parent(using pathBounds: PathBounds[PathType]): {pathBounds} PathType2 =
    pathBounds.hasParent(path.elements.isEmpty)
    hierarchy.parent(path)
  
  def text: Text =
    hierarchy.elements(path).reverse.map(_.show).join(hierarchy.prefix(root), hierarchy.separator, t"")

  def depth: Int = hierarchy.elements(path).length
  
  @targetName("child")
  infix def /(element: PathElement[hierarchy.ForbiddenType]): PathType2 = hierarchy.child(path, element)
  
  def ancestor(n: Int): PathType2 =
    def recur(path: PathType2, n: Int): PathType2 = if n == 0 then path else recur(hierarchy.parent(path), n - 1)
    if depth > n then recur(path, n) else hierarchy.root(path)
  
  def take(n: Int): PathType2 = ancestor(depth - n)
  
  def conjunction(other: PathType2): PathType2 =
    take(elements.reverse.zip(other.elements.reverse).takeWhile(_ == _).length)

  def relative: Relative = Relative(0, elements.map(_.show))

  def relativeTo(other: PathType2): Relative =
    val common: Int = conjunction(other).depth
    Relative(depth - common, other.elements.reverse.drop(common).map(_.show).reverse)
  
  def precedes(other: PathType): Boolean = conjunction(other).elements == elements

  @targetName("add") 
  infix def ++
      (relative: Relative)
      (using pathError: CanThrow[PathError], pathBounds: PathBounds[PathType])
      : {pathBounds, pathError} PathType2 =
    if relative.ascent > depth then throw PathError(PathError.Reason.ParentOfRoot) else
      def recur(path: PathType2, elements: List[Text]): PathType2 = elements match
        case head :: tail => recur(hierarchy.child(path, PathElement(head)), tail)
        case Nil          => path
      
      recur(ancestor(relative.ascent), relative.elements.reverse)
