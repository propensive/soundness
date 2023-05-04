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

object Hierarchy:
  given show[PathType](using hierarchy: Hierarchy[PathType]): Show[PathType] = path =>
    val separator = hierarchy.separator
    t"${hierarchy.root(path).prefix}${path.elements.map(_.show).reverse.join(separator)}"

trait PathRoot:
  def prefix: Text

trait Hierarchy[PathType]:
  type Forbidden <: Label
  type RootType <: PathRoot
  
  def selfName: Text
  def parentName: Text
  def separator: Text
  
  def root(path: PathType): RootType
  def parseElement(text: Text): PathElement[Forbidden] throws PathError
  def elements(path: PathType): List[PathElement[Forbidden]]
  def remake(path: {*} PathType, elements: List[PathElement[Forbidden]]): {path} PathType

  def child(path: PathType, element: PathElement[Forbidden]): PathType =
    remake(path, element :: elements(path))
  
  def ancestor
      (path: PathType, count: Int)(using pathBounds: PathBounds[PathType])
      : {pathBounds} PathType = pathBounds.ancestor(path, count)
  
  def parent
      (path: PathType)(using pathBounds: PathBounds[PathType])
      : {pathBounds} PathType = pathBounds.parent(path)

object PathBounds:
  given [PathType](using pathError: CanThrow[PathError]): PathBounds[PathType] =
    path => throw PathError(PathError.Reason.ParentOfRoot)

@capability
trait PathBounds[PathType]:
  def parent(path: PathType): PathType
  
  def ancestor(path: PathType, count: Int): PathType =
    if count == 0 then path else ancestor(parent(path), count - 1)

extension [PathType, PathType2 >: PathType](path: PathType)(using hierarchy: Hierarchy[PathType2])
  def elements: List[PathElement[hierarchy.Forbidden]] = hierarchy.elements(path)
  def root: hierarchy.RootType = hierarchy.root(path)
  
  def text: Text =
    val separator = hierarchy.separator
    hierarchy.elements(path).reverse.map(_.show).join(hierarchy.root(path).prefix, separator, t"")

  @targetName("child")
  infix def /(element: PathElement[hierarchy.Forbidden]): PathType2 = hierarchy.child(path, element)
  
  def parent(using pathBounds: PathBounds[PathType2]): {pathBounds} PathType2 =
    ancestor(1)
  
  def ancestor(count: Int)(using pathBounds: PathBounds[PathType2]): {pathBounds} PathType2 =
    if depth < count then pathBounds.ancestor(path, count - depth)
    else hierarchy.remake(path, elements.drop(count))
    
  def depth: Int = hierarchy.elements(path).length
  def keep(count: Int): PathType2 = hierarchy.remake(path, hierarchy.elements(path).take(count))
  def precedes(other: PathType): Boolean = conjunction(other).elements == elements
  def relative: Relative[hierarchy.Forbidden] = Relative(0, elements)
  
  def conjunction(other: PathType2): PathType2 =
    keep(elements.reverse.zip(other.elements.reverse).takeWhile(_ == _).length)

  def relativeTo(other: PathType2): Relative[hierarchy.Forbidden] =
    val common: Int = conjunction(other).depth
    Relative(depth - common, other.elements.reverse.drop(common).reverse)

  @targetName("add") 
  infix def ++
      (relative: Relative[hierarchy.Forbidden])(using pathBounds: PathBounds[PathType2])
      : {pathBounds} PathType2 =
    val base: {pathBounds} PathType2 = ancestor(relative.ascent)
    hierarchy.remake(base, relative.elements ::: elements)
