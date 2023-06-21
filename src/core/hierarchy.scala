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
import digression.*
import spectacular.*
import gossamer.*

import scala.quoted.*

import language.experimental.captureChecking

object PathError:
  enum Reason:
    case InvalidChar(char: Char)
    case InvalidPrefix(prefix: Text)
    case InvalidSuffix(suffix: Text)
    case InvalidName(name: Text)
    case ParentOfRoot
    case NotRooted

  given Show[Reason] =
    case Reason.InvalidChar(char)     => t"the character $char may not appear in a path name"
    case Reason.InvalidPrefix(prefix) => t"the path name cannot begin with $prefix"
    case Reason.InvalidSuffix(suffix) => t"the path name cannot end with $suffix"
    case Reason.InvalidName(name)     => t"the name $name is not valid"
    case Reason.ParentOfRoot          => t"the root has no parent"
    case Reason.NotRooted             => t"the path is not rooted"

object SerpentineOpaques:
  opaque type PathName[ForbiddenType <: Label] = String

  object PathName:
    inline def apply[ForbiddenType <: Label](text: Text): PathName[ForbiddenType] =
      ${SerpentineMacros.runtimeParse[ForbiddenType]('text)}

  extension [ForbiddenType <: Label](pathName: PathName[ForbiddenType])
    def text: Text = Text(pathName)

export SerpentineOpaques.*

case class PathError(reason: PathError.Reason)
extends Error(err"the path is invalid because ${reason.show}")

@targetName("root")
def %
    [AbsolutePathType <: Matchable]
    (using hierarchy: Hierarchy[AbsolutePathType, ?])
    (using mainRoot: MainRoot[AbsolutePathType])
    : AbsolutePathType =
  mainRoot.empty()

@targetName("relative")
def ?
    [AbsolutePathType <: Matchable, RelativePathType <: Matchable, NameType <: Label]
    (using hierarchy: Hierarchy[AbsolutePathType, RelativePathType])
    (using reachable: RelativeReachable[RelativePathType, NameType])
    : RelativePathType =
  reachable.make(0, Nil)

@targetName("relativeParent")
def ?^
    [AbsolutePathType <: Matchable, RelativePathType <: Matchable, NameType <: Label]
    (using hierarchy: Hierarchy[AbsolutePathType, RelativePathType])
    (using reachable: RelativeReachable[RelativePathType, NameType])
    : RelativePathType =
  reachable.make(1, Nil)

@targetName("relativeParent2")
def ?^^
    [AbsolutePathType <: Matchable, RelativePathType <: Matchable, NameType <: Label]
    (using hierarchy: Hierarchy[AbsolutePathType, RelativePathType])
    (using reachable: RelativeReachable[RelativePathType, NameType])
    : RelativePathType =
  reachable.make(2, Nil)

@targetName("relativeParent3")
def ?^^^
    [AbsolutePathType <: Matchable, RelativePathType <: Matchable, NameType <: Label]
    (using hierarchy: Hierarchy[AbsolutePathType, RelativePathType])
    (using reachable: RelativeReachable[RelativePathType, NameType])
    : RelativePathType =
  reachable.make(3, Nil)

erased trait Hierarchy[AbsolutePathType <: Matchable, RelativePathType <: Matchable]
  
extension
    [AbsolutePathType <: Matchable, RelativePathType <: Matchable, NameType <: Label]
    (left: RelativePathType)
    (using hierarchy: Hierarchy[AbsolutePathType, RelativePathType])
  
  def ascent(using reachable: RelativeReachable[RelativePathType, NameType]): Int =
    reachable.ascent(left)

  @targetName("relativeKeep")
  def keep(n: Int)(using reachable: RelativeReachable[RelativePathType, NameType]): RelativePathType =
    reachable.make(reachable.ascent(left), left.descent.takeRight(n))

extension
    [AbsolutePathType <: Matchable, RelativePathType <: Matchable, NameType <: Label]
    (left: AbsolutePathType)
    (using hierarchy: Hierarchy[AbsolutePathType, RelativePathType])
  
  def root(using reachable: AbsoluteReachable[AbsolutePathType, NameType]): reachable.Root =
    reachable.root(left)
  
  def relativeTo
      (right: AbsolutePathType)
      (using reachable: RelativeReachable[RelativePathType, NameType])
      (using absoluteReachable: AbsoluteReachable[AbsolutePathType, NameType])
      : RelativePathType =
    
    val common = left.conjunction(right).depth
    reachable.make(left.depth - common, right.descent.dropRight(common))
  
  def keep
      (n: Int)(using reachable: AbsoluteReachable[AbsolutePathType, NameType])
      : AbsolutePathType =
    reachable.make(reachable.root(left), left.descent.takeRight(n))
    
  def conjunction
      (right: AbsolutePathType)(using reachable: AbsoluteReachable[AbsolutePathType, NameType])
      : AbsolutePathType =
    
    lazy val leftElements: IArray[Text] = IArray.from(left.descent.reverse.map(_.text))
    lazy val rightElements: IArray[Text] = IArray.from(right.descent.reverse.map(_.text))
    
    @tailrec
    def count(n: Int): Int =
      if leftElements.length > n && rightElements.length > n && leftElements(n) == rightElements(n)
      then count(n + 1)
      else n
    
    reachable.make(reachable.root(left), left.descent.takeRight(count(0)))
 
  def precedes
      (path: AbsolutePathType)(using reachable: AbsoluteReachable[AbsolutePathType, NameType])
      : Boolean =
    left.conjunction(path).descent == left.descent && reachable.root(path) == reachable.root(left)

  @targetName("plus")
  def ++
      (relative: RelativePathType)
      (using absoluteReachable: AbsoluteReachable[AbsolutePathType, NameType])
      (using relativeReachable: RelativeReachable[RelativePathType, NameType])
      : AbsolutePathType throws PathError =
    if relativeReachable.ascent(relative) > left.depth
    then throw PathError(PathError.Reason.ParentOfRoot)
    else
      val common: AbsolutePathType =
        absoluteReachable.ancestor(left, relativeReachable.ascent(relative)).avow
      
      val descent = absoluteReachable.descent(common)
      
      absoluteReachable.make(absoluteReachable.root(left), relative.descent ::: descent)

trait Reachable[PathType <: Matchable, NameType <: Label]:
  def pathSeparator: Text
  def child(path: PathType, name: PathName[NameType]): PathType
  def descent(path: PathType): List[PathName[NameType]]
  def text(path: PathType): Text
  inline def parse(text: Text)(using path: CanThrow[PathError]): PathType^{path}

trait MainRoot[PathType <: Matchable]:
  def empty(): PathType

trait AbsoluteReachable[PathType <: Matchable, NameType <: Label](val pathSeparator: Text)
extends Reachable[PathType, NameType]:

  type Root

  def prefix(root: Root): Text
  def root(path: PathType): Root
  def make(root: Root, descent: List[PathName[NameType]]): PathType
  def parseRoot(text: Text): Maybe[(Root, Text)]
  
  def child(path: PathType, name: PathName[NameType]): PathType =
    make(root(path), name :: descent(path))
  
  def ancestor(path: PathType, n: Int): Maybe[PathType] =
    if descent(path).length < n then Unset else make(root(path), descent(path).drop(n))
  
  def parent(path: PathType): Maybe[PathType] = ancestor(path, 1)
  
  def text(path: PathType): Text =
    t"${prefix(root(path))}${descent(path).reverse.map(_.text).join(pathSeparator)}"
  
  inline def parse(text: Text)(using path: CanThrow[PathError]): PathType^{path} =
    val (root, rest) = parseRoot(text).or(throw PathError(PathError.Reason.NotRooted))
    
    val names = rest.cut(pathSeparator).reverse match
      case t"" :: tail => tail
      case names       => names

    make(root, names.map(PathName(_)))
  
trait RelativeReachable
    [PathType <: Matchable, NameType <: Label]
    (val pathSeparator: Text, val parentRef: Text, val selfRef: Text)
extends Reachable[PathType, NameType]:
  def ascent(path: PathType): Int
  def make(ascent: Int, descent: List[PathName[NameType]]): PathType
  
  def parent(path: PathType): PathType = ancestor(path, 1)

  def ancestor(path: PathType, n: Int): PathType =
    val depth = descent(path).length
    val descent2 = descent(path).drop(n)
    make(ascent(path) + (if n > depth then n - depth else 0), descent2)
  
  def child(path: PathType, name: PathName[NameType]): PathType =
    make(ascent(path), name :: descent(path))
  
  def text(path: PathType): Text =
    val prefix = t"${t"$parentRef$pathSeparator"*(ascent(path))}"
    
    if descent(path).isEmpty then
      if ascent(path) == 0 then selfRef
      else t"${t"$parentRef$pathSeparator"*(ascent(path) - 1)}$parentRef"
    else t"$prefix${descent(path).reverse.map(_.text).join(pathSeparator)}"

  inline def parse(text: Text)(using path: CanThrow[PathError]): PathType^{path} =
    val ascentPrefix: Text = t"$parentRef$pathSeparator"
    
    def recur(text: Text, ascent: Int = 0): PathType =
      if text.starts(ascentPrefix) then recur(text.drop(ascentPrefix.length), ascent + 1)
      else if text == parentRef then make(ascent + 1, Nil)
      else
        val names = text.cut(pathSeparator).reverse match
          case t"" :: tail => tail
          case names       => names
        
        make(ascent, names.map(PathName(_)))
    
    if text == selfRef then make(0, Nil) else recur(text)
  
extension
    [PathType <: Matchable, NameType <: Label]
    (path: PathType)
    (using reachable: Reachable[PathType, NameType])
  
  @targetName("child")
  infix def /(name: PathName[NameType]): PathType = reachable.child(path, name)
  
  def descent: List[PathName[NameType]] = reachable.descent(path)
  def depth: Int = descent.length
  def text: Text = reachable.text(path)

  transparent inline def parent: Maybe[PathType] = compiletime.summonFrom:
    case reachable: AbsoluteReachable[PathType, NameType] => reachable.parent(path)
    case reachable: RelativeReachable[PathType, NameType] => reachable.parent(path)
  
  transparent inline def ancestor(n: Int): Maybe[PathType] = compiletime.summonFrom:
    case reachable: AbsoluteReachable[PathType, NameType] => reachable.ancestor(path, n)
    case reachable: RelativeReachable[PathType, NameType] => reachable.ancestor(path, n)

extension (inline context: StringContext)
  inline def p[ForbiddenType <: Label](): PathName[ForbiddenType] =
    ${SerpentineMacros.parse[ForbiddenType]('context)}