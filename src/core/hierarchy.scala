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

object Serpentine:
  opaque type PathName[NameType <: Label] = String

  object PathName:
    given [NameType <: Label]: Show[PathName[NameType]] = Text(_)

    inline def apply[NameType <: Label](text: Text): PathName[NameType] =
      ${SerpentineMacros.runtimeParse[NameType]('text)}

  extension [NameType <: Label](pathName: PathName[NameType])
    def render: Text = Text(pathName)

export Serpentine.*

case class PathError(reason: PathError.Reason)
extends Error(err"the path is invalid because ${reason.show}")

@targetName("root")
def %
    [PathType <: Matchable]
    (using hierarchy: Hierarchy[PathType, ?])
    (using mainRoot: MainRoot[PathType])
    : PathType =
  mainRoot.empty()

@targetName("relative")
def ?
    [PathType <: Matchable, LinkType <: Matchable, NameType <: Label]
    (using hierarchy: Hierarchy[PathType, LinkType])
    (using pathlike: Followable[LinkType, NameType, ?, ?, ?])
    : LinkType =
  pathlike.make(0, Nil)

@targetName("relativeParent")
def ?^
    [PathType <: Matchable, LinkType <: Matchable, NameType <: Label]
    (using hierarchy: Hierarchy[PathType, LinkType])
    (using pathlike: Followable[LinkType, NameType, ?, ?, ?])
    : LinkType =
  pathlike.make(1, Nil)

@targetName("relativeParent2")
def ?^^
    [PathType <: Matchable, LinkType <: Matchable, NameType <: Label]
    (using hierarchy: Hierarchy[PathType, LinkType])
    (using pathlike: Followable[LinkType, NameType, ?, ?, ?])
    : LinkType =
  pathlike.make(2, Nil)

@targetName("relativeParent3")
def ?^^^
    [PathType <: Matchable, LinkType <: Matchable, NameType <: Label]
    (using hierarchy: Hierarchy[PathType, LinkType])
    (using pathlike: Followable[LinkType, NameType, ?, ?, ?])
    : LinkType =
  pathlike.make(3, Nil)

erased trait Hierarchy[PathType <: Matchable, LinkType <: Matchable]
  
extension
    [PathType <: Matchable, LinkType <: Matchable, NameType <: Label]
    (left: LinkType)
    (using hierarchy: Hierarchy[PathType, LinkType])
  
  def ascent(using pathlike: Followable[LinkType, NameType, ?, ?, ?]): Int =
    pathlike.ascent(left)

  @targetName("relativeKeep")
  def keep
      (n: Int)
      (using pathlike: Followable[LinkType, NameType, ?, ?, ?])
      : LinkType =
    pathlike.make(pathlike.ascent(left), left.descent.takeRight(n))

extension
    [PathType <: Matchable, LinkType <: Matchable, NameType <: Label]
    (left: PathType)
    (using hierarchy: Hierarchy[PathType, LinkType])
  
  def root(using pathlike: Reachable[PathType, NameType, ?]): pathlike.Root =
    pathlike.root(left)
  
  def relativeTo
      (right: PathType)
      (using pathlike: Followable[LinkType, NameType, ?, ?, ?])
      (using reachable: Reachable[PathType, NameType, ?])
      : LinkType =
    
    val common = left.conjunction(right).depth
    pathlike.make(left.depth - common, right.descent.dropRight(common))
  
  def keep
      (n: Int)(using pathlike: Reachable[PathType, NameType, ?])
      : PathType =
    pathlike.make(pathlike.root(left), left.descent.takeRight(n))
    
  def conjunction
      (right: PathType)(using pathlike: Reachable[PathType, NameType, ?])
      : PathType =
    
    lazy val leftElements: IArray[Text] = IArray.from(left.descent.reverse.map(_.render))
    lazy val rightElements: IArray[Text] = IArray.from(right.descent.reverse.map(_.render))
    
    @tailrec
    def count(n: Int): Int =
      if leftElements.length > n && rightElements.length > n && leftElements(n) == rightElements(n)
      then count(n + 1)
      else n
    
    pathlike.make(pathlike.root(left), left.descent.takeRight(count(0)))
 
  def precedes
      (path: PathType)(using pathlike: Reachable[PathType, NameType, ?])
      : Boolean =
    left.conjunction(path).descent == left.descent && pathlike.root(path) == pathlike.root(left)

  @targetName("plus")
  def ++
      (relative: LinkType)
      (using reachable: Reachable[PathType, NameType, ?])
      (using followable: Followable[LinkType, NameType, ?, ?, ?])
      : PathType throws PathError =
    if followable.ascent(relative) > left.depth
    then throw PathError(PathError.Reason.ParentOfRoot)
    else
      val common: PathType =
        reachable.ancestor(left, followable.ascent(relative)).avow
      
      val descent = reachable.descent(common)
      
      reachable.make(reachable.root(left), relative.descent ::: descent)

trait Pathlike
    [DirectionType <: Matchable, NameType <: Label, SeparatorType <: Label]
    (using ValueOf[SeparatorType]):

  val separator: Text = Text(summon[ValueOf[SeparatorType]].value)
  def child(path: DirectionType, name: PathName[NameType]): DirectionType
  def descent(path: DirectionType): List[PathName[NameType]]
  def render(path: DirectionType): Text

trait MainRoot[DirectionType <: Matchable]:
  def empty(): DirectionType

abstract class ParsableReachable
    [DirectionType <: Matchable, NameType <: Label, SeparatorType <: Label]
    (using ValueOf[SeparatorType])
extends Reachable[DirectionType, NameType, SeparatorType]:
  type Root
  def parseRoot(text: Text): Maybe[(Root, Text)]
  
  inline def parse(text: Text)(using path: CanThrow[PathError]): DirectionType^{path} =
    val (root, rest) = parseRoot(text).or(throw PathError(PathError.Reason.NotRooted))
    
    val names = rest.cut(separator).reverse match
      case t"" :: tail => tail
      case names       => names

    make(root, names.map(PathName(_)))

trait Reachable
    [DirectionType <: Matchable, NameType <: Label, SeparatorType <: Label]
    (using ValueOf[SeparatorType])
extends Pathlike[DirectionType, NameType, SeparatorType]:
  type Root
  def prefix(root: Root): Text
  def root(path: DirectionType): Root
  def make(root: Root, descent: List[PathName[NameType]]): DirectionType
  
  def child(path: DirectionType, name: PathName[NameType]): DirectionType =
    make(root(path), name :: descent(path))
  
  def ancestor(path: DirectionType, n: Int): Maybe[DirectionType] =
    if descent(path).length < n then Unset else make(root(path), descent(path).drop(n))
  
  def parent(path: DirectionType): Maybe[DirectionType] = ancestor(path, 1)
  
  def render(path: DirectionType): Text =
    t"${prefix(root(path))}${descent(path).reverse.map(_.render).join(separator)}"
  
trait Followable
    [DirectionType <: Matchable, NameType <: Label, SeparatorType <: Label, ParentRefType <: Label,
        SelfRefType <: Label]
    (using ValueOf[SeparatorType], ValueOf[ParentRefType], ValueOf[SelfRefType])
extends Pathlike[DirectionType, NameType, SeparatorType]:
  val parentRef: Text = Text(summon[ValueOf[ParentRefType]].value)
  val selfRef: Text = Text(summon[ValueOf[SelfRefType]].value)
  def ascent(path: DirectionType): Int
  def make(ascent: Int, descent: List[PathName[NameType]]): DirectionType
  def parent(path: DirectionType): DirectionType = ancestor(path, 1)

  def ancestor(path: DirectionType, n: Int): DirectionType =
    val depth = descent(path).length
    val descent2 = descent(path).drop(n)
    make(ascent(path) + (if n > depth then n - depth else 0), descent2)
  
  def child(path: DirectionType, name: PathName[NameType]): DirectionType =
    make(ascent(path), name :: descent(path))
  
  def render(path: DirectionType): Text =
    val prefix = t"${t"$parentRef$separator"*(ascent(path))}"
    
    if descent(path).isEmpty then
      if ascent(path) == 0 then selfRef
      else t"${t"$parentRef$separator"*(ascent(path) - 1)}$parentRef"
    else t"$prefix${descent(path).reverse.map(_.render).join(separator)}"

  inline def parse(text: Text)(using path: CanThrow[PathError]): DirectionType^{path} =
    val ascentPrefix: Text = t"$parentRef$separator"
    
    def recur(text: Text, ascent: Int = 0): DirectionType =
      if text.starts(ascentPrefix) then recur(text.drop(ascentPrefix.length), ascent + 1)
      else if text == parentRef then make(ascent + 1, Nil)
      else
        val names = text.cut(separator).reverse match
          case t"" :: tail => tail
          case names       => names
        
        make(ascent, names.map(PathName(_)))
    
    if text == selfRef then make(0, Nil) else recur(text)

extension
    [DirectionType <: Matchable, NameType <: Label]
    (path: DirectionType)
    (using pathlike: Pathlike[DirectionType, NameType, ?])
  
  @targetName("child")
  infix def /(name: PathName[NameType]): DirectionType = pathlike.child(path, name)
  
  def descent: List[PathName[NameType]] = pathlike.descent(path)
  def depth: Int = descent.length
  def render: Text = pathlike.render(path)

  transparent inline def parent: Maybe[DirectionType] = compiletime.summonFrom:
    case pathlike: Reachable[DirectionType, NameType, ?] => pathlike.parent(path)
    case pathlike: Followable[DirectionType, NameType, ?, ?, ?]    => pathlike.parent(path)
  
  transparent inline def ancestor(n: Int): Maybe[DirectionType] = compiletime.summonFrom:
    case pathlike: Reachable[DirectionType, NameType, ?] => pathlike.ancestor(path, n)
    case pathlike: Followable[DirectionType, NameType, ?, ?, ?]    => pathlike.ancestor(path, n)

extension (inline context: StringContext)
  inline def p[NameType <: Label](): PathName[NameType] =
    ${SerpentineMacros.parse[NameType]('context)}
