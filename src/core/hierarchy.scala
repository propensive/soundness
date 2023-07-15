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
    case NotRooted(path: Text)

  given Show[Reason] =
    case Reason.InvalidChar(char)     => t"the character $char may not appear in a path name"
    case Reason.InvalidPrefix(prefix) => t"the path name cannot begin with $prefix"
    case Reason.InvalidSuffix(suffix) => t"the path name cannot end with $suffix"
    case Reason.InvalidName(name)     => t"the name $name is not valid"
    case Reason.ParentOfRoot          => t"the root has no parent"
    case Reason.NotRooted(path)       => t"${path} is not rooted"

export Serpentine.PathName

case class PathError(reason: PathError.Reason)
extends Error(msg"the path is invalid because ${reason.show}")

object `%`:

  erased given
      [PathType <: Matchable, LinkType <: Matchable]
      (using erased hierarchy: Hierarchy[PathType, LinkType])
      : Hierarchy[%.type, LinkType] = ###

  def precedes
      [PathType <: Matchable]
      (using erased hierarchy: Hierarchy[PathType, ?])
      (path: PathType)
      : Boolean =
    true

  given
      [PathType <: Matchable, LinkType <: Matchable, NameType <: Label, RootType]
      (using erased hierarchy: Hierarchy[PathType, LinkType])
      (using reachable: Reachable[PathType, NameType, RootType])
      (using mainRoot: MainRoot[PathType])
      : Reachable[%.type, NameType, RootType] =
    new Reachable[%.type, NameType, RootType]:
      def separator(path: %.type): Text = reachable.separator(mainRoot.empty())
      def prefix(root: RootType): Text = reachable.prefix(reachable.root(mainRoot.empty()))
      def root(path: %.type): RootType = reachable.root(mainRoot.empty())
      def descent(path: %.type): List[PathName[NameType]] = Nil
  
  given
      [PathType <: Matchable]
      (using hierarchy: Hierarchy[PathType, ?])
      (using mainRoot: MainRoot[PathType], show: Show[PathType]): Show[%.type] = root =>
    mainRoot.empty().show
    

  @targetName("child")
  def /
      [PathType <: Matchable, NameType <: Label, AscentType]
      (using hierarchy: Hierarchy[PathType, ?])
      (using mainRoot: MainRoot[PathType])
      (using pathlike: Pathlike[PathType, NameType, AscentType])
      (name: PathName[NameType])
      (using creator: PathCreator[PathType, NameType, AscentType])
      : PathType =
    mainRoot.empty() / name

@targetName("relative")
def ?
    [PathType <: Matchable, LinkType <: Matchable, NameType <: Label]
    (using hierarchy: Hierarchy[PathType, LinkType], creator: PathCreator[LinkType, NameType, Int])
    : LinkType =
  creator.path(0, Nil)

@targetName("relativeParent")
def ?^
    [PathType <: Matchable, LinkType <: Matchable, NameType <: Label]
    (using hierarchy: Hierarchy[PathType, LinkType], creator: PathCreator[LinkType, NameType, Int])
    : LinkType =
  creator.path(1, Nil)

@targetName("relativeParent2")
def ?^^
    [PathType <: Matchable, LinkType <: Matchable, NameType <: Label]
    (using hierarchy: Hierarchy[PathType, LinkType], creator: PathCreator[LinkType, NameType, Int])
    : LinkType =
  creator.path(2, Nil)

@targetName("relativeParent3")
def ?^^^
    [PathType <: Matchable, LinkType <: Matchable, NameType <: Label]
    (using hierarchy: Hierarchy[PathType, LinkType], creator: PathCreator[LinkType, NameType, Int])
    : LinkType =
  creator.path(3, Nil)

erased trait Hierarchy[PathType <: Matchable, LinkType <: Matchable]
  
extension
    [PathType <: Matchable, LinkType <: Matchable, NameType <: Label]
    (left: LinkType)
  
  def ascent(using followable: Followable[LinkType, NameType, ?, ?]): Int =
    followable.ascent(left)

extension
    [PathType <: Matchable, LinkType <: Matchable, NameType <: Label, RootType]
    (left: PathType)
    (using hierarchy: Hierarchy[PathType, LinkType])
  
  def root(using pathlike: Reachable[PathType, NameType, RootType]): RootType = pathlike.root(left)
  
  def relativeTo
      [PathType2 <: PathType]
      (right: PathType)
      (using pathlike: Followable[LinkType, NameType, ?, ?])
      (using reachable: Reachable[PathType, NameType, RootType],
          pathCreator: PathCreator[PathType, NameType, RootType],
          linkCreator: PathCreator[LinkType, NameType, Int],
          reachable2: Reachable[PathType2, NameType, RootType])
      : LinkType =
    
    val common = reachable.depth(left.conjunction(right))
    linkCreator.path(reachable.depth(left) - common, reachable.descent(right).dropRight(common))
  
  def keep
      (n: Int)
      (using reachable: Reachable[PathType, NameType, RootType],
          creator: PathCreator[PathType, NameType, RootType])
      : PathType =
    creator.path(reachable.root(left), reachable.descent(left).takeRight(n))
    
  def conjunction
      (right: PathType)
      (using reachable: Reachable[PathType, NameType, RootType],
          creator: PathCreator[PathType, NameType, RootType])
      : PathType =
    
    lazy val leftElements: IArray[Text] = IArray.from(reachable.descent(left).reverse.map(_.render))
    
    lazy val rightElements: IArray[Text] =
      IArray.from(reachable.descent(right).reverse.map(_.render))
    
    @tailrec
    def count(n: Int): Int =
      if leftElements.length > n && rightElements.length > n && leftElements(n) == rightElements(n)
      then count(n + 1)
      else n
    
    creator.path(reachable.root(left), reachable.descent(left).takeRight(count(0)))

  def precedes(path: %.type): Boolean = false
  
  def precedes
      (path: PathType)
      (using reachable: Reachable[PathType, NameType, RootType],
          creator: PathCreator[PathType, NameType, RootType])
      : Boolean =
    reachable.descent(left.conjunction(path)) == reachable.descent(left) &&
      reachable.root(path) == reachable.root(left)

  @targetName("plus")
  def ++
      (relative: LinkType)
      (using reachable: Reachable[PathType, NameType, RootType], path: CanThrow[PathError])
      (using followable: Followable[LinkType, NameType, ?, ?],
          creator: PathCreator[PathType, NameType, RootType])
      : PathType^{reachable, followable, creator, path} =
    if followable.ascent(relative) > reachable.depth(left)
    then throw PathError(PathError.Reason.ParentOfRoot)
    else
      val common: PathType = reachable.ancestor(left, followable.ascent(relative)).avow(using Unsafe)
      val descent = reachable.descent(common)
      
      creator.path(reachable.root(left), followable.descent(relative) ::: descent)

trait Pathlike[-PathType <: Matchable, NameType <: Label, AscentType]:
  def separator(path: PathType): Text
  def descent(path: PathType): List[PathName[NameType]]
  def render(path: PathType): Text
  def ascent(path: PathType): AscentType
  
  def ancestor
      [PathType2 <: PathType]
      (path: PathType, n: Int)
      (using creator: PathCreator[PathType2, NameType, AscentType])
      : Maybe[PathType2]
  
  def parent
      [PathType2 <: PathType]
      (path: PathType)
      (using creator: PathCreator[PathType2, NameType, AscentType])
      : Maybe[PathType2] =
    ancestor(path, 1)

  def child
      [PathType2 <: PathType]
      (path: PathType, name: PathName[NameType])
      (using creator: PathCreator[PathType2, NameType, AscentType])
      : PathType2 =
    creator.path(ascent(path), name :: descent(path))

trait MainRoot[PathType <: Matchable]:
  def empty(): PathType

trait RootParser[PathType <: Matchable, +RootType]:
  def parse(text: Text): Maybe[(RootType, Text)]
  
@capability
trait PathCreator[+PathType <: Matchable, NameType <: Label, AscentType]:
  def path(ascent: AscentType, descent: List[PathName[NameType]]): PathType

object Reachable:
  inline def decode
      [PathType <: Matchable]
      (text: Text)
      (using path: CanThrow[PathError])
      [NameType <: Label, RootType]
      (using reachable: Reachable[PathType, NameType, RootType],
          rootParser: RootParser[PathType, RootType],
          creator: PathCreator[PathType, NameType, RootType])
      : PathType =
    val (root, rest) = rootParser.parse(text).or(throw PathError(PathError.Reason.NotRooted(text)))
    
    val names = rest.cut(reachable.separator(creator.path(root, Nil))).reverse match
      case t"" :: tail => tail
      case names       => names

    creator.path(root, names.map(PathName(_)))
  

@capability
trait Reachable[-PathType <: Matchable, NameType <: Label, RootType]
extends Pathlike[PathType, NameType, RootType]:
  def separator(path: PathType): Text
  def prefix(root: RootType): Text
  def root(path: PathType): RootType
  def depth(path: PathType): Int = descent(path).length
  def ascent(path: PathType): RootType = root(path)
  
  def render(path: PathType): Text =
    t"${prefix(root(path))}${descent(path).reverse.map(_.render).join(separator(path))}"
  
  def ancestor
      [PathType2 <: PathType]
      (path: PathType, n: Int)
      (using creator: PathCreator[PathType2, NameType, RootType])
      : Maybe[PathType2] =
    if descent(path).length < n then Unset else creator.path(root(path), descent(path).drop(n))

object Followable:
  inline def decoder
      [LinkType <: Matchable]
      (using path: CanThrow[PathError])
      [NameType <: Label, ParentRefType <: Label, SelfRefType <: Label]
      (using followable: Followable[LinkType, NameType, ParentRefType, SelfRefType],
          creator: PathCreator[LinkType, NameType, Int]): Decoder[LinkType] =
    new Decoder[LinkType]:
      def decode(text: Text): LinkType =
        import followable.*
        
        val foundSeparator: Char = unsafely(text.where(separators.contains(_)).mm(text(_))).or('/')
        val ascentPrefix: Text = t"$parentRef$foundSeparator"
        
        def recur(text: Text, ascent: Int = 0): LinkType =
          if text.starts(ascentPrefix) then recur(text.drop(ascentPrefix.length), ascent + 1)
          else if text == parentRef then creator.path(ascent + 1, Nil)
          else
            val names = text.cut(foundSeparator).reverse match
              case t"" :: tail => tail
              case names       => names
            
            creator.path(ascent, names.map(PathName(_)))
        
        if text == selfRef then creator.path(0, Nil) else recur(text)
    

@capability  
trait Followable
    [-LinkType <: Matchable, NameType <: Label, ParentRefType <: Label, SelfRefType <: Label]
    (using ValueOf[ParentRefType], ValueOf[SelfRefType])
extends Pathlike[LinkType, NameType, Int]:
  val parentRef: Text = Text(summon[ValueOf[ParentRefType]].value)
  val selfRef: Text = Text(summon[ValueOf[SelfRefType]].value)
  def separators: Set[Char]
  def ascent(path: LinkType): Int
  
  def ancestor
      [LinkType2 <: LinkType]
      (link: LinkType, n: Int)
      (using creator: PathCreator[LinkType2, NameType, Int])
      : LinkType2 =
    val depth = descent(link).length
    creator.path(ascent(link) + (if n > depth then n - depth else 0), descent(link).drop(n))
  
  override def parent
      [LinkType2 <: LinkType]
      (path: LinkType)
      (using creator: PathCreator[LinkType2, NameType, Int])
      : LinkType2 =
    ancestor(path, 1)

  
  def render(path: LinkType): Text =
    val prefix = t"${t"$parentRef${separator(path)}"*(ascent(path))}"
    
    if descent(path).isEmpty then
      if ascent(path) == 0 then selfRef
      else t"${t"$parentRef${separator(path)}"*(ascent(path) - 1)}$parentRef"
    else t"$prefix${descent(path).reverse.map(_.render).join(separator(path))}"

extension
    [PathType <: Matchable, NameType <: Label, AscentType]
    (path: PathType)
    (using pathlike: Pathlike[PathType, NameType, AscentType])
    (using creator: PathCreator[PathType, NameType, AscentType])
  
  @targetName("child")
  infix def /[PathType2 <: PathType](name: PathName[NameType]): PathType =
    pathlike.child(path, name)

  // FIXME: This should be called `/`, but it causes a spurious compiler error. 
  @targetName("child2")
  inline infix def /-[PathType2 <: PathType](name: Text): PathType throws PathError =
    pathlike.child(path, PathName(name))
  
  def render: Text = pathlike.render(path)
  def descent: List[PathName[NameType]] = pathlike.descent(path)
  def depth: Int = pathlike.descent(path).length

  transparent inline def parent: Maybe[PathType] = pathlike.parent(path)
  transparent inline def ancestor(n: Int): Maybe[PathType] = pathlike.ancestor(path, n)

extension (inline context: StringContext)
  inline def p[NameType <: Label](): PathName[NameType] = ${Serpentine.parse[NameType]('context)}
