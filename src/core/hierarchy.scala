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
import fulminate.*
import spectacular.*
import anticipation.*
import gossamer.*
import contingency.*
import symbolism.*

import scala.quoted.*
import scala.compiletime.*
import scala.reflect.*

//import language.experimental.captureChecking

object PathError:
  enum Reason:
    case InvalidChar(char: Char)
    case InvalidPrefix(prefix: Text)
    case InvalidSuffix(suffix: Text)
    case InvalidName(name: Text)
    case ParentOfRoot
    case NotRooted

  given Communicable[Reason] =
    case Reason.InvalidChar(char)     => msg"the character $char may not appear in its name"
    case Reason.InvalidPrefix(prefix) => msg"its name cannot begin with $prefix"
    case Reason.InvalidSuffix(suffix) => msg"its name cannot end with $suffix"
    case Reason.InvalidName(name)     => msg"the name $name is not valid"
    case Reason.ParentOfRoot          => msg"it has no parent"
    case Reason.NotRooted             => msg"it is not rooted"

export Serpentine.PathName

case class PathError(path: Text, reason: PathError.Reason)
extends Error(msg"the path $path is invalid because $reason")

@targetName("relative")
def ? [PathType <: Matchable, LinkType <: Matchable, NameType <: Label]
    (using hierarchy: Hierarchy[PathType, LinkType], creator: PathCreator[LinkType, NameType, Int])
        : LinkType =

  creator.path(0, Nil)

@targetName("relativeParent")
def ?^ [PathType <: Matchable, LinkType <: Matchable, NameType <: Label]
    (using hierarchy: Hierarchy[PathType, LinkType], creator: PathCreator[LinkType, NameType, Int])
        : LinkType =

  creator.path(1, Nil)

@targetName("relativeParent2")
def ?^^ [PathType <: Matchable, LinkType <: Matchable, NameType <: Label]
    (using hierarchy: Hierarchy[PathType, LinkType], creator: PathCreator[LinkType, NameType, Int])
        : LinkType =
  creator.path(2, Nil)

@targetName("relativeParent3")
def ?^^^ [PathType <: Matchable, LinkType <: Matchable, NameType <: Label]
    (using hierarchy: Hierarchy[PathType, LinkType], creator: PathCreator[LinkType, NameType, Int])
        : LinkType =
  creator.path(3, Nil)

erased trait Hierarchy[PathType <: Matchable, LinkType <: Matchable]
  
extension [PathType <: Matchable, LinkType <: Matchable, NameType <: Label](left: LinkType)
    (using followable: Followable[LinkType, NameType, ?, ?])

  def ascent: Int = followable.ascent(left)

  def inWorkingDirectory[RootType](using hierarchy: Hierarchy[PathType, LinkType])
      (using WorkingDirectory,
             Directional[PathType, NameType, RootType],
             SpecificPath[PathType],
             PathCreator[PathType, NameType, RootType],
             Raises[PathError])
          : PathType =
    workingDirectory + left

extension [PathType <: Matchable, LinkType <: Matchable, NameType <: Label, RootType](left: PathType)
    (using hierarchy: Hierarchy[PathType, LinkType])
  
  def root(using directional: Navigable[PathType, NameType, RootType]): RootType = directional.root(left)

  @targetName("add")
  infix def + (link: LinkType)
      (using Directional[PathType, NameType, RootType],
             PathCreator[PathType, NameType, RootType],
             Followable[LinkType, NameType, ?, ?],
             Raises[PathError])
          : PathType =

    left.append(link)
  
  def relativeTo[PathType2 <: PathType](right: PathType)
      (using directional: Followable[LinkType, NameType, ?, ?],
             navigable:   Navigable[PathType, NameType, RootType],
             pathCreator: PathCreator[PathType, NameType, RootType],
             linkCreator: PathCreator[LinkType, NameType, Int],
             navigable2:  Navigable[PathType2, NameType, RootType])
      : LinkType =
    
    val common = navigable.depth(left.conjunction(right))
    linkCreator.path(navigable.depth(left) - common, navigable.descent(right).dropRight(common))
  
extension[PathType <: Matchable, NameType <: Label, RootType](left: PathType)
  def keep(n: Int)
      (using navigable: Navigable[PathType, NameType, RootType],
             creator:   PathCreator[PathType, NameType, RootType])
      : PathType =

    creator.path(navigable.root(left), navigable.descent(left).takeRight(n))
    
  def conjunction(right: PathType)
      (using navigable: Navigable[PathType, NameType, RootType],
             creator:   PathCreator[PathType, NameType, RootType])
          : PathType =
    
    lazy val leftElements: IArray[Text] = IArray.from(navigable.descent(left).reverse.map(_.render))
    lazy val rightElements: IArray[Text] = IArray.from(navigable.descent(right).reverse.map(_.render))
    
    @tailrec
    def count(n: Int): Int =
      if leftElements.length > n && rightElements.length > n && leftElements(n) == rightElements(n)
      then count(n + 1)
      else n
    
    creator.path(navigable.root(left), navigable.descent(left).takeRight(count(0)))

  def precedes(path: %.type): Boolean = false
  
  def precedes(path: PathType)
      (using navigable: Navigable[PathType, NameType, RootType],
             creator:   PathCreator[PathType, NameType, RootType])
          : Boolean =

    navigable.descent(left.conjunction(path)) == navigable.descent(left) &&
      navigable.root(path) == navigable.root(left)

trait Directional[-PathType <: Matchable, NameType <: Label, AscentType]:
  def separator(path: PathType): Text
  def descent(path: PathType): List[PathName[NameType]]
  def render(path: PathType): Text
  def ascent(path: PathType): AscentType
  
  def ancestor[PathType2 <: PathType](path: PathType, n: Int)
      (using creator: PathCreator[PathType2, NameType, AscentType])
          : Optional[PathType2]
  
  def parent[PathType2 <: PathType](path: PathType)
      (using creator: PathCreator[PathType2, NameType, AscentType])
          : Optional[PathType2] =
    ancestor(path, 1)

  def child[PathType2 <: PathType](path: PathType, name: PathName[NameType])
      (using creator: PathCreator[PathType2, NameType, AscentType])
          : PathType2 =
    creator.path(ascent(path), name :: descent(path))

trait MainRoot[PathType <: Matchable]:
  def empty(): PathType

trait RootParser[PathType <: Matchable, +RootType]:
  def parse(text: Text): Optional[(RootType, Text)]
  
@capability
trait PathCreator[+PathType <: Matchable, NameType <: Label, AscentType]:
  def path(ascent: AscentType, descent: List[PathName[NameType]]): PathType

object Navigable:
  inline def decode[PathType <: Matchable](text: Text)[NameType <: Label, RootType]
      (using navigable:  Navigable[PathType, NameType, RootType],
             rootParser: RootParser[PathType, RootType],
             creator:    PathCreator[PathType, NameType, RootType])
      (using path: Raises[PathError])
      : PathType =
    val rootRest: Optional[(RootType, Text)] = rootParser.parse(text)
    if rootRest.absent
    then raise(PathError(text, PathError.Reason.NotRooted)):
      creator.path(summonInline[Default[RootType]](), Nil)
    else
      // FIXME: The casts below avoid an error in the compiler which just prints an AST without explanation
      val root: RootType = rootRest.asInstanceOf[(RootType, Text)](0)
      val rest: Text = rootRest.asInstanceOf[(RootType, Text)](1)
      
      val names = rest.cut(navigable.separator(creator.path(root, Nil))).reverse match
        case t"" :: tail => tail
        case names       => names
  
      creator.path(root, names.map(PathName(_)))
  

@capability
trait Navigable[-PathType <: Matchable, NameType <: Label, RootType]
extends Directional[PathType, NameType, RootType]:
  def separator(path: PathType): Text
  def prefix(root: RootType): Text
  def root(path: PathType): RootType
  def depth(path: PathType): Int = descent(path).length
  def ascent(path: PathType): RootType = root(path)
  
  def render(path: PathType): Text =
    t"${prefix(root(path))}${descent(path).reverse.map(_.render).join(separator(path))}"
  
  def ancestor[PathType2 <: PathType](path: PathType, n: Int)
      (using creator: PathCreator[PathType2, NameType, RootType])
          : Optional[PathType2] =
    if descent(path).length < n then Unset else creator.path(root(path), descent(path).drop(n))

object Followable:
  def add[LinkType <: Matchable, NameType <: Label]
      (using creator: PathCreator[LinkType, NameType, Int], followable: Followable[LinkType, NameType, ?, ?])
          : AddOperator[LinkType, LinkType]/*^{followable, creator}*/ =
    new AddOperator[LinkType, LinkType]:
      type Result = LinkType

      def add(left: LinkType, right: LinkType): LinkType =
        val ascent2 =
          if followable.descent(left).length < followable.ascent(right)
          then followable.ascent(left) + followable.ascent(right) - followable.descent(left).length
          else followable.ascent(left)
      
        val descent2 =
          followable.descent(right) ++ followable.descent(left).drop(followable.ascent(right))

        creator.path(ascent2, descent2)

  inline def decoder[LinkType <: Matchable](using path: Raises[PathError])
      [NameType <: Label, ParentRefType <: Label, SelfRefType <: Label]
      (using followable: Followable[LinkType, NameType, ParentRefType, SelfRefType],
             creator: PathCreator[LinkType, NameType, Int])
          : Decoder[LinkType] =

    new Decoder[LinkType]:
      def decode(text: Text): LinkType =
        import followable.*
        
        val foundSeparator: Char = unsafely(text.where(separators.contains(_)).let(text(_))).or('/')
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
trait Followable[-LinkType <: Matchable, NameType <: Label, ParentRefType <: Label, SelfRefType <: Label]
    (using ValueOf[ParentRefType], ValueOf[SelfRefType])
extends Directional[LinkType, NameType, Int]:
  val parentRef: Text = Text(summon[ValueOf[ParentRefType]].value)
  val selfRef: Text = Text(summon[ValueOf[SelfRefType]].value)
  def separators: Set[Char]
  def ascent(path: LinkType): Int
  
  def ancestor[LinkType2 <: LinkType](link: LinkType, n: Int)
      (using creator: PathCreator[LinkType2, NameType, Int])
          : LinkType2 =

    val depth = descent(link).length
    creator.path(ascent(link) + (if n > depth then n - depth else 0), descent(link).drop(n))
  
  override def parent[LinkType2 <: LinkType](path: LinkType)
      (using creator: PathCreator[LinkType2, NameType, Int])
          : LinkType2 =

    ancestor(path, 1)

  
  def render(path: LinkType): Text =
    val prefix = t"${t"$parentRef${separator(path)}"*(ascent(path))}"
    
    if descent(path).isEmpty then
      if ascent(path) == 0 then selfRef
      else t"${t"$parentRef${separator(path)}"*(ascent(path) - 1)}$parentRef"
    else t"$prefix${descent(path).reverse.map(_.render).join(separator(path))}"

implicit class Slash[PathType <: Matchable](path: PathType):
  @targetName("child")
  infix def / [NameType <: Label, AscentType](using directional: Directional[PathType, NameType, AscentType])
      (name: PathName[NameType])
      (using creator: PathCreator[PathType, NameType, AscentType])
          : PathType =
    directional.child(path, name)

extension [PathType <: Matchable, NameType <: Label, AscentType](path: PathType)
    (using directional: Directional[PathType, NameType, AscentType],
           creator:     PathCreator[PathType, NameType, AscentType])
  
  // @targetName("child")
  // infix def /[PathType2 <: PathType](name: PathName[NameType]): PathType =
  //   directional.child(path, name)

  // FIXME: This should be called `/`, but it causes a spurious compiler error. 
  @targetName("child2")
  inline infix def /- [PathType2 <: PathType](name: Text)(using pathError: Raises[PathError]): PathType =
    directional.child(path, PathName(name))
  
  def render: Text = directional.render(path)
  def descent: List[PathName[NameType]] = directional.descent(path)
  def depth: Int = directional.descent(path).length

  transparent inline def parent: Optional[PathType] = directional.parent(path)
  transparent inline def ancestor(n: Int): Optional[PathType] = directional.ancestor(path, n)
  
  inline def append[LinkType <: Matchable](inline link: LinkType)
      (using followable: Followable[LinkType, NameType, ?, ?], pathHandler: Raises[PathError])
          : PathType =

    if followable.ascent(link) > directional.descent(path).length
    then raise(PathError(path.render, PathError.Reason.ParentOfRoot))(path)
    else
      val common: PathType = directional.ancestor(path, followable.ascent(link)).vouch(using Unsafe)
      val descent = directional.descent(common)
  
      creator.path(directional.ascent(path), followable.descent(link) ::: descent)

trait PExtractor[NameType <: Label]():
  def apply(): PathName[NameType]
  def unapply(name: PathName[NameType]): Boolean

extension (inline context: StringContext)
  inline def p[NameType <: Label]: PExtractor[NameType] =
    ${SerpentineMacro.parse[NameType]('context)}
  
trait PathEquality[PathType <: Matchable](using directional: Directional[PathType, ?, ?])
    (using TypeTest[Any, PathType]):
  this: PathType & Matchable =>
  
  override def equals(other: Any): Boolean = other.asMatchable match
    case `%` =>
      directional.descent(this) == Nil
    
    case other: PathType =>
      directional.descent(other) == directional.descent(this) && directional.ascent(other) == directional.ascent(this)
    
    case other =>
      false
  
  override def hashCode: Int = if directional.descent(this) == Nil then 0 else super.hashCode

export Serpentine.`/`
