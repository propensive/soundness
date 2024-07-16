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
import anticipation.*
import contingency.*

import scala.compiletime.*
import scala.reflect.*

import language.experimental.pureFunctions

package pathHierarchies:
  erased given Hierarchy[SimplePath, SimpleLink] as simple = ###

val Root = %

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

extension [PathType <: Matchable, LinkType <: Matchable, NameType <: Label](left: LinkType)
    (using followable: LinkType is Followable[NameType, ?, ?])

  def ascent: Int = followable.ascent(left)

  def inWorkingDirectory[RootType](using hierarchy: Hierarchy[PathType, LinkType])
      (using WorkingDirectory,
             PathType is Directional[NameType, RootType],
             PathType is SpecificPath,
             PathCreator[PathType, NameType, RootType],
             Tactic[PathError])
          : PathType =
    workingDirectory + left

extension [PathType <: Matchable, LinkType <: Matchable, NameType <: Label, RootType](left: PathType)
    (using hierarchy: Hierarchy[PathType, LinkType])

  def root(using directional: PathType is Navigable[NameType, RootType]): RootType = directional.root(left)

  @targetName("add")
  infix def + (link: LinkType)
      (using PathType is Directional[NameType, RootType],
             PathCreator[PathType, NameType, RootType],
             LinkType is Followable[NameType, ?, ?],
             Tactic[PathError])
          : PathType =

    left.append(link)

  def relativeTo[PathType2 <: PathType](right: PathType)
      (using directional: LinkType is Followable[NameType, ?, ?],
             navigable:   PathType is Navigable[NameType, RootType],
             pathCreator: PathCreator[PathType, NameType, RootType],
             linkCreator: PathCreator[LinkType, NameType, Int],
             navigable2:  PathType2 is Navigable[NameType, RootType])
      : LinkType =

    val common = navigable.depth(right.conjunction(left))
    linkCreator.path(navigable.depth(right) - common, navigable.descent(left).dropRight(common))

extension[PathType <: Matchable, NameType <: Label, RootType](left: PathType)
  def keep(n: Int)
      (using navigable: PathType is Navigable[NameType, RootType],
             creator:   PathCreator[PathType, NameType, RootType])
      : PathType =

    creator.path(navigable.root(left), navigable.descent(left).takeRight(n))

  def conjunction(right: PathType)
      (using navigable: PathType is Navigable[NameType, RootType],
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
      (using navigable: PathType is Navigable[NameType, RootType],
             creator:   PathCreator[PathType, NameType, RootType])
          : Boolean =

    navigable.descent(left.conjunction(path)) == navigable.descent(left) &&
      navigable.root(path) == navigable.root(left)

implicit class Slash[PathType <: Matchable](path: PathType):
  @targetName("child")
  infix def / [NameType <: Label, AscentType](using directional: PathType is Directional[NameType, AscentType])
      (name: Name[NameType])
      (using creator: PathCreator[PathType, NameType, AscentType])
          : PathType =
    directional.child(path, name)

extension [PathType <: Matchable, NameType <: Label, AscentType](path: PathType)
    (using directional: PathType is Directional[NameType, AscentType],
           creator:     PathCreator[PathType, NameType, AscentType])

  // @targetName("child")
  // infix def /[PathType2 <: PathType](name: Name[NameType]): PathType =
  //   directional.child(path, name)

  // FIXME: This should be called `/`, but it causes an error because there's already an object called
  // `/` exported from `Serpentine`.
  @targetName("child2")
  inline infix def /- [PathType2 <: PathType](name: Text)(using pathError: Tactic[PathError]): PathType =
    directional.child(path, Name(name))

  def render: Text = directional.render(path)
  def descent: List[Name[NameType]] = directional.descent(path)
  def depth: Int = directional.descent(path).length

  transparent inline def parent: Optional[PathType] = directional.parent(path)
  transparent inline def ancestor(n: Int): Optional[PathType] = directional.ancestor(path, n)

  inline def append[LinkType <: Matchable](inline link: LinkType)
      (using followable: LinkType is Followable[NameType, ?, ?], pathHandler: Tactic[PathError])
          : PathType =

    if followable.ascent(link) > directional.descent(path).length
    then raise(PathError(path.render, PathError.Reason.ParentOfRoot), path)
    else
      val common: PathType = directional.ancestor(path, followable.ascent(link)).vouch(using Unsafe)
      val descent = directional.descent(common)

      creator.path(directional.ascent(path), followable.descent(link) ::: descent)

extension (inline context: StringContext)
  inline def p[NameType <: Label]: PExtractor[NameType] =
    ${Serpentine.parse[NameType]('context)}
