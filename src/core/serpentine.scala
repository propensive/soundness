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
import spectacular.*
import anticipation.*
import contingency.*

import scala.compiletime.*
import scala.quoted.*

//import language.experimental.captureChecking

object Serpentine:
  opaque type Name[NameType <: Label] = String

  @targetName("Slash")
  object `/`:
    def unapply[PathType <: Matchable, NameType <: Label, RootType]
        (using hierarchy: Hierarchy[PathType, ?],
               navigable: PathType is Navigable[NameType, RootType],
               creator:   PathCreator[PathType, NameType, RootType])
        (path: PathType)
            : Option[(PathType | RootType | %.type, Name[NameType])] =

      navigable.descent(path) match
        case Nil          => None
        case head :: Nil  => Some((navigable.root(path), head))
        case head :: tail => Some((creator.path(navigable.root(path), tail), head))

  object Name:
    given [NameType <: Label] => Name[NameType] is Showable = _.tt

    inline def apply[NameType <: Label](text: Text)(using Tactic: Tactic[PathError]): Name[NameType] =
      ${SerpentineMacro.runtimeParse[NameType]('text, 'Tactic)}

    def unsafe[NameType <: Label](text: Text): Name[NameType] = text.s: Name[NameType]

    def unapply[NameType <: Label](name: Name[NameType]): Some[Text] = Some(name.render)

  extension [NameType <: Label](name: Name[NameType])
    def render: Text = name.tt
    def widen[NameType2 <: NameType]: Name[NameType2] = name
    inline def narrow[NameType2 >: NameType <: Label]: Name[NameType2] raises PathError =
      Name(render)

  @targetName("Root")
  object `%`:
    erased given [PathType <: Matchable, LinkType <: Matchable]
        (using erased hierarchy: Hierarchy[PathType, LinkType])
        => Hierarchy[%.type, LinkType] as hierarchy = ###

    override def equals(other: Any): Boolean = other.asMatchable match
      case anyRef: AnyRef => (anyRef eq %) || anyRef.match
        case other: PathEquality[?] => other.equals(this)
        case other                  => false
      case _              => false

    override def hashCode: Int = 0

    def precedes[PathType <: Matchable](using erased hierarchy: Hierarchy[PathType, ?])(path: PathType)
            : Boolean =

      true

    given [PathType <: Matchable: Radical, LinkType <: Matchable, NameType <: Label, RootType]
        (using erased hierarchy: Hierarchy[PathType, LinkType],
                      navigable: PathType is Navigable[NameType, RootType])
        => %.type is Navigable[NameType, RootType] as navigable:

      def separator(path: %.type): Text = navigable.separator(PathType.empty())
      def prefix(root: RootType): Text = navigable.prefix(navigable.root(PathType.empty()))
      def root(path: %.type): RootType = navigable.root(PathType.empty())
      def descent(path: %.type): List[Name[NameType]] = Nil

    given [PathType <: Matchable: {Radical as radical, Showable as showable}]
        (using hierarchy: Hierarchy[PathType, ?]) => %.type is Showable =

      root => PathType.empty().show

    @targetName("child")
    infix def / [PathType <: Matchable: Radical, NameType <: Label, AscentType]
        (using Hierarchy[PathType, ?], PathType is Directional[NameType, AscentType])
        (name: Name[NameType])
        (using creator: PathCreator[PathType, NameType, AscentType])
            : PathType =

      PathType.empty() / name

    @targetName("child2")
    inline infix def / [PathType <: Matchable: Radical, NameType <: Label, AscentType]
        (using hierarchy:   Hierarchy[PathType, ?],
               directional: PathType is Directional[NameType, AscentType])
        (name: Text)
        (using creator: PathCreator[PathType, NameType, AscentType])
            : PathType raises PathError =

      PathType.empty() / Name(name)

export Serpentine.%
export Serpentine./
export Serpentine.Name
