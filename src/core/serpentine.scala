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
  opaque type PathName[NameType <: Label] = String

  @targetName("Slash")
  object `/`:
    def unapply
        [PathType <: Matchable, NameType <: Label, RootType]
        (using hierarchy: Hierarchy[PathType, ?])
        (using navigable: Navigable[PathType, NameType, RootType])
        (using creator: PathCreator[PathType, NameType, RootType])
        (path: PathType)
        : Option[(PathType | RootType | %.type, PathName[NameType])] =
      navigable.descent(path) match
        case Nil          => None
        case head :: Nil  => Some((navigable.root(path), head))
        case head :: tail => Some((creator.path(navigable.root(path), tail), head))
    

  object PathName:
    given [NameType <: Label]: Show[PathName[NameType]] = Text(_)

    inline def apply[NameType <: Label](text: Text)(using errorHandler: Raises[PathError]): PathName[NameType] =
      ${SerpentineMacro.runtimeParse[NameType]('text, 'errorHandler)}
    
    def unsafe[NameType <: Label](text: Text): PathName[NameType] = text.s: PathName[NameType]

  extension [NameType <: Label](pathName: PathName[NameType])
    def render: Text = Text(pathName)
    def widen[NameType2 <: NameType]: PathName[NameType2] = pathName
    inline def narrow[NameType2 >: NameType <: Label](using Raises[PathError]): PathName[NameType2] = PathName(render)
  
  @targetName("Root")
  object `%`:
    erased given hierarchy
        [PathType <: Matchable, LinkType <: Matchable]
        (using erased hierarchy: Hierarchy[PathType, LinkType])
        : Hierarchy[%.type, LinkType] = ###

    override def equals(other: Any): Boolean = other.asMatchable match
      case anyRef: AnyRef         => (anyRef eq %) || {
        anyRef match
          case other: PathEquality[?] => other.equals(this)
          case other                  => false
      }
      case _                      => false
    
    override def hashCode: Int = 0

    def precedes
        [PathType <: Matchable]
        (using erased hierarchy: Hierarchy[PathType, ?])
        (path: PathType)
        : Boolean =
      true

    given navigable
        [PathType <: Matchable, LinkType <: Matchable, NameType <: Label, RootType]
        (using erased hierarchy: Hierarchy[PathType, LinkType])
        (using navigable: Navigable[PathType, NameType, RootType])
        (using mainRoot: MainRoot[PathType])
        : Navigable[%.type, NameType, RootType] =
      new Navigable[%.type, NameType, RootType]:
        def separator(path: %.type): Text = navigable.separator(mainRoot.empty())
        def prefix(root: RootType): Text = navigable.prefix(navigable.root(mainRoot.empty()))
        def root(path: %.type): RootType = navigable.root(mainRoot.empty())
        def descent(path: %.type): List[PathName[NameType]] = Nil
    
    given show
        [PathType <: Matchable]
        (using hierarchy: Hierarchy[PathType, ?])
        (using mainRoot: MainRoot[PathType], show: Show[PathType]): Show[%.type] = root =>
      mainRoot.empty().show
      
    @targetName("child")
    infix def / [PathType <: Matchable, NameType <: Label, AscentType]
        (using hierarchy: Hierarchy[PathType, ?])
        (using mainRoot: MainRoot[PathType])
        (using directional: Directional[PathType, NameType, AscentType])
        (name: PathName[NameType])
        (using creator: PathCreator[PathType, NameType, AscentType])
        : PathType =
      mainRoot.empty() / name

    @targetName("child2")
    inline infix def / [PathType <: Matchable, NameType <: Label, AscentType]
        (using hierarchy: Hierarchy[PathType, ?])
        (using mainRoot: MainRoot[PathType])
        (using directional: Directional[PathType, NameType, AscentType])
        (name: Text)
        (using creator: PathCreator[PathType, NameType, AscentType])
        (using path: Raises[PathError])
        : PathType =
      mainRoot.empty() / PathName(name)

export Serpentine.%
