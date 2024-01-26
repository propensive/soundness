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
import perforate.*

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
        (using reachable: Reachable[PathType, NameType, RootType])
        (using creator: PathCreator[PathType, NameType, RootType])
        (path: PathType)
        : Option[(PathType | RootType | %.type, PathName[NameType])] =
      reachable.descent(path) match
        case Nil          => None
        case head :: Nil  => Some((reachable.root(path), head))
        case head :: tail => Some((creator.path(reachable.root(path), tail), head))
    

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

    given reachable
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
    
    given show
        [PathType <: Matchable]
        (using hierarchy: Hierarchy[PathType, ?])
        (using mainRoot: MainRoot[PathType], show: Show[PathType]): Show[%.type] = root =>
      mainRoot.empty().show
      
    @targetName("child")
    infix def / [PathType <: Matchable, NameType <: Label, AscentType]
        (using hierarchy: Hierarchy[PathType, ?])
        (using mainRoot: MainRoot[PathType])
        (using pathlike: Pathlike[PathType, NameType, AscentType])
        (name: PathName[NameType])
        (using creator: PathCreator[PathType, NameType, AscentType])
        : PathType =
      mainRoot.empty() / name

    @targetName("child2")
    inline infix def / [PathType <: Matchable, NameType <: Label, AscentType]
        (using hierarchy: Hierarchy[PathType, ?])
        (using mainRoot: MainRoot[PathType])
        (using pathlike: Pathlike[PathType, NameType, AscentType])
        (name: Text)
        (using creator: PathCreator[PathType, NameType, AscentType])
        (using path: Raises[PathError])
        : PathType =
      mainRoot.empty() / PathName(name)

export Serpentine.%
