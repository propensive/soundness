                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.63.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package galilei

import anticipation.*
import aperture.*
import contingency.*
import nomenclature.*
import prepositional.*
import rudiments.*
import serpentine.*
import turbulence.*
import vacuous.*
import zephyrine.*

// The common upper bound of every `DirectoryHandle`'s fresh plane. Serpentine givens defined
// generically over `plane <: Subtree` supply the naming rules for paths on any handle's plane,
// so `dir / "name"` validates its names at compile time: in particular, `.` and `..` are
// inadmissible, making escape from an opened directory inexpressible rather than checked.
//
// Deliberately, a subtree plane has no `Filesystem` instance: galilei's absolute-path
// operations (bounded by `[plane: Filesystem]`) therefore cannot apply to subtree paths, which
// would resolve them against the working directory instead of the handle.
trait Subtree

object Subtree:
  type Rules = MustNotContain["/"] & MustNotEqual["."] & MustNotEqual[".."] & MustNotEqual[""]

  inline given nominative: [plane <: Subtree] => plane is Nominative under Rules = !!

  // Operations on subtree paths are `transparent inline`, taking their handle as a direct
  // `using` parameter selected by plane refinement: routing them through typeclass instances
  // capturing the handle fails under capture checking (given resolution mints fresh roots the
  // scoped capability cannot flow into). The names are chosen not to collide with lexically
  // imported generic extensions (turbulence's `read`, vacuous's `present`, galilei's `exists`
  // and `delete`), which would otherwise be tried first and fail without falling through.
  extension [plane <: Subtree](path: Path on plane)
    transparent inline def contents[result]
      ( using handle: ((DirectoryHandle { type Plane = plane }) & Granting[Grant.Read])^ )
      ( using filesystem: handle.Under is Filesystem )
      ( using readable: (Data is Readable to result)^, tactic: Tactic[IoError] )
    :   result =
      readResolved[handle.Under, result](handle.resolve(path))

    transparent inline def overwrite[content](content: content)
      ( using handle: ((DirectoryHandle { type Plane = plane }) & Granting[Grant.Write])^ )
      ( using filesystem: handle.Under is Filesystem )
      ( using streamable: (content is Streamable by Data over Credit)^ )
      ( using tactic: Tactic[IoError] )
    :   Unit =
      writeResolved(handle.resolve(path), content)

    transparent inline def extant()
      ( using handle: ((DirectoryHandle { type Plane = plane }) & Granting[Grant.Read])^ )
      ( using filesystem: handle.Under is Filesystem )
      ( using backend: FilesystemBackend on handle.Under )
    :   Boolean =
      existsResolved(handle.resolve(path))

    transparent inline def entries
      ( using handle: ((DirectoryHandle { type Plane = plane }) & Granting[Grant.Read])^ )
      ( using filesystem: handle.Under is Filesystem )
      ( using backend: FilesystemBackend on handle.Under, tactic: Tactic[IoError] )
    :   LazyList[Path on plane] =
      entriesResolved(handle.resolve(path)).map: child =>
        path.child(child.name)(using Unsafe)

    transparent inline def remove()
      ( using handle: ((DirectoryHandle { type Plane = plane }) & Granting[Grant.Write])^ )
      ( using filesystem: handle.Under is Filesystem )
      ( using backend: FilesystemBackend on handle.Under, tactic: Tactic[IoError] )
    :   Unit =
      removeResolved(handle.resolve(path))

  // Helpers are public: `private` helpers called from the transparent-inline operations above
  // generate inline-accessor bridges whose fresh capability roots fail capture checking.
  def readResolved[under <: Platform, result](path: Path on under)
    ( using filesystem: under is Filesystem )
    ( using readable: (Data is Readable to result)^, tactic: Tactic[IoError] )
  :   result =
    Platform.pathReadable[under, result].read(path)

  def writeResolved[under, content](path: Path on under, content: content)
    ( using filesystem: under is Filesystem )
    ( using streamable: (content is Streamable by Data over Credit)^ )
    ( using tactic: Tactic[IoError] )
  :   Unit =
    path.write(content)

  def existsResolved[under](path: Path on under)
    ( using filesystem: under is Filesystem )
    ( using backend: FilesystemBackend on under )
  :   Boolean =
    path.exists()

  def entriesResolved[under](path: Path on under)
    ( using filesystem: under is Filesystem )
    ( using backend: FilesystemBackend on under, tactic: Tactic[IoError] )
  :   LazyList[Path on under] =
    path.children

  def removeResolved[under](path: Path on under)
    ( using filesystem: under is Filesystem )
    ( using backend: FilesystemBackend on under, tactic: Tactic[IoError] )
  :   Unit =
    import filesystemOptions.deleteRecursively.disabled
    path.delete()
