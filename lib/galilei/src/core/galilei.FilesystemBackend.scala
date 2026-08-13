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
┃    Soundness, version 0.64.0.                                                                    ┃
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
import contingency.*
import prepositional.*
import serpentine.*

// The pluggable low-level filesystem backend for a plane: the complete set of primitive
// operations that galilei's user-facing API is defined in terms of, expressed without reference
// to any platform API. `galilei.jvm` provides the `java.nio` implementation
// (`filesystemBackends.virtualMachineFilesystem`); other platforms (e.g. WASI's `wasi:filesystem`) supply
// their own.
//
// Operations that compose several primitives (recursive deletion, copy-into, creating parents)
// live in the user-facing API, not here; an operation a backend cannot support raises an
// `IoError` with `Reason.Unsupported` rather than approximating. `dereference` selects whether a
// final-component symlink is followed. Errors are raised as `IoError`s, with each backend
// responsible for mapping its native failures (exceptions, error codes) to the common
// `IoError.Reason` vocabulary — as informatively as it can.
object FilesystemBackend:
  enum Attribute:
    case Readable, Writable, Executable

trait FilesystemBackend extends Planar:
  // The entry's type, size and timestamps, in one read. (`created` is `Unset` on filesystems
  // that do not record creation times.)
  def stat(path: Path on Plane, dereference: Boolean)(using Tactic[IoError]): Stat

  def exists(path: Path on Plane, dereference: Boolean): Boolean

  // The names (not paths) of the directory's immediate children.
  def children(path: Path on Plane)(using Tactic[IoError]): Chain[Text]

  def createDirectory(path: Path on Plane)(using Tactic[IoError]): Unit
  def createFile(path: Path on Plane)(using Tactic[IoError]): Unit
  def createFifo(path: Path on Plane)(using Tactic[IoError]): Unit
  def delete(path: Path on Plane)(using Tactic[IoError]): Unit
  def deleteIfExists(path: Path on Plane)(using Tactic[IoError]): Unit

  def symlink(link: Path on Plane, target: Path on Plane)(using Tactic[IoError]): Unit
  def hardLink(link: Path on Plane, target: Path on Plane)(using Tactic[IoError]): Unit

  def copy(source: Path on Plane, destination: Path on Plane, dereference: Boolean)
    ( using Tactic[IoError] )
  :   Unit

  def move(source: Path on Plane, destination: Path on Plane, atomic: Boolean, dereference: Boolean)
    ( using Tactic[IoError] )
  :   Unit

  // Sets the modification time to the present moment.
  def touch(path: Path on Plane)(using Tactic[IoError]): Unit

  def hidden(path: Path on Plane)(using Tactic[IoError]): Boolean
  def volume(path: Path on Plane)(using Tactic[IoError]): Volume
  def hardLinkCount(path: Path on Plane, dereference: Boolean)(using Tactic[IoError]): Int

  // Coarse-grained permission flags on the entry, from the perspective of the current user.
  // These express the *current* model; a richer permissions API can be layered on later without
  // touching the primitives above.
  def attribute(path: Path on Plane, attribute: FilesystemBackend.Attribute): Boolean

  def update(path: Path on Plane, attribute: FilesystemBackend.Attribute, value: Boolean)
    ( using Tactic[IoError] )
  :   Unit

  // Opens the entry's content for streaming, applies `lambda` to the open handle, and closes it,
  // whatever the outcome.
  def open[result](path: Path on Plane, flags: List[OpenFlag])(lambda: Handle => result)
    ( using Tactic[IoError] )
  :   result
