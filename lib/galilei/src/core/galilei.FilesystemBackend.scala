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
import vacuous.*

// The pluggable low-level filesystem backend for a plane: the complete set of primitive
// operations that galilei's user-facing API is defined in terms of, expressed without reference
// to any platform API. `galilei.jvm` provides the `java.nio` implementation
// (`filesystemBackends.virtualMachineFilesystem`); other platforms (e.g. WASI's `wasi:filesystem`) supply
// their own.
//
// Operations that compose several primitives (recursive deletion, copy-into, creating parents)
// live in the user-facing API, not here; an operation a backend cannot support raises an
// `Io.Error` with `Reason.Unsupported` rather than approximating. `dereference` selects whether a
// final-component symlink is followed. Errors are raised as `Io.Error`s, with each backend
// responsible for mapping its native failures (exceptions, error codes) to the common
// `Io.Error.Reason` vocabulary — as informatively as it can.
object FilesystemBackend:
  enum Attribute:
    case Readable, Writable, Executable

trait FilesystemBackend extends Planar:
  // The entry's type, size and timestamps, in one read. (`created` is `Unset` on filesystems
  // that do not record creation times.)
  def stat(path: Path on Plane, dereference: Boolean)(using Tactic[Io.Error]): Stat

  def exists(path: Path on Plane, dereference: Boolean): Boolean

  // The names (not paths) of the directory's immediate children.
  def children(path: Path on Plane)(using Tactic[Io.Error]): Chain[Text]

  def createDirectory(path: Path on Plane)(using Tactic[Io.Error]): Unit
  def createFile(path: Path on Plane)(using Tactic[Io.Error]): Unit
  def createFifo(path: Path on Plane)(using Tactic[Io.Error]): Unit
  def delete(path: Path on Plane)(using Tactic[Io.Error]): Unit
  def deleteIfExists(path: Path on Plane)(using Tactic[Io.Error]): Unit

  def symlink(link: Path on Plane, target: Path on Plane)(using Tactic[Io.Error]): Unit
  def hardLink(link: Path on Plane, target: Path on Plane)(using Tactic[Io.Error]): Unit

  def copy(source: Path on Plane, destination: Path on Plane, dereference: Boolean)
    ( using Tactic[Io.Error] )
  :   Unit

  def move(source: Path on Plane, destination: Path on Plane, atomic: Boolean, dereference: Boolean)
    ( using Tactic[Io.Error] )
  :   Unit

  // Sets the modification time to the present moment.
  def touch(path: Path on Plane)(using Tactic[Io.Error]): Unit

  def hidden(path: Path on Plane)(using Tactic[Io.Error]): Boolean
  def volume(path: Path on Plane)(using Tactic[Io.Error]): Volume
  def hardLinkCount(path: Path on Plane, dereference: Boolean)(using Tactic[Io.Error]): Int

  // Coarse-grained permission flags on the entry, from the perspective of the current user.
  // These express the *current* model; a richer permissions API can be layered on later without
  // touching the primitives above.
  def attribute(path: Path on Plane, attribute: FilesystemBackend.Attribute): Boolean

  def update(path: Path on Plane, attribute: FilesystemBackend.Attribute, value: Boolean)
    ( using Tactic[Io.Error] )
  :   Unit

  // Opens the entry's content for streaming, applies `lambda` to the open handle, and closes it,
  // whatever the outcome.
  // Scoped positional (random-access) reading (issue #1608): opens the file for reading and
  // passes a positional view, valid for the scope of the call, to `lambda`. Reads are
  // pread-style — each independent of any sequential position, and no whole-file mapping is
  // taken — so files beyond 2 GiB are addressable, unlike `Ram`'s single `Int`-addressed map.
  def expanse[result](path: Path on Plane)(lambda: zephyrine.Expanse => result)
    ( using Tactic[Io.Error] )
  :   result

  // Extended (user-defined) attributes (issue #567). Support depends on the storage
  // filesystem, so the typed accessors are gated on the `Attributed` axis marker; at the
  // backend seam the operations are plain, and a backend or filesystem without extended
  // attributes reports `Unsupported`.
  def attributes(path: Path on Plane)(using Tactic[Io.Error]): List[Text]
  def attribute(path: Path on Plane, name: Text)(using Tactic[Io.Error]): Optional[Data]
  def attribute(path: Path on Plane, name: Text, value: Data)(using Tactic[Io.Error]): Unit

  // Range-scoped positional access (issues #566, #1878): like `expanse`, but the view is a
  // `Slice.Window` confined to `[offset, offset + extent)` — its `size` is the window's,
  // reads are relative to the window's start and clamped to it, and writes store as much as
  // fits and return the count, `pwrite`-style. When `flags` request it, an OS advisory lock
  // over exactly that byte range is held for the scope; a write is only meaningful when
  // `flags` include `OpenFlag.Write`.
  def slice[result]
    ( path: Path on Plane, offset: Long, extent: Long, flags: List[OpenFlag] )
    ( lambda: Slice.Window => result )
    ( using Tactic[Io.Error] )
  :   result

  protected def window(view: zephyrine.Expanse, offset: Long, extent: Long): zephyrine.Expanse =
    new zephyrine.Expanse:
      def size: Long = (view.size - offset).max(0L).min(extent)

      def read(readOffset: Long, length: Int): Data =
        val available = (size - readOffset).max(0L).min(length.toLong).toInt
        view.read(offset + readOffset, available)

  def open[result](path: Path on Plane, flags: List[OpenFlag])(lambda: Handle => result)
    ( using Tactic[Io.Error] )
  :   result
