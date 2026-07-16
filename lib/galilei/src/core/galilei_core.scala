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

import java.io as ji
import java.nio.file as jnf

import anticipation.*
import contingency.*
import denominative.*
import prepositional.*
import rudiments.*
import serpentine.*
import spectacular.*
import symbolism.*
import turbulence.{Aggregable, Streamable}
import zephyrine.Credit
import vacuous.*

import IoError.{Operation, Reason}

final val C: Drive = Drive('C')
final val D: Drive = Drive('D')

extension (inline context: StringContext)
  transparent inline def p(): Path = ${galilei.internal.path('context)}


extension [target: Substantiable](value: target)
  def exists(): Boolean = target.existence(value)

// The contextual file handle within an `open` block, in the manner of facsimile's `pdf`.
// Transparent inline so the handle's precise (grant-refined, capturing) type is preserved: a
// non-inline accessor would widen the scoped capture set, losing the `Granting` refinement
// that gates read and write operations.
transparent inline def file(using handle: galilei.Handle^): handle.type = handle

// The contextual directory handle within an `open[Directory]` block.
transparent inline def dir(using handle: galilei.DirectoryHandle^): handle.type = handle

package filesystemTraversal:
  given preOrderTraversal: TraversalOrder = TraversalOrder.PreOrder
  given postOrderTraversal: TraversalOrder = TraversalOrder.PostOrder

extension [plane: Filesystem](path: Path on plane)

  inline def children(using explorable: plane is Explorable): LazyList[Path on plane] =
    explorable.children(path)

  // Write `content` to the file in its entirety as a single, direct operation: the whole file is
  // written at once, holding no handle and needing no scope — unlike streaming to a path, which
  // must be `open`ed and consumed within a scope.
  def write[content](content: content)
    ( using streamable: (content is Streamable by Data over Credit)^ )
  :   Unit raises IoError =
    val bytes: Data = summon[Data is Aggregable by Data].accept(streamable.stream(content))
    protect(Operation.Write)(jnf.Files.write(javaPath, bytes.mutable(using Unsafe)))

  // Append `content` to the file in its entirety as a single, direct operation, creating the file
  // if it does not exist — the eager counterpart of `Eof(path).open(Write)(file.write(content))`.
  def append[content](content: content)
    ( using streamable: (content is Streamable by Data over Credit)^ )
  :   Unit raises IoError =
    val bytes: Data = summon[Data is Aggregable by Data].accept(streamable.stream(content))

    protect(Operation.Write):
      jnf.Files.write
        ( javaPath,
          bytes.mutable(using Unsafe),
          jnf.StandardOpenOption.CREATE,
          jnf.StandardOpenOption.APPEND )

  private[galilei] def protect[result](operation: Operation)(block: => result)
  :   result raises IoError =

    import Reason.*

    try block catch
      case break: boundary.Break[?]          => throw break
      case _: jnf.NoSuchFileException        => abort(IoError(path, operation, Nonexistent))
      case _: jnf.FileAlreadyExistsException => abort(IoError(path, operation, AlreadyExists))
      case _: jnf.DirectoryNotEmptyException => abort(IoError(path, operation, DirectoryNotEmpty))
      case _: jnf.AccessDeniedException      => abort(IoError(path, operation, PermissionDenied))
      case _: jnf.NotDirectoryException      => abort(IoError(path, operation, IsNotDirectory))
      case _: SecurityException              => abort(IoError(path, operation, PermissionDenied))
      case _: jnf.FileSystemLoopException    => abort(IoError(path, operation, Cycle))
      case _: jnf.FileSystemException        => abort(IoError(path, operation, IsDirectory))
      case other                             => abort(IoError(path, operation, Unsupported))

  def javaPath: jnf.Path = jnf.Path.of(Path.encodable.encode(path).s).nn
  def javaFile: ji.File = javaPath.toFile.nn


  def descendants(using DereferenceSymlinks, TraversalOrder, plane is Explorable)
  :   LazyList[Path on plane] raises IoError =

    path.children.flatMap: child =>
      summon[TraversalOrder] match
        case TraversalOrder.PreOrder  => child #:: child.descendants
        case TraversalOrder.PostOrder => child.descendants #::: LazyList(child)


  def size()(using plane is Explorable, FilesystemBackend on plane): Bytes raises IoError =
    import filesystemOptions.dereferenceSymlinks.disabled
    given TraversalOrder = TraversalOrder.PreOrder

    descendants.fuse(summon[FilesystemBackend on plane].stat(path, false).size.b):
      state + next.size()


  def delete()(using deleteRecursively: DeleteRecursively on plane)
    ( using backend: FilesystemBackend on plane )
  ( using Tactic[IoError], (IoEvent is Loggable)^ )
  :   Path on plane =

    deleteRecursively.conditionally(path)(backend.delete(path))
    Log.info(IoEvent.Delete(path.show))
    path


  def wipe()(using deleteRecursively: DeleteRecursively on plane)(using io: Tactic[IoError])
    ( using backend: FilesystemBackend on plane )
    ( using (IoEvent is Loggable)^ )
  :   Path on plane =

    deleteRecursively.conditionally(path)(backend.deleteIfExists(path))
    Log.info(IoEvent.Delete(path.show))
    path


  def volume()(using backend: FilesystemBackend on plane): Volume raises IoError =
    backend.volume(path)


  def hardLinkTo(destination: Path on plane)
    ( using overwritePreexisting: OverwritePreexisting on plane,
            createNonexistentParents: CreateNonexistentParents on plane,
            backend:                  FilesystemBackend on plane )
  ( using Tactic[IoError], (IoEvent is Loggable)^ )
  :   Path on plane =

    createNonexistentParents(destination):
      overwritePreexisting(destination):
        backend.hardLink(destination, path)

    Log.info(IoEvent.HardLink(path.show, destination.show))
    destination


  def entry()(using symlinks: DereferenceSymlinks)
    ( using backend: FilesystemBackend on plane )
  :   Entry raises IoError =

    backend.stat(path, symlinks.dereference).entry


  def copyTo(destination: Path on plane)
    ( using overwritePreexisting: OverwritePreexisting on plane,
            dereferenceSymlinks:      DereferenceSymlinks,
            createNonexistentParents: CreateNonexistentParents on plane )
    ( using FilesystemBackend on plane )
  ( using Tactic[IoError], (IoEvent is Loggable)^ )
  :   Path on plane =

    createNonexistentParents(destination):
      overwritePreexisting(destination):
        summon[FilesystemBackend on plane].copy(path, destination, dereferenceSymlinks.dereference)

    Log.info(IoEvent.Copy(path.show, destination.show))
    destination


  def copyInto(destination: Path on plane)
    ( using overwritePreexisting: OverwritePreexisting on plane,
            dereferenceSymlinks:  DereferenceSymlinks,
            substantiable:        (Path on plane) is Substantiable )
    ( using FilesystemBackend on plane )
  :   Path on plane raises IoError =

    given CreateNonexistentParents on plane =
      filesystemOptions.createNonexistentParents.enabled[plane]

    val file2: Path on plane = unsafely(destination.child(path.descent.head))
    copyTo(file2)


  def moveTo(destination: Path on plane)
    ( using overwritePreexisting: OverwritePreexisting on plane,
            moveAtomically:           MoveAtomically,
            dereferenceSymlinks:      DereferenceSymlinks,
            createNonexistentParents: CreateNonexistentParents on plane )
    ( using backend: FilesystemBackend on plane )
  ( using Tactic[IoError], (IoEvent is Loggable)^ )
  :   Path on plane =

    createNonexistentParents(destination):
      overwritePreexisting(destination):
        backend.move(path, destination, moveAtomically.atomic, dereferenceSymlinks.dereference)

    Log.info(IoEvent.Move(path.show, destination.show))
    destination


  def moveInto
    ( destination: Path on plane )
    ( using overwritePreexisting: OverwritePreexisting on plane,
            moveAtomically:       MoveAtomically,
            substantiable:        (Path on plane) is Substantiable,
            dereferenceSymlinks:  DereferenceSymlinks )
    ( using FilesystemBackend on plane )
  :   Path on plane raises IoError =

    import filesystemOptions.createNonexistentParents.enabled
    moveTo(unsafely(destination.child(path.descent.head)))


  def symlinkTo(destination: Path on plane)
    ( using overwritePreexisting: OverwritePreexisting on plane,
            createNonexistentParents: CreateNonexistentParents on plane,
            backend:                  FilesystemBackend on plane )
  ( using Tactic[IoError], (IoEvent is Loggable)^ )
  :   Path on plane =

    createNonexistentParents(destination):
      overwritePreexisting(destination):
        backend.symlink(destination, path)

    Log.info(IoEvent.Symlink(destination.show, path.show))
    destination


  def symlinkInto
    ( destination: Path on plane )
    ( using overwritePreexisting: OverwritePreexisting on plane,
            moveAtomically:       MoveAtomically,
            substantiable:        (Path on plane) is Substantiable,
            dereferenceSymlinks:  DereferenceSymlinks )
    ( using FilesystemBackend on plane )
  :   Path on plane raises IoError =

    import filesystemOptions.createNonexistentParents.enabled
    symlinkTo(unsafely(destination.child(path.descent.head)))


  def modified[instant: Instantiable across Instants from Long]()
    ( using backend: FilesystemBackend on plane )
  :   instant raises IoError =

    instant(backend.stat(path, true).modified)

  def accessed[instant: Instantiable across Instants from Long]()
    ( using backend: FilesystemBackend on plane )
  :   instant raises IoError =

    instant(backend.stat(path, true).accessed)

  def readable(using FilesystemBackend on plane): FilesystemAttribute.Readable[plane] =
    FilesystemAttribute.Readable(path)

  def writable(using FilesystemBackend on plane): FilesystemAttribute.Writable[plane] =
    FilesystemAttribute.Writable(path)

  def hidden()(using backend: FilesystemBackend on plane): Boolean raises IoError =
    backend.hidden(path)

  def touch()(using backend: FilesystemBackend on plane)
    ( using Tactic[IoError], (IoEvent is Loggable)^ )
  :   Unit =
    backend.touch(path)
    Log.fine(IoEvent.Touch(path.show))

  transparent inline def create[entry]()
    ( using creatable: (entry is Creatable on plane)^, log: (IoEvent is Loggable)^ )
  :   creatable.Result =

    Log.info(IoEvent.Create(path.show))
    creatable.create(path)

extension (path: Path on Windows)
  def created[instant: Instantiable across Instants from Long]()
    ( using backend: FilesystemBackend on Windows )
  :   instant raises IoError =

    instant:
      backend.stat(path, true).created.or:
        abort(IoError(path, Operation.Metadata, Reason.Unsupported))

extension [plane <: Posix: Filesystem](path: Path on plane)
  def executable(using FilesystemBackend on plane): FilesystemAttribute.Executable[plane] =
    FilesystemAttribute.Executable(path)

  def hardLinks()(using dereferenceSymlinks: DereferenceSymlinks)
    ( using backend: FilesystemBackend on plane )
  :   Int raises IoError =

    backend.hardLinkCount(path, dereferenceSymlinks.dereference)

package filesystemOptions:
  object dereferenceSymlinks:
    given enabled: DereferenceSymlinks:
      def dereference = true

    given disabled: DereferenceSymlinks:
      def dereference = false

  object moveAtomically:
    given enabled: MoveAtomically:
      def atomic = true

    given disabled: MoveAtomically:
      def atomic = false

  object copyAttributes:
    given enabled: CopyAttributes:
      def attributes = true

    given disabled: CopyAttributes:
      def attributes = false

  object deleteRecursively:
    given enabled: [plane: Filesystem]
    =>  ( explorable: plane is Explorable, backend: FilesystemBackend on plane )
    =>  DeleteRecursively on plane:

      type Plane = plane

      def recur(path: Path on plane): Unit raises IoError =
        path.children.each(recur(_))
        backend.delete(path)

      def conditionally[result](path: Path on Plane)(operation: => result): result raises IoError =
        path.children.each(recur(_)) yet operation


    given disabled: [plane: {Filesystem, Explorable}] => DeleteRecursively on plane:

      type Plane = plane

      def conditionally[result](path: Path on Plane)(operation: => result): result raises IoError =
        if !path.children.nil
        then abort(IoError(path, IoError.Operation.Delete, Reason.DirectoryNotEmpty))
        else operation

  object overwritePreexisting:
    given enabled: [plane: Filesystem]
    =>  ( deleteRecursively: DeleteRecursively on plane )
    =>  OverwritePreexisting on plane:

      type Plane = plane

      def apply[result](path: Path on Plane)(operation: => result): result raises IoError =
        deleteRecursively.conditionally(path)(operation)


    // The backend raises `AlreadyExists` itself when the operation collides with an existing
    // entry, so nothing needs intercepting here.
    given disabled: [plane: Filesystem] => OverwritePreexisting on plane:

      type Plane = plane

      def apply[result](path: Path on Plane)(operation: => result): result raises IoError =
        operation

  object createNonexistentParents:
    given enabled: [plane: Filesystem]
    =>  ( backend: FilesystemBackend on plane )
    =>  CreateNonexistentParents on plane:

      def apply[result](path: Path on plane)(operation: => result): result raises IoError =
        def ensure(directory: Path on plane): Unit =
          if !backend.exists(directory, true) then
            safely(directory.parent).let(ensure(_))
            backend.createDirectory(directory)

        safely(path.parent).let(ensure(_))
        operation


    given disabled: [plane: Filesystem] => CreateNonexistentParents on plane:

      type Plane = plane

      def apply[result](path: Path on plane)(block: => result): result raises IoError =
        block

