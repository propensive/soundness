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
import contingency.*
import denominative.*
import prepositional.*
import rudiments.*
import serpentine.*
import spectacular.*
import symbolism.*
import vacuous.*

import IoError.{Operation, Reason}

final val C: Drive = Drive('C')
final val D: Drive = Drive('D')


extension [target: Substantiable](value: target)
  def exists(): Boolean = target.existence(value)

extension [target: Openable](value: target)
  def open[result]
    ( lambda:  target.Result => result, options: List[target.Operand] = Nil )
  :   result =

    target.open(value, lambda, options)

package filesystemTraversal:
  given preOrderTraversal: TraversalOrder = TraversalOrder.PreOrder
  given postOrderTraversal: TraversalOrder = TraversalOrder.PostOrder

extension [plane: Filesystem](path: Path on plane)

  inline def children(using explorable: plane is Explorable): LazyList[Path on plane] =
    explorable.children(path)


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
  :   Path on plane logs IoEvent raises IoError =

    deleteRecursively.conditionally(path)(backend.delete(path))
    Log.info(IoEvent.Delete(path.show))
    path


  def wipe()(using deleteRecursively: DeleteRecursively on plane)(using io: Tactic[IoError])
    ( using backend: FilesystemBackend on plane )
  :   Path on plane logs IoEvent raises IoError =

    deleteRecursively.conditionally(path)(backend.deleteIfExists(path))
    Log.info(IoEvent.Delete(path.show))
    path


  def volume()(using backend: FilesystemBackend on plane): Volume raises IoError =
    backend.volume(path)


  def hardLinkTo(destination: Path on plane)
    ( using overwritePreexisting:     OverwritePreexisting on plane,
            createNonexistentParents: CreateNonexistentParents on plane,
            backend:                  FilesystemBackend on plane )
  :   Path on plane logs IoEvent raises IoError =

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
    ( using overwritePreexisting:     OverwritePreexisting on plane,
            dereferenceSymlinks:      DereferenceSymlinks,
            createNonexistentParents: CreateNonexistentParents on plane )
    ( using FilesystemBackend on plane )
  :   Path on plane logs IoEvent raises IoError =

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
    ( using overwritePreexisting:     OverwritePreexisting on plane,
            moveAtomically:           MoveAtomically,
            dereferenceSymlinks:      DereferenceSymlinks,
            createNonexistentParents: CreateNonexistentParents on plane )
    ( using backend: FilesystemBackend on plane )
  :   Path on plane logs IoEvent raises IoError =

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
    ( using overwritePreexisting:     OverwritePreexisting on plane,
            createNonexistentParents: CreateNonexistentParents on plane,
            backend:                  FilesystemBackend on plane )
  :   Path on plane logs IoEvent raises IoError =

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

  def touch()(using backend: FilesystemBackend on plane): Unit logs IoEvent raises IoError =
    backend.touch(path)
    Log.fine(IoEvent.Touch(path.show))

  def create[entry: Creatable on plane](): entry.Result logs IoEvent =
    Log.info(IoEvent.Create(path.show))
    entry.create(path)

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
  object readAccess:
    given enabled: ReadAccess:
      type Transform[HandleType] = HandleType & ReadAccess.Ability

      def flags(): List[OpenFlag] = List(OpenFlag.Read)

    given disabled: ReadAccess:
      type Transform[HandleType] = HandleType

      def flags(): List[OpenFlag] = Nil

  object writeAccess:
    given enabled: WriteAccess:
      type Transform[HandleType] = HandleType & WriteAccess.Ability

      def flags(): List[OpenFlag] = List(OpenFlag.Write)

    given disabled: WriteAccess:
      type Transform[HandleType] = HandleType

      def flags(): List[OpenFlag] = Nil

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
    given enabled: [plane: Filesystem] => Tactic[IoError]
    =>  ( explorable: plane is Explorable, backend: FilesystemBackend on plane )
    =>  DeleteRecursively on plane:

      type World = plane

      def recur(path: Path on plane): Unit =
        path.children.each(recur(_))
        backend.delete(path)

      def conditionally[result](path: Path on Plane)(operation: => result): result =
        path.children.each(recur(_)) yet operation


    given disabled: [plane: {Filesystem, Explorable}] => Tactic[IoError]
    =>  DeleteRecursively on plane:

      type Plane = plane

      def conditionally[result](path: Path on Plane)(operation: => result): result =
        if !path.children.nil
        then abort(IoError(path, IoError.Operation.Delete, Reason.DirectoryNotEmpty))
        else operation

  object overwritePreexisting:
    given enabled: [plane: Filesystem]
    =>  ( deleteRecursively: DeleteRecursively on plane )
    =>  OverwritePreexisting on plane:

      type Plane = plane

      def apply[result](path: Path on Plane)(operation: => result): result =
        deleteRecursively.conditionally(path)(operation)


    // The backend raises `AlreadyExists` itself when the operation collides with an existing
    // entry, so nothing needs intercepting here.
    given disabled: [plane: Filesystem] => Tactic[IoError]
    =>  OverwritePreexisting on plane:

      type Plane = plane

      def apply[result](path: Path on Plane)(operation: => result): result = operation

  object createNonexistentParents:
    given enabled: [plane: Filesystem] => Tactic[IoError]
    =>  ( backend: FilesystemBackend on plane )
    =>  CreateNonexistentParents on plane:

      def apply[result](path: Path on plane)(operation: => result): result =
        def ensure(directory: Path on plane): Unit =
          if !backend.exists(directory, true) then
            safely(directory.parent).let(ensure(_))
            backend.createDirectory(directory)

        safely(path.parent).let(ensure(_))
        operation


    given disabled: [plane: Filesystem] => Tactic[IoError]
    =>  CreateNonexistentParents on plane:

      type Plane = plane

      def apply[result](path: Path on plane)(block: => result): result = block

  object createNonexistent:
    given enabled: [plane: Filesystem]
    =>  ( create: CreateNonexistentParents on plane )
    =>  (Path on plane) is Substantiable
    =>  CreateNonexistent on plane:

      type Plane = plane

      def error(path: Path on Plane, operation: IoError.Operation): Nothing =
        import strategies.throwUnsafely
        abort(IoError(path, operation, Reason.Nonexistent))

      def apply(path: Path on Plane)(operation: => Unit): Unit =
        if !path.exists() then create(path)(operation)

      def flags(): List[OpenFlag] = List(OpenFlag.Create)


    given disabled: [plane: Filesystem] => Tactic[IoError]
    =>  CreateNonexistent on plane:

      type Plane = plane

      def error(path: Path on Plane, operation: IoError.Operation): Nothing =
        abort(IoError(path, operation, Reason.Nonexistent))

      def apply(path: Path on Plane)(operation: => Unit): Unit = ()
      def flags(): List[OpenFlag] = List()

  object writeSynchronously:
    given enabled: WriteSynchronously = () => List(OpenFlag.Sync)
    given disabled: WriteSynchronously = () => Nil
