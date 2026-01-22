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
┃    Soundness, version 0.53.0.                                                                    ┃
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
import jnf.attribute as jnfa

import scala.jdk.StreamConverters.*

import anticipation.*
import contingency.*
import denominative.*
import fulminate.*
import nomenclature.*
import prepositional.*
import proscenium.*
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
     (lambda:  target.Result => result, options: List[target.Operand] = Nil)
  : result =

      target.open(value, lambda, options)

package filesystemTraversal:
  given preOrder: TraversalOrder = TraversalOrder.PreOrder
  given postOrder: TraversalOrder = TraversalOrder.PostOrder

extension [plane: Filesystem](path: Path on plane)

  inline def children(using explorable: plane is Explorable): Stream[Path on plane] =
    explorable.children(path)

  private[galilei] def protect[result](operation: Operation)(block: => result)
  : result raises IoError =

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
        case other                             =>
          println(other)
          other.printStackTrace()
          abort(IoError(path, operation, Unsupported))

  def javaPath: jnf.Path = jnf.Path.of(Path.encodable.encode(path).s).nn
  def javaFile: ji.File = javaPath.toFile.nn


  def descendants(using DereferenceSymlinks, TraversalOrder, plane is Explorable)
  : Stream[Path on plane] raises IoError =

      path.children.flatMap: child =>
        summon[TraversalOrder] match
          case TraversalOrder.PreOrder  => child #:: child.descendants
          case TraversalOrder.PostOrder => child.descendants #::: Stream(child)


  def size()(using plane is Explorable): Bytes raises IoError =
    import filesystemOptions.dereferenceSymlinks.disabled
    given TraversalOrder = TraversalOrder.PreOrder
    descendants.fuse(jnf.Files.size(path.javaPath).b)(state + next.size())


  def delete()(using deleteRecursively: DeleteRecursively on plane)
  : Path on plane raises IoError =

      protect(Operation.Delete):
        deleteRecursively.conditionally(path)(jnf.Files.delete(path.javaPath))

      path


  def wipe()(using deleteRecursively: DeleteRecursively on plane)(using io: Tactic[IoError])
  : Path on plane raises IoError =

      deleteRecursively.conditionally(path)(jnf.Files.deleteIfExists(javaPath))
      path


  def volume(): Volume =
    val fileStore = jnf.Files.getFileStore(path.javaPath).nn
    Volume(fileStore.name.nn.tt, fileStore.`type`.nn.tt)


  def hardLinkTo(destination: Path on plane)
       (using overwritePreexisting:     OverwritePreexisting on plane,
              createNonexistentParents: CreateNonexistentParents on plane)
  : Path on plane raises IoError =

      createNonexistentParents(destination):
        overwritePreexisting(destination):
          jnf.Files.createLink(destination.javaPath, path.javaPath)

      destination


  def entry()(using symlinks: DereferenceSymlinks): Entry =
    if !symlinks.dereference && jnf.Files.isSymbolicLink(javaPath) then Symlink
    else if jnf.Files.isRegularFile(javaPath) then File
    else if jnf.Files.isDirectory(javaPath) then Directory
    else
      val mode = jnf.Files.getAttribute(javaPath, "unix:mode", symlinks.options()*)
      mode.absolve match
        case mode: Int => (mode & 61440) match
          case  4096 => Fifo
          case  8192 => CharDevice
          case 24576 => BlockDevice
          case 49152 => Socket
          case _     => panic(m"an unexpected POSIX mode value was returned")


  def copyTo(destination: Path on plane)
       (using overwritePreexisting:    OverwritePreexisting on plane,
              dereferenceSymlinks:     DereferenceSymlinks,
              createNonexistentParents: CreateNonexistentParents on plane)
  : Path on plane raises IoError =

      createNonexistentParents(destination):
        overwritePreexisting(destination):
          jnf.Files.copy(path.javaPath, destination.javaPath, dereferenceSymlinks.options()*)

      destination


  def copyInto(destination: Path on plane)
       (using overwritePreexisting: OverwritePreexisting on plane,
              dereferenceSymlinks:  DereferenceSymlinks,
              substantiable:        (Path on plane) is Substantiable)
  : Path on plane raises IoError =

      given CreateNonexistentParents on plane =
        filesystemOptions.createNonexistentParents.enabled[plane]

      val file2: Path on plane = unsafely(destination.child(path.descent.head))
      copyTo(file2)


  def moveTo(destination: Path on plane)
       (using overwritePreexisting:     OverwritePreexisting on plane,
              moveAtomically:           MoveAtomically,
              dereferenceSymlinks:      DereferenceSymlinks,
              createNonexistentParents: CreateNonexistentParents on plane)
  : Path on plane raises IoError =

      val options: Seq[jnf.CopyOption] = dereferenceSymlinks.options() ++ moveAtomically.options()

      createNonexistentParents(destination):
        overwritePreexisting(destination):
          jnf.Files.move(path.javaPath, destination.javaPath, options*)

      destination


  def moveInto
       (destination: Path on plane)
       (using overwritePreexisting: OverwritePreexisting on plane,
              moveAtomically:       MoveAtomically,
              substantiable:        (Path on plane) is Substantiable,
              dereferenceSymlinks:  DereferenceSymlinks)
  : Path on plane raises IoError =

      import filesystemOptions.createNonexistentParents.enabled
      moveTo(unsafely(destination.child(path.descent.head)))


  def symlinkTo(destination: Path on plane)
       (using overwritePreexisting:     OverwritePreexisting on plane,
              createNonexistentParents: CreateNonexistentParents on plane)
  : Path on plane raises IoError =

      createNonexistentParents(destination):
        overwritePreexisting(destination):
          jnf.Files.createSymbolicLink(destination.javaPath, path.javaPath)

      destination


  def symlinkInto
       (destination: Path on plane)
       (using overwritePreexisting: OverwritePreexisting on plane,
              moveAtomically:       MoveAtomically,
              substantiable:        (Path on plane) is Substantiable,
              dereferenceSymlinks:  DereferenceSymlinks)
  : Path on plane raises IoError =

      import filesystemOptions.createNonexistentParents.enabled
      symlinkTo(unsafely(destination.child(path.descent.head)))


  def modified[instant: Instantiable across Instants from Long](): instant =
    instant(jnf.Files.getLastModifiedTime(path.javaPath).nn.toInstant.nn.toEpochMilli)

  def accessed[instant: Instantiable across Instants from Long](): instant =
    val attributes = jnf.Files.readAttributes(path.javaPath, classOf[jnfa.BasicFileAttributes]).nn
    instant(attributes.lastAccessTime().nn.toInstant.nn.toEpochMilli)

  def readable: FilesystemAttribute.Readable[plane] = FilesystemAttribute.Readable(path)
  def writable: FilesystemAttribute.Writable[plane] = FilesystemAttribute.Writable(path)

  def hidden(): Boolean raises IoError =
    protect(Operation.Metadata)(jnf.Files.isHidden(path.javaPath))

  def touch(): Unit raises IoError = protect(Operation.Metadata):
    jnf.Files.setLastModifiedTime
     (path.javaPath, jnfa.FileTime.fromMillis(java.lang.System.currentTimeMillis))

  def make[entry: Makable on plane](): entry.Result = entry.make(path)

extension (path: Path on Windows)
  def created[instant: Instantiable across Instants from Long](): instant raises IoError =
    path.protect(Operation.Metadata):
      val attributes = jnf.Files.readAttributes(path.javaPath, classOf[jnfa.BasicFileAttributes]).nn
      instant(attributes.creationTime().nn.toInstant.nn.toEpochMilli)

extension [plane <: Posix: Filesystem](path: Path on plane)
  def executable: FilesystemAttribute.Executable[plane] =
    FilesystemAttribute.Executable(path)

  def hardLinks()(using dereferenceSymlinks: DereferenceSymlinks): Int raises IoError =
    path.protect(Operation.Metadata):
      jnf.Files.getAttribute(jnf.Path.of(path.show.s), "unix:nlink", dereferenceSymlinks.options()*)
      . match
          case count: Int => count
          case _          => raise(IoError(path, Operation.Metadata, Reason.Unsupported)) yet 1

package filesystemOptions:
  object readAccess:
    given enabled: ReadAccess:
      type Transform[HandleType] = HandleType & ReadAccess.Ability
      def options(): List[jnf.OpenOption] = List(jnf.StandardOpenOption.READ)

    given disabled: ReadAccess:
      type Transform[HandleType] = HandleType
      def options(): List[jnf.OpenOption] = Nil

  object writeAccess:
    given enabled: WriteAccess:
      type Transform[HandleType] = HandleType & WriteAccess.Ability
      def options(): List[jnf.OpenOption] = List(jnf.StandardOpenOption.WRITE)

    given disabled: WriteAccess:
      type Transform[HandleType] = HandleType
      def options(): List[jnf.OpenOption] = Nil

  object dereferenceSymlinks:
    given enabled: DereferenceSymlinks:
      def dereference = true
      def options() = Nil

    given disabled: DereferenceSymlinks:
      def dereference = false
      def options() = List(jnf.LinkOption.NOFOLLOW_LINKS)

  object moveAtomically:
    given enabled: MoveAtomically = () => List(jnf.StandardCopyOption.ATOMIC_MOVE)
    given disabled: MoveAtomically = () => Nil

  object copyAttributes:
    given enabled: CopyAttributes = () => List(jnf.StandardCopyOption.COPY_ATTRIBUTES)
    given disabled: CopyAttributes = () => Nil

  object deleteRecursively:
    given enabled: [plane: Filesystem] => Tactic[IoError]
          => (explorable: plane is Explorable)
          =>  DeleteRecursively on plane:

      import filesystemOptions.dereferenceSymlinks.disabled

      type World = plane

      def recur(path: Path on plane): Unit =
        path.children.each(recur(_))
        jnf.Files.delete(jnf.Path.of(path.show.s))

      def conditionally[result](path: Path on Plane)(operation: => result): result =
        path.children.each(recur(_)) yet operation

    given disabled: [plane: {Filesystem, Explorable}] => Tactic[IoError]
          =>  DeleteRecursively on plane:

      type Plane = plane

      def conditionally[result](path: Path on Plane)(operation: => result): result =
        import filesystemOptions.dereferenceSymlinks.disabled
        if !path.children.nil
        then abort(IoError(path, IoError.Operation.Delete, Reason.DirectoryNotEmpty))
        else operation

  object overwritePreexisting:
    given enabled: [plane: Filesystem]
          => (deleteRecursively: DeleteRecursively on plane)
          =>  OverwritePreexisting on plane:
      type Plane = plane

      def apply[result](path: Path on Plane)(operation: => result): result =
        deleteRecursively.conditionally(path)(operation)

    given disabled: [plane: Filesystem] => Tactic[IoError]
    =>  OverwritePreexisting on plane:

      type Plane = plane

      def apply[result](path: Path on Plane)(operation: => result): result =
        try operation catch case error: jnf.FileAlreadyExistsException =>
          abort(IoError(path, IoError.Operation.Write, Reason.AlreadyExists))

  object createNonexistentParents:
    given enabled: [plane: Filesystem] => Tactic[IoError] => (Path on plane) is Substantiable
          =>  CreateNonexistentParents on plane:

      def apply[result](path: Path on plane)(operation: => result): result =
        val parent: Optional[Path on plane] = safely(path.parent)
        parent.let: parent =>
          import dereferenceSymlinks.disabled

          if !parent.exists() || parent.entry() != Directory
          then jnf.Files.createDirectories(jnf.Path.of(parent.show.s))

        operation

    given disabled: [plane: Filesystem] => Tactic[IoError]
          =>  CreateNonexistentParents on plane:
      type Plane = plane

      def apply[result](path: Path on plane)(block: => result): result =
        path.protect(Operation.Write)(block)

  object createNonexistent:
    given enabled: [plane: Filesystem]
          => (create: CreateNonexistentParents on plane)
          =>  (Path on plane) is Substantiable
          =>  CreateNonexistent on plane:
      type Plane = plane

      def error(path: Path on Plane, operation: IoError.Operation): Nothing =
        import strategies.throwUnsafely
        abort(IoError(path, operation, Reason.Nonexistent))

      def apply(path: Path on Plane)(operation: => Unit): Unit =
        if !path.exists() then create(path)(operation)

      def options(): List[jnf.OpenOption] = List(jnf.StandardOpenOption.CREATE)

    given disabled: [plane: Filesystem] => Tactic[IoError]
          =>  CreateNonexistent on plane:

      type Plane = plane

      def error(path: Path on Plane, operation: IoError.Operation): Nothing =
        abort(IoError(path, operation, Reason.Nonexistent))

      def apply(path: Path on Plane)(operation: => Unit): Unit = ()
      def options(): List[jnf.OpenOption] = List()

  object writeSynchronously:
    given enabled: WriteSynchronously = () => List(jnf.StandardOpenOption.SYNC)
    given disabled: WriteSynchronously = () => Nil
