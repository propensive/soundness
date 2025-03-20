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
┃    Soundness, version 0.27.0.                                                                    ┃
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
import fulminate.*
import nomenclature.*
import prepositional.*
import proscenium.*
import rudiments.*
import serpentine.*
import vacuous.*

import IoError.{Operation, Reason}

final val C: Drive = Drive('C')
final val D: Drive = Drive('D')

extension [ValueType: Openable](value: ValueType)
  def open[ResultType]
     (lambda:  ValueType.Result => ResultType, options: List[ValueType.Operand] = Nil)
  :     ResultType =
    ValueType.open(value, lambda, options)

extension [PlatformType: System](path: Path on PlatformType)
  private[galilei] def protect[ResultType](operation: Operation)(block: => ResultType)
  :     ResultType raises IoError =
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

  def children(using symlinks: DereferenceSymlinks): Stream[Path on PlatformType] raises IoError =
    val list = safely(jnf.Files.list(path.javaPath).nn.toScala(Stream)).or(Stream())

    list.map: child =>
      unsafely(path.child(child.getFileName.nn.toString.nn.tt))

  def descendants(using DereferenceSymlinks, TraversalOrder)
      : Stream[Path on PlatformType] raises IoError =
    children.flatMap: child =>
      summon[TraversalOrder] match
        case TraversalOrder.PreOrder  => child #:: child.descendants
        case TraversalOrder.PostOrder => child.descendants #::: Stream(child)

  def size(): Memory raises IoError =
    import filesystemOptions.dereferenceSymlinks.disabled
    given TraversalOrder = TraversalOrder.PreOrder
    descendants.fuse(jnf.Files.size(path.javaPath).b)(state + next.size())

  def delete()(using deleteRecursively: DeleteRecursively on PlatformType)
  :     Path on PlatformType raises IoError =
    protect(Operation.Delete):
      deleteRecursively.conditionally(path)(jnf.Files.delete(path.javaPath))

    path

  def wipe()(using deleteRecursively: DeleteRecursively on PlatformType)(using io: Tactic[IoError])
  :     Path on PlatformType raises IoError =
    deleteRecursively.conditionally(path)(jnf.Files.deleteIfExists(javaPath))
    path

  def volume(): Volume =
    val fileStore = jnf.Files.getFileStore(path.javaPath).nn
    Volume(fileStore.name.nn.tt, fileStore.`type`.nn.tt)

  def hardLinkTo(destination: Path on PlatformType)
     (using overwritePreexisting: OverwritePreexisting on PlatformType,
            createNonexistentParents: CreateNonexistentParents on PlatformType)
  :     Path on PlatformType raises IoError =

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

  def copyTo(destination: Path on PlatformType)
     (using overwritePreexisting:     OverwritePreexisting on PlatformType,
            dereferenceSymlinks:      DereferenceSymlinks,
            createNonexistentParents: CreateNonexistentParents on PlatformType)
  :     Path on PlatformType raises IoError =

    createNonexistentParents(destination):
      overwritePreexisting(destination):
        jnf.Files.copy(path.javaPath, destination.javaPath, dereferenceSymlinks.options()*)

    destination

  def copyInto
     (destination: Path on PlatformType)
     (using overwritePreexisting: OverwritePreexisting on PlatformType,
            dereferenceSymlinks:  DereferenceSymlinks,
            substantiable:        (Path on PlatformType) is Substantiable)
  :     Path on PlatformType raises IoError =

    given CreateNonexistentParents on PlatformType =
      filesystemOptions.createNonexistentParents.enabled[PlatformType]

    copyTo(unsafely(destination.child(path.textDescent.head)))

  def renameTo
     (using overwritePreexisting:     OverwritePreexisting on PlatformType,
            moveAtomically:           MoveAtomically,
            dereferenceSymlinks:      DereferenceSymlinks,
            createNonexistentParents: CreateNonexistentParents on PlatformType)
     (name: (prior: Text) ?=> Text)
  :     Path on PlatformType raises IoError raises PathError =
    val name0 = path.name.or:
      abort(IoError(path, IoError.Operation.Metadata, Reason.Unsupported))

    path.moveTo(path.peer(name(using name0)))

  def moveTo(destination: Path on PlatformType)
     (using overwritePreexisting:     OverwritePreexisting on PlatformType,
            moveAtomically:           MoveAtomically,
            dereferenceSymlinks:      DereferenceSymlinks,
            createNonexistentParents: CreateNonexistentParents on PlatformType)
  :     Path on PlatformType raises IoError =

    val options: Seq[jnf.CopyOption] = dereferenceSymlinks.options() ++ moveAtomically.options()

    createNonexistentParents(destination):
      overwritePreexisting(destination):
        jnf.Files.move(path.javaPath, destination.javaPath, options*)

    destination

  def moveInto
     (destination: Path on PlatformType)
     (using overwritePreexisting: OverwritePreexisting on PlatformType,
            moveAtomically:       MoveAtomically,
            substantiable:        (Path on PlatformType) is Substantiable,
            dereferenceSymlinks:  DereferenceSymlinks)
  :     Path on PlatformType raises IoError =

    import filesystemOptions.createNonexistentParents.enabled
    moveTo(unsafely(destination.child(path.textDescent.head)))

  def symlinkTo(destination: Path on PlatformType)
     (using overwritePreexisting:     OverwritePreexisting on PlatformType,
            createNonexistentParents: CreateNonexistentParents on PlatformType)
  :     Path on PlatformType raises IoError =

    createNonexistentParents(destination):
      overwritePreexisting(destination):
        jnf.Files.createSymbolicLink(destination.javaPath, path.javaPath)

    destination

  def symlinkInto
     (destination: Path on PlatformType)
     (using overwritePreexisting: OverwritePreexisting on PlatformType,
            moveAtomically:       MoveAtomically,
            substantiable:        (Path on PlatformType) is Substantiable,
            dereferenceSymlinks:  DereferenceSymlinks)
  :     Path on PlatformType raises IoError =

    import filesystemOptions.createNonexistentParents.enabled
    symlinkTo(unsafely(destination.child(path.textDescent.head)))

  def modified[InstantType: Instantiable across Instants from Long](): InstantType =
    InstantType(jnf.Files.getLastModifiedTime(path.javaPath).nn.toInstant.nn.toEpochMilli)

  def accessed[InstantType: Instantiable across Instants from Long](): InstantType =
    val attributes = jnf.Files.readAttributes(path.javaPath, classOf[jnfa.BasicFileAttributes]).nn
    InstantType(attributes.lastAccessTime().nn.toInstant.nn.toEpochMilli)

  def readable: FilesystemAttribute.Readable[PlatformType] = FilesystemAttribute.Readable(path)
  def writable: FilesystemAttribute.Writable[PlatformType] = FilesystemAttribute.Writable(path)

  def hidden(): Boolean raises IoError =
    protect(Operation.Metadata)(jnf.Files.isHidden(path.javaPath))

  def touch(): Unit raises IoError = protect(Operation.Metadata):
    jnf.Files.setLastModifiedTime
     (path.javaPath, jnfa.FileTime.fromMillis(System.currentTimeMillis))

  def make[EntryType: Makable on PlatformType](): EntryType.Result = EntryType.make(path)

extension [PlatformType <: Windows](path: Path on PlatformType)
  def created[InstantType: Instantiable across Instants from Long](): InstantType raises IoError =
    path.protect(Operation.Metadata):
      val attributes = jnf.Files.readAttributes(path.javaPath, classOf[jnfa.BasicFileAttributes]).nn
      InstantType(attributes.creationTime().nn.toInstant.nn.toEpochMilli)

extension [PlatformType <: Posix](path: Path on PlatformType)
  def executable: FilesystemAttribute.Executable[PlatformType] =
    FilesystemAttribute.Executable(path)

  def hardLinks()(using dereferenceSymlinks: DereferenceSymlinks): Int raises IoError =
    path.protect(Operation.Metadata):
      jnf.Files.getAttribute(path.javaPath, "unix:nlink", dereferenceSymlinks.options()*) match
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
    given enabled: [PlatformType <: System] => Tactic[IoError]
    =>    DeleteRecursively on PlatformType:

      import filesystemOptions.dereferenceSymlinks.disabled

      type Platform = PlatformType

      def recur(path: Path on PlatformType): Unit =
        path.children.each(recur(_))
        jnf.Files.delete(path.javaPath)

      def conditionally[ResultType](path: Path on Platform)(operation: => ResultType): ResultType =
        path.children.each(recur(_)) yet operation

    given disabled: [PlatformType <: System] => Tactic[IoError]
    =>    DeleteRecursively on PlatformType:

      type Platform = PlatformType

      def conditionally[ResultType](path: Path on Platform)(operation: => ResultType): ResultType =
        import filesystemOptions.dereferenceSymlinks.disabled
        if !path.children.isEmpty
        then abort(IoError(path, IoError.Operation.Delete, Reason.DirectoryNotEmpty))
        else operation

  object overwritePreexisting:
    given enabled: [PlatformType <: System]
    =>   (deleteRecursively: DeleteRecursively on PlatformType)
    =>    OverwritePreexisting on PlatformType:
      type Platform = PlatformType

      def apply[ResultType](path: Path on Platform)(operation: => ResultType): ResultType =
        deleteRecursively.conditionally(path)(operation)

    given disabled: [PlatformType <: System] => Tactic[IoError]
    =>   OverwritePreexisting on PlatformType:

      type Platform = PlatformType

      def apply[ResultType](path: Path on Platform)(operation: => ResultType): ResultType =
        try operation catch case error: jnf.FileAlreadyExistsException =>
          abort(IoError(path, IoError.Operation.Write, Reason.AlreadyExists))

  object createNonexistentParents:
    given enabled: [PlatformType <: System] => Tactic[IoError]
    =>    (Path on PlatformType) is Substantiable
    =>    CreateNonexistentParents on PlatformType:

      def apply[ResultType](path: Path on PlatformType)(operation: => ResultType): ResultType =
        safely(path.parent).let: parent =>
          import dereferenceSymlinks.disabled

          if !parent.exists() || parent.entry() != Directory
          then jnf.Files.createDirectories(parent.javaPath)

        operation

    given disabled: [PlatformType <: System] => Tactic[IoError]
    =>    CreateNonexistentParents on PlatformType:
      type Platform = PlatformType

      def apply[ResultType](path: Path on PlatformType)(block: => ResultType): ResultType =
        path.protect(Operation.Write)(block)

  object createNonexistent:
    given enabled: [PlatformType <: System]
    =>    (create: CreateNonexistentParents on PlatformType)
    =>    (Path on PlatformType) is Substantiable
    =>    CreateNonexistent on PlatformType:
      type Platform = PlatformType

      def error(path: Path on Platform, operation: IoError.Operation): Nothing =
        import strategies.throwUnsafely
        abort(IoError(path, operation, Reason.Nonexistent))

      def apply(path: Path on Platform)(operation: => Unit): Unit =
        if !path.exists() then create(path)(operation)

      def options(): List[jnf.OpenOption] = List(jnf.StandardOpenOption.CREATE)

    given disabled: [PlatformType <: System] => Tactic[IoError]
    =>    CreateNonexistent on PlatformType:

      type Platform = PlatformType

      def error(path: Path on Platform, operation: IoError.Operation): Nothing =
        abort(IoError(path, operation, Reason.Nonexistent))

      def apply(path: Path on Platform)(operation: => Unit): Unit = ()
      def options(): List[jnf.OpenOption] = List()

  object writeSynchronously:
    given enabled: WriteSynchronously = () => List(jnf.StandardOpenOption.SYNC)
    given disabled: WriteSynchronously = () => Nil
