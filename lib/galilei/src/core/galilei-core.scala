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

package pathNavigation:
  export Linux.navigable as linux
  export Windows.navigable as windows
  export MacOs.navigable as macOs
  export Posix.navigable as posix
  export Filesystem.navigable as operatingSystem

final val C: WindowsDrive = WindowsDrive('C')
final val D: WindowsDrive = WindowsDrive('D')

@targetName("LinuxRoot")
final val `%`: Linux.Root = Linux.RootSingleton

@targetName("MacOsRoot")
final val `$`: MacOs.Root = MacOs.RootSingleton

extension [openable: Openable](value: openable)
  def open[result](lambda: openable.Result => result, options: List[openable.Operand] = Nil)
  :     result =
    openable.open(value, lambda, options)

extension [platform <: Filesystem](path: Path on platform)
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

  def children(using symlinks: DereferenceSymlinks): Stream[Path on platform] raises IoError =
    val list = safely(jnf.Files.list(path.javaPath).nn.toScala(Stream)).or(Stream())

    list.map: child =>
      unsafely(path.child(child.getFileName.nn.toString.nn.tt))

  def descendants(using DereferenceSymlinks, TraversalOrder)
      : Stream[Path on platform] raises IoError =
    children.flatMap: child =>
      summon[TraversalOrder] match
        case TraversalOrder.PreOrder  => child #:: child.descendants
        case TraversalOrder.PostOrder => child.descendants #::: Stream(child)

  def size(): Memory raises IoError =
    import filesystemOptions.dereferenceSymlinks.disabled
    given TraversalOrder = TraversalOrder.PreOrder
    descendants.fuse(jnf.Files.size(path.javaPath).b)(state + next.size())

  def delete()(using deleteRecursively: DeleteRecursively on platform)
  :     Path on platform raises IoError =
    protect(Operation.Delete):
      deleteRecursively.conditionally(path)(jnf.Files.delete(path.javaPath))

    path

  def wipe()(using deleteRecursively: DeleteRecursively on platform)(using io: Tactic[IoError])
  :     Path on platform raises IoError =
    deleteRecursively.conditionally(path)(jnf.Files.deleteIfExists(javaPath))
    path

  def volume(): Volume =
    val fileStore = jnf.Files.getFileStore(path.javaPath).nn
    Volume(fileStore.name.nn.tt, fileStore.`type`.nn.tt)

  def hardLinkTo(destination: Path on platform)
     (using overwritePreexisting: OverwritePreexisting on platform,
            createNonexistentParents: CreateNonexistentParents on platform)
  :     Path on platform raises IoError =

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

  def copyTo(destination: Path on platform)
     (using overwritePreexisting:     OverwritePreexisting on platform,
            dereferenceSymlinks:      DereferenceSymlinks,
            createNonexistentParents: CreateNonexistentParents on platform)
  :     Path on platform raises IoError =

    createNonexistentParents(destination):
      overwritePreexisting(destination):
        jnf.Files.copy(path.javaPath, destination.javaPath, dereferenceSymlinks.options()*)

    destination

  def copyInto
     (destination: Path on platform)
     (using overwritePreexisting: OverwritePreexisting on platform,
            dereferenceSymlinks:  DereferenceSymlinks,
            substantiable:        (Path on platform) is Substantiable)
  :     Path on platform raises IoError =

    given CreateNonexistentParents on platform =
      filesystemOptions.createNonexistentParents.enabled[platform]

    copyTo(unsafely(destination.child(path.textDescent.head)))

  def renameTo
     (using navigable: platform is Navigable,
            overwritePreexisting:     OverwritePreexisting on platform,
            moveAtomically:           MoveAtomically,
            dereferenceSymlinks:      DereferenceSymlinks,
            createNonexistentParents: CreateNonexistentParents on platform)
     (name: (prior: navigable.Operand) ?=> navigable.Operand)
  :     Path on platform raises IoError raises PathError =
    val name0 = path.name.or:
      abort(IoError(path, IoError.Operation.Metadata, Reason.Unsupported))

    path.moveTo(path.peer(name(using name0)))

  def moveTo(destination: Path on platform)
     (using overwritePreexisting:     OverwritePreexisting on platform,
            moveAtomically:           MoveAtomically,
            dereferenceSymlinks:      DereferenceSymlinks,
            createNonexistentParents: CreateNonexistentParents on platform)
  :     Path on platform raises IoError =

    val options: Seq[jnf.CopyOption] = dereferenceSymlinks.options() ++ moveAtomically.options()

    createNonexistentParents(destination):
      overwritePreexisting(destination):
        jnf.Files.move(path.javaPath, destination.javaPath, options*)

    destination

  def moveInto
     (destination: Path on platform)
     (using overwritePreexisting: OverwritePreexisting on platform,
            moveAtomically:       MoveAtomically,
            substantiable:        (Path on platform) is Substantiable,
            dereferenceSymlinks:  DereferenceSymlinks)
  :     Path on platform raises IoError =

    import filesystemOptions.createNonexistentParents.enabled
    moveTo(unsafely(destination.child(path.textDescent.head)))

  def symlinkTo(destination: Path on platform)
     (using overwritePreexisting:    OverwritePreexisting on platform,
            createNonexistentParents: CreateNonexistentParents on platform)
  :     Path on platform raises IoError =

    createNonexistentParents(destination):
      overwritePreexisting(destination):
        jnf.Files.createSymbolicLink(destination.javaPath, path.javaPath)

    destination

  def symlinkInto
     (destination: Path on platform)
     (using overwritePreexisting: OverwritePreexisting on platform,
            moveAtomically:       MoveAtomically,
            substantiable:        (Path on platform) is Substantiable,
            dereferenceSymlinks:  DereferenceSymlinks)
  :     Path on platform raises IoError =

    import filesystemOptions.createNonexistentParents.enabled
    symlinkTo(unsafely(destination.child(path.textDescent.head)))

  def modified[instant: Instantiable across Instants from Long](): instant =
    instant(jnf.Files.getLastModifiedTime(path.javaPath).nn.toInstant.nn.toEpochMilli)

  def accessed[instant: Instantiable across Instants from Long](): instant =
    val attributes = jnf.Files.readAttributes(path.javaPath, classOf[jnfa.BasicFileAttributes]).nn
    instant(attributes.lastAccessTime().nn.toInstant.nn.toEpochMilli)

  def readable: FilesystemAttribute.Readable[platform] = FilesystemAttribute.Readable(path)
  def writable: FilesystemAttribute.Writable[platform] = FilesystemAttribute.Writable(path)

  def hidden(): Boolean raises IoError =
    protect(Operation.Metadata)(jnf.Files.isHidden(path.javaPath))

  def touch(): Unit raises IoError = protect(Operation.Metadata):
    jnf.Files.setLastModifiedTime
     (path.javaPath, jnfa.FileTime.fromMillis(System.currentTimeMillis))

  def make[EntryType: Makable on platform](): EntryType.Result = EntryType.make(path)

extension [platform <: Windows](path: Path on platform)
  def created[instant: Instantiable across Instants from Long](): instant raises IoError =
    path.protect(Operation.Metadata):
      val attributes = jnf.Files.readAttributes(path.javaPath, classOf[jnfa.BasicFileAttributes]).nn
      instant(attributes.creationTime().nn.toInstant.nn.toEpochMilli)

extension [platform <: Posix](path: Path on platform)
  def executable: FilesystemAttribute.Executable[platform] =
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
    given enabled: [platform <: Filesystem] => Tactic[IoError]
    =>    DeleteRecursively on platform:

      import filesystemOptions.dereferenceSymlinks.disabled

      type Platform = platform

      def recur(path: Path on platform): Unit =
        path.children.each(recur(_))
        jnf.Files.delete(path.javaPath)

      def conditionally[ResultType](path: Path on Platform)(operation: => ResultType): ResultType =
        path.children.each(recur(_)) yet operation

    given disabled: [platform <: Filesystem] => Tactic[IoError]
    =>    DeleteRecursively on platform:

      type Platform = platform

      def conditionally[ResultType](path: Path on Platform)(operation: => ResultType): ResultType =
        import filesystemOptions.dereferenceSymlinks.disabled
        if !path.children.isEmpty
        then abort(IoError(path, IoError.Operation.Delete, Reason.DirectoryNotEmpty))
        else operation

  object overwritePreexisting:
    given enabled: [platform <: Filesystem]
    =>   (deleteRecursively: DeleteRecursively on platform)
    =>    OverwritePreexisting on platform:
      type Platform = platform

      def apply[ResultType](path: Path on Platform)(operation: => ResultType): ResultType =
        deleteRecursively.conditionally(path)(operation)

    given disabled: [platform <: Filesystem] => Tactic[IoError]
    =>   OverwritePreexisting on platform:

      type Platform = platform

      def apply[ResultType](path: Path on Platform)(operation: => ResultType): ResultType =
        try operation catch case error: jnf.FileAlreadyExistsException =>
          abort(IoError(path, IoError.Operation.Write, Reason.AlreadyExists))

  object createNonexistentParents:
    given enabled: [platform <: Filesystem] => Tactic[IoError]
    =>   (Path on platform) is Substantiable
    =>    CreateNonexistentParents on platform:

      def apply[ResultType](path: Path on platform)(operation: => ResultType): ResultType =
        path.parent.let: parent =>
          import dereferenceSymlinks.disabled

          if !parent.exists() || parent.entry() != Directory
          then jnf.Files.createDirectories(parent.javaPath)

        operation

    given disabled: [platform <: Filesystem] => Tactic[IoError]
    =>    CreateNonexistentParents on platform:
      type Platform = platform

      def apply[ResultType](path: Path on platform)(block: => ResultType): ResultType =
        path.protect(Operation.Write)(block)

  object createNonexistent:
    given enabled: [platform <: Filesystem]
    =>   (create: CreateNonexistentParents on platform)
    =>    (Path on platform) is Substantiable
    =>    CreateNonexistent on platform:
      type Platform = platform

      def error(path: Path on Platform, operation: IoError.Operation): Nothing =
        import strategies.throwUnsafely
        abort(IoError(path, operation, Reason.Nonexistent))

      def apply(path: Path on Platform)(operation: => Unit): Unit =
        if !path.exists() then create(path)(operation)

      def options(): List[jnf.OpenOption] = List(jnf.StandardOpenOption.CREATE)

    given disabled: [platform <: Filesystem] => Tactic[IoError]
    =>    CreateNonexistent on platform:

      type Platform = platform

      def error(path: Path on Platform, operation: IoError.Operation): Nothing =
        abort(IoError(path, operation, Reason.Nonexistent))

      def apply(path: Path on Platform)(operation: => Unit): Unit = ()
      def options(): List[jnf.OpenOption] = List()

  object writeSynchronously:
    given enabled: WriteSynchronously = () => List(jnf.StandardOpenOption.SYNC)
    given disabled: WriteSynchronously = () => Nil
