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
  :     result =
    target.open(value, lambda, options)

extension [platform: System](path: Path on platform)

  inline def children(using explorable: platform is Explorable): Stream[Path on platform] =
    explorable.children(path)

  private[galilei] def protect[result](operation: Operation)(block: => result)
  :     result raises IoError =
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

  def descendants(using DereferenceSymlinks, TraversalOrder, platform is Explorable)
      : Stream[Path on platform] raises IoError =
    path.children.flatMap: child =>
      summon[TraversalOrder] match
        case TraversalOrder.PreOrder  => child #:: child.descendants
        case TraversalOrder.PostOrder => child.descendants #::: Stream(child)

  def size()(using platform is Explorable): Memory raises IoError =
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
     (using overwritePreexisting:    OverwritePreexisting on platform,
            dereferenceSymlinks:     DereferenceSymlinks,
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

    val file2: Path on platform = unsafely(destination.child(path.descent.head))
    copyTo(file2)

  // def renameTo
  //    (using overwritePreexisting:     OverwritePreexisting on platform,
  //           moveAtomically:           MoveAtomically,
  //           dereferenceSymlinks:      DereferenceSymlinks,
  //           createNonexistentParents: CreateNonexistentParents on platform,
  //           nominative:               platform is Nominative,
  //           admissible:               Text is Admissible on platform)
  //    (name: (prior: Text) ?=> Text)
  // :     Path on platform raises IoError raises NameError raises PathError =
  //   val name2: Text = name(using path.name)
  //   val peer = path.peer(name2)
  //   path.moveTo(peer)

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
    moveTo(unsafely(destination.child(path.descent.head)))

  def symlinkTo(destination: Path on platform)
     (using overwritePreexisting:     OverwritePreexisting on platform,
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
    symlinkTo(unsafely(destination.child(path.descent.head)))

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
     (path.javaPath, jnfa.FileTime.fromMillis(java.lang.System.currentTimeMillis))

  def make[EntryType: Makable on platform](): EntryType.Result = EntryType.make(path)

extension (path: Path on Windows)
  def created[instant: Instantiable across Instants from Long](): instant raises IoError =
    path.protect(Operation.Metadata):
      val attributes = jnf.Files.readAttributes(path.javaPath, classOf[jnfa.BasicFileAttributes]).nn
      instant(attributes.creationTime().nn.toInstant.nn.toEpochMilli)

extension [platform <: Posix: System](path: Path on platform)
  def executable: FilesystemAttribute.Executable[platform] =
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
    given enabled: [platform: System] => Tactic[IoError]
    =>    (explorable: platform is Explorable)
    =>    DeleteRecursively on platform:

      import filesystemOptions.dereferenceSymlinks.disabled

      type Platform = platform

      def recur(path: Path on platform): Unit =
        path.children.each(recur(_))
        jnf.Files.delete(jnf.Path.of(path.show.s))

      def conditionally[result](path: Path on Platform)(operation: => result): result =
        path.children.each(recur(_)) yet operation

    given disabled: [platform: {System, Explorable}] => Tactic[IoError]
    =>    DeleteRecursively on platform:

      type Platform = platform

      def conditionally[result](path: Path on Platform)(operation: => result): result =
        import filesystemOptions.dereferenceSymlinks.disabled
        if !path.children.isEmpty
        then abort(IoError(path, IoError.Operation.Delete, Reason.DirectoryNotEmpty))
        else operation

  object overwritePreexisting:
    given enabled: [platform: System]
    =>   (deleteRecursively: DeleteRecursively on platform)
    =>    OverwritePreexisting on platform:
      type Platform = platform

      def apply[result](path: Path on Platform)(operation: => result): result =
        deleteRecursively.conditionally(path)(operation)

    given disabled: [platform: System] => Tactic[IoError]
    =>   OverwritePreexisting on platform:

      type Platform = platform

      def apply[result](path: Path on Platform)(operation: => result): result =
        try operation catch case error: jnf.FileAlreadyExistsException =>
          abort(IoError(path, IoError.Operation.Write, Reason.AlreadyExists))

  object createNonexistentParents:
    given enabled: [platform: System] => Tactic[IoError]
    =>    (Path on platform) is Substantiable
    =>    CreateNonexistentParents on platform:

      def apply[result](path: Path on platform)(operation: => result): result =
        val parent: Optional[Path on platform] = safely(path.parent)
        parent.let: parent =>
          import dereferenceSymlinks.disabled

          if !parent.exists() || parent.entry() != Directory
          then jnf.Files.createDirectories(jnf.Path.of(parent.show.s))

        operation

    given disabled: [platform: System] => Tactic[IoError]
    =>    CreateNonexistentParents on platform:
      type Platform = platform

      def apply[result](path: Path on platform)(block: => result): result =
        path.protect(Operation.Write)(block)

  object createNonexistent:
    given enabled: [platform: System]
    =>    (create: CreateNonexistentParents on platform)
    =>    (Path on platform) is Substantiable
    =>    CreateNonexistent on platform:
      type Platform = platform

      def error(path: Path on Platform, operation: IoError.Operation): Nothing =
        import strategies.throwUnsafely
        abort(IoError(path, operation, Reason.Nonexistent))

      def apply(path: Path on Platform)(operation: => Unit): Unit =
        if !path.exists() then create(path)(operation)

      def options(): List[jnf.OpenOption] = List(jnf.StandardOpenOption.CREATE)

    given disabled: [platform: System] => Tactic[IoError]
    =>    CreateNonexistent on platform:

      type Platform = platform

      def error(path: Path on Platform, operation: IoError.Operation): Nothing =
        abort(IoError(path, operation, Reason.Nonexistent))

      def apply(path: Path on Platform)(operation: => Unit): Unit = ()
      def options(): List[jnf.OpenOption] = List()

  object writeSynchronously:
    given enabled: WriteSynchronously = () => List(jnf.StandardOpenOption.SYNC)
    given disabled: WriteSynchronously = () => Nil
