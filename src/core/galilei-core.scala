package galilei

import java.io as ji
import java.nio.file as jnf
import jnf.attribute as jnfa

import scala.jdk.StreamConverters.*

import contingency.*
import prepositional.*
import rudiments.*
import nomenclature.*
import fulminate.*
import serpentine.*
import spectacular.*
import anticipation.*
import vacuous.*

import language.experimental.captureChecking

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

extension [ValueType: Openable](value: ValueType)
  def open[ResultType]
      (lambda: ValueType.Result => ResultType, options: List[ValueType.Operand] = Nil)
          : ResultType =
    ValueType.open(value, lambda, options)

extension [PlatformType <: Filesystem](path: Path on PlatformType)
  private[galilei] def protect[ResultType](operation: Operation)(block: => ResultType)
          : ResultType raises IoError =
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
      case other                             => abort(IoError(path, operation, Unsupported))

  def javaPath: jnf.Path = jnf.Path.of(path.encode.s).nn
  def javaFile: ji.File = javaPath.toFile.nn
  def exists(): Boolean = jnf.Files.exists(javaPath)

  def children(using symlinks: DereferenceSymlinks): LazyList[Path on PlatformType] raises IoError =
    val list = safely(jnf.Files.list(path.javaPath).nn.toScala(LazyList)).or(LazyList())
    
    list.map: child =>
      unsafely(path.child(child.getFileName.nn.toString.nn.tt))

  def descendants(using DereferenceSymlinks, TraversalOrder)
      : LazyList[Path on PlatformType] raises IoError =
    children.flatMap: child =>
      summon[TraversalOrder] match
        case TraversalOrder.PreOrder  => child #:: child.descendants 
        case TraversalOrder.PostOrder => child.descendants #::: LazyList(child)
  
  def size(): ByteSize raises IoError =
    import filesystemOptions.dereferenceSymlinks.disabled
    given TraversalOrder = TraversalOrder.PreOrder
    descendants.foldLeft(jnf.Files.size(path.javaPath).b)(_ + _.size())
  
  def delete()(using deleteRecursively: DeleteRecursively on PlatformType)
          : Path on PlatformType raises IoError =
    protect(Operation.Delete):
      deleteRecursively.conditionally(path)(jnf.Files.delete(path.javaPath))

    path

  def wipe()(using deleteRecursively: DeleteRecursively on PlatformType)(using io: Tactic[IoError])
          : Path on PlatformType raises IoError =
    deleteRecursively.conditionally(path)(jnf.Files.deleteIfExists(javaPath))
    path
  
  def volume(): Volume =
    val fileStore = jnf.Files.getFileStore(path.javaPath).nn
    Volume(fileStore.name.nn.tt, fileStore.`type`.nn.tt)
  
  def hardLinkTo(destination: Path on PlatformType)
      (using overwritePreexisting: OverwritePreexisting on PlatformType,
             createNonexistentParents: CreateNonexistentParents on PlatformType)
          : Path on PlatformType raises IoError =

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
      (mode: @unchecked) match
        case mode: Int => (mode & 61440) match
          case  4096 => Fifo
          case  8192 => CharDevice
          case 24576 => BlockDevice
          case 49152 => Socket
          case _     => throw Panic(m"an unexpected POSIX mode value was returned")
  
  def copyTo(destination: Path on PlatformType)
      (using overwritePreexisting:     OverwritePreexisting on PlatformType,
             dereferenceSymlinks:      DereferenceSymlinks,
             createNonexistentParents: CreateNonexistentParents on PlatformType)
          : Path on PlatformType raises IoError =

    createNonexistentParents(destination):
      overwritePreexisting(destination):
        jnf.Files.copy(path.javaPath, destination.javaPath, dereferenceSymlinks.options()*)

    destination
  
  def copyInto
      (destination: Path on PlatformType)
      (using overwritePreexisting: OverwritePreexisting on PlatformType,
             dereferenceSymlinks:  DereferenceSymlinks)
          : Path on PlatformType raises IoError =

    given CreateNonexistentParents on PlatformType =
      filesystemOptions.createNonexistentParents.enabled[PlatformType]

    copyTo(unsafely(destination.child(path.textDescent.head)))

  def renameTo
      (using navigable: PlatformType is Navigable,
             overwritePreexisting:     OverwritePreexisting on PlatformType,
             moveAtomically:           MoveAtomically,
             dereferenceSymlinks:      DereferenceSymlinks,
             createNonexistentParents: CreateNonexistentParents on PlatformType)
      (name: (prior: navigable.Operand) ?=> navigable.Operand)
          : Path on PlatformType raises IoError raises PathError =
    val name0 = path.name.or:
      abort(IoError(path, IoError.Operation.Metadata, Reason.Unsupported))

    path.moveTo(path.peer(name(using name0)))

  def moveTo(destination: Path on PlatformType)
      (using overwritePreexisting:     OverwritePreexisting on PlatformType,
             moveAtomically:           MoveAtomically,
             dereferenceSymlinks:      DereferenceSymlinks,
             createNonexistentParents: CreateNonexistentParents on PlatformType)
          : Path on PlatformType raises IoError =

    val options: Seq[jnf.CopyOption] = dereferenceSymlinks.options() ++ moveAtomically.options()

    createNonexistentParents(destination):
      overwritePreexisting(destination):
        jnf.Files.move(path.javaPath, destination.javaPath, options*)

    destination
  
  def moveInto
      (destination: Path on PlatformType)
      (using overwritePreexisting: OverwritePreexisting on PlatformType,
             moveAtomically:       MoveAtomically,
             dereferenceSymlinks:  DereferenceSymlinks)
          : Path on PlatformType raises IoError =

    import filesystemOptions.createNonexistentParents.enabled
    moveTo(unsafely(destination.child(path.textDescent.head)))

  def symlinkTo(destination: Path on PlatformType)
      (using overwritePreexisting:     OverwritePreexisting on PlatformType,
             createNonexistentParents: CreateNonexistentParents on PlatformType)
          : Path on PlatformType raises IoError =

    createNonexistentParents(destination):
      overwritePreexisting(destination):
        jnf.Files.createSymbolicLink(destination.javaPath, path.javaPath)

    destination

  def symlinkInto
      (destination: Path on PlatformType)
      (using overwritePreexisting: OverwritePreexisting on PlatformType,
             moveAtomically:       MoveAtomically,
             dereferenceSymlinks:  DereferenceSymlinks)
          : Path on PlatformType raises IoError =

    import filesystemOptions.createNonexistentParents.enabled
    symlinkTo(unsafely(destination.child(path.textDescent.head)))

  def modified[InstantType: SpecificInstant](): InstantType =
    SpecificInstant(jnf.Files.getLastModifiedTime(path.javaPath).nn.toInstant.nn.toEpochMilli)
  
  def accessed[InstantType: SpecificInstant](): InstantType =
    val attributes = jnf.Files.readAttributes(path.javaPath, classOf[jnfa.BasicFileAttributes]).nn
    SpecificInstant(attributes.lastAccessTime().nn.toInstant.nn.toEpochMilli)

  def readable: FilesystemAttribute.Readable[PlatformType] = FilesystemAttribute.Readable(path)
  def writable: FilesystemAttribute.Writable[PlatformType] = FilesystemAttribute.Writable(path)
  
  def hidden(): Boolean raises IoError =
    protect(Operation.Metadata)(jnf.Files.isHidden(path.javaPath))

  def touch(): Unit raises IoError = protect(Operation.Metadata):
    jnf.Files.setLastModifiedTime
     (path.javaPath, jnfa.FileTime.fromMillis(System.currentTimeMillis))
  
  def create[EntryType: Creatable on PlatformType](): EntryType.Result = EntryType.create(path)

extension [PlatformType <: Windows](path: Path on PlatformType)
  def created[InstantType: SpecificInstant](): InstantType raises IoError =
    path.protect(Operation.Metadata):
      val attributes = jnf.Files.readAttributes(path.javaPath, classOf[jnfa.BasicFileAttributes]).nn
      SpecificInstant(attributes.creationTime().nn.toInstant.nn.toEpochMilli)

extension [PlatformType <: Posix](path: Path on PlatformType)
  def executable: FilesystemAttribute.Executable[PlatformType] =
    FilesystemAttribute.Executable(path)

  def hardLinks()(using dereferenceSymlinks: DereferenceSymlinks): Int raises IoError =
    path.protect(Operation.Metadata):
      jnf.Files.getAttribute(path.javaPath, "unix:nlink", dereferenceSymlinks.options()*) match
        case count: Int => count
        case _          => raise(IoError(path, Operation.Metadata, Reason.Unsupported), 1)
  
package filesystemOptions:
  object readAccess:
    given ReadAccess as enabled:
      type Transform[HandleType] = HandleType & ReadAccess.Ability
      def options(): List[jnf.OpenOption] = List(jnf.StandardOpenOption.READ)

    given ReadAccess as disabled:
      type Transform[HandleType] = HandleType
      def options(): List[jnf.OpenOption] = Nil
  
  object writeAccess:
    given WriteAccess as enabled:
      type Transform[HandleType] = HandleType & WriteAccess.Ability
      def options(): List[jnf.OpenOption] = List(jnf.StandardOpenOption.WRITE)

    given WriteAccess as disabled:
      type Transform[HandleType] = HandleType
      def options(): List[jnf.OpenOption] = Nil

  object dereferenceSymlinks:
    given DereferenceSymlinks as enabled:
      def dereference = true
      def options() = Nil
    
    given DereferenceSymlinks as disabled:
      def dereference = false
      def options() = List(jnf.LinkOption.NOFOLLOW_LINKS)

  object moveAtomically:
    given MoveAtomically as enabled = () => List(jnf.StandardCopyOption.ATOMIC_MOVE)
    given MoveAtomically as disabled = () => Nil

  object copyAttributes:
    given CopyAttributes as enabled = () => List(jnf.StandardCopyOption.COPY_ATTRIBUTES)
    given CopyAttributes as disabled = () => Nil

  object deleteRecursively:
    given [PlatformType <: Filesystem](using Tactic[IoError])
        => DeleteRecursively on PlatformType as enabled:

      import filesystemOptions.dereferenceSymlinks.disabled
    
      type Platform = PlatformType

      def recur(path: Path on PlatformType): Unit =
        path.children.each(recur(_))
        jnf.Files.delete(path.javaPath)
      
      def conditionally[ResultType](path: Path on Platform)(operation: => ResultType): ResultType =
        path.children.each(recur(_)) yet operation
      
    given [PlatformType <: Filesystem](using Tactic[IoError])
        => DeleteRecursively on PlatformType as disabled:

      type Platform = PlatformType
      
      def conditionally[ResultType](path: Path on Platform)(operation: => ResultType): ResultType =
        import filesystemOptions.dereferenceSymlinks.disabled
        if !path.children.isEmpty
        then abort(IoError(path, IoError.Operation.Delete, Reason.DirectoryNotEmpty))
        else operation
  
  object overwritePreexisting:
    given [PlatformType <: Filesystem](using deleteRecursively: DeleteRecursively on PlatformType)
        => OverwritePreexisting on PlatformType as enabled:
      type Platform = PlatformType

      def apply[ResultType](path: Path on Platform)(operation: => ResultType): ResultType =
        deleteRecursively.conditionally(path)(operation)

    given [PlatformType <: Filesystem](using Tactic[IoError])
        => OverwritePreexisting on PlatformType as disabled:

      type Platform = PlatformType

      def apply[ResultType](path: Path on Platform)(operation: => ResultType): ResultType =
        try operation catch case error: jnf.FileAlreadyExistsException =>
          abort(IoError(path, IoError.Operation.Write, Reason.AlreadyExists))

  object createNonexistentParents:
    given [PlatformType <: Filesystem](using Tactic[IoError])
        => CreateNonexistentParents on PlatformType as enabled:

      def apply[ResultType](path: Path on PlatformType)(operation: => ResultType): ResultType =
        path.parent.let: parent =>
          import dereferenceSymlinks.disabled

          if !parent.exists() || parent.entry() != Directory
          then jnf.Files.createDirectories(parent.javaPath)

        operation

    given [PlatformType <: Filesystem](using Tactic[IoError])
        => CreateNonexistentParents on PlatformType as disabled:
      type Platform = PlatformType

      def apply[ResultType](path: Path on PlatformType)(block: => ResultType): ResultType =
        path.protect(Operation.Write)(block)

  object createNonexistent:
    given [PlatformType <: Filesystem](using create: CreateNonexistentParents on PlatformType)
        => CreateNonexistent on PlatformType as enabled:
      type Platform = PlatformType

      def error(path: Path on Platform, operation: IoError.Operation): Nothing =
        import strategies.throwUnsafely
        abort(IoError(path, operation, Reason.Nonexistent))
      
      def apply(path: Path on Platform)(operation: => Unit): Unit =
        if !path.exists() then create(path)(operation)
      
      def options(): List[jnf.OpenOption] = List(jnf.StandardOpenOption.CREATE)

    given [PlatformType <: Filesystem](using Tactic[IoError])
        => CreateNonexistent on PlatformType as disabled:

      type Platform = PlatformType

      def error(path: Path on Platform, operation: IoError.Operation): Nothing =
        abort(IoError(path, operation, Reason.Nonexistent))

      def apply(path: Path on Platform)(operation: => Unit): Unit = ()
      def options(): List[jnf.OpenOption] = List()

  object writeSynchronously:
    given WriteSynchronously as enabled = () => List(jnf.StandardOpenOption.SYNC)
    given WriteSynchronously as disabled = () => Nil
