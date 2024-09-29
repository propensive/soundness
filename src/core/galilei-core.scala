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

package pathNavigation:
  export Linux.navigable as linux
  export Windows.navigable as windows
  export MacOs.navigable as macOs

final val C: WindowsDrive = WindowsDrive('C')
final val D: WindowsDrive = WindowsDrive('D')

@targetName("LinuxRoot")
final val `%`: Linux.Root = Linux.RootSingleton

@targetName("MacOsRoot")
final val `$`: MacOs.Root = MacOs.RootSingleton

extension [PlatformType](path: Path on PlatformType)
  def open[ResultType](lambda: Handle => ResultType): ResultType =
    val file = Handle(ji.FileInputStream(ji.File(path.encode.s)))
    try lambda(file) finally file.close()

  def javaPath: jnf.Path = jnf.Path.of(path.encode.s).nn
  def javaFile: ji.File = javaPath.toFile.nn
  def exists(): Boolean = jnf.Files.exists(javaPath)

  def children(using symlinks: DereferenceSymlinks): LazyList[Path on PlatformType] =
    jnf.Files.list(path.javaPath).nn.toScala(LazyList).map: child =>
      unsafely(path.child(child.getFileName.nn.toString.nn.tt))

  def descendants(using DereferenceSymlinks, TraversalOrder): LazyList[Path] =
    children.flatMap: child =>
      summon[TraversalOrder] match
        case TraversalOrder.PreOrder  => child #:: child.descendants 
        case TraversalOrder.PostOrder => child.descendants #::: LazyList(child)
  
  def size(): ByteSize raises IoError =
    import filesystemOptions.dereferenceSymlinks.disabled
    given TraversalOrder = TraversalOrder.PreOrder
    descendants.foldLeft(jnf.Files.size(path.javaPath).b)(_ + _.size())
  
  def delete()(using deleteRecursively: DeleteRecursively): Path raises IoError =
    try deleteRecursively.conditionally(path)(jnf.Files.delete(path.javaPath)) catch
      case error: jnf.NoSuchFileException  =>
        raise(IoError(path, IoError.Operation.Delete, IoError.Reason.Nonexistent))
      
      case error: ji.FileNotFoundException =>
        raise(IoError(path, IoError.Operation.Delete, IoError.Reason.Nonexistent))
      
      case error: ji.IOException =>
        raise(IoError(path, IoError.Operation.Delete, IoError.Reason.Unsupported))
      
      case error: SecurityException        =>
        raise(IoError(path, IoError.Operation.Delete, IoError.Reason.PermissionDenied))

    path

  def volume: Volume =
    val fileStore = jnf.Files.getFileStore(path.javaPath).nn
    Volume(fileStore.name.nn.tt, fileStore.`type`.nn.tt)
  
  def hardLinkTo(destination: Path)
      (using overwritePreexisting: OverwritePreexisting,
             createNonexistentParents: CreateNonexistentParents)
          : Path raises IoError =

    createNonexistentParents(destination):
      overwritePreexisting(destination):
        jnf.Files.createLink(destination.javaPath, path.javaPath)

    destination

  def entry(using symlinks: DereferenceSymlinks): Entry =
    if jnf.Files.isRegularFile(javaPath) then File
    else if jnf.Files.isDirectory(javaPath) then Directory
    else if jnf.Files.isSymbolicLink(javaPath) then Symlink
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
      (using overwritePreexisting:     OverwritePreexisting,
             dereferenceSymlinks:      DereferenceSymlinks,
             createNonexistentParents: CreateNonexistentParents)
          : Path on PlatformType raises IoError =

    createNonexistentParents(destination):
      overwritePreexisting(destination):
        jnf.Files.copy(path.javaPath, destination.javaPath, dereferenceSymlinks.options()*)

    destination
  
  def copyInto
      (destination: Path on PlatformType)
      (using overwritePreexisting: OverwritePreexisting, dereferenceSymlinks:  DereferenceSymlinks)
          : Path raises IoError =

    import filesystemOptions.createNonexistentParents.enabled
    copyTo(unsafely(destination.child(path.textDescent.head)))

  def moveTo(destination: Path on PlatformType)
      (using overwritePreexisting:     OverwritePreexisting,
             moveAtomically:           MoveAtomically,
             dereferenceSymlinks:      DereferenceSymlinks,
             createNonexistentParents: CreateNonexistentParents)
          : Path on PlatformType raises IoError =

    val options: Seq[jnf.CopyOption] = dereferenceSymlinks.options() ++ moveAtomically.options()

    createNonexistentParents(destination):
      overwritePreexisting(destination):
        jnf.Files.move(path.javaPath, destination.javaPath, options*)

    destination
  
  def moveInto
      (destination: Path on PlatformType)
      (using overwritePreexisting: OverwritePreexisting,
             moveAtomically:       MoveAtomically,
             dereferenceSymlinks:  DereferenceSymlinks)
          : Path raises IoError =

    import filesystemOptions.createNonexistentParents.enabled
    moveTo(unsafely(destination.child(path.textDescent.head)))

  def symlinkTo(destination: Path)
      (using overwritePreexisting:     OverwritePreexisting,
             createNonexistentParents: CreateNonexistentParents)
          : Path raises IoError =

    createNonexistentParents(destination):
      overwritePreexisting(destination):
        jnf.Files.createSymbolicLink(destination.javaPath, path.javaPath)

    destination

  def lastModified[InstantType: SpecificInstant]: InstantType =
    SpecificInstant(jnf.Files.getLastModifiedTime(path.javaPath).nn.toInstant.nn.toEpochMilli)

  def hardLinks()(using dereferenceSymlinks: DereferenceSymlinks): Int raises IoError =
    try jnf.Files.getAttribute(path.javaPath, "unix:nlink", dereferenceSymlinks.options()*) match
      case count: Int => count
      
      case _ =>
        raise(IoError(path, IoError.Operation.Metadata, IoError.Reason.Unsupported), 1)

    catch case error: IllegalArgumentException =>
      raise(IoError(path, IoError.Operation.Metadata, IoError.Reason.Unsupported), 1)
  
  def executable: FilesystemAttribute.Executable.Target = FilesystemAttribute.Executable(path)
  def readable: FilesystemAttribute.Readable.Target = FilesystemAttribute.Readable(path)
  def writable: FilesystemAttribute.Writable.Target = FilesystemAttribute.Writable(path)
  
  def hidden(): Boolean raises IoError =
    try jnf.Files.isHidden(path.javaPath) catch case error: ji.IOException =>
      raise(IoError(path, IoError.Operation.Metadata, IoError.Reason.Unsupported), false)

  def touch(): Unit raises IoError =
    try
      jnf.Files.setLastModifiedTime
       (path.javaPath, jnfa.FileTime.fromMillis(System.currentTimeMillis))
    catch case error: ji.IOException =>
      raise(IoError(path, IoError.Operation.Metadata, IoError.Reason.Unsupported), false)
  
  def create[EntryType: Creatable]: EntryType.Result = EntryType.create(path)

package filesystemOptions:
  object dereferenceSymlinks:
    given DereferenceSymlinks as enabled = () => Nil
    given DereferenceSymlinks as disabled = () => List(jnf.LinkOption.NOFOLLOW_LINKS)

  object moveAtomically:
    given MoveAtomically as enabled = () => List(jnf.StandardCopyOption.ATOMIC_MOVE)
    given MoveAtomically as disabled = () => Nil

  object copyAttributes:
    given CopyAttributes as enabled = () => List(jnf.StandardCopyOption.COPY_ATTRIBUTES)
    given CopyAttributes as disabled = () => Nil

  object deleteRecursively:
    given (using Tactic[IoError]) => DeleteRecursively as enabled:
      def conditionally[ResultType](path: Path)(operation: => ResultType): ResultType =
        import dereferenceSymlinks.disabled
        if path.exists() then
          if path.entry == Directory then path.children.each(conditionally(_)(()))
          jnf.Files.delete(path.javaPath)

        operation

    given (using Tactic[IoError]) => DeleteRecursively as disabled:
      def conditionally[ResultType](path: Path)(operation: => ResultType): ResultType =
        try operation catch case error: jnf.DirectoryNotEmptyException =>
          abort(IoError(path, IoError.Operation.Delete, IoError.Reason.DirectoryNotEmpty))
  
  object overwritePreexisting:
    given (using deleteRecursively: DeleteRecursively) => OverwritePreexisting as enabled:
      def apply[ResultType](path: Path)(operation: => ResultType): ResultType =
        deleteRecursively.conditionally(path)(operation)

    given (using Tactic[IoError]) => OverwritePreexisting as disabled:
      def apply[ResultType](path: Path)(operation: => ResultType): ResultType =
        try operation catch case error: jnf.FileAlreadyExistsException =>
          abort(IoError(path, IoError.Operation.Write, IoError.Reason.AlreadyExists))

  object createNonexistentParents:
    given (using Tactic[IoError]) => CreateNonexistentParents as enabled:
      def apply[ResultType](path: Path)(operation: => ResultType): ResultType =
        path.parent.let: parent =>
          import dereferenceSymlinks.disabled

          if !parent.exists() || parent.entry != Directory
          then jnf.Files.createDirectories(parent.javaPath)

        operation

    given (using Tactic[IoError]) => CreateNonexistentParents as disabled:
      def apply[ResultType](path: Path)(operation: => ResultType): ResultType =
        try operation catch case error: ji.FileNotFoundException =>
          abort(IoError(path, IoError.Operation.Write, IoError.Reason.Nonexistent))

  object createNonexistent:
    given (using create: CreateNonexistentParents) => CreateNonexistent as enabled:
      def apply(path: Path)(operation: => Unit): Unit =
        if !path.exists() then create(path)(operation)

    given CreateNonexistent as disabled:
      def apply(path: Path)(operation: => Unit): Unit = ()

  object writeSynchronously:
    given WriteSynchronously as enabled = () => List(jnf.StandardOpenOption.SYNC)
    given WriteSynchronously as disabled = () => Nil
