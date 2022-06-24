/*
    Jovian, version 0.4.0. Copyright 2020-22 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package jovian

import kaleidoscope.*
import gastronomy.*
import slalom.*
import rudiments.*
import turbulence.*
import eucalyptus.*
import gossamer.*
import anticipation.*

import scala.util.*
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.HashMap
import scala.concurrent.*

import java.net.URI
import java.nio.file as jnf
import java.io as ji

import jnf.{FileSystems, FileVisitResult, Files, Paths, SimpleFileVisitor, StandardCopyOption,
    DirectoryNotEmptyException, Path as JavaPath}, jnf.StandardCopyOption.*,
    jnf.attribute.BasicFileAttributes

import ji.{Closeable, InputStream, File as JavaFile}
enum Recursion:
  case Recursive, Nonrecursive

type Majuscule = 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' | 'L' | 'M' | 'N' |
    'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W' | 'X' | 'Y' | 'Z'

case class ClasspathRefError(classpath: Classpath)(path: classpath.ClasspathRef)
extends Error((t"the resource ", path, t" could not be accessed on the classpath")):
  def message: Text = t"the resource $path could not be accessed"

case class PwdError()
extends Error(t"the current working directory cannot be determined" *: EmptyTuple):
  def message: Text = t"the current working directory cannot be determined"

object Fifo:
  given Sink[Fifo] with
    type E = IoError
    def write(value: Fifo, stream: DataStream): Unit throws E | StreamCutError =
      try Util.write(stream, value.out)
      catch case e => throw IoError(IoError.Op.Write, IoError.Reason.AccessDenied, value.file.path)

case class Fifo(file: File):
  val out = ji.FileOutputStream(file.javaFile, false)
  def close(): Unit = out.close()

enum Permission:
  case Read, Write, Exec

object Inode:
  given Show[Inode] = _.fullname

trait Inode:
  def javaPath: jnf.Path
  def javaFile: ji.File
  def name: Text
  def fullname: Text
  def path: DiskPath
  def uriString: Text
  def exists(): Boolean = javaFile.exists()
  def parent: Directory throws RootParentError
  def modified(using time: anticipation.Timekeeper): time.Type
  //def copyTo(dest: DiskPath): Inode throws IoError
  //def delete(): Unit throws IoError
  def readable: Boolean = Files.isReadable(javaPath)
  def writable: Boolean = Files.isWritable(javaPath)

  def setPermissions(readable: Maybe[Boolean] = Unset, writable: Maybe[Boolean] = Unset,
                         executable: Maybe[Boolean] = Unset)
                    : Unit throws IoError =
    val f = javaFile
    
    if !readable.option.fold(true)(f.setReadable(_)) |
        !writable.option.fold(true)(f.setWritable(_)) |
        !executable.option.fold(true)(f.setExecutable(_))
    then throw IoError(IoError.Op.Permissions, IoError.Reason.AccessDenied, path)

object File:
  given FileProvider[File] with
    def make(str: String, readOnly: Boolean = false): Option[File] =
      Some(unsafely(Filesystem.parse(Text(str)).file(Expect)))
    
    def path(file: File): String = file.path.fullname.toString

  given Sink[File] with
    type E = IoError
    def write(value: File, stream: DataStream): Unit throws E | StreamCutError =
      val out = ji.FileOutputStream(value.javaFile, false)
      try Util.write(stream, out)
      catch case e => throw IoError(IoError.Op.Write, IoError.Reason.AccessDenied, value.path)
      finally try out.close() catch _ => ()
  
  given Source[File] with
    type E = IoError
    def read(file: File): DataStream throws E =
      try Util.readInputStream(ji.FileInputStream(file.javaFile), 64.kb)
      catch case e: ji.FileNotFoundException =>
        if e.getMessage.nn.contains("(Permission denied)")
        then throw IoError(IoError.Op.Read, IoError.Reason.AccessDenied, file.path)
        else throw IoError(IoError.Op.Read, IoError.Reason.DoesNotExist, file.path)

trait File extends Inode:
  def path: DiskPath
  def executable: Boolean = Files.isExecutable(javaPath)
  def copyTo(dest: DiskPath): File throws IoError
  
  def read[T](limit: ByteSize = 64.kb)(using readable: Readable[T])
             : T throws readable.E | IoError | StreamCutError
  
  def touch(): Unit throws IoError =
    try
      if !exists() then ji.FileOutputStream(javaFile).close()
      else javaFile.setLastModified(System.currentTimeMillis())
    catch case e => throw IoError(IoError.Op.Write, IoError.Reason.NotSupported, path)
  
  def size(): ByteSize = javaFile.length.b

trait Symlink extends Inode

object DiskPath:
  given Show[DiskPath] = _.fullname
  
  given DirectoryProvider[DiskPath] with
    def make(str: String, readOnly: Boolean = false): Option[DiskPath] =
      safely(Filesystem.parse(Text(str))).option
    
    def path(path: DiskPath): String = path.fullname.toString

trait DiskPath:
  def javaPath: jnf.Path
  def javaFile: ji.File
  def exists(): Boolean
  def parent: DiskPath throws RootParentError
  def file(creation: Creation = Creation.Ensure): File throws IoError
  def parts: List[Text]
  def directory(creation: Creation = Creation.Ensure): Directory throws IoError
  def symlink: Symlink throws IoError
  def separator: Text
  def fullname: Text
  def rename(fn: Text => Text): DiskPath
  def name: Text
  def length: Int
  def prefix: Text
  def filesystem: Filesystem
  
  @targetName("add")
  def +(relative: Relative): DiskPath throws RootParentError
  
  def ancestorOf(child: DiskPath): Boolean =
    this == child || length < child.length && unsafely(ancestorOf(child.parent))

  def relativeTo(path: DiskPath): Option[Relative] =
    val fs = filesystem
    
    this match
      case src: fs.DiskPath => path match
        case dest: fs.DiskPath   => Some(src.relativeTo(dest))
        case _                 => None
      case _                 => None
  
  def isFile: Boolean = javaFile.exists() && !javaFile.isDirectory
  def isDirectory: Boolean = javaFile.exists() && javaFile.isDirectory
  def isSymlink: Boolean = javaFile.exists() && Files.isSymbolicLink(javaFile.toPath)
    
  @targetName("access")
  def /(child: Text): DiskPath throws RootParentError
  
  def descendantFiles(descend: (Directory => Boolean) = _ => true): LazyList[File] throws IoError

object Directory:
  given DirectoryProvider[Directory] with
    def make(str: String, readOnly: Boolean = false): Option[Directory] =
      safely(Filesystem.parse(Text(str)).directory(Expect)).option
    
    def path(directory: Directory): String = directory.path.fullname.s

  
trait Directory extends Inode:
  def path: DiskPath
  def files: List[File] throws IoError
  def children: List[Inode] throws IoError
  def subdirectories: List[Directory] throws IoError
  def descendants: LazyList[Inode] throws IoError
  def copyTo(dest: DiskPath): Directory throws IoError
  def tmpFile(suffix: Maybe[Text] = Unset): File throws IoError
  def tmpPath(suffix: Maybe[Text] = Unset): DiskPath
  
  @targetName("access")
  def /(child: Text): DiskPath throws RootParentError

object IoError:
  object Reason:
    given Show[Reason] =
      case WrongType            => t"the path refers to the wrong type of node"
      case DoesNotExist         => t"no node exists at this path"
      case AlreadyExists        => t"a node already exists at this path"
      case AccessDenied         => t"the operation is not permitted on this path"
      case DifferentFilesystems => t"the source and destination are on different filesystems"
      case NotSupported         => t"the filesystem does not support it"

  enum Reason:
    case WrongType, DoesNotExist, AlreadyExists, AccessDenied, DifferentFilesystems, NotSupported

  object Op:
    given Show[Op] = Showable(_).show.lower

  enum Op:
    case Read, Write, Access, Permissions, Create, Delete

case class IoError(operation: IoError.Op, reason: IoError.Reason, path: DiskPath)
extends Error((t"the ", operation, t" operation at ", path, t" failed because ", reason)):
  def message: Text = t"the $operation operation at ${path.show} failed because $reason"

case class InotifyError()
extends Error(t"the limit on the number of paths that can be watched has been exceeded" *:
    EmptyTuple):
  def message: Text = t"the limit on the number of paths that can be watched has been exceeded"

open class Classpath(classLoader: ClassLoader = getClass.nn.getClassLoader.nn)
extends Root(t"/", t""):
  protected inline def classpath: this.type = this

  type AbsolutePath = ClasspathRef

  def makeAbsolute(parts: List[Text]): ClasspathRef = ClasspathRef(parts)

  @targetName("access")
  def /(child: Text): ClasspathRef = ClasspathRef(List(child))

  object ClasspathRef:
    given Show[ClasspathRef] = _.parts.join(t"classpath:", t"/", t"")

  case class ClasspathRef(elements: List[Text]) extends Path.Absolute(elements):
    def resource: Resource = Resource(makeAbsolute(parts))

  case class Resource(path: ClasspathRef):
    def read[T](limit: ByteSize = 64.kb)(using readable: Readable[T])
            : T throws ClasspathRefError | StreamCutError | readable.E =
      val resource = classLoader.getResourceAsStream(path.show.drop(10).s)
      if resource == null then throw ClasspathRefError(classpath)(path)
      val stream = Util.readInputStream(resource.nn, limit)
      readable.read(stream)
    
    def name: Text = path.parts.lastOption.getOrElse(prefix)
    def parent: Resource throws RootParentError = Resource(path.parent)

enum Creation:
  case Expect, Create, Ensure

export Creation.{Expect, Create, Ensure}

class Filesystem(pathSeparator: Text, fsPrefix: Text) extends Root(pathSeparator, fsPrefix):
  fs =>
  type AbsolutePath = DiskPath

  override def toString(): String = fsPrefix.s

  val root: DiskPath = DiskPath(Nil)
  lazy val javaFilesystem: jnf.FileSystem = root.javaPath.getFileSystem.nn

  def makeAbsolute(parts: List[Text]): DiskPath = DiskPath(parts)

  def unapply(path: jnf.Path): Some[DiskPath] =
    Some(DiskPath((0 until path.getNameCount).map(path.getName(_).toString.show).to(List)))

  def parse(value: Text, pwd: Maybe[DiskPath] = Unset): DiskPath throws InvalidPathError =
    if value.startsWith(prefix) then DiskPath(List(value.drop(prefix.length).cut(separator)*))
    else try
      pwd.option.map: path =>
        (path + Relative.parse(value)) match
          case p: fs.DiskPath => p
      .getOrElse:
        throw InvalidPathError(value)
    catch case err: RootParentError => throw InvalidPathError(value)
  
  @targetName("access")
  def /(child: Text): Path.Absolute = Path.Absolute(List(child))

  case class DiskPath(elements: List[Text]) extends Path.Absolute(elements), jovian.DiskPath:
    lazy val javaFile: ji.File = ji.File(fullname.s)
    lazy val javaPath: jnf.Path = javaFile.toPath.nn
    def filesystem: Filesystem = fs
    def prefix: Text = fsPrefix
    def separator: Text = pathSeparator
    def name: Text = elements.last
    def fullname: Text = elements.join(fsPrefix, separator, t"")
    def length: Int = elements.length

    @targetName("add")
    def +(relative: Relative): DiskPath throws RootParentError =
      if relative.ascent == 0 then DiskPath(elements ++ relative.parts)
      else parent + relative.copy(ascent = relative.ascent - 1)

    def file(creation: Creation = Creation.Ensure): File throws IoError =
      fs.synchronized:
        import IoError.*
        creation match
          case Creation.Create if exists() =>
            throw IoError(Op.Create, Reason.AlreadyExists, this)
          
          case Creation.Expect if !exists() =>
            throw IoError(Op.Access, Reason.DoesNotExist, this)
          
          case Creation.Ensure if !exists() =>
          
          case _ =>
            ()

        val file = File(this)
        try
          if !parent.exists() && !parent.javaFile.mkdirs()
          then throw IoError(Op.Create, Reason.AccessDenied, this)
        catch case err: RootParentError => throw IoError(Op.Create, Reason.AccessDenied, this)
        
        if !exists() then File(this).touch()
        if !isFile then throw IoError(IoError.Op.Access, IoError.Reason.WrongType, this)
        
        File(this)

    def directory(creation: Creation = Creation.Ensure): Directory throws IoError = fs.synchronized:
      import IoError.*
      creation match
        case Creation.Create if exists() =>
          throw IoError(Op.Create, Reason.AlreadyExists, this)
        
        case Creation.Expect if !exists() =>
          throw IoError(Op.Access, Reason.DoesNotExist, this)
        
        case Creation.Ensure if !exists() =>
          if !javaFile.mkdirs() then throw IoError(Op.Create, Reason.AccessDenied, this)
        
        case _ =>
          ()
      
      Directory(this)
      
      if !exists() && creation == Creation.Expect
      then throw IoError(IoError.Op.Access, IoError.Reason.DoesNotExist, this)
      
      if !isDirectory then throw IoError(IoError.Op.Access, IoError.Reason.WrongType, this)
      
      Directory(this)
  
    def symlink: Symlink throws IoError =
      if !javaFile.exists()
      then throw IoError(IoError.Op.Access, IoError.Reason.DoesNotExist, this)
      
      if !Files.isSymbolicLink(javaFile.toPath)
      then throw IoError(IoError.Op.Access, IoError.Reason.WrongType, this)
      
      Symlink(this, unsafely(parse(Showable(Files.readSymbolicLink(Paths.get(fullname.s))).show)))

    def descendantFiles(descend: (jovian.Directory => Boolean) = _ => true)
                       : LazyList[File] throws IoError =
      if javaFile.isDirectory
      then directory(Expect).files.to(LazyList) #::: directory(Expect).subdirectories.filter(
          descend).to(LazyList).flatMap(_.path.descendantFiles(descend))
      else LazyList(file(Expect))

    def inode: Inode throws IoError =
      
      if !javaFile.exists()
      then throw IoError(IoError.Op.Access, IoError.Reason.DoesNotExist, this)
      
      if isDirectory then Directory(this)
      else if isFile
      then
        try Symlink(this, unsafely(parse(Showable(Files.readSymbolicLink(javaPath)).show)))
        catch NoSuchElementException => File(this)
      else File(this)
    
    def exists(): Boolean = javaFile.exists()
  
  sealed trait Inode(val path: DiskPath) extends jovian.Inode:
    lazy val javaFile: ji.File = ji.File(fullname.s)
    lazy val javaPath: jnf.Path = javaFile.toPath.nn

    def name: Text = path.parts.lastOption.getOrElse(prefix)
    def fullname: Text = path.javaFile.getAbsolutePath.nn.show
    def uriString: Text = Showable(javaFile.toURI).show
    def parent: Directory throws RootParentError = Directory(path.parent)
    def directory: Option[Directory]
    def file: Option[File]
    def symlink: Option[Symlink]
    
    def modified(using time: anticipation.Timekeeper): time.Type =
      time.from(javaFile.lastModified)

  object File:
    given Sink[File] with
      type E = IoError
      def write(value: File, stream: DataStream): Unit throws E | StreamCutError =
        val out = ji.FileOutputStream(value.javaFile, false)
        try Util.write(stream, out)
        catch case e => throw IoError(IoError.Op.Write, IoError.Reason.AccessDenied, value.path)
        finally try out.close() catch _ => ()
    
    given Source[File] with
      type E = IoError
      def read(file: File): DataStream throws E =
        try Util.readInputStream(ji.FileInputStream(file.javaFile), 64.kb)
        catch case e: ji.FileNotFoundException =>
          if e.getMessage.nn.contains("(Permission denied)")
          then throw IoError(IoError.Op.Read, IoError.Reason.AccessDenied, file.path)
          else throw IoError(IoError.Op.Read, IoError.Reason.DoesNotExist, file.path)

  case class File(filePath: DiskPath) extends Inode(filePath), jovian.File:
    def directory: Option[Directory] = None
    def file: Option[File] = Some(this)
    def symlink: Option[Symlink] = None
    
    def delete(): Unit throws IoError =
      try javaFile.delete()
      catch e => throw IoError(IoError.Op.Delete, IoError.Reason.AccessDenied, filePath)
    
    def read[T](limit: ByteSize = 64.kb)(using readable: Readable[T])
        : T throws readable.E | IoError | StreamCutError =
      val stream = Util.readInputStream(ji.FileInputStream(javaFile), limit)
      try readable.read(stream) catch
        case err: java.io.FileNotFoundException =>
          throw IoError(IoError.Op.Read, IoError.Reason.AccessDenied, path)

    def copyTo(dest: jovian.DiskPath): jovian.File throws IoError =
      if dest.exists()
      then throw IoError(IoError.Op.Create, IoError.Reason.AlreadyExists, dest)
      
      try Files.copy(javaPath, dest.javaPath)
      catch IOException =>
        throw IoError(IoError.Op.Write, IoError.Reason.AccessDenied, dest)
      
      dest.file(Expect)

    def hardLinkCount(): Int throws IoError =
      try Files.getAttribute(javaPath, "unix:nlink") match
        case i: Int => i
        case _      => throw Mistake("Should never match")
      catch e => throw IoError(IoError.Op.Read, IoError.Reason.NotSupported, path)
    
    def hardLinkTo(dest: DiskPath): File throws IoError =
      if dest.exists()
      then throw IoError(IoError.Op.Create, IoError.Reason.AlreadyExists, dest)
      
      try Files.createLink(javaPath, Paths.get(fullname.s))
      catch
        case e: jnf.NoSuchFileException =>
          throw IoError(IoError.Op.Write, IoError.Reason.DoesNotExist, path)
        case e: jnf.FileAlreadyExistsException =>
          throw IoError(IoError.Op.Write, IoError.Reason.AlreadyExists, dest)
        case e: jnf.AccessDeniedException =>
          throw IoError(IoError.Op.Write, IoError.Reason.AccessDenied, dest)

      dest.file(Expect)

  case class Symlink(symlinkPath: DiskPath, target: jovian.DiskPath)
  extends Inode(symlinkPath), jovian.Symlink:
    def apply(): jovian.DiskPath = target
    
    def hardLinkTo(dest: DiskPath): Inode throws IoError =
      Files.createSymbolicLink(dest.javaPath, target.javaPath)

      Symlink(path, dest)
    
    def directory: Option[Directory] = None
    def file: Option[File] = None
    def symlink: Option[Symlink] = Some(this)
    
    def delete(): Unit throws IoError =
      try javaFile.delete()
      catch e => throw IoError(IoError.Op.Delete, IoError.Reason.AccessDenied, path)
    
    def copyTo(dest: jovian.DiskPath): jovian.Symlink throws IoError =
      Files.createSymbolicLink(Paths.get(dest.show.s), Paths.get(target.fullname.s))

      Symlink(path, dest)

  enum FileEvent:
    case NewFile(keyDir: Directory, file: File)
    case NewDirectory(keyDir: Directory, directory: Directory)
    case Modify(keyDir: Directory, file: File)
    case Delete(keyDir: Directory, file: DiskPath)

    def keyDir: Directory

    def path: DiskPath = this match
      case NewFile(_, file)     => file.path
      case NewDirectory(_, dir) => dir.path
      case Modify(_, file)      => file.path
      case Delete(_, path)      => path

  case class Watcher(private val svc: jnf.WatchService,
                     private val watches: HashMap[jnf.WatchKey, Directory],
                     private val dirs: HashMap[Directory, jnf.WatchKey]):
    import java.nio.file.*, StandardWatchEventKinds.*, collection.JavaConverters.*

    private val funnel = Funnel[Maybe[FileEvent]]
    private val pumpTask = Task(pump())
    pumpTask()
    
    def stream: LazyList[FileEvent] = funnel.stream.takeWhile(_ != Unset).sift[FileEvent]
    def removeAll()(using Log): Unit = watches.values.foreach(remove(_))

    @tailrec
    private def pump(): Unit =
      svc.take().nn match
        case k: WatchKey =>
          val key = k.nn
          key.pollEvents().nn.iterator.nn.asScala.flatMap(process(key, _)).foreach(funnel.put(_))
          key.reset()
      
      pump()

    private def process(key: WatchKey, event: WatchEvent[?]): List[FileEvent] =
      val keyDir = watches(key)
      
      val diskPath = event.context.nn match
        case path: jnf.Path =>
          unsafely(keyDir.path + Relative.parse(Showable(path).show)) match
            case path: fs.DiskPath => path
        
        case _ => throw Mistake("the event context should always be a path")
      
      try event.kind match
        case ENTRY_CREATE =>
          if diskPath.isDirectory
          then List(FileEvent.NewDirectory(keyDir, diskPath.directory(Expect)))
          else List(FileEvent.NewFile(keyDir, diskPath.file(Expect)))
        
        case ENTRY_MODIFY =>
          List(FileEvent.Modify(keyDir, diskPath.file(Expect)))
        
        case ENTRY_DELETE =>
          List(FileEvent.Delete(keyDir, diskPath))
        
        case _ =>
          Nil
      
      catch case err: Exception => List()

    def remove(dir: Directory)(using Log): Unit = synchronized:
      val watchKey = dirs(dir)
      watchKey.cancel()
      dirs.remove(dir)
      watches.remove(watchKey)
      Log.info(t"Stopped watching ${dir.path}")
      if dirs.isEmpty then funnel.put(Unset)
    
    def add(dir: Directory)(using Log): Unit = synchronized:
      val watchKey = dir.javaPath.register(svc, ENTRY_CREATE, ENTRY_MODIFY, ENTRY_DELETE).nn
      watches(watchKey) = dir
      dirs(dir) = watchKey
      Log.info(t"Started watching ${dir.path}")
    
    def directories: Set[Directory] = dirs.keySet.to(Set)


  def watch(dirs: Iterable[Directory])(using Log): Watcher throws IoError | InotifyError =
    import java.nio.file.*, StandardWatchEventKinds.*, collection.JavaConverters.*
    val svc: jnf.WatchService = javaFilesystem.newWatchService().nn
    val watches: HashMap[jnf.WatchKey, Directory] = HashMap()
    val directories: HashMap[Directory, jnf.WatchKey] = HashMap()
    
    dirs.foreach: dir =>
      val watchKey = dir.javaPath.register(svc, ENTRY_CREATE, ENTRY_MODIFY, ENTRY_DELETE).nn
      watches(watchKey) = dir
      directories(dir) = watchKey
      Log.info(t"Started watching ${dir.path}")
    
    Watcher(svc, watches, directories)

  object Directory:
    given DirectoryProvider[Directory] with
      def make(str: String, readOnly: Boolean = false): Option[Directory] =
        safely(parse(Text(str)).directory(Expect)).option
      
      def path(directory: Directory): String = directory.path.fullname.s
  
  case class Directory(dirPath: DiskPath) extends Inode(dirPath), jovian.Directory:
    def directory: Option[Directory] = Some(this)
    def file: Option[File] = None
    def symlink: Option[Symlink] = None
    
    def tmpPath(suffix: Maybe[Text] = Unset): DiskPath =
      unsafely(this / t"${Uuid().show}${suffix.otherwise(t"")}")
    
    def tmpFile(suffix: Maybe[Text] = Unset): File throws IoError =
      unsafely:
        val file = tmpPath(suffix).file(Create)
        file.javaFile.deleteOnExit()
        file

    def delete(): Unit throws IoError =
      def recur(file: JavaFile): Boolean =
        if Files.isSymbolicLink(file.toPath) then file.delete()
        else if file.isDirectory
        then file.listFiles.nn.map(_.nn).forall(recur(_)) && file.delete()
        else file.delete()

      try recur(javaFile).unit
      catch e => throw IoError(IoError.Op.Delete, IoError.Reason.AccessDenied, dirPath)

    def watch()(using Log): Watcher throws InotifyError | IoError = fs.watch(List(this))

    def children: List[Inode] throws IoError = Option(javaFile.list).fold(Nil): files =>
      files.nn.immutable(using Unsafe).to(List).map(_.nn).map(Text(_)).map(path.parts :+ _).map(
          makeAbsolute(_)).map(_.inode)
    
    def descendants: LazyList[Inode] throws IoError =
      children.to(LazyList) ++ subdirectories.flatMap(_.descendants)
    
    def subdirectories: List[Directory] throws IoError =
      children.collect:
        case dir: Directory => dir
    
    def deepSubdirectories: LazyList[Directory] throws IoError =
      val subdirs = subdirectories.to(LazyList).filter(!_.name.startsWith(t"."))
      subdirs #::: subdirs.flatMap(_.deepSubdirectories)

    def files: List[File] throws IoError = children.collect:
      case file: File => file

    def copyTo(dest: jovian.DiskPath): jovian.Directory throws IoError =
      if dest.exists()
      then throw IoError(IoError.Op.Write, IoError.Reason.AlreadyExists, dest)
      
      try Files.copy(javaPath, Paths.get(dest.show.s))
      catch e => throw IoError(IoError.Op.Write, IoError.Reason.AccessDenied, dirPath)
      
      dest.directory(Expect)

    @targetName("access")
    def /(child: Text): DiskPath throws RootParentError = makeAbsolute((path / child).parts)


// object OldPath:
//   def apply(jpath: JavaPath): Path = Path(jpath.show.s match
//     case ""    => "."
//     case other => other
//   )
  
//   def apply(file: JavaFile): Path = Path(file.getAbsolutePath)
//   def apply(uri: URI): Path = Path(Paths.get(uri))

//   def unapply(str: String): Option[Path] = str match
//     case r"""${dir: String}@([^*?:;,&|"\%<>]*)""" => Some(Path(dir))
//     case _                              => None

//   private class CopyFileVisitor(sourcePath: JavaPath, targetPath: JavaPath) extends SimpleFileVisitor[JavaPath]:

//     override def preVisitDirectory(dir: JavaPath, attrs: BasicFileAttributes): FileVisitResult =
//       targetPath.resolve(sourcePath.relativize(dir)).toFile.mkdirs()
//       FileVisitResult.CONTINUE

//     override def visitFile(file: JavaPath, attrs: BasicFileAttributes): FileVisitResult =
//       Files.copy(file, targetPath.resolve(sourcePath.relativize(file)), REPLACE_EXISTING)
//       FileVisitResult.CONTINUE


//   def apply(input: String): Path =
//     if input == "/" then ji.File.separator
//     else
//       def canonicalize(str: List[String], drop: Int = 0): List[String] = str match
//         case ".." :: tail => canonicalize(tail, drop + 1)
//         case head :: tail => if drop > 0 then canonicalize(tail, drop - 1) else head :: canonicalize(tail)
//         case Nil          => List.fill(drop)("..")
      
//       canonicalize((input.cut("/").to(List) match
//         case "" :: xs => "" :: xs.filter { p => p != "." && p != "" }
//         case xs       => xs.filter { p => p != "." && p != "" }
//       ).reverse).reverse match
//         case Nil => "."
//         case xs  => xs.mkString("/")

  // extension (path: Path)
  //   def filename: String = path
  //   def name: String = Showable(javaPath.getFileName).show

  //   def setReadOnly(recursion: Recursion): Unit throws UnchangeablePermissions =
  //     if !javaFile.setWritable(false) then throw UnchangeablePermissions(path)
  //     if recursion == Recursion.Recursive then children.foreach(_.setWritable(recursion))

  //   def setWritable(recursion: Recursion): Unit throws UnchangeablePermissions =
  //     if !javaFile.setWritable(true) then throw UnchangeablePermissions(path)
  //     if recursion == Recursion.Recursive then children.foreach(_.setWritable(recursion))

  //   def uniquify(): Path =
  //     if !exists() then path else LazyList.from(2).map { i => rename(_+"-"+i) }.find(!_.exists()).get

  //   def directory: Boolean = Files.isDirectory(javaPath)

  //   def extantParents(): Path =
  //     parent.mkdir()
  //     path

  //   def setExecutable(exec: Boolean): Unit throws UnchangeablePermissions =
  //     try javaFile.setExecutable(exec).unit catch e => throw UnchangeablePermissions(path)

  //   def moveTo(dest: Path): Unit throws NotWritable =
  //     try
  //       path.parent.extant()
  //       Files.move(javaPath, dest.javaPath, StandardCopyOption.REPLACE_EXISTING).unit
  //     catch
  //       case e: DirectoryNotEmptyException =>
  //         copyTo(dest)
  //         delete().unit
  //       case e =>
  //         throw NotWritable(dest)

  //   def relativeSubdirsContaining(pred: String => Boolean): Set[Path] =
  //     findSubdirsContaining(pred).map { p => Path(p.filename.drop(path.length + 1)) }

  //   def unlink(): Unit throws NotSymbolicLink | NotWritable =
  //     try if Files.isSymbolicLink(javaPath) then Files.delete(javaPath) else throw NotSymbolicLink(path)
  //     catch e => throw NotWritable(path)

  //   def rename(fn: String => String): Path = parent / fn(name)
    
  //   def symlinkTo(target: Path): Unit throws NotWritable =
  //     try Files.createSymbolicLink(target.javaPath, javaPath)
  //     catch case e: ji.IOException => throw NotWritable(path)

object Filesystem:
  lazy val roots: Set[Filesystem] =
    Option(ji.File.listRoots).fold(Set())(_.nn.immutable(using Unsafe).to(Set)).map(_.nn.getAbsolutePath.nn)
        .collect:
      case "/" =>
        Unix
      
      case s"""$drive:\""" if drive.length == 1 =>
        unsafely:
          drive.charAt(0).toUpper match
            case ch: Majuscule => WindowsRoot(ch)
            case _             => throw Mistake("Filesystem must always start with a letter")
    .to(Set)
 
  def defaultSeparator: "/" | "\\" = if ji.File.separator == "\\" then "\\" else "/"

  def parse(text: Text): jovian.DiskPath throws InvalidPathError =
    roots.flatMap: fs =>
      safely(fs.parse(text)).option
    .headOption.getOrElse:
      throw InvalidPathError(text)
    match
      case path: jovian.DiskPath => path

object Unix extends Filesystem(t"/", t"/"):
  def Pwd: DiskPath throws PwdError | InvalidPathError =
    val dir = try Sys.user.dir().show catch case e: KeyNotFoundError => throw PwdError()
    makeAbsolute(parse(dir).parts)

case class WindowsRoot(drive: Majuscule) extends Filesystem(t"\\", t"${drive}:\\")
object windows:
  object DriveC extends WindowsRoot('C')
  object DriveD extends WindowsRoot('D')
  object DriveE extends WindowsRoot('E')
  object DriveF extends WindowsRoot('F')

given realm: Realm = Realm(t"jovian")
