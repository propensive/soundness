/*
    Jovian, version 0.11.0. Copyright 2020-21 Jon Pretty, Propensive OÜ.

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
import eucalyptus.*
import gossamer.*

import scala.util.*
import scala.collection.generic.CanBuildFrom
import annotation.targetName

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
extends JovianError(t"the resource $path could not be accessed")

case class PwdError() extends JovianError(t"the current working directory cannot be determined")

object IoError:
  object Reason:
    given Show[Reason] =
      case NotFile              => t"the path does not refer to a file"
      case NotDirectory         => t"the path does not refer to a directory"
      case NotSymlink           => t"the path does not refer to a symlink"
      case DoesNotExist         => t"no node exists at this path"
      case AlreadyExists        => t"a node already exists at this path"
      case AccessDenied         => t"the operation is not permitted on this path"
      case DifferentFilesystems => t"the source and destination are on different filesystems"

  enum Reason:
    case NotFile, NotDirectory, NotSymlink, DoesNotExist, AlreadyExists, AccessDenied,
        DifferentFilesystems

  object Op:
    given Show[Op] = Showable(_).show.lower

  enum Op:
    case Read, Write, Access, Create

case class IoError(operation: IoError.Op, reason: IoError.Reason, path: Any)
extends JovianError(t"the $operation operation at ${path.toString} did not succeed because $reason")

case class InotifyError()
extends JovianError(t"the limit on the number of paths that can be watched has been exceeded")

open class Classpath(classLoader: ClassLoader = getClass.nn.getClassLoader.nn)
extends Root(t"/", t""):
  protected inline def classpath: this.type = this

  type AbsolutePath = ClasspathRef

  def makeAbsolute(parts: List[Text]): ClasspathRef = ClasspathRef(parts)

  @targetName("access")
  infix def /(resource: Text): ClasspathRef = ClasspathRef(List(resource))

  object ClasspathRef:
    given Show[ClasspathRef] = _.parts.join(t"classpath:", t"/", t"")

  case class ClasspathRef(elements: List[Text]) extends Path.Absolute(elements):
    def resource: Resource = Resource(makeAbsolute(parts))

  case class Resource(path: ClasspathRef):
    def read[T](limit: ByteSize = 64.kb)
               (using readable: Readable[T, ?])
               : T throws ClasspathRefError | readable.E =
      val in = ji.BufferedInputStream(classLoader.getResourceAsStream(path.show.s))
      
      try readable.read(in, limit)
      catch case e: NullPointerException => throw ClasspathRefError(classpath)(path)
    
    def name: Text = path.parts.lastOption.getOrElse(prefix)
    def parent: Resource throws RootParentError = Resource(path.parent)

class Filesystem(pathSeparator: Text, fsPrefix: Text) extends Root(pathSeparator, fsPrefix):
  type AbsolutePath = IoPath

  case class IoPath(elements: List[Text]) extends Path.Absolute(elements):
    private[jovian] lazy val javaFile: ji.File = ji.File(this.show.s)
    private[jovian] lazy val javaPath: jnf.Path = javaFile.toPath.nn

    def prefix: Text = fsPrefix
    def separator: Text = pathSeparator
    def name: Text = elements.last

    def file: File throws IoError =
      if !exists() then throw IoError(IoError.Op.Access, IoError.Reason.DoesNotExist, this)
      if isDirectory then throw IoError(IoError.Op.Access, IoError.Reason.NotFile, this)
      
      File(this)
    
    def directory: Directory throws IoError =
      if !exists() then throw IoError(IoError.Op.Access, IoError.Reason.DoesNotExist, this)
      if !isDirectory then throw IoError(IoError.Op.Access, IoError.Reason.NotDirectory, this)
      
      Directory(this)
  
    def symlink: Symlink throws IoError =
      if !javaFile.exists()
      then throw IoError(IoError.Op.Access, IoError.Reason.DoesNotExist, this)
      
      if !Files.isSymbolicLink(javaFile.toPath)
      then throw IoError(IoError.Op.Access, IoError.Reason.NotSymlink, this)
      
      Symlink(this, parse(Showable(Files.readSymbolicLink(Paths.get(this.show.s))).show).get)

    def isFile: Boolean = javaFile.exists() && !javaFile.isDirectory
    def isDirectory: Boolean = javaFile.exists() && javaFile.isDirectory
    def isSymlink: Boolean = javaFile.exists() && Files.isSymbolicLink(javaFile.toPath)
    
    def descendantFiles(descend: (Directory => Boolean) = _ => true): LazyList[File] throws IoError =
      if javaFile.isDirectory
      then directory.files.to(LazyList) #::: directory.subdirectories.filter(descend).to(LazyList)
          .flatMap(_.path.descendantFiles(descend))
      else LazyList(file)

    def inode: Inode throws IoError =
      
      if !javaFile.exists()
      then throw IoError(IoError.Op.Access, IoError.Reason.DoesNotExist, this)
      
      if isDirectory then Directory(this)
      else if isFile
      then
        try Symlink(this, parse(Showable(Files.readSymbolicLink(javaPath)).show).get)
        catch NoSuchElementException => File(this)
      else File(this)
    
    def createDirectory(): Directory throws IoError =
      if exists() then throw IoError(IoError.Op.Create, IoError.Reason.AlreadyExists, this)
      
      if !javaFile.mkdirs()
      then throw IoError(IoError.Op.Create, IoError.Reason.AccessDenied, this)
  
      Directory(this)
    
    def exists(): Boolean = javaFile.exists()
  
    def createFile(overwrite: Boolean = false): File throws IoError =
      if !overwrite && exists()
      then throw IoError(IoError.Op.Create, IoError.Reason.AlreadyExists, this)
      
      try ji.FileOutputStream(javaFile).close()
      catch case e =>
        throw IoError(IoError.Op.Create, IoError.Reason.AccessDenied, this)
  
      File(this)
    
  def makeAbsolute(parts: List[Text]): IoPath = IoPath(parts)

  def fromJavaPath(path: jnf.Path): IoPath =
    IoPath((0 until path.getNameCount).map(path.getName(_).toString.show).to(List))

  def parse(value: Text): Option[IoPath] =
    if value.startsWith(prefix)
    then
      val parts: List[Text] = value.drop(prefix.length).cut(separator)
      Some(IoPath(List(parts*)))
    else None
  
  @targetName("access")
  infix def /(filename: Text): Path.Absolute = Path.Absolute(List(filename))

  sealed trait Inode(val path: IoPath):
    lazy val javaFile: ji.File = ji.File(path.show.s)
    lazy val javaPath: jnf.Path = javaFile.toPath.nn

    def name: Text = path.parts.lastOption.getOrElse(prefix)
    def fullname: Text = javaFile.getAbsolutePath.nn.show
    def uriString: Text = Showable(javaFile.toURI).show
    def exists(): Boolean = Files.exists(javaPath)
    def parent: Directory throws RootParentError = Directory(path.parent)
    def directory: Option[Directory]
    def file: Option[File]
    def symlink: Option[Symlink]
    def lastModified: Long = javaFile.lastModified
    
    def copyTo(dest: IoPath): Inode throws IoError
    def hardLinkTo(dest: IoPath): Inode throws IoError

  object File:
    given Show[File] = _.fullname
    
    given Sink[File] with
      type E = IoError
      def write(value: File, stream: LazyList[Bytes]): Unit throws IoError =
        val out = ji.FileOutputStream(value.javaPath.toFile, false)
        try summon[Writable[LazyList[Bytes]]].write(out, stream)
        catch case e => throw IoError(IoError.Op.Write, IoError.Reason.AccessDenied, value.path)
        finally try out.close() catch _ => ()

    given Source[File] with
      type E = IoError
      def read(file: File): DataStream throws E =
        try
          val in = ji.BufferedInputStream(ji.FileInputStream(file.javaPath.toFile))
          Util.read(in, 64.kb)
        catch case e: ji.FileNotFoundException =>
          if e.getMessage.nn.contains("(Permission denied)")
          then throw IoError(IoError.Op.Read, IoError.Reason.AccessDenied, file.initPath)
          else throw IoError(IoError.Op.Read, IoError.Reason.DoesNotExist, file.initPath)

  case class File(initPath: IoPath) extends Inode(initPath):
    def directory: Option[Directory] = None
    def file: Option[File] = Some(this)
    def symlink: Option[Symlink] = None
    def modified: Long = javaFile.lastModified
    
    def write[T: Writable](content: T, append: Boolean = false): Unit throws IoError =
      val out = ji.FileOutputStream(javaPath.toFile, append)
      try summon[Writable[T]].write(out, content)
      catch case e => throw IoError(IoError.Op.Write, IoError.Reason.AccessDenied, initPath)
      finally try out.close() catch _ => ()

    def read[T](limit: ByteSize = 64.kb)(using readable: Readable[T, ?])
        : T throws readable.E | IoError =
      val in = ji.BufferedInputStream(ji.FileInputStream(javaPath.toFile))
      try readable.read(in, limit)
      catch case e =>
        throw IoError(IoError.Op.Read, IoError.Reason.AccessDenied, initPath)

    def copyTo(dest: IoPath): File throws IoError =
      if dest.exists()
      then throw IoError(IoError.Op.Create, IoError.Reason.AlreadyExists, dest)
      
      try Files.copy(javaPath, dest.javaPath)
      catch IOException =>
        throw IoError(IoError.Op.Write, IoError.Reason.AccessDenied, dest)
      
      dest.file

    def hardLinkTo(dest: IoPath): File throws IoError =
      if dest.exists()
      then throw IoError(IoError.Op.Create, IoError.Reason.AlreadyExists, dest)
      
      try Files.createLink(javaPath, Paths.get(dest.show.s))
      catch
        case e: jnf.NoSuchFileException =>
          throw IoError(IoError.Op.Write, IoError.Reason.DoesNotExist, initPath)
        case e: jnf.FileAlreadyExistsException =>
          throw IoError(IoError.Op.Write, IoError.Reason.AlreadyExists, dest)
        case e: jnf.AccessDeniedException =>
          throw IoError(IoError.Op.Write, IoError.Reason.AccessDenied, dest)

      dest.file

  case class Symlink(initPath: IoPath, target: IoPath) extends Inode(initPath):
    def apply(): IoPath = target
    
    def hardLinkTo(dest: IoPath): Inode throws IoError = copyTo(dest)
    
    def directory: Option[Directory] = None
    def file: Option[File] = None
    def symlink: Option[Symlink] = Some(this)
    
    def copyTo(dest: IoPath): Symlink throws IoError =
      Files.createSymbolicLink(Paths.get(dest.show.s), Paths.get(target.show.s))

      Symlink(path, dest)

  object Directory:
    given Show[Directory] = _.initPath.show
  
  enum FileEvent:
    case NewFile(file: File)
    case NewDirectory(directory: Directory)
    case Modify(file: File)
    case Delete(file: IoPath)

  case class Watcher(startStream: () => LazyList[FileEvent], stop: () => Unit):
    def stream: LazyList[FileEvent] = startStream()

  case class Directory(initPath: IoPath) extends Inode(initPath):
    def directory: Option[Directory] = Some(this)
    def file: Option[File] = None
    def symlink: Option[Symlink] = None

    def watch(recursive: Boolean = true, interval: Int = 100)(using Log)
             : Watcher throws IoError | InotifyError =
      import java.nio.file.*, StandardWatchEventKinds.*
      import collection.JavaConverters.*
      var continue: Boolean = true
      
      val dirs: Set[Directory] = if recursive then deepSubdirectories.to(Set) + this else Set(this)
      val svc = javaPath.getFileSystem.nn.newWatchService().nn
      
      def watchKey(dir: Directory): WatchKey =
        // Calls to `Log.fine` seem to result in an AssertionError at compiletime
        //Log.fine(t"Started monitoring for changes in ${dir.path.show}")
        dir.javaPath.register(svc, ENTRY_CREATE, ENTRY_MODIFY, ENTRY_DELETE).nn
      
      def poll(index: Map[WatchKey, Directory]): LazyList[FileEvent] =
        erased given CanThrow[RootParentError] = compiletime.erasedValue
        val events = index.map:
          (key, dir) => key.pollEvents().nn.iterator.nn.asScala.to(List).flatMap:
            event =>
              val path: IoPath = event.context match
                case path: jnf.Path => dir.path ++ Relative.parse(Showable(path).show)
                case _              => throw Impossible("Watch service should always return a Path")
              
              event.kind match
                case ENTRY_CREATE => if path.isDirectory
                                     then List(FileEvent.NewDirectory(path.directory))
                                     else List(FileEvent.NewFile(path.file))
                case ENTRY_MODIFY => List(FileEvent.Modify(path.file))
                case ENTRY_DELETE => List(FileEvent.Delete(path))
                case _            => Nil
        .flatten
      
        if continue then
          val newIndex = events.foldLeft(index):
            case (index, FileEvent.NewDirectory(dir)) =>
              // Calls to `Log.fine` seem to result in an AssertionError at compiletime
              //Log.fine(t"Starting monitoring new directory ${dir.path.show}")
              index.updated(watchKey(dir), dir)
            
            case (index, FileEvent.Delete(path)) =>
              // Calls to `Log.fine` seem to result in an AssertionError at compiletime
              //if path.isDirectory then Log.fine(t"Stopping monitoring of deleted directory $path")
              val deletions = index.filter(_(1).path == path)
              deletions.keys.foreach(_.cancel())
              index -- deletions.keys
            
            case _ =>
              index
          
          events.to(LazyList) #::: :
            Thread.sleep(interval)
            poll(newIndex)
        else
          index.keys.foreach(_.cancel())
          LazyList()

      Watcher(() => poll(dirs.mtwin.map(watchKey(_) -> _).to(Map)), () => continue = false)

    def children: List[Inode] throws IoError =
      Option(javaFile.list).fold(Nil):
        files =>
          files.nn.to(List).map(_.nn).map(Text(_)).map(path.parts :+ _).map(makeAbsolute(_)).map(_.inode)
    
    def descendants: LazyList[Inode] throws IoError =
      children.to(LazyList).flatMap(_.directory).flatMap { f => f +: f.descendants }
    
    def subdirectories: List[Directory] throws IoError =
      children.collect:
        case dir: Directory => dir
    
    def deepSubdirectories: LazyList[Directory] throws IoError =
      val subdirs = subdirectories.to(LazyList).filter(!_.name.startsWith(t"."))
      subdirs #::: subdirs.flatMap(_.deepSubdirectories)

    def files: List[File] throws IoError = children.collect:
      case file: File => file

    def copyTo(dest: IoPath): Directory throws IoError =
      if dest.exists()
      then throw IoError(IoError.Op.Write, IoError.Reason.AlreadyExists, dest)
      
      try Files.copy(javaPath, Paths.get(dest.show.s))
      catch e => throw IoError(IoError.Op.Write, IoError.Reason.AccessDenied, initPath)
      
      dest.directory

    def hardLinkTo(dest: IoPath): Directory throws IoError =
      if dest.exists()
      then throw IoError(IoError.Op.Write, IoError.Reason.AlreadyExists, dest)
      
      try Files.createLink(javaPath, Paths.get(dest.show.s)).unit
      catch case e =>
        throw IoError(IoError.Op.Write, IoError.Reason.DifferentFilesystems, initPath)

      dest.directory
    
    @targetName("access")
    infix def /(child: Text): IoPath throws RootParentError = makeAbsolute((path / child).parts)


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

  //   def empty: Boolean =
  //     val filesStream = Files.walk(javaPath)
  //     try filesStream.allMatch { p => Files.isDirectory(p) } finally filesStream.close()

  //   def size: ByteSize = ByteSize(javaFile.length)

  //   def setReadOnly(recursion: Recursion): Unit throws UnchangeablePermissions =
  //     if !javaFile.setWritable(false) then throw UnchangeablePermissions(path)
  //     if recursion == Recursion.Recursive then children.foreach(_.setWritable(recursion))

  //   def setWritable(recursion: Recursion): Unit throws UnchangeablePermissions =
  //     if !javaFile.setWritable(true) then throw UnchangeablePermissions(path)
  //     if recursion == Recursion.Recursive then children.foreach(_.setWritable(recursion))

  //   def uniquify(): Path =
  //     if !exists() then path else LazyList.from(2).map { i => rename(_+"-"+i) }.find(!_.exists()).get

  //   def hardLink(dest: Path): Unit throws PathAlreadyExists =
  //     if dest.exists() then throw PathAlreadyExists(dest)
  //     try Files.createLink(javaPath, dest.javaPath).unit
  //     catch case ex: java.nio.file.NoSuchFileException => copyTo(dest).unit

  //   def touch(): Unit =
  //     try
  //       if !exists() then ji.FileOutputStream(javaFile).close()
  //       else javaFile.setLastModified(System.currentTimeMillis()).unit
  //     catch case e => NotWritable(path)

  //   def extant(): Path =
  //     mkdir()
  //     path

  //   def directory: Boolean = Files.isDirectory(javaPath)

  //   def extantParents(): Path =
  //     parent.mkdir()
  //     path

  //   def executable: Boolean = Files.isExecutable(javaPath)
  //   def readable: Boolean = Files.isReadable(javaPath)
  //   def writable: Boolean = Files.isWritable(javaPath)

  //   def setExecutable(exec: Boolean): Unit throws UnchangeablePermissions =
  //     try javaFile.setExecutable(exec).unit catch e => throw UnchangeablePermissions(path)

  //   def resolve(rel: Path): Path = Path(javaPath.resolve(rel.javaPath))

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

  //   def findChildren(pred: String => Boolean): Set[Path] =
  //     def search(dir: JavaFile): Set[JavaFile] =
  //       val files = dir.listFiles.to(Set)
  //       files.filter(_.isDirectory).flatMap(search(_)) ++ files.filter { f => !f.isDirectory && pred(f.getName) }

  //     search(javaFile).map(Path(_))

  //   def findSubdirsContaining(pred: String => Boolean): Set[Path] =
  //     Option(javaFile.listFiles).map { files =>
  //       val found = if files.exists { f => pred(f.getName) } then Set(path) else Set()
  //       val subdirs = files.filter(_.isDirectory).filterNot(_.getName.startsWith(".")).map(Path(_)).to(Set)

  //       subdirs.flatMap(_.findSubdirsContaining(pred)) ++ found
  //     }.getOrElse(Set())

  //   def delete(): Unit throws NotWritable =
  //     def delete(file: JavaFile): Boolean =
  //       if Files.isSymbolicLink(file.toPath) then file.delete()
  //       else if file.isDirectory then file.listFiles.forall(delete(_)) && file.delete()
  //       else file.delete()

  //     try delete(javaFile).unit catch e => throw NotWritable(path)

  //   def linkTarget(): Option[Path] =
  //     if Files.isSymbolicLink(javaPath) then Some(Path(javaPath.toRealPath())) else None

  //   def unlink(): Unit throws NotSymbolicLink | NotWritable =
  //     try if Files.isSymbolicLink(javaPath) then Files.delete(javaPath) else throw NotSymbolicLink(path)
  //     catch e => throw NotWritable(path)

  //   def append[T: Writable](content: T): Unit = write(content, true)

  //   def copyTo(dest: Path): Path throws PathAlreadyExists | NotWritable =
  //     if dest.exists() then throw PathAlreadyExists(dest)
  //     try
  //       Files.walkFileTree(javaPath, Path.CopyFileVisitor(javaPath, dest.javaPath))
  //       dest
  //     catch e => throw NotWritable(path)

  //   def hardLinkTo(dest: Path): Unit throws NotWritable =
  //     try Files.createLink(dest.javaPath, javaPath) catch e => throw NotWritable(path)

  //   def hardLinkCount(): Int throws FileReadError =
  //     try Files.getAttribute(javaPath, "unix:nlink") match
  //       case i: Int => i
  //     catch e => throw FileReadError(path, e)

  //   def walkTree: LazyList[Path] =
  //     if directory then LazyList(path) ++: children.to(LazyList).flatMap(_.walkTree) else LazyList(path)

  //   def absolutePath(): Path = Path(Showable(javaPath.toAbsolutePath.normalize).show)
  //   def relativizeTo(dir: Path): Path = Path(dir.javaPath.relativize(path.javaPath))
  //   def parent: Path = Showable(javaPath.getParent).show
  //   def rename(fn: String => String): Path = parent / fn(name)
    
  //   def symlinkTo(target: Path): Unit throws NotWritable =
  //     try Files.createSymbolicLink(target.javaPath, javaPath)
  //     catch case e: ji.IOException => throw NotWritable(path)

object Writable:
  given Writable[LazyList[IArray[Byte]]] =
    (out, stream) => stream.map(_.unsafeMutable).foreach(out.write(_))
  
  given (using enc: Encoding): Writable[LazyList[Text]] = (out, stream) =>
    val writer = ji.BufferedWriter(ji.OutputStreamWriter(out, enc.name.s))
    stream.foreach:
      part =>
        writer.write(part.s)
        writer.flush()
    writer.close()

  given (using enc: Encoding): Writable[Text] =
    (out, string) =>
      val writer = ji.BufferedWriter(ji.OutputStreamWriter(out, enc.name.s))
      writer.write(string.s)
      writer.close()
  
  given Writable[IArray[Byte]] = (out, bytes) => out.write(bytes.unsafeMutable)

trait Writable[T]:
  def write(stream: ji.OutputStream, value: T): Unit
  def contramap[S](fn: S => T): Writable[S] = (stream, value) => write(stream, fn(value))

object Readable:
  given dataStream: Readable[DataStream, Nothing] with
    def read(in: ji.BufferedInputStream, limit: ByteSize = 64.kb): DataStream throws Nothing =
      Util.read(in, limit)
    
  given stringStream(using enc: Encoding)
      : Readable[LazyList[Text], StreamCutError | ExcessDataError] with
    def read(in: ji.BufferedInputStream, limit: ByteSize = 64.kb) =

      def read(prefix: Array[Byte], remaining: Int): LazyList[Text] =
        try
          val avail: Long = in.available
          if avail == 0 then LazyList()
          else if avail > remaining
          then throw ExcessDataError((limit.long + avail - remaining).toInt.b, limit)
          else
            val buf = new Array[Byte](in.available.min(limit.long.toInt) + prefix.length)
            if prefix.length > 0 then System.arraycopy(prefix, 0, buf, 0, prefix.length)
            val count = in.read(buf, prefix.length, buf.length - prefix.length)
            if count + prefix.length < 0 then LazyList(Text(String(buf, enc.name.s)))
            else
              val carry = enc.carry(buf)
              (if carry == 0 then Text(String(buf, enc.name.s)) else Text(String(buf, 0, buf.length -
                  carry, enc.name.s))) #:: read(buf.takeRight(carry), limit.long.toInt - buf.length)
        catch IOException => throw StreamCutError()
      
      read(Array.empty[Byte], limit.long.toInt)

  given readableString: Readable[Text, ExcessDataError | StreamCutError] with
    def read(in: ji.BufferedInputStream, limit: ByteSize = 64.kb) =
      given enc: Encoding = encodings.Utf8
      val stream = stringStream.read(in, limit)
      if stream.length == 0 then t""
      else if stream.length > 1 then throw ExcessDataError(stream.length.b + limit, limit)
      else stream.head
  
  given readableBytes: Readable[Bytes, ExcessDataError | StreamCutError] with
    def read(in: ji.BufferedInputStream, limit: ByteSize) = Util.read(in, limit).slurp(limit)

trait Readable[T, Ex <: Exception]:
  type E = Ex
  
  private inline def readable: Readable[T, E] = this
  
  def read(stream: ji.BufferedInputStream, limit: ByteSize = 64.kb): T throws E
  
  def map[S](fn: T => S): Readable[S, E] = new Readable[S, E]:
    def read(stream: ji.BufferedInputStream, limit: ByteSize = 64.kb): S throws E =
      fn(readable.read(stream, limit))

object encodings:
  given Utf8: Encoding with
    def carry(arr: Array[Byte]): Int =
      val len = arr.length
      def last = arr(len - 1)
      def last2 = arr(len - 2)
      def last3 = arr(len - 3)
      
      if len > 0 && ((last & -32) == -64 || (last & -16) == -32 || (last & -8) == -16) then 1
      else if len > 1 && ((last2 & -16) == -32 || (last2 & -8) == -16) then 2
      else if len > 2 && ((last3 & -8) == -16) then 3
      else 0
    
    def name: Text = t"UTF-8"
  
  given Ascii: Encoding with
    def carry(arr: Array[Byte]): Int = 0
    def name: Text = t"ASCII"
  
  @targetName("ISO_8859_1")
  given `ISO-8859-1`: Encoding with
    def name: Text = t"ISO-8859-1"
    def carry(arr: Array[Byte]): Int = 0

object Encoding:
  given acceptCharset[T]: clairvoyant.HtmlAttribute["acceptCharset", Encoding] with
    def serialize(enc: Encoding): String = enc.name.s
    def name: String = "accept-charset"
  
  given charset[T]: clairvoyant.HtmlAttribute["charset", Encoding] with
    def serialize(enc: Encoding): String = enc.name.s
    def name: String = "charset"

trait Encoding:
  def name: Text
  def carry(array: Array[Byte]): Int

open class JovianError(message: Text) extends Exception(t"jovian: $message".s)

object Filesystem:
  
  lazy val roots: Set[Filesystem] =
    Option(ji.File.listRoots).fold(Set())(_.nn.to(Set)).map(_.nn.getAbsolutePath.nn).collect:
      case "/"                                  => Unix
      case s"""$drive:\""" if drive.length == 1 =>
        val letter = try drive(0) catch case e: OutOfRangeError => throw Impossible(e)
        letter.toUpper match
          case ch: Majuscule =>
            WindowsRoot(ch)
          
          case ch =>
            throw Impossible(s"a drive letter with an unexpected name was found: '$ch'")
    .to(Set)
 
  def defaultSeparator: "/" | "\\" = if ji.File.separator == "\\" then "\\" else "/"

object Unix extends Filesystem(t"/", t"/"):
  def Pwd: IoPath throws PwdError =
    val dir = try Sys.user.dir().show catch case e: KeyNotFound => throw PwdError()
    makeAbsolute(parse(dir).get.parts)

case class WindowsRoot(drive: Majuscule) extends Filesystem(t"\\", t"${drive}:\\")
object windows:
  object DriveC extends WindowsRoot('C')
  object DriveD extends WindowsRoot('D')
  object DriveE extends WindowsRoot('E')
  object DriveF extends WindowsRoot('F')

given realm: Realm = Realm(t"jovian")
