/*
    Jovian, version 0.1.0. Copyright 2019-21 Jon Pretty, Propensive OÜ.

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

import scala.util.*
import scala.collection.generic.CanBuildFrom
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

case class InsufficientPermissions(inode: Filesystem#Inode)
extends JovianError("the permissions of the path cannot be changed")

case class NotFile(path: Filesystem#Path)
extends JovianError(str"the path ${path.toString} is not a file")

case class NotSymlink(path: Filesystem#Path)
extends JovianError(str"the path ${path.toString} is not a symlink")

case class NotDirectory(path: Filesystem#Path)
extends JovianError(str"the path ${path.toString} is not a directory")

case class Nonexistent(path: Filesystem#Path)
extends JovianError(str"the path ${path.toString} does not exist")

case class AlreadyExistent(path: Filesystem#Path)
extends JovianError(str"the path ${path.toString} already exists")

case class MissingResource(path: Classpath#Path)
extends JovianError(str"the resource ${path.toString} could not be accessed")

case class UnknownPwd()
extends JovianError("the current working directory cannot be determined")

open class Classpath(classLoader: ClassLoader = getClass.nn.getClassLoader.nn) extends Root("/", ""):
  type AbsolutePath = CpPath
  type RelativePath = Path.Relative

  def makeAbsolute(parts: Vector[String]): CpPath = CpPath(parts)
  def makeRelative(ascent: Int, parts: Vector[String]) = Path.Relative(ascent, parts)
  
  def /(resource: String): CpPath = CpPath(Vector(resource))

  case class CpPath(parts: Vector[String]) extends Path.Absolute(parts):
    def resource: Resource = Resource(makeAbsolute(parts))

  case class Resource(path: CpPath):
    def read[T: Readable](limit: Int = 65536): T exposes FileReadError | MissingResource |
        BufferOverflow | StreamInterrupted =
      val in = ji.BufferedInputStream(classLoader.getResourceAsStream(path.toString()))
      try summon[Readable[T]].read(in, limit)
      catch case e: NullPointerException => throw MissingResource(path)
    
    def name: String = path.path.lastOption.getOrElse(prefix)
    def parent: Resource exposes RootBoundaryExceeded = Resource(path.parent)

class Filesystem(pathSeparator: "/" | "\\", fsPrefix: String)
extends Root(pathSeparator, fsPrefix):

  type RelativePath = Path.Relative
  type AbsolutePath = FsPath

  case class FsPath(parts: Vector[String]) extends Path.Absolute(parts):
    def file: File exposes Nonexistent | NotFile =
      val javaFile = ji.File(toString)
      if !javaFile.exists() then throw Nonexistent(this)
      if javaFile.isDirectory then throw NotFile(this)
      
      File(this)
    
    def directory: Directory exposes Nonexistent | NotDirectory | NotFile =
      val javaFile = ji.File(toString)
      if !javaFile.exists() then throw Nonexistent(this)
      if !javaFile.isDirectory then throw NotFile(this)
      
      Directory(this)
  
    def symlink: Symlink exposes Nonexistent | NotSymlink =
      val javaFile = ji.File(toString)
      if !javaFile.exists() then throw Nonexistent(this)
      if !Files.isSymbolicLink(javaFile.toPath) then throw NotSymlink(this)
      
      Symlink(this, parse(Files.readSymbolicLink(Paths.get(toString)).toString).get)
  
    def inode: Inode exposes Nonexistent =
      val javaFile = ji.File(toString)
      val javaPath = javaFile.toPath
      if !javaFile.exists() then throw Nonexistent(this)
      
      if javaFile.isDirectory then Directory(this)
      else if Files.isSymbolicLink(javaPath)
      then
        try Symlink(this, parse(Files.readSymbolicLink(javaPath).toString).get)
        catch NoSuchElementException => File(this)
      else File(this)
    
    def createDirectory(): Directory exposes NotWritable | AlreadyExistent =
      val javaFile = ji.File(toString)
      if javaFile.exists() then throw AlreadyExistent(this)
      if !javaFile.mkdirs() then throw NotWritable(???)
  
      Directory(this)
    
    def exists(): Boolean exposes NotReadable = ji.File(path.toString).exists()
  
    def createFile(overwrite: Boolean = true): File exposes NotWritable | AlreadyExistent =
      val javaFile = ji.File(path.toString)
      if !overwrite && javaFile.exists() then throw AlreadyExistent(this)
      try ji.FileOutputStream(javaFile).close() catch case e => NotWritable(???)
  
      File(this)

    
  def makeAbsolute(parts: Vector[String]): FsPath = FsPath(parts)
  def makeRelative(ascent: Int, path: Vector[String]): Path.Relative = Path.Relative(ascent, path)

  def parse(value: String): Option[Path.Absolute] =
    if value.startsWith(prefix)
    then Some(Path.Absolute(value.drop(prefix.length).cut(separator.toString).to(Vector)))
    else None
  
  def /(filename: String): Path.Absolute = Path.Absolute(Vector(filename))

  sealed trait Inode(val path: Path.Absolute):
    lazy val javaFile: ji.File = ji.File(path.toString)
    lazy val javaPath: JavaPath = Paths.get(path.toString).nn
    def name: String = path.path.lastOption.getOrElse(prefix)
    def fullname: String = javaFile.getAbsolutePath.nn
    def uriString: String = javaFile.toURI.toString
    def exists(): Boolean = Files.exists(javaPath)
    def parent: Directory exposes RootBoundaryExceeded = Directory(path.parent)
    def directory: Option[Directory]
    def file: Option[File]
    def symlink: Option[Symlink]
    def lastModified: Long = javaFile.lastModified
    
    def copyTo(dest: FsPath): Inode exposes AlreadyExistent | NotWritable | NotReadable |
        NotDirectory | Nonexistent | NotFile
    
    def hardLinkTo(dest: FsPath): Inode exposes AlreadyExistent | DifferentFilesystems | NotFile |
        NotReadable | Nonexistent | NotDirectory | NotWritable

  case class File(initPath: Path.Absolute) extends Inode(initPath):
    def directory: Option[Directory] = None
    def file: Option[File] = Some(this)
    def symlink: Option[Symlink] = None
    
    def write[T: Writable](content: T, append: Boolean = false): Unit exposes NotWritable =
      val out = ji.FileOutputStream(javaPath.toFile, append)
      try summon[Writable[T]].write(out, content) catch case e => throw NotWritable(this)
      finally try out.close() catch _ => ()

    def read[T: Readable](limit: Int = 65536): T exposes FileReadError | BufferOverflow |
        StreamInterrupted =
      val in = ji.BufferedInputStream(ji.FileInputStream(javaPath.toFile))
      try summon[Readable[T]].read(in, limit)
      catch case e => throw FileReadError(this, e)

    def copyTo(dest: FsPath): File exposes AlreadyExistent | NotWritable | NotReadable |
        NotDirectory | Nonexistent | NotFile =
      if dest.exists() then throw AlreadyExistent(dest)
      
      try Files.copy(javaPath, Paths.get(dest.toString))
      catch IOException => throw NotWritable(this)
      
      dest.file

    def hardLinkTo(dest: FsPath): File exposes AlreadyExistent | DifferentFilesystems | NotFile |
        NotReadable | Nonexistent | NotDirectory | NotWritable =
      if dest.exists() then throw AlreadyExistent(dest)
      try Files.createLink(javaPath, Paths.get(dest.toString))
      catch case e => throw DifferentFilesystems(path, dest)

      dest.file

  case class Symlink(initPath: Path.Absolute, target: Path) extends Inode(initPath):
    def apply(): Path = target
    
    def hardLinkTo(dest: FsPath): Inode exposes AlreadyExistent | DifferentFilesystems | NotFile |
        NotReadable | Nonexistent | NotDirectory | NotWritable = copyTo(dest)
    
    def directory: Option[Directory] = None
    def file: Option[File] = None
    def symlink: Option[Symlink] = Some(this)
    
    def copyTo(dest: FsPath): Symlink exposes AlreadyExistent | NotWritable | NotReadable |
        NotDirectory | Nonexistent | NotFile =
      Files.createSymbolicLink(Paths.get(dest.toString), Paths.get(target.toString))
      Symlink(path, dest)

  case class Directory(initPath: Path.Absolute) extends Inode(initPath):
    def directory: Option[Directory] = Some(this)
    def file: Option[File] = None
    def symlink: Option[Symlink] = None
    
    def children: List[Inode] exposes Nonexistent =
      Option(javaFile.list).fold(Nil) { files =>
        files.nn.to(List).map(_.nn).map(initPath.path :+ _).map(makeAbsolute(_)).map(_.inode)
      }
    
    def descendants: LazyList[Inode] exposes Nonexistent =
      children.to(LazyList).flatMap(_.directory).flatMap { f => f +: f.descendants }
    
    def subdirectories: List[Directory] exposes Nonexistent =
      children.collect { case dir: Directory => dir }
    
    def files: List[File] exposes Nonexistent = children.collect { case file: File => file }

    def copyTo(dest: FsPath): Directory exposes AlreadyExistent | NotWritable | NotReadable |
        NotDirectory | Nonexistent | NotFile =
      if dest.exists() then throw AlreadyExistent(dest)
      
      try Files.copy(javaPath, Paths.get(dest.toString))
      catch IOException => throw NotWritable(this)
      
      dest.directory

    def hardLinkTo(dest: FsPath): Directory exposes AlreadyExistent | DifferentFilesystems | NotFile |
        NotReadable | Nonexistent | NotDirectory | NotWritable =
      if dest.exists() then throw AlreadyExistent(dest)
      try Files.createLink(javaPath, Paths.get(dest.toString)).unit
      catch case e => throw DifferentFilesystems(path, dest)

      dest.directory
    
    def /(child: String): FsPath exposes RootBoundaryExceeded = makeAbsolute((path / child).path)

// object OldPath:
//   def apply(jpath: JavaPath): Path = Path(jpath.toString match
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
  //   def name: String = javaPath.getFileName.toString

  //   def empty: Boolean =
  //     val filesStream = Files.walk(javaPath)
  //     try filesStream.allMatch { p => Files.isDirectory(p) } finally filesStream.close()

  //   def size: ByteSize = ByteSize(javaFile.length)

  //   def setReadOnly(recursion: Recursion): Unit exposes UnchangeablePermissions =
  //     if !javaFile.setWritable(false) then throw UnchangeablePermissions(path)
  //     if recursion == Recursion.Recursive then children.foreach(_.setWritable(recursion))

  //   def setWritable(recursion: Recursion): Unit exposes UnchangeablePermissions =
  //     if !javaFile.setWritable(true) then throw UnchangeablePermissions(path)
  //     if recursion == Recursion.Recursive then children.foreach(_.setWritable(recursion))

  //   def uniquify(): Path =
  //     if !exists() then path else LazyList.from(2).map { i => rename(_+"-"+i) }.find(!_.exists()).get

  //   def hardLink(dest: Path): Unit exposes PathAlreadyExists =
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

  //   def setExecutable(exec: Boolean): Unit exposes UnchangeablePermissions =
  //     try javaFile.setExecutable(exec).unit catch e => throw UnchangeablePermissions(path)

  //   def resolve(rel: Path): Path = Path(javaPath.resolve(rel.javaPath))

  //   def moveTo(dest: Path): Unit exposes NotWritable =
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

  //   def delete(): Unit exposes NotWritable =
  //     def delete(file: JavaFile): Boolean =
  //       if Files.isSymbolicLink(file.toPath) then file.delete()
  //       else if file.isDirectory then file.listFiles.forall(delete(_)) && file.delete()
  //       else file.delete()

  //     try delete(javaFile).unit catch e => throw NotWritable(path)

  //   def linkTarget(): Option[Path] =
  //     if Files.isSymbolicLink(javaPath) then Some(Path(javaPath.toRealPath())) else None

  //   def unlink(): Unit exposes NotSymbolicLink | NotWritable =
  //     try if Files.isSymbolicLink(javaPath) then Files.delete(javaPath) else throw NotSymbolicLink(path)
  //     catch e => throw NotWritable(path)

  //   def append[T: Writable](content: T): Unit = write(content, true)

  //   def copyTo(dest: Path): Path exposes PathAlreadyExists | NotWritable =
  //     if dest.exists() then throw PathAlreadyExists(dest)
  //     try
  //       Files.walkFileTree(javaPath, Path.CopyFileVisitor(javaPath, dest.javaPath))
  //       dest
  //     catch e => throw NotWritable(path)

  //   def hardLinkTo(dest: Path): Unit exposes NotWritable =
  //     try Files.createLink(dest.javaPath, javaPath) catch e => throw NotWritable(path)

  //   def hardLinkCount(): Int exposes FileReadError =
  //     try Files.getAttribute(javaPath, "unix:nlink") match
  //       case i: Int => i
  //     catch e => throw FileReadError(path, e)

  //   def walkTree: LazyList[Path] =
  //     if directory then LazyList(path) ++: children.to(LazyList).flatMap(_.walkTree) else LazyList(path)

  //   def absolutePath(): Path = Path(javaPath.toAbsolutePath.normalize.toString)
  //   def relativizeTo(dir: Path): Path = Path(dir.javaPath.relativize(path.javaPath))
  //   def parent: Path = javaPath.getParent.toString
  //   def rename(fn: String => String): Path = parent / fn(name)
    
  //   def symlinkTo(target: Path): Unit exposes NotWritable =
  //     try Files.createSymbolicLink(target.javaPath, javaPath)
  //     catch case e: ji.IOException => throw NotWritable(path)

object Writable:
  given Writable[LazyList[IArray[Byte]]] =
    (out, stream) => stream.map(_.unsafeMutable).foreach(out.write(_))
  
  given (using enc: Encoding): Writable[LazyList[String]] = (out, stream) =>
    val writer = ji.BufferedWriter(ji.OutputStreamWriter(out, enc.name))
    stream.foreach { part =>
      writer.write(part)
      writer.flush()
    }
    writer.close()

  given (using enc: Encoding): Writable[String] =
    (out, string) =>
      val writer = ji.BufferedWriter(ji.OutputStreamWriter(out, enc.name))
      writer.write(string)
      writer.close()
  
  given Writable[IArray[Byte]] = (out, bytes) => out.write(bytes.unsafeMutable)

trait Writable[T]:
  def write(stream: ji.OutputStream, value: T): Unit
  def contramap[S](fn: S => T): Writable[S] = (stream, value) => write(stream, fn(value))

object Readable:
  given Readable[LazyList[IArray[Byte]]] with
    def read(in: ji.BufferedInputStream, limit: Int = 65536): LazyList[IArray[Byte]] exposes 
        StreamInterrupted | BufferOverflow =

      def read(): LazyList[IArray[Byte]] =
        val avail = in.available
        if avail == 0 then LazyList() else
          val buf = new Array[Byte](in.available.min(limit))
          val count = in.read(buf, 0, buf.length)
          if count < 0 then LazyList(buf.unsafeImmutable)
          else buf.asInstanceOf[IArray[Byte]] #:: read()
      
      read()
    
  given (using enc: Encoding): Readable[LazyList[String]] with
    def read(in: ji.BufferedInputStream, limit: Int = 65536): LazyList[String] =

      def read(prefix: Array[Byte], remaining: Int): LazyList[String] =
        try
          val avail = in.available
          if avail == 0 then LazyList()
          else if avail > remaining then throw BufferOverflow()
          else
            val buf = new Array[Byte](in.available.min(limit) + prefix.length)
            if prefix.length > 0 then System.arraycopy(prefix, 0, buf, 0, prefix.length)
            val count = in.read(buf, prefix.length, buf.length - prefix.length)
            if count + prefix.length < 0 then LazyList(String(buf, enc.name))
            else
              val carry = enc.carry(buf)
              (if carry == 0 then String(buf, enc.name) else String(buf, 0, buf.length -
                  carry, enc.name)) #:: read(buf.takeRight(carry), limit - buf.length)
        catch IOException => throw StreamInterrupted()
      
      read(Array.empty[Byte], limit)

  given readableString: Readable[String] with
    def read(in: ji.BufferedInputStream, limit: Int = 65536): String =
      given enc: Encoding = encodings.Utf8
      val stream = summon[Readable[LazyList[String]]].read(in, limit)
      if stream.length == 0 then ""
      else if stream.length > 1 then throw BufferOverflow()
      else stream.head

trait Readable[T]:
  
  private inline def readable: Readable[T] = this
  
  def read(stream: ji.BufferedInputStream, limit: Int = 65536): T
  
  def map[S](fn: T => S): Readable[S] = new Readable[S]:
    def read(stream: ji.BufferedInputStream, limit: Int = 65536): S =
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
    
    def name: String = "UTF-8"
  
  given Ascii: Encoding with
    def carry(arr: Array[Byte]): Int = 0
    def name: String = "ASCII"
  
  given `ISO-8859-1`: Encoding with
    def name: String = "ISO-8859-1"
    def carry(arr: Array[Byte]): Int = 0

object Encoding:
  given acceptCharset[T]: simplistic.HtmlAttribute["acceptCharset", Encoding] with
    def serialize(enc: Encoding): String = enc.name
    def name: String = "accept-charset"
  
  given charset[T]: simplistic.HtmlAttribute["charset", Encoding] with
    def serialize(enc: Encoding): String = enc.name
    def name: String = "charset"

trait Encoding:
  def name: String
  def carry(array: Array[Byte]): Int

opaque type ByteSize = Long

object ByteSize:
  def apply(bytes: Long): ByteSize = bytes

  extension (byteSize: ByteSize)
    def +(that: ByteSize): ByteSize = byteSize + that
    def value: Long = byteSize

open class JovianError(message: String) extends Exception(str"jovian: $message")

case class DifferentFilesystems(source: Filesystem#Path, destination: Filesystem#Path)
extends JovianError(
    str"${source.toString} and ${destination.toString} are not on the same filesystem")

case class FileNotFound(inode: Filesystem#Inode)
extends JovianError(str"the file or directory ${inode.name} was not found")

case class NotSymbolicLink(inode: Filesystem#Inode)
extends JovianError(str"the path ${inode.name} is not a symbolic link")

case class BufferOverflow()
extends JovianError(str"the stream contained more data than the buffer could hold")

case class StreamInterrupted() extends JovianError(str"the stream was interrupted")

case class NotWritable(inode: Filesystem#Inode)
extends JovianError(str"could not write to ${inode.name}")

case class NotReadable(inode: Filesystem#Inode)
extends JovianError(str"could not read from ${inode.name}")

case class FileReadError(inode: Filesystem#Inode, e: Throwable)
extends JovianError(str"could not read from ${inode.name}")

object Filesystem:
  lazy val roots: Set[Filesystem] =
    Option(ji.File.listRoots).fold(Set())(_.nn.to(Set)).map(_.nn.getAbsolutePath.nn).collect {
      case "/"                                  => Unix
      case s"""$drive:\""" if drive.length == 1 => drive(0).toUpper match
        case ch: Majuscule =>
          WindowsRoot(ch)
        
        case ch =>
          throw Impossible(str"a drive letter with an unexpected name was found: '$ch'")
    }.to(Set)
 
  def defaultSeparator: "/" | "\\" = if ji.File.separator == "\\" then "\\" else "/"

object Unix extends Filesystem("/", "/"):
  def Pwd: FsPath exposes UnknownPwd =
    val dir = try Sys.user.dir() catch case KeyNotFound(_) => throw UnknownPwd()
    makeAbsolute(parse(dir).get.path)

case class WindowsRoot(drive: Majuscule) extends Filesystem("\\", str"${drive.toString}:\\")

object windows:
  object DriveC extends WindowsRoot('C')
  object DriveD extends WindowsRoot('D')
  object DriveE extends WindowsRoot('E')
  object DriveF extends WindowsRoot('F')
