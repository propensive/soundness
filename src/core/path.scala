/*

    Fury, version 0.33.0. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

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
    DirectoryNotEmptyException, Path as JavaPath}, jnf.StandardCopyOption.*, jnf.attribute.BasicFileAttributes

import ji.{Closeable, InputStream, File as JavaFile}

case class InsufficientPermissions(inode: Inode[Filesystem])
extends JovianException("the permissions of the path cannot be changed")

case class NotFile(path: Path[Filesystem])
extends JovianException(str"the path ${path.toString} is not a file")

case class NotDir(path: Path[Filesystem])
extends JovianException(str"the path ${path.toString} is not a directory")

case class Nonexistant(path: Path[Filesystem])
extends JovianException(str"the path ${path.toString} does not exist")

case class AlreadyExistent(path: Path[Filesystem])
extends JovianException(str"the path ${path.toString} already exists")

type Majuscule = 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' | 'L' | 'M' | 'N' | 'O' | 'P' |
    'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W' | 'X' | 'Y' | 'Z'

enum Recursion:
  case Recursive, Nonrecursive

extension [Fs <: Filesystem](path: Path.Absolute[Fs])
  def file: File[Fs] raises Nonexistant | NotFile =
    val javaFile = ji.File(path.toString)
    if !javaFile.exists() then throw Nonexistant(path)
    if javaFile.isDirectory then throw NotFile(path)
    
    File(path)
  
  def dir: Dir[Fs] raises Nonexistant | NotDir =
    val javaFile = ji.File(path.toString)
    if !javaFile.exists() then throw Nonexistant(path)
    if !javaFile.isDirectory then throw NotFile(path)
    
    Dir(path)

  def inode: Inode[Fs] raises Nonexistant =
    val javaFile = ji.File(path.toString)
    if !javaFile.exists() then throw Nonexistant(path)
    
    if javaFile.isDirectory then Dir(path) else File(path)
  
  def createDir(): Dir[Fs] raises NotWritable | AlreadyExistent =
    val javaFile = ji.File(path.toString)
    if javaFile.exists() then throw AlreadyExistent(path)
    if !javaFile.mkdirs() then throw NotWritable(???)

    Dir(path)
  
  def exists(): Boolean raises NotReadable = ji.File(path.toString).exists()

  def createFile(): File[Fs] raises NotWritable | AlreadyExistent =
    val javaFile = ji.File(path.toString)
    if javaFile.exists() then throw AlreadyExistent(path)
    try ji.FileOutputStream(javaFile).close() catch case e => NotWritable(???)

    File(path)

sealed trait Inode[+Fs <: Filesystem](val path: Path.Absolute[Fs]):
  lazy val javaFile: ji.File = ji.File(path.toString)
  lazy val javaPath: JavaPath = Paths.get(path.toString)
  def name: String = path.path.lastOption.getOrElse(path.root.prefix)
  def fullname: String = javaFile.getAbsolutePath
  def uriString: String = javaFile.toURI.toString
  def exists(): Boolean = Files.exists(javaPath)
  def copyTo[Fs2 <: Filesystem](dest: Path.Absolute[Fs2]): Inode[Fs2]
  def hardLinkTo[Fs2 <: Filesystem](dest: Path.Absolute[Fs2]): Inode[Fs2]

case class File[+Fs <: Filesystem](initPath: Path.Absolute[Fs]) extends Inode[Fs](initPath):
  def write[T: Writable](content: T, append: Boolean = false): Unit raises NotWritable =
    val out = ji.FileOutputStream(javaPath.toFile, append)
    try summon[Writable[T]].write(out, content) catch case e => throw NotWritable(this)
    finally try out.close() catch _ => ()

  def read[T: Readable](limit: Int = 65536): T raises FileReadError =
    val in = ji.BufferedInputStream(ji.FileInputStream(javaPath.toFile))
    try summon[Readable[T]].read(in, limit) catch case e => throw FileReadError(this, e)

  def copyTo[Fs2 <: Filesystem](dest: Path.Absolute[Fs2]): File[Fs2] raises AlreadyExistent | NotWritable =
    if dest.exists() then throw AlreadyExistent(dest)
    try Files.copy(javaPath, Paths.get(dest.toString)) catch IOException => throw NotWritable(this)
    
    dest.file

  def hardLinkTo[Fs2 <: Filesystem](dest: Path.Absolute[Fs2]): File[Fs2] raises AlreadyExistent =
    if dest.exists() then throw AlreadyExistent(dest)
    try Files.createLink(javaPath, Paths.get(dest.toString))
    catch case e => throw DifferentFilesystems(path, dest)

    dest.file

case class Dir[+Fs <: Filesystem](initPath: Path.Absolute[Fs]) extends Inode[Fs](initPath):
  def children: IArray[Inode[Fs]] = IArray.from(javaFile.list.map { child => (initPath / child).inode })

  def copyTo[Fs2 <: Filesystem](dest: Path.Absolute[Fs2]): Dir[Fs2] raises AlreadyExistent | NotWritable =
    if dest.exists() then throw AlreadyExistent(dest)
    try Files.copy(javaPath, Paths.get(dest.toString)) catch IOException => throw NotWritable(this)
    
    dest.dir

  def hardLinkTo[Fs2 <: Filesystem](dest: Path.Absolute[Fs2]): Dir[Fs2] raises AlreadyExistent =
    if dest.exists() then throw AlreadyExistent(dest)
    try Files.createLink(javaPath, Paths.get(dest.toString)).unit
    catch case e => throw DifferentFilesystems(path, dest)

    dest.dir
    
  








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
  //   def /(child: String): Path = if filename == "/" then Path(s"/$child") else Path(s"$filename/$child")
  //   def in(root: Path): Path = if path == "." then root else Path(s"$root/$path")
  //   def lastModified: Long = javaFile.lastModified()

  //   def empty: Boolean =
  //     val filesStream = Files.walk(javaPath)
  //     try filesStream.allMatch { p => Files.isDirectory(p) } finally filesStream.close()

  //   def size: ByteSize = ByteSize(javaFile.length)

  //   def setReadOnly(recursion: Recursion): Unit raises UnchangeablePermissions =
  //     if !javaFile.setWritable(false) then throw UnchangeablePermissions(path)
  //     if recursion == Recursion.Recursive then children.foreach(_.setWritable(recursion))

  //   def setWritable(recursion: Recursion): Unit raises UnchangeablePermissions =
  //     if !javaFile.setWritable(true) then throw UnchangeablePermissions(path)
  //     if recursion == Recursion.Recursive then children.foreach(_.setWritable(recursion))

  //   def uniquify(): Path =
  //     if !exists() then path else LazyList.from(2).map { i => rename(_+"-"+i) }.find(!_.exists()).get

  //   def hardLink(dest: Path): Unit raises PathAlreadyExists =
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

  //   def setExecutable(exec: Boolean): Unit raises UnchangeablePermissions =
  //     try javaFile.setExecutable(exec).unit catch e => throw UnchangeablePermissions(path)

  //   def resolve(rel: Path): Path = Path(javaPath.resolve(rel.javaPath))

  //   def moveTo(dest: Path): Unit raises NotWritable =
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

  //   def delete(): Unit raises NotWritable =
  //     def delete(file: JavaFile): Boolean =
  //       if Files.isSymbolicLink(file.toPath) then file.delete()
  //       else if file.isDirectory then file.listFiles.forall(delete(_)) && file.delete()
  //       else file.delete()

  //     try delete(javaFile).unit catch e => throw NotWritable(path)

  //   def linkTarget(): Option[Path] =
  //     if Files.isSymbolicLink(javaPath) then Some(Path(javaPath.toRealPath())) else None

  //   def unlink(): Unit raises NotSymbolicLink | NotWritable =
  //     try if Files.isSymbolicLink(javaPath) then Files.delete(javaPath) else throw NotSymbolicLink(path)
  //     catch e => throw NotWritable(path)

  //   def append[T: Writable](content: T): Unit = write(content, true)

  //   def copyTo(dest: Path): Path raises PathAlreadyExists | NotWritable =
  //     if dest.exists() then throw PathAlreadyExists(dest)
  //     try
  //       Files.walkFileTree(javaPath, Path.CopyFileVisitor(javaPath, dest.javaPath))
  //       dest
  //     catch e => throw NotWritable(path)

  //   def hardLinkTo(dest: Path): Unit raises NotWritable =
  //     try Files.createLink(dest.javaPath, javaPath) catch e => throw NotWritable(path)

  //   def hardLinkCount(): Int raises FileReadError =
  //     try Files.getAttribute(javaPath, "unix:nlink") match
  //       case i: Int => i
  //     catch e => throw FileReadError(path, e)

  //   def walkTree: LazyList[Path] =
  //     if directory then LazyList(path) ++: children.to(LazyList).flatMap(_.walkTree) else LazyList(path)

  //   def children: IArray[Path] =
  //     if exists() then
  //       val files = javaFile.listFiles
  //       if files == null then IArray[Path]() else IArray.from(files.map(_.getAbsolutePath))
  //     else IArray[Path]()
    
  //   def descendants: LazyList[Path] = children.to(LazyList).flatMap { f => f +: f.descendants }
  //   def absolutePath(): Path = Path(javaPath.toAbsolutePath.normalize.toString)
  //   def relativizeTo(dir: Path): Path = Path(dir.javaPath.relativize(path.javaPath))
  //   def parent: Path = javaPath.getParent.toString
  //   def rename(fn: String => String): Path = parent / fn(name)
    
  //   def symlinkTo(target: Path): Unit raises NotWritable =
  //     try Files.createSymbolicLink(target.javaPath, javaPath)
  //     catch case e: ji.IOException => throw NotWritable(path)

object Writable:
  given Writable[LazyList[IArray[Byte]]] =
    (out, stream) => stream.map(_.asInstanceOf[Array[Byte]]).foreach(out.write(_))
  
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
  
  given Writable[IArray[Byte]] = (out, bytes) => out.write(bytes.asInstanceOf[Array[Byte]])

trait Writable[T]:
  def write(stream: ji.OutputStream, value: T): Unit
  def contramap[S](fn: S => T): Writable[S] = (stream, value) => write(stream, fn(value))

object Readable:
  given Readable[LazyList[IArray[Byte]]] = (in, limit) =>
    def read(): LazyList[IArray[Byte]] =
      val avail = in.available
      if avail == 0 then LazyList() else
        val buf = new Array[Byte](in.available.min(limit))
        val count = in.read(buf, 0, buf.length)
        if count < 0 then LazyList(buf.asInstanceOf[IArray[Byte]])
        else buf.asInstanceOf[IArray[Byte]] #:: read()
    
    read()
    
  given (using enc: Encoding): Readable[LazyList[String]] = (in, limit) =>
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
            (if carry == 0 then String(buf, enc.name) else String(buf, 0, buf.length - carry, enc.name)) #::
                read(buf.takeRight(carry), limit - buf.length)
      catch IOException => throw StreamInterrupted()
    
    read(Array.empty[Byte], limit)

  given (using enc: Encoding): Readable[String] = (in, limit) =>
    val stream = summon[Readable[LazyList[String]]].read(in, limit)
    if stream.length > 1 then throw BufferOverflow() else stream.head

trait Readable[T]:
  def read(stream: ji.BufferedInputStream, limit: Int = 65536): T
  def map[S](fn: T => S): Readable[S] = (stream, limit) => fn(read(stream, limit))

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

trait Encoding:
  def name: String
  def carry(array: Array[Byte]): Int

opaque type ByteSize = Long

object ByteSize:
  def apply(bytes: Long): ByteSize = bytes

  extension (byteSize: ByteSize)
    def +(that: ByteSize): ByteSize = byteSize + that
    def value: Long = byteSize

open class JovianException(message: String) extends Exception(str"jovian: $message")

case class DifferentFilesystems(source: Path[Filesystem], destination: Path[Filesystem])
extends JovianException(str"${source.toString} and ${destination.toString} are not on the same filesystem")

case class FileNotFound(inode: Inode[Filesystem])
extends JovianException(str"the file or directory ${inode.name} was not found")

case class NotSymbolicLink(inode: Inode[Filesystem])
extends JovianException(str"the path ${inode.name} is not a symbolic link")

case class BufferOverflow()
extends JovianException(str"the stream contained more data than the buffer could hold")

case class StreamInterrupted() extends JovianException(str"the stream was interrupted")
case class NotWritable(inode: Inode[Filesystem]) extends JovianException(str"could not write to ${inode.name}")
case class NotReadable(inode: Inode[Filesystem]) extends JovianException(str"could not read from ${inode.name}")

case class FileReadError(inode: Inode[Filesystem], e: Throwable)
extends JovianException(str"could not read from ${inode.name}")

case class ZipfileEntry(name: String, inputStream: () => ji.InputStream)

object Filesystem:
  lazy val roots: Set[Filesystem] = ji.File.listRoots.to(Set).map(_.getAbsolutePath).collect {
    case "/"                                  => UnixRoot
    case s"""$drive:\""" if drive.length == 1 => drive(0).toUpper match
      case ch: Majuscule => WindowsRoot(ch)
  }.to(Set)
 
  def defaultSeparator: "/" | "\\" = if ji.File.separator == "\\" then "\\" else "/"

class Filesystem(separator: "/" | "\\", prefix: String) extends Root(separator, prefix):
  def parse(value: String): Option[Path.Absolute[this.type]] =
    if value.startsWith(prefix)
    then Some(Path.Absolute(this, value.drop(prefix.length).cut(separator.toString).to(Vector)))
    else None
  
  def /(filename: String): Path.Absolute[this.type] = Path.Absolute(this, Vector(filename))


object UnixRoot extends Filesystem("/", "")
case class WindowsRoot(drive: Majuscule) extends Filesystem("\\", str"${drive.toString}:\/")

object FsPath:
  def parse(value: String): Path[Filesystem] = Filesystem.roots.find(_.parse(value).nonEmpty).map { fs =>
    Path.Absolute(fs, value.drop(fs.prefix.length).cut(fs.separator.toString).to(Vector))
  }.getOrElse(Path.Relative(0, value.cut(Filesystem.defaultSeparator.toString).to(Vector)))