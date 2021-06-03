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
import rudiments.*

import scala.util.*
import scala.collection.generic.CanBuildFrom
import java.net.URI

import java.nio.file as jnf
import java.io as ji

import jnf.{FileSystems, FileVisitResult, Files, Paths, SimpleFileVisitor, StandardCopyOption,
    DirectoryNotEmptyException, Path as JavaPath}, jnf.StandardCopyOption.*, jnf.attribute.BasicFileAttributes

import ji.{Closeable, InputStream, File as JavaFile}

case class UnchangeablePermissions(path: Path) extends
    Exception("the permissions of the file or directory cannot be changed")

enum Recursion:
  case Recursive, Nonrecursive

opaque type Path = String

object Path:
  def apply(jpath: JavaPath): Path = Path(jpath.toString match
    case ""    => "."
    case other => other
  )
  
  def apply(file: JavaFile): Path = Path(file.getAbsolutePath)
  def apply(uri: URI): Path = Path(Paths.get(uri))

  def unapply(str: String): Option[Path] = str match
    case r"""${dir: String}@([^*?:;,&|"\%<>]*)""" => Some(Path(dir))
    case _                              => None

  private class CopyFileVisitor(sourcePath: JavaPath, targetPath: JavaPath) extends SimpleFileVisitor[JavaPath]:

    override def preVisitDirectory(dir: JavaPath, attrs: BasicFileAttributes): FileVisitResult =
      targetPath.resolve(sourcePath.relativize(dir)).toFile.mkdirs()
      FileVisitResult.CONTINUE

    override def visitFile(file: JavaPath, attrs: BasicFileAttributes): FileVisitResult =
      Files.copy(file, targetPath.resolve(sourcePath.relativize(file)), REPLACE_EXISTING)
      FileVisitResult.CONTINUE


  def apply(input: String): Path =
    if input == "/" then "/"
    else
      def canonicalize(str: List[String], drop: Int = 0): List[String] = str match
        case ".." :: tail => canonicalize(tail, drop + 1)
        case head :: tail => if drop > 0 then canonicalize(tail, drop - 1) else head :: canonicalize(tail)
        case Nil          => List.fill(drop)("..")
      
      canonicalize((input.cut("/").to(List) match
        case "" :: xs => "" :: xs.filter { p => p != "." && p != "" }
        case xs       => xs.filter { p => p != "." && p != "" }
      ).reverse).reverse match
        case Nil => "."
        case xs  => xs.mkString("/")

  extension (path: Path)
    def filename: String = path
    def javaPath: JavaPath = Paths.get(path)
    def javaFile: JavaFile = javaPath.toFile
    def uriString: String = javaFile.toURI.toString
    def name: String = javaPath.getFileName.toString
    def /(child: String): Path = if filename == "/" then Path(s"/$child") else Path(s"$filename/$child")
    def in(root: Path): Path = if path == "." then root else Path(s"$root/$path")
    def lastModified: Long = javaFile.lastModified()

    def empty: Boolean =
      val filesStream = Files.walk(javaPath)
      try filesStream.allMatch(p => Files.isDirectory(p))
      finally filesStream.close()

    def size: ByteSize = ByteSize(javaFile.length)

    def setReadOnly(recursion: Recursion): Unit =
      if !javaFile.setWritable(false) then throw UnchangeablePermissions(path)
      if recursion == Recursion.Recursive then children.foreach(_.setWritable(recursion))

    def setWritable(recursion: Recursion): Unit =
      if !javaFile.setWritable(true) then throw UnchangeablePermissions(path)
      if recursion == Recursion.Recursive then children.foreach(_.setWritable(recursion))

    def uniquify(): Path =
      if !exists() then path else LazyList.from(2).map { i => rename(_+"-"+i) }.find(!_.exists()).get

    def hardLink(dest: Path): Unit =
      if dest.exists() then throw PathAlreadyExists(dest)
      try Files.createLink(javaPath, dest.javaPath).unit
      catch case ex: java.nio.file.NoSuchFileException => copyTo(dest).unit

    def touch(): Unit =
      try
        if !exists() then ji.FileOutputStream(javaFile).close()
        else javaFile.setLastModified(System.currentTimeMillis()).unit
      catch case e => FileWriteError(path, e)

    def extant(): Path =
      mkdir()
      path

    def directory: Boolean = Files.isDirectory(javaPath)

    def extantParents(): Path =
      parent.mkdir()
      path

    def executable: Boolean = Files.isExecutable(javaPath)
    def readable: Boolean = Files.isReadable(javaPath)
    def writable: Boolean = Files.isWritable(javaPath)

    def setExecutable(exec: Boolean): Unit =
      try javaFile.setExecutable(exec).unit catch e => throw UnchangeablePermissions(path)

    def resolve(rel: Path): Path = Path(javaPath.resolve(rel.javaPath))

    def moveTo(dest: Path): Unit =
      try
        path.parent.extant()
        Files.move(javaPath, dest.javaPath, StandardCopyOption.REPLACE_EXISTING).unit
      catch
        case e: DirectoryNotEmptyException =>
          copyTo(dest)
          delete().unit
        case e =>
          throw FileWriteError(dest, e)

    def relativeSubdirsContaining(pred: String => Boolean): Set[Path] =
      findSubdirsContaining(pred).map { p => Path(p.filename.drop(path.length + 1)) }

    def findChildren(pred: String => Boolean): Set[Path] =
      def search(dir: JavaFile): Set[JavaFile] =
        val files = dir.listFiles.to(Set)
        files.filter(_.isDirectory).flatMap(search(_)) ++ files.filter { f => !f.isDirectory && pred(f.getName) }

      search(javaFile).map(Path(_))

    def findSubdirsContaining(pred: String => Boolean): Set[Path] =
      Option(javaFile.listFiles).map { files =>
        val found = if files.exists { f => pred(f.getName) } then Set(path) else Set()
        val subdirs = files.filter(_.isDirectory).filterNot(_.getName.startsWith(".")).map(Path(_)).to(Set)

        subdirs.flatMap(_.findSubdirsContaining(pred)) ++ found
      }.getOrElse(Set())

    def delete(): Unit =
      def delete(file: JavaFile): Boolean =
        if Files.isSymbolicLink(file.toPath) then file.delete()
        else if file.isDirectory then file.listFiles.forall(delete(_)) && file.delete()
        else file.delete()

      try delete(javaFile).unit catch e => throw FileWriteError(path, e)

    def linkTarget(): Option[Path] =
      if Files.isSymbolicLink(javaPath) then Some(Path(javaPath.toRealPath())) else None

    def unlink(): Unit =
      try if Files.isSymbolicLink(javaPath) then Files.delete(javaPath) else throw NotSymbolicLink(path)
      catch e => throw FileWriteError(path, e)

    def writeSync(content: String, append: Boolean = false): Unit =
      val writer = ji.BufferedWriter(ji.FileWriter(javaPath.toFile, append))
      try writer.write(content) catch case e => throw FileWriteError(path, e)
      finally try writer.close() catch _ => ()

    def lines(): Iterator[String] =
      try scala.io.Source.fromFile(javaFile).getLines() catch e => throw FileReadError(path, e)
    
    def bytes(): IArray[Byte] =
      try IArray.from(Files.readAllBytes(javaPath)) catch e => throw FileReadError(path, e)

    def copyTo(dest: Path): Path =
      if dest.exists() then throw PathAlreadyExists(dest)
      try
        Files.walkFileTree(javaPath, Path.CopyFileVisitor(javaPath, dest.javaPath))
        dest
      catch e => throw FileWriteError(path, e)

    def hardLinkTo(dest: Path): Unit =
      try Files.createLink(dest.javaPath, javaPath) catch e => throw FileWriteError(path, e)

    def hardLinkCount(): Int =
      try Files.getAttribute(javaPath, "unix:nlink") match
        case i: Int => i
      catch e => throw FileReadError(path, e)

    def walkTree: LazyList[Path] =
      if directory then LazyList(path) ++: children.to(LazyList).flatMap(_.walkTree) else LazyList(path)

    def children: IArray[Path] =
      if exists() then
        val files = javaFile.listFiles
        if files == null then IArray[Path]() else IArray.from(files.map(_.getAbsolutePath))
      else IArray[Path]()
    
    def descendants: LazyList[Path] = children.to(LazyList).flatMap { f => f +: f.descendants }
    def exists(): Boolean = Files.exists(javaPath)
    def absolutePath(): Path = Path(javaPath.toAbsolutePath.normalize.toString)
    def mkdir(): Unit = javaPath.toFile.mkdirs()
    def relativizeTo(dir: Path): Path = Path(dir.javaPath.relativize(path.javaPath))
    def parent: Path = javaPath.getParent.toString
    def rename(fn: String => String): Path = parent / fn(name)
    
    def symlinkTo(target: Path): Unit =
      try Files.createSymbolicLink(target.javaPath, javaPath)
      catch case e: ji.IOException => throw FileWriteError(path, e)

    //TODO consider wrapping into a buffered stream
    def inputStream(): InputStream = Files.newInputStream(javaPath)

case class ByteSize(bytes: Long):
  def +(that: ByteSize): ByteSize = ByteSize(bytes + that.bytes)

case class FileNotFound(path: Path) extends Exception(str"the file or directory ${path.filename} was not found")
case class PathAlreadyExists(path: Path) extends Exception(str"the path ${path.filename} already exists")
case class NotSymbolicLink(path: Path) extends Exception(str"the path ${path.filename} is not a symbolic link")

case class FileWriteError(path: Path, e: Throwable) extends Exception(str"could not write to ${path.filename}"):
  override def getCause: Throwable = e

case class FileReadError(path: Path, e: Throwable) extends Exception(str"could not read from ${path.filename}"):
  override def getCause: Throwable = e

case class ZipfileEntry(name: String, inputStream: () => ji.InputStream)
