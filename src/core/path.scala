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

import scala.util.*
import scala.collection.generic.CanBuildFrom
import java.net.URI
import java.nio.file.attribute.BasicFileAttributes

import java.nio.file.{FileSystems, FileVisitResult, Files, Paths, SimpleFileVisitor, StandardCopyOption,
    DirectoryNotEmptyException, Path as JavaPath}

import java.nio.file.StandardCopyOption.*
import java.io.{ Closeable, InputStream, File => JavaFile }

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
      createDirectories(targetPath.resolve(sourcePath.relativize(dir)))
      FileVisitResult.CONTINUE

    override def visitFile(file: JavaPath, attrs: BasicFileAttributes): FileVisitResult =
      Files.copy(file, targetPath.resolve(sourcePath.relativize(file)), REPLACE_EXISTING)
      FileVisitResult.CONTINUE

  def createDirectories(path: JavaPath): Boolean = path.toFile.mkdirs()

case class Path(input: String):
  val value: String = if input == "/" then "/" else
    def canonicalize(str: List[String], drop: Int = 0): List[String] = str match
      case ".." :: tail => canonicalize(tail, drop + 1)
      case head :: tail => if drop > 0 then canonicalize(tail, drop - 1) else head :: canonicalize(tail)
      case Nil          => List.fill(drop)("..")
    
    canonicalize((input.split("/").to(List) match
      case "" :: xs => "" :: xs.filter { p => p != "." && p != "" }
      case xs       => xs.filter { p => p != "." && p != "" }
    ).reverse).reverse match
      case Nil => "."
      case xs  => xs.mkString("/")

  def filename: String = value
  lazy val javaPath: JavaPath = Paths.get(value)
  lazy val javaFile: JavaFile = javaPath.toFile
  def uriString: String = javaFile.toURI.toString
  def name: String = javaPath.getFileName.toString
  def /(child: String): Path = if filename == "/" then Path(s"/$child") else Path(s"$filename/$child")
  def in(root: Path): Path = if value == "." then root else Path(s"${root.value}/$value")

  def lastModified: Long = javaFile.lastModified()

  def empty: Boolean =
    val filesStream = Files.walk(javaPath)
    try filesStream.allMatch(p => Files.isDirectory(p))
    finally filesStream.close()

  def size: ByteSize = ByteSize(javaFile.length)

  def setReadOnly(): Try[Unit] = childPaths.traverse(_.setReadOnly()).flatMap { _ =>
    Try(javaFile.setReadOnly())
  }

  def setWritable(): Try[Unit] = Try(javaFile.setWritable(true)).flatMap { _ =>
    childPaths.traverse(_.setWritable()).map { _ => () }
  }

  def uniquify(): Path =
    if !exists() then this else LazyList.from(2).map { i => this.rename(_+"-"+i) }.find(!_.exists()).get

  def hardLink(path: Path): Try[Unit] =
    Try(Files.createLink(javaPath, path.javaPath)).map { _ => () }.recoverWith {
      case ex: java.nio.file.NoSuchFileException => copyTo(path).map { _ => () }
    }

  def touch(): Try[Unit] = Try {
    if !exists() then java.io.FileOutputStream(javaFile).close()
    else
      javaFile.setLastModified(System.currentTimeMillis())
      ()
  }.recoverWith { case e => Failure(FileWriteError(this, e)) }

  def extant(): Path =
    mkdir()
    this

  def directory: Boolean = Files.isDirectory(javaPath)

  def extantParents(): Path =
    parent.mkdir()
    this

  def isExecutable: Boolean = Files.isExecutable(javaPath)

  def setExecutable(exec: Boolean): Try[Unit] = Try(javaFile.setExecutable(exec)).flatMap {
    case true =>
      Success(())
    case false =>
      Failure(new IllegalStateException(
          s"Could not ${if (exec) "set" else "unset"} execution permission for ${value}"))
  }

  def resolve(rel: Path) = Path(javaPath.resolve(rel.javaPath))

  def moveTo(path: Path): Try[Unit] = Try {
    path.parent.extant()
    Files.move(javaPath, path.javaPath, StandardCopyOption.REPLACE_EXISTING)
    ()
  }.recoverWith {
    case e: DirectoryNotEmptyException => copyTo(path).map { _ => delete(); () }
    case e                             => Failure(FileWriteError(this, e))
  }

  def relativeSubdirsContaining(pred: String => Boolean): Set[Path] =
    findSubdirsContaining(pred).map { p => Path(p.value.drop(value.length + 1)) }

  def findChildren(pred: String => Boolean): Set[Path] = {
    def search(dir: JavaFile): Set[JavaFile] = {
      val files = dir.listFiles.to(Set)
      files.filter(_.isDirectory).flatMap(search(_)) ++ files.filter { f => !f.isDirectory && pred(f.getName) }
    }

    search(javaFile).map(Path(_))
  }

  def findSubdirsContaining(pred: String => Boolean): Set[Path] =
    Option(javaFile.listFiles).map { files =>
      val found = if files.exists { f => pred(f.getName) } then Set(this) else Set()
      val subdirs = files.filter(_.isDirectory).filterNot(_.getName.startsWith(".")).map(Path(_)).to(Set)

      subdirs.flatMap(_.findSubdirsContaining(pred)) ++ found
    }.getOrElse(Set())

  def delete(): Try[Boolean] = {
    def delete(file: JavaFile): Boolean =
      if Files.isSymbolicLink(file.toPath) then file.delete()
      else if(file.isDirectory) file.listFiles.forall(delete(_)) && file.delete()
      else file.delete()

    Try(delete(javaFile)).recoverWith { case e => Failure(FileWriteError(this, e)) }
  }

  def linkTarget(): Option[Path] =
    if Files.isSymbolicLink(javaPath) then Some(Path(javaPath.toRealPath())) else None

  def unlink(): Try[Unit] =
    val result = if Files.isSymbolicLink(javaPath) then Try(Files.delete(javaPath))
    else Failure(new IllegalArgumentException(s"Not a symbolic link: $name"))

    result.recoverWith { case e => Failure(FileWriteError(this, e)) }

  def writeSync(content: String, append: Boolean = false): Try[Unit] = {
    tryWith(new java.io.BufferedWriter(new java.io.FileWriter(javaPath.toFile, append))) { writer =>
      writer.write(content)
    }.recoverWith { case e => Failure(FileWriteError(this, e)) }
  }

  def lines(): Try[Iterator[String]] = Try(scala.io.Source.fromFile(javaFile).getLines())
  def bytes(): Try[IArray[Byte]] = Try(IArray.from(Files.readAllBytes(javaPath)))

  def copyTo(path: Path): Try[Path] = Try {
    Files.walkFileTree(javaPath, new Path.CopyFileVisitor(javaPath, path.javaPath))
    path
  }

  def hardLinkTo(path: Path): Try[Path] = Try {
    Files.createLink(path.javaPath, javaPath)
    path
  }

  def hardLinks(): Try[Int] = Try(Files.getAttribute(javaPath, "unix:nlink")).collect { case i: Integer => i }

  def walkTree: LazyList[Path] =
    if directory then LazyList(this) ++: childPaths.to(LazyList).flatMap(_.walkTree) else LazyList(this)

  def children: List[String] = if exists() then Option(javaFile.listFiles).to(List).flatten.map(_.getName) else Nil
  def childPaths: List[Path] = children.map(this / _)
  def descendants: LazyList[Path] = childPaths.to(LazyList).flatMap { f => f +: f.descendants }
  def exists(): Boolean = Files.exists(javaPath)
  def ifExists(): Option[Path] = if exists() then Some(this) else None
  def absolutePath(): Try[Path] = Try(this.javaPath.toAbsolutePath.normalize.toString).map(Path(_))
  def mkdir(): Try[Unit] = Try(Path.createDirectories(javaPath))
  def relativizeTo(dir: Path) = Path(dir.javaPath.relativize(this.javaPath))
  def parent = Path(javaPath.getParent.toString)
  def rename(fn: String => String): Path = parent / fn(name)
  
  def mkParents(): Try[Path] =
    Try {
      Path.createDirectories(parent.javaPath)
      this
    }.recoverWith { case e: Exception => Failure(FileWriteError(parent, e)) }

  def symlinkTo(target: Path): Try[Path] = Try {
    Files.createSymbolicLink(target.javaPath, javaPath)
    this
  }.recover { case e: java.io.IOException => this }

  //TODO consider wrapping into a buffered stream
  def inputStream(): InputStream = Files.newInputStream(javaPath)

  private def tryWith[R <: Closeable, T](resource: => R)(f: R => T): Try[T] =
    val res = Try { resource }
    val result = res.flatMap{ r => Try(f(r)) }
    val closed = res.flatMap{ r => Try(r.close()) }
    List(res, closed, result).sequence.map(_ => result.get)

object Glob:
  val All = Glob("**")

case class Glob(pattern: String):
  def apply[Coll[T] <: Iterable[T]]
           (dir: Path, xs: Coll[Path])
           (implicit cbf: CanBuildFrom[Coll[Path], Path, Coll[Path]]): Coll[Path] =
    val javaGlob = FileSystems.getDefault.getPathMatcher(s"glob:$dir/$pattern")
    val b = cbf()
    xs.foreach { x => if javaGlob.matches(x.javaPath) then b += x }
    b.result

case class ByteSize(bytes: Long):
  def +(that: ByteSize): ByteSize = ByteSize(bytes + that.bytes)

case class FileNotFound(path: Path) extends Exception

case class FileWriteError(path: Path, e: Throwable) extends Exception:
  override def getCause: Throwable = e

case class ConfigFormatError(path: Path) extends Exception
case class ZipfileEntry(name: String, inputStream: () => java.io.InputStream)
