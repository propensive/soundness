/*
    Galilei, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package galilei

import rudiments.*
import digression.*
import gossamer.*
import turbulence.*
import serpentine.*
import eucalyptus.*
import anticipation.*

import java.io as ji
import java.nio.file as jnf

import jnf.{Files, Paths, StandardCopyOption, DirectoryNotEmptyException}, jnf.StandardCopyOption.*
import ji.{File as JavaFile}

object IoError:
  object Reason:
    given Show[Reason] =
      case NotFile              => t"the path does not refer to a file"
      case NotDirectory         => t"the path does not refer to a directory"
      case NotSymlink           => t"the path does not refer to a symbolic link"
      case DoesNotExist         => t"no node exists at this path"
      case AlreadyExists        => t"a node already exists at this path"
      case AccessDenied         => t"the operation is not permitted on this path"
      case DifferentFilesystems => t"the source and destination are on different filesystems"
      case NotSupported         => t"the filesystem does not support it"

  enum Reason:
    case NotFile, NotDirectory, NotSymlink, DoesNotExist, AlreadyExists, AccessDenied, DifferentFilesystems,
        NotSupported

  object Op:
    given Show[Op] = Showable(_).show.lower

  enum Op:
    case Read, Write, Access, Permissions, Create, Delete

case class IoError(operation: IoError.Op, reason: IoError.Reason, path: DiskPath)
extends Error(err"the $operation operation at $path failed because $reason")

enum Creation:
  case Expect, Create, Ensure

export Creation.{Expect, Create, Ensure}

extension (inodes: Seq[Inode])
  transparent inline def files: Seq[File] = inodes.collect:
    case file: File => file
  
  transparent inline def directories: Seq[Directory] = inodes.collect:
    case dir: Directory => dir

sealed trait Inode(val path: DiskPath):
  lazy val javaFile: ji.File = ji.File(fullname.s)
  lazy val javaPath: jnf.Path = javaFile.toPath.nn

  def name: Text = path.parts.lastOption.getOrElse(path.root.prefix)
  def fullname: Text = path.javaFile.getAbsolutePath.nn.show
  def uriString: Text = Showable(javaFile.toURI).show
  
  def parent: Directory throws RootParentError =
    if path.parts.isEmpty then throw RootParentError(path.root)
    else Directory(path.root.make(path.parts.init))
  
  def directory: Optional[Directory]
  def file: Optional[File]
  def symlink: Optional[Symlink]
  def modified[InstantType: GenericInstant]: InstantType = makeInstant(javaFile.lastModified)
  def exists(): Boolean = javaFile.exists()
  def delete(): Unit throws IoError

  def readable: Boolean = Files.isReadable(javaPath)
  def writable: Boolean = Files.isWritable(javaPath)
  def setPermissions(readable: Optional[Boolean] = Unset, writable: Optional[Boolean] = Unset,
                         executable: Optional[Boolean]): Unit throws IoError =
    if !readable.option.fold(true)(javaFile.setReadable(_)) |
        !writable.option.fold(true)(javaFile.setWritable(_)) |
        !executable.option.fold(true)(javaFile.setExecutable(_))
    then throw IoError(IoError.Op.Permissions, IoError.Reason.AccessDenied, path)
  
  def touch(): Unit throws IoError =
    try
      if !exists() then ji.FileOutputStream(javaFile).close()
      else javaFile.setLastModified(System.currentTimeMillis())
    catch case e => throw IoError(IoError.Op.Write, IoError.Reason.NotSupported, path)

object File:
  given Show[File] = t"ᶠ｢"+_.path.fullname+t"｣"
  
  given provider(using fs: Filesystem): (GenericFileMaker[File] & GenericFileReader[File]) =
    new GenericFileMaker[File] with GenericFileReader[File]:
      def makeFile(str: String, readOnly: Boolean = false): Option[File] =
        safely(fs.parse(Text(str)).file(Expect)).option
      
      def filePath(file: File): String = file.path.fullname.toString

  given writable
      (using io: CanThrow[IoError], streamCut: CanThrow[StreamCutError])
      : (/*{io, streamCut}*/ Writable[File, Bytes]) =
    Appendable.outputStreamBytes.asWritable.contraMap: file =>
      if !file.javaFile.canWrite()
      then throw IoError(IoError.Op.Write, IoError.Reason.AccessDenied, file.path)
      
      ji.BufferedOutputStream(ji.FileOutputStream(file.javaFile, false))

  given appendable
      (using io: CanThrow[IoError], streamCut: CanThrow[StreamCutError])
      : (/*{io, streamCut}*/ Appendable[File, Bytes]) =
    Appendable.outputStreamBytes.contraMap: file =>
      if !file.javaFile.canWrite()
      then throw IoError(IoError.Op.Write, IoError.Reason.AccessDenied, file.path)
      
      ji.BufferedOutputStream(ji.FileOutputStream(file.javaFile, true))

  given readableBytes
      (using io: CanThrow[IoError], streamCut: CanThrow[StreamCutError])
      : (/*{io, streamCut}*/ Readable[File, Bytes]) =
    Readable.inputStream.contraMap: file =>
      if !file.javaFile.canRead() then throw IoError(IoError.Op.Read, IoError.Reason.AccessDenied, file.path)
      else ji.BufferedInputStream(ji.FileInputStream(file.javaFile))
  
  given readableLine
      (using io: CanThrow[IoError], streamCut: CanThrow[StreamCutError])
      : (/*{io, streamCut}*/ Readable[File, Line]) =
    Readable.bufferedReader.contraMap: file =>
      if !file.javaFile.canRead() then throw IoError(IoError.Op.Read, IoError.Reason.AccessDenied, file.path)
      else ji.BufferedReader(ji.FileReader(file.javaFile))
    
case class File(filePath: DiskPath) extends Inode(filePath), Shown[File]:
  def directory: Unset.type = Unset
  def file: File = this
  def symlink: Unset.type = Unset
  
  def delete(): Unit throws IoError =
    try javaFile.delete()
    catch e => throw IoError(IoError.Op.Delete, IoError.Reason.AccessDenied, path)
  
  def copyTo(dest: DiskPath): File throws IoError =
    if dest.exists()
    then throw IoError(IoError.Op.Create, IoError.Reason.AlreadyExists, dest)
    
    try Files.copy(javaPath, dest.javaPath)
    catch IOException =>
      throw IoError(IoError.Op.Write, IoError.Reason.AccessDenied, dest)
    
    dest.file(Expect)

  def moveTo(dest: DiskPath): Directory throws IoError =
    try
      unsafely(dest.parent.exists())
      Files.move(javaPath, dest.javaPath, StandardCopyOption.REPLACE_EXISTING).unit
    catch
      case e: DirectoryNotEmptyException =>
        copyTo(dest)
        delete().unit
      case e =>
        throw IoError(IoError.Op.Write, IoError.Reason.AccessDenied, dest)

    dest.directory(Expect)
    
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

  def size(): ByteSize = javaFile.length.b


object Fifo:
  given Show[Fifo] = t"ˢ｢"+_.path.fullname+t"｣"
  
  given appendable[ChunkType](using io: CanThrow[IoError],
                                  appendable: /*{*}*/ Appendable[ji.OutputStream, ChunkType])
        : (/*{io, appendable}*/ Appendable[Fifo, ChunkType]) =
    appendable.contraMap: fifo =>
      if !fifo.writable()
      then throw IoError(IoError.Op.Write, IoError.Reason.AccessDenied, fifo.path)
      
      fifo.out

  given readable[ChunkType](using io: CanThrow[IoError], readable: /*{*}*/ Readable[ji.InputStream, ChunkType])
                     : (/*{io, readable}*/ Readable[Fifo, ChunkType]) =
    readable.contraMap: fifo =>
      if !fifo.readable() then throw IoError(IoError.Op.Read, IoError.Reason.AccessDenied, fifo.path)
      else fifo.in
  
    
case class Fifo(path: DiskPath) extends Shown[Fifo]:
  def writable(): Boolean = path.javaFile.canWrite()
  def readable(): Boolean = path.javaFile.canRead()
  lazy val out = ji.FileOutputStream(path.javaFile, false)
  lazy val in = ji.FileInputStream(path.javaFile)
  def close(): Unit = out.close()

object Symlink:
  given Show[Symlink] = t"ˢʸᵐ｢"+_.path.fullname+t"｣"

case class Symlink(symlinkPath: DiskPath, target: DiskPath)
extends Inode(symlinkPath), Shown[Symlink]:
  def apply(): DiskPath = target
  
  def hardLinkTo(dest: DiskPath): Symlink throws IoError =
    Files.createSymbolicLink(dest.javaPath, target.javaPath)
    dest.symlink
  
  def directory: Unset.type = Unset
  def file: Unset.type = Unset
  def symlink: Symlink = this
  
  def delete(): Unit throws IoError =
    try javaFile.delete()
    catch e => throw IoError(IoError.Op.Delete, IoError.Reason.AccessDenied, path)
  
  def copyTo(dest: DiskPath): Symlink throws IoError =
    Files.createSymbolicLink(Paths.get(dest.show.s), Paths.get(target.fullname.s))
    
    dest.symlink

object Directory:
  given Show[Directory] = t"ᵈ｢"+_.path.fullname+t"｣"
  given GenericWatchService[Directory] = () => Unix.javaFilesystem.newWatchService().nn

  given provider(using fs: Filesystem)
                : (GenericDirectoryMaker[Directory] & GenericDirectoryReader[Directory]) =
    new GenericDirectoryMaker[Directory] with GenericDirectoryReader[Directory]:
      def makeDirectory(str: String, readOnly: Boolean = false): Option[Directory] =
        safely(fs.parse(Text(str)).directory(Expect)).option
      
      def directoryPath(dir: Directory): String = dir.path.fullname.s
  
  given pathReader(using fs: Filesystem): GenericPathReader[Directory] =
    new GenericPathReader[Directory]:
      def getPath(dir: Directory): String = dir.path.fullname.s
  
  given pathMaker(using fs: Filesystem): GenericPathMaker[Directory] =
    new GenericPathMaker[Directory]:
      def makePath(str: String, readOnly: Boolean = false): Option[Directory] =
        safely(fs.parse(Text(str)).directory(Expect)).option

case class Directory(directoryPath: DiskPath)
extends Inode(directoryPath), Shown[Directory]:
  def directory: Directory = this
  def file: Unset.type = Unset
  def symlink: Unset.type = Unset
  
  def tmpPath(suffix: Optional[Text] = Unset): DiskPath =
    val part = unsafely(PathElement(t"${Uuid().show}${suffix.or(t"")}"))
    path.root.make(path.parts :+ part.value)
  
  def tmpFile(suffix: Optional[Text] = Unset): File throws IoError =
    val file = tmpPath(suffix).file(Create)
    file.javaFile.deleteOnExit()
    file

  @targetName("child")
  infix def /(element: Text): DiskPath throws PathError =
    path / PathElement(element)
  
  @targetName("safeChild")
  infix def /(element: PathElement): DiskPath = path / element


  def delete(): Unit throws IoError =
    def recur(file: JavaFile): Boolean =
      if Files.isSymbolicLink(file.toPath) then file.delete()
      else if file.isDirectory
      then file.listFiles.nn.map(_.nn).forall(recur(_)) && file.delete()
      else file.delete()

    try recur(javaFile).unit
    catch e => throw IoError(IoError.Op.Delete, IoError.Reason.AccessDenied, path)

  def children: List[Inode] throws IoError =
    Option(javaFile.list).fold(Nil): files =>
      files.nn.immutable(using Unsafe).to(List).map(_.nn.show).map(path.parts :+ _).map: parts =>
        path.root.make(parts)
      .map(_.inode)
  
  def descendants: LazyList[Inode] throws IoError =
    children.to(LazyList) ++ subdirectories.flatMap(_.descendants)
  
  def subdirectories: List[Directory] throws IoError =
    children.collect:
      case dir: Directory => dir
  
  def deepSubdirectories: LazyList[Directory] throws IoError =
    val subdirs = subdirectories.to(LazyList).filter(!_.name.starts(t"."))
    subdirs #::: subdirs.flatMap(_.deepSubdirectories)

  def files: List[File] throws IoError = children.collect:
    case file: File => file

  def copyTo(dest: DiskPath): Directory throws IoError =
    if dest.exists()
    then throw IoError(IoError.Op.Write, IoError.Reason.AlreadyExists, dest)
    
    try Files.copy(javaPath, Paths.get(dest.show.s))
    catch e => throw IoError(IoError.Op.Write, IoError.Reason.AccessDenied, path)
    
    dest.directory(Expect)

  def renameTo(name: Text): Directory throws IoError =
    val dest = unsafely(parent / name)
    if javaFile.renameTo(dest.javaFile) then dest.directory(Expect)
    else throw IoError(IoError.Op.Write, IoError.Reason.AlreadyExists, dest)
  
  def moveTo(dest: DiskPath): Directory throws IoError =
    try
      unsafely(dest.parent.exists())
      Files.move(javaPath, dest.javaPath, StandardCopyOption.REPLACE_EXISTING).unit
    catch
      case e: DirectoryNotEmptyException =>
        copyTo(dest)
        delete()
      case e =>
        throw IoError(IoError.Op.Write, IoError.Reason.AccessDenied, dest)
  
    dest.directory(Expect)
  
  //def size(): ByteSize throws IoError = path.descendantFiles().map(_.size().long).sum.b

object DiskPath:
  given Show[DiskPath] = t"ᵖ｢"+_.fullname+t"｣"

  // given (using filesystem: Filesystem, invalidPath: CanThrow[PathError])
  //       : (/*{invalidPath}*/ Canonical[DiskPath]) =
  //   Canonical(filesystem.parse(_), _.show)

  given provider(using fs: Filesystem): (GenericPathMaker[DiskPath] & GenericPathReader[DiskPath]) =
    new GenericPathMaker[DiskPath] with GenericPathReader[DiskPath]:
      def makePath(str: String, readOnly: Boolean = false): Option[DiskPath] =
        safely(fs.parse(Text(str))).option
    
      def getPath(path: DiskPath): String = path.fullname.s

case class DiskPath(filesystem: Filesystem, elements: List[Text])
extends Absolute(elements), Shown[DiskPath]:
  type RootType = Filesystem
  val root: Filesystem = filesystem
  lazy val javaFile: ji.File = ji.File(fullname.s)
  lazy val javaPath: jnf.Path = javaFile.toPath.nn
  
  def exists(): Boolean = javaFile.exists()
  def isFile: Boolean = exists() && !javaFile.isDirectory
  def isDirectory: Boolean = exists() && javaFile.isDirectory
  def isSymlink: Boolean = exists() && Files.isSymbolicLink(javaFile.toPath)
  def name: Text = elements.last
  def fullname: Text = elements.join(root.prefix, root.separator, t"")
  def length: Int = elements.length
  def rename(fn: Text => Text): DiskPath = DiskPath(root, elements.init :+ fn(name))

  // @targetName("add")
  // infix def +(relative: Relative): DiskPath throws RootParentError =
  //   if relative.ascent == 0 then fs.make(elements ++ relative.parts)
  //   else parent + relative.copy(ascent = relative.ascent - 1)

  def fifo(creation: Creation = Creation.Ensure): Fifo throws IoError =
    import IoError.*

    val existant: Boolean = exists()

    creation match
      case Create if existant  => throw IoError(Op.Create, Reason.AlreadyExists, this)
      case Expect if !existant => throw IoError(Op.Access, Reason.DoesNotExist, this)
      case _                   => ()

    val fifo = Fifo(this)
    
    try
      if !parent.exists() && !parent.javaFile.mkdirs()
      then throw IoError(Op.Create, Reason.AccessDenied, this)
    catch case err: RootParentError => throw IoError(Op.Create, Reason.AccessDenied, this)
    
    if !exists()
    then Runtime.getRuntime.nn.exec(Array[String | Null]("sh", "-c", t"mkfifo $fullname".s)).nn.waitFor()
    
    if !isFile then throw IoError(IoError.Op.Access, IoError.Reason.NotFile, this)
    
    fifo

  def file(): Optional[File] = if exists() && isFile then unsafely(file(Expect)) else Unset

  def file(creation: Creation): File throws IoError =
    import IoError.*
    
    val existant: Boolean = exists()
    creation match
      case Create if existant  => throw IoError(Op.Create, Reason.AlreadyExists, this)
      case Expect if !existant => throw IoError(Op.Access, Reason.DoesNotExist, this)
      case _                   => ()

    val file: File = File(this)
    
    try
      if !parent.exists() && !parent.javaFile.mkdirs()
      then throw IoError(Op.Create, Reason.AccessDenied, this)
    catch case err: RootParentError => throw IoError(Op.Create, Reason.AccessDenied, this)
    
    if !exists() then file.touch()
    if !isFile then throw IoError(IoError.Op.Access, IoError.Reason.NotFile, this)
    
    file

  def directory(creation: Creation): Directory throws IoError =
    import IoError.*
    val existant: Boolean = exists()
    creation match
      case Create if existant =>
        throw IoError(Op.Create, Reason.AlreadyExists, this)
      
      case Expect if !existant =>
        throw IoError(Op.Access, Reason.DoesNotExist, this)
      
      case Ensure if !existant =>
        if !javaFile.mkdirs() then throw IoError(Op.Create, Reason.AccessDenied, this)
      
      case _ =>
        ()
    
    Directory(this)
    
    if !exists() && creation == Expect
    then throw IoError(IoError.Op.Access, IoError.Reason.DoesNotExist, this)
    
    if !isDirectory then throw IoError(IoError.Op.Access, IoError.Reason.NotDirectory, this)
    
    Directory(this)
  
  def directory(): Optional[Directory] = if exists() && isDirectory then unsafely(directory(Expect)) else Unset

  def symlink: Symlink throws IoError =
    if !javaFile.exists()
    then throw IoError(IoError.Op.Access, IoError.Reason.DoesNotExist, this)
    
    if !Files.isSymbolicLink(javaFile.toPath)
    then throw IoError(IoError.Op.Access, IoError.Reason.NotSymlink, this)
    
    Symlink(this, unsafely(root.parse(Showable(Files.readSymbolicLink(Paths.get(fullname.s))).show)))

  def descendantFiles(descend: (Directory => Boolean) = _ => true)
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
      try Symlink(this, unsafely(root.parse(Showable(Files.readSymbolicLink(javaPath)).show)))
      catch NoSuchElementException => File(this)
    else File(this)

object Filesystem:

  given Show[Filesystem] = fs => t"ᶠˢ｢${fs.name}:${fs.prefix}...${fs.separator}｣"

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

  def parse(value: Text, pwd: Optional[DiskPath] = Unset): DiskPath throws PathError =
    roots.flatMap: fs =>
      safely(fs.parse(value)).option
    .headOption.getOrElse:
      val rel = Relative.parse(value)
      pwd.option.flatMap: path =>
        try Some(path + rel) catch case err: RootParentError => None
      .getOrElse:
        throw PathError(value)

abstract class Filesystem(val name: Text, fsPrefix: Text, fsSeparator: Text)
extends Root(fsPrefix, fsSeparator), Shown[Filesystem]:
  type PathType = DiskPath
  
  def root: Directory = Directory(DiskPath(this, Nil))
  lazy val javaFilesystem: jnf.FileSystem = root.javaPath.getFileSystem.nn

  def make(parts: List[Text]): DiskPath = DiskPath(this, parts.filter(_ != t""))

  def parse(value: Text, pwd: Optional[DiskPath] = Unset)
           : DiskPath throws PathError =
    if value.starts(prefix) then make(List(value.drop(prefix.length).cut(separator)*))
    else try pwd.option.map(_ + Relative.parse(value)).getOrElse(throw PathError(value))
    catch case err: RootParentError => throw PathError(value)

object Unix extends Filesystem(t"unix", t"/", t"/")

type Unix = Unix.type

case class WindowsRoot(drive: Majuscule) extends Filesystem(t"win", t"\\", t"${drive}:\\")

package filesystems:
  given unix: Unix = Unix

type Majuscule = 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' | 'L' | 'M' | 'N' |
    'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W' | 'X' | 'Y' | 'Z'

object windows:
  object DriveC extends WindowsRoot('C')
  object DriveD extends WindowsRoot('D')
  object DriveE extends WindowsRoot('E')
  object DriveF extends WindowsRoot('F')

given realm: Realm = Realm(t"galilei")

object Classpath:
  given Show[Classpath] = cp =>
    def recur(cp: ClassLoader): Text =
      val start = Option(cp.getParent).map(_.nn).map(recur(_)).getOrElse(t"")
      t"$start/${Option(cp.getName).fold(t"-")(_.nn.show)}"
    
    recur(cp.classLoader)

open class Classpath(val classLoader: ClassLoader = getClass.nn.getClassLoader.nn)
extends Root(t"/", t""), Shown[Classpath]:
  type PathType = ClasspathRef
  protected inline def classpath: this.type = this
  def make(parts: List[Text]): ClasspathRef = ClasspathRef(this, parts)

object ClasspathRef:
  given Show[ClasspathRef] = t"ᶜᵖ｢"+_.fullname+t"｣"

  given readable(using classpathRef: CanThrow[ClasspathRefError], streamCut: CanThrow[StreamCutError])
                : (/*{classpathRef, streamCut}*/ Readable[ClasspathRef, Bytes]) =
    Readable.inputStream.contraMap: ref =>
      ref.classpath.classLoader.getResourceAsStream(ref.fullname.drop(1).s) match
        case null => throw ClasspathRefError(ref)
        case in   => in.nn

case class ClasspathRef(classpath: Classpath, elements: List[Text])
extends Absolute(elements), Shown[ClasspathRef]:
  type RootType = Classpath
  val root: Classpath = classpath
  def fullname = parts.join(t"/", t"/", t"")

case class ClasspathRefError(path: ClasspathRef)
extends Error(err"the resource $path is not on the claspath")
