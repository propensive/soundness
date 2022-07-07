package joviality

import rudiments.*
import gossamer.*
import turbulence.*
import serpentine.*
import eucalyptus.*
import anticipation.*

import java.io as ji
import java.nio.file as jnf

import jnf.{FileSystems, FileVisitResult, Files, Paths, SimpleFileVisitor, StandardCopyOption,
    DirectoryNotEmptyException, Path as JavaPath}, jnf.StandardCopyOption.*,
    jnf.attribute.BasicFileAttributes

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
    case NotFile, NotDirectory, NotSymlink, DoesNotExist, AlreadyExists, AccessDenied, DifferentFilesystems, NotSupported

  object Op:
    given Show[Op] = Showable(_).show.lower

  enum Op:
    case Read, Write, Access, Permissions, Create, Delete

case class IoError(operation: IoError.Op, reason: IoError.Reason, path: DiskPath[Filesystem])
extends Error((t"the ", operation, t" operation at ", path, t" failed because ", reason))

case class InotifyError()
extends Error(t"the limit on the number of paths that can be watched has been exceeded" *:
    EmptyTuple)

enum Creation:
  case Expect, Create, Ensure

export Creation.{Expect, Create, Ensure}

sealed trait Inode[+Fs <: Filesystem](val path: DiskPath[Fs]):
  type PathType = path.root.PathType
  lazy val javaFile: ji.File = ji.File(fullname.s)
  lazy val javaPath: jnf.Path = javaFile.toPath.nn

  def name: Text = path.parts.lastOption.getOrElse(path.root.prefix)
  def fullname: Text = path.javaFile.getAbsolutePath.nn.show
  def uriString: Text = Showable(javaFile.toURI).show
  
  def parent: Directory[Fs] throws RootParentError =
    if path.parts.isEmpty then throw RootParentError(path.root)
    else Directory(path.root.make(path.parts.init))
  
  def directory: Maybe[Directory[Fs]]
  def file: Maybe[File[Fs]]
  def symlink: Maybe[Symlink[Fs]]
  def modified(using time: anticipation.Timekeeper): time.Type = time.from(javaFile.lastModified)
  def exists(): Boolean = javaFile.exists()

  def readable: Boolean = Files.isReadable(javaPath)
  def writable: Boolean = Files.isWritable(javaPath)
  def setPermissions(readable: Maybe[Boolean] = Unset, writable: Maybe[Boolean] = Unset,
                         executable: Maybe[Boolean]): Unit throws IoError =
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
  given Show[File[?]] = _.path.fullname
  
  given provider[Fs <: Filesystem: ValueOf]: FileProvider[File[Fs]]
      with FileInterpreter[File[Filesystem]] with
    def make(str: String, readOnly: Boolean = false): Option[File[Fs]] =
      safely(summon[ValueOf[Fs]].value.parse(Text(str)).file(Expect)).option
    
    def path(file: File[Filesystem]): String = file.path.fullname.toString

  given [Fs <: Filesystem]: Sink[File[Fs]] with
    type E = IoError
    def write(value: File[Fs], stream: DataStream): Unit throws E | StreamCutError =
      val out = ji.FileOutputStream(value.javaFile, false)
      try Util.write(stream, out)
      catch case e => throw IoError(IoError.Op.Write, IoError.Reason.AccessDenied, value.path)
      finally try out.close() catch _ => ()
  
  given [Fs <: Filesystem]: Source[File[Fs]] with
    type E = IoError
    def read(file: File[Fs]): DataStream throws E =
      try Util.readInputStream(ji.FileInputStream(file.javaFile), 64.kb)
      catch case e: ji.FileNotFoundException =>
        if e.getMessage.nn.contains("(Permission denied)")
        then throw IoError(IoError.Op.Read, IoError.Reason.AccessDenied, file.path)
        else throw IoError(IoError.Op.Read, IoError.Reason.DoesNotExist, file.path)

case class File[+Fs <: Filesystem](override val path: DiskPath[Fs]) extends Inode[Fs](path):
  def directory: Unset.type = Unset
  def file: File[Fs] = this
  def symlink: Unset.type = Unset
  
  def delete(): Unit throws IoError =
    try javaFile.delete()
    catch e => throw IoError(IoError.Op.Delete, IoError.Reason.AccessDenied, path)
  
  def read[T](limit: ByteSize = 64.kb)(using readable: Readable[T])
      : T throws readable.E | IoError | StreamCutError =
    val stream = Util.readInputStream(ji.FileInputStream(javaFile), limit)
    try readable.read(stream) catch
      case err: java.io.FileNotFoundException =>
        throw IoError(IoError.Op.Read, IoError.Reason.AccessDenied, path)

  def copyTo[Fs2 >: Fs <: Filesystem](dest: DiskPath[Fs2]): File[Fs2] throws IoError =
    if dest.exists()
    then throw IoError(IoError.Op.Create, IoError.Reason.AlreadyExists, dest)
    
    try Files.copy(javaPath, dest.javaPath)
    catch IOException =>
      throw IoError(IoError.Op.Write, IoError.Reason.AccessDenied, dest)
    
    dest.file(Expect)

  def moveTo[Fs2 >: Fs <: Filesystem](dest: DiskPath[Fs2]): Directory[Fs2] throws IoError =
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
  
  def hardLinkTo[Fs2 >: Fs <: Filesystem](dest: DiskPath[Fs2]): File[Fs2] throws IoError =
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


object Fifo:
  given [Fs <: Unix]: Sink[Fifo[Fs]] with
    type E = IoError
    def write(value: Fifo[Fs], stream: DataStream): Unit throws E | StreamCutError =
      try Util.write(stream, value.out)
      catch case e => throw IoError(IoError.Op.Write, IoError.Reason.AccessDenied, value.file.path)

case class Fifo[+Fs <: Unix](file: File[Fs]):
  val out = ji.FileOutputStream(file.javaFile, false)
  def close(): Unit = out.close()

object Symlink:
  given Show[Symlink[?]] = _.path.fullname

case class Symlink[+Fs <: Filesystem](override val path: DiskPath[Fs], target: DiskPath[Fs])
extends Inode[Fs](path):
  def apply(): DiskPath[Fs] = target
  
  def hardLinkTo(dest: DiskPath[path.root.type]): Symlink[path.root.type] throws IoError =
    Files.createSymbolicLink(dest.javaPath, target.javaPath)
    dest.symlink
  
  def directory: Unset.type = Unset
  def file: Unset.type = Unset
  def symlink: Symlink[Fs] = this
  
  def delete(): Unit throws IoError =
    try javaFile.delete()
    catch e => throw IoError(IoError.Op.Delete, IoError.Reason.AccessDenied, path)
  
  def copyTo(dest: DiskPath[path.root.type]): Symlink[path.root.type] throws IoError =
    Files.createSymbolicLink(Paths.get(dest.show.s), Paths.get(target.fullname.s))
    
    dest.symlink

object Directory:
  given Show[Directory[?]] = _.path.fullname
    
  given provider(using fs: Filesystem): DirectoryProvider[Directory[fs.type]]
      with DirectoryInterpreter[Directory[fs.type]] with
    def make(str: String, readOnly: Boolean = false): Option[Directory[fs.type]] =
      safely(fs.parse(Text(str)).directory(Expect)).option
    
    def path(dir: Directory[fs.type]): String = dir.path.fullname.s

case class Directory[+Fs <: Filesystem](override val path: DiskPath[Fs]) extends Inode[Fs](path):
  def directory: Directory[Fs] = this
  def file: Unset.type = Unset
  def symlink: Unset.type = Unset
  
  def tmpPath(suffix: Maybe[Text] = Unset): DiskPath[Fs] =
    val part = unsafely(PathElement(t"${Uuid().show}${suffix.otherwise(t"")}"))
    path.root.make(path.parts :+ part.value)
  
  def tmpFile(suffix: Maybe[Text] = Unset): File[Fs] throws IoError =
    val file = tmpPath(suffix).file(Create)
    file.javaFile.deleteOnExit()
    file

  @targetName("child")
  infix def /(element: Text): DiskPath[Fs] throws InvalidPathError =
    path / PathElement(element)
  
  @targetName("safeChild")
  infix def /(element: PathElement): DiskPath[Fs] = path / element

  def delete(): Unit throws IoError =
    def recur(file: JavaFile): Boolean =
      if Files.isSymbolicLink(file.toPath) then file.delete()
      else if file.isDirectory
      then file.listFiles.nn.map(_.nn).forall(recur(_)) && file.delete()
      else file.delete()

    try recur(javaFile).unit
    catch e => throw IoError(IoError.Op.Delete, IoError.Reason.AccessDenied, path)

  def children: List[Inode[Fs]] throws IoError =
    Option(javaFile.list).fold(Nil): files =>
      files.nn.immutable(using Unsafe).to(List).map(_.nn.show).map(path.parts :+ _).map: parts =>
        path.root.make(parts)
      .map(_.inode)
  
  def descendants: LazyList[Inode[Fs]] throws IoError =
    children.to(LazyList) ++ subdirectories.flatMap(_.descendants)
  
  def subdirectories: List[Directory[Fs]] throws IoError =
    children.collect:
      case dir: Directory[Fs] => dir
  
  def deepSubdirectories: LazyList[Directory[Fs]] throws IoError =
    val subdirs = subdirectories.to(LazyList).filter(!_.name.startsWith(t"."))
    subdirs #::: subdirs.flatMap(_.deepSubdirectories)

  def files: List[File[Fs]] throws IoError = children.collect:
    case file: File[Fs] => file

  def copyTo[Fs2 >: Fs <: Filesystem](dest: DiskPath[Fs2]): Directory[Fs2] throws IoError =
    if dest.exists()
    then throw IoError(IoError.Op.Write, IoError.Reason.AlreadyExists, dest)
    
    try Files.copy(javaPath, Paths.get(dest.show.s))
    catch e => throw IoError(IoError.Op.Write, IoError.Reason.AccessDenied, path)
    
    dest.directory(Expect)
  
  def moveTo[Fs2 >: Fs <: Filesystem](dest: DiskPath[Fs2]): Directory[Fs2] throws IoError =
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

object DiskPath:
  given Show[DiskPath[?]] = _.fullname
  
  given provider[Fs <: Filesystem](using fs: Fs): PathProvider[DiskPath[Fs]]
      with PathInterpreter[DiskPath[Fs]] with
    def make(str: String, readOnly: Boolean = false): Option[DiskPath[Fs]] =
      safely(fs.parse(Text(str))).option
  
    def path(path: DiskPath[Fs]): String = path.fullname.s

case class DiskPath[+Fs <: Filesystem](filesystem: Fs, elements: List[Text])
extends Absolute[Fs](filesystem, elements):
  lazy val javaFile: ji.File = ji.File(fullname.s)
  lazy val javaPath: jnf.Path = javaFile.toPath.nn
  
  def exists(): Boolean = javaFile.exists()
  def isFile: Boolean = exists() && !javaFile.isDirectory
  def isDirectory: Boolean = exists() && javaFile.isDirectory
  def isSymlink: Boolean = exists() && Files.isSymbolicLink(javaFile.toPath)
  def name: Text = elements.last
  def fullname: Text = elements.join(root.prefix, root.separator, t"")
  def length: Int = elements.length
  def rename(fn: Text => Text): DiskPath[Fs] = DiskPath(root, elements.init :+ fn(name))

  // @targetName("add")
  // infix def +(relative: Relative): DiskPath[Fs] throws RootParentError =
  //   if relative.ascent == 0 then fs.make(elements ++ relative.parts)
  //   else parent + relative.copy(ascent = relative.ascent - 1)

  def file(creation: Creation = Creation.Ensure): File[Fs] throws IoError = root.synchronized:
    import IoError.*
    
    creation match
      case Create if exists()  => throw IoError(Op.Create, Reason.AlreadyExists, this)
      case Expect if !exists() => throw IoError(Op.Access, Reason.DoesNotExist, this)
      case Ensure if !exists() => ()
      case _                   => ()

    val file = File(this)
    
    try
      if !parent.exists() && !parent.javaFile.mkdirs()
      then throw IoError(Op.Create, Reason.AccessDenied, this)
    catch case err: RootParentError => throw IoError(Op.Create, Reason.AccessDenied, this)
    
    if !exists() then file.touch()
    if !isFile then throw IoError(IoError.Op.Access, IoError.Reason.NotFile, this)
    
    file

  def directory(creation: Creation = Ensure): Directory[Fs] throws IoError = root.synchronized:
    import IoError.*
    creation match
      case Create if exists() =>
        throw IoError(Op.Create, Reason.AlreadyExists, this)
      
      case Expect if !exists() =>
        throw IoError(Op.Access, Reason.DoesNotExist, this)
      
      case Ensure if !exists() =>
        if !javaFile.mkdirs() then throw IoError(Op.Create, Reason.AccessDenied, this)
      
      case _ =>
        ()
    
    Directory(this)
    
    if !exists() && creation == Expect
    then throw IoError(IoError.Op.Access, IoError.Reason.DoesNotExist, this)
    
    if !isDirectory then throw IoError(IoError.Op.Access, IoError.Reason.NotDirectory, this)
    
    Directory(this)

  def symlink: Symlink[Fs] throws IoError =
    if !javaFile.exists()
    then throw IoError(IoError.Op.Access, IoError.Reason.DoesNotExist, this)
    
    if !Files.isSymbolicLink(javaFile.toPath)
    then throw IoError(IoError.Op.Access, IoError.Reason.NotSymlink, this)
    
    Symlink(this, unsafely(root.parse(Showable(Files.readSymbolicLink(Paths.get(fullname.s))).show)))

  def descendantFiles(descend: (Directory[Fs] => Boolean) = _ => true)
                      : LazyList[File[Fs]] throws IoError =
    if javaFile.isDirectory
    then directory(Expect).files.to(LazyList) #::: directory(Expect).subdirectories.filter(
        descend).to(LazyList).flatMap(_.path.descendantFiles(descend))
    else LazyList(file(Expect))

  def inode: Inode[Fs] throws IoError =
    
    if !javaFile.exists()
    then throw IoError(IoError.Op.Access, IoError.Reason.DoesNotExist, this)
    
    if isDirectory then Directory(this)
    else if isFile
    then
      try Symlink(this, unsafely(root.parse(Showable(Files.readSymbolicLink(javaPath)).show)))
      catch NoSuchElementException => File(this)
    else File(this)

type Majuscule = 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' | 'L' | 'M' | 'N' |
    'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W' | 'X' | 'Y' | 'Z'

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

  def parse(value: Text, pwd: Maybe[DiskPath[Filesystem]] = Unset)
           : DiskPath[Filesystem] throws InvalidPathError =
    roots.flatMap: fs =>
      safely(fs.parse(value)).option
    .headOption.getOrElse:
      val rel = Relative.parse(value)
      pwd.option.flatMap: path =>
        try Some(path + rel) catch case err: RootParentError => None
      .getOrElse:
        throw InvalidPathError(value)

abstract class Filesystem(fsPrefix: Text, fsSeparator: Text) extends Root(fsPrefix, fsSeparator):
  type PathType = DiskPath[this.type]
  
  def root: Directory[this.type] = Directory(DiskPath(this, Nil))
  lazy val javaFilesystem: jnf.FileSystem = root.javaPath.getFileSystem.nn

  def make(parts: List[Text]): DiskPath[this.type] = DiskPath(this, parts)

  def parse(value: Text, pwd: Maybe[DiskPath[this.type]] = Unset)
           : DiskPath[this.type] throws InvalidPathError =
    if value.startsWith(prefix) then make(List(value.drop(prefix.length).cut(separator)*))
    else try pwd.option.map(_ + Relative.parse(value)).getOrElse(throw InvalidPathError(value))
    catch case err: RootParentError => throw InvalidPathError(value)

object Unix extends Filesystem(t"/", t"/"):
  type PathType = DiskPath[Unix]

type Unix = Unix.type

case class WindowsRoot(drive: Majuscule) extends Filesystem(t"\\", t"${drive}:\\")

package filesystems:
  given unix: Unix.type = Unix

object windows:
  object DriveC extends WindowsRoot('C')
  object DriveD extends WindowsRoot('D')
  object DriveE extends WindowsRoot('E')
  object DriveF extends WindowsRoot('F')

given realm: Realm = Realm(t"joviality")

case class PwdError()
extends Error(t"the current working directory cannot be determined" *: EmptyTuple)

open class Classpath(val classLoader: ClassLoader = getClass.nn.getClassLoader.nn)
extends Root(t"/", t""):
  type PathType = ClasspathRef[this.type]
  protected inline def classpath: this.type = this
  def make(parts: List[Text]): ClasspathRef[this.type] = ClasspathRef[this.type](this, parts)

object ClasspathRef:
  given Show[ClasspathRef[Classpath]] = _.parts.join(t"classpath:", t"/", t"")

case class ClasspathRef[+Cp <: Classpath](classpath: Cp, elements: List[Text])
extends Absolute[Cp](classpath, elements):
  def resource: ClasspathResource = ClasspathResource(classpath.make(parts))
  
case class ClasspathResource(path: ClasspathRef[Classpath]):
  def read[T](limit: ByteSize = 64.kb)(using readable: Readable[T])
          : T throws ClasspathRefError | StreamCutError | readable.E =
    val resource = path.root.classLoader.getResourceAsStream(path.show.drop(10).s)
    if resource == null then throw ClasspathRefError(path.classpath)(path)
    val stream = Util.readInputStream(resource.nn, limit)
    readable.read(stream)
  
  def name: Text = path.parts.lastOption.getOrElse(path.classpath.prefix)

case class ClasspathRefError(classpath: Classpath)(path: ClasspathRef[Classpath])
extends Error((t"the resource ", path, t" could not be accessed on the classpath"))