/*
    Galilei, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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
import eucalyptus.*
import turbulence.*
import galilei.*
import serpentine.*
import guillotine.*
import ambience.*
import spectacular.*
import contextual.*
import kaleidoscope.*
import gossamer.*
import symbolism.*
import anticipation.*

import scala.compiletime.*
import scala.jdk.StreamConverters.*

import java.io as ji
import java.nio as jn
import java.nio.file as jnf
import java.nio.channels as jnc

import language.experimental.captureChecking

type GeneralForbidden = Windows.Forbidden | Unix.Forbidden

object Path:
  inline given add
      (using path: CanThrow[PathError], followable: Followable[Link, GeneralForbidden, ?, ?]): Operator["+", Path, Link] with
    type Result = Path
    def apply(left: Path, right: Link): Path = left.append(right)
  
  inline given add2
      (using path: CanThrow[PathError], followable: Followable[SafeLink, GeneralForbidden, ?, ?]): Operator["+", Path, SafeLink] with
    type Result = Path
    def apply(left: Path, right: SafeLink): Path = left.append(right)
  
  given Insertion[Sh.Params, Path] = path => Sh.Params(path.fullname)
  
  given reachable: Reachable[Path, GeneralForbidden, Maybe[Windows.Drive]] with
    def root(path: Path): Maybe[Windows.Drive] = path match
      case path: Windows.SafePath => path.drive
      case path: Windows.Path     => path.drive
      case _                      => Unset
    
    def prefix(root: Maybe[Windows.Drive]): Text =
      root.mm(Windows.Path.reachable.prefix(_)).or(Unix.Path.reachable.prefix(Unset))
    
    def descent(path: Path): List[PathName[GeneralForbidden]] = (path: @unchecked) match
      case path: Unix.SafePath    => path.safeDescent
      case path: Windows.SafePath => path.safeDescent
    
    def separator(path: Path): Text = path match
      case path: Unix.SafePath    => t"/"
      case path: Unix.Path        => t"/"
      case path: Windows.SafePath => t"\\"
      case path: Windows.Path     => t"\\"
  
  given rootParser: RootParser[Path, Maybe[Windows.Drive]] = text =>
    Windows.Path.rootParser.parse(text).or(Unix.Path.rootParser.parse(text))
  
  given PathCreator[Path, GeneralForbidden, Maybe[Windows.Drive]] with
    def path(root: Maybe[Windows.Drive], descent: List[PathName[GeneralForbidden]]) = root match
      case drive@Windows.Drive(_) => Windows.SafePath(drive, descent)
      case _                      => Unix.SafePath(descent)

  given AsMessage[Path] = path => Message(path.render)

  inline given decoder(using CanThrow[PathError]): Decoder[Path] = new Decoder[Path]:
    def decode(text: Text): Path = Reachable.decode(text)

  given show: Show[Path] = _.render
  given encoder: Encoder[Path] = _.render
  given debug: Debug[Path] = _.render

sealed trait Path:
  this: Path =>
  
  def fullname: Text
  def name: Text
  def java: jnf.Path = jnf.Path.of(fullname.s).nn
  
  def exists(): Boolean = jnf.Files.exists(java)
  
  def wipe()(using deleteRecursively: DeleteRecursively)(using io: CanThrow[IoError]): Path =
    deleteRecursively.conditionally(this):
      jnf.Files.deleteIfExists(java)
    
    this
    
  def inodeType
      ()(using dereferenceSymlinks: DereferenceSymlinks)(using io: CanThrow[IoError])
      : InodeType =
    
    try jnf.Files.getAttribute(java, "unix:mode", dereferenceSymlinks.options()*) match
      case mode: Int => (mode & 61440) match
        case  4096 => InodeType.Fifo
        case  8192 => InodeType.CharDevice
        case 16384 => InodeType.Directory
        case 24576 => InodeType.BlockDevice
        case 32768 => InodeType.File
        case 40960 => InodeType.Symlink
        case 49152 => InodeType.Socket
        case _     => throw Mistake(msg"an unexpected POSIX mode value was returned")
    
    catch
      case error: UnsupportedOperationException =>
        throw Mistake(msg"the file attribute unix:mode could not be accessed")
      
      case error: ji.FileNotFoundException =>
        throw IoError(this)
      
      case error: ji.IOException =>
        throw IoError(this)

  def as[InodeType <: Inode](using resolver: PathResolver[InodeType, this.type]): InodeType =
    resolver(this)
  
  inline def is
      [InodeType <: Inode]
      (using DereferenceSymlinks, CanThrow[IoError])
      : Boolean =
    inline erasedValue[InodeType] match
      case _: Directory   => inodeType() == InodeType.Directory
      case _: File        => inodeType() == InodeType.File
      case _: Symlink     => inodeType() == InodeType.Symlink
      case _: Socket      => inodeType() == InodeType.Socket
      case _: Fifo        => inodeType() == InodeType.Fifo
      case _: BlockDevice => inodeType() == InodeType.BlockDevice
      case _: CharDevice  => inodeType() == InodeType.CharDevice
  
  def make[InodeType <: Inode]()(using maker: InodeMaker[InodeType, this.type]): InodeType =
    maker(this)

object Link:
  given creator: PathCreator[Link, GeneralForbidden, Int] with
    def path(ascent: Int, descent: List[PathName[GeneralForbidden]]): SafeLink =
      SafeLink(ascent, descent)
  
  inline given decoder(using CanThrow[PathError]): Decoder[Link] = text =>
    if text.contains(t"\\") then text.decodeAs[Windows.Link] else text.decodeAs[Unix.Link]
  
  given show: Show[Link] =
    case link: Unix.Link    => link.render
    case link: Windows.Link => link.render
    case link: SafeLink     => link.render
  
  given encoder: Encoder[Link] = show(_)
  given debug: Debug[Link] = show(_)

sealed trait Link

object Windows:
  type Forbidden = ".*[\\cA-\\cZ].*" | "(CON|PRN|AUX|NUL|COM[1-9]|LPT[1-9])(\\.*)?" |
      "\\.\\." | "\\." | ".*[:<>/\\\\|?\"*].*"

  object Path:
    inline given decoder(using CanThrow[PathError]): Decoder[Path] = new Decoder[Path]:
      def decode(text: Text): Path = Reachable.decode(text)
    
    given reachable: Reachable[Path, Forbidden, Drive] with
      def root(path: Path): Drive = path.drive
      def prefix(drive: Drive): Text = t"${drive.letter}:\\"
      def descent(path: Path): List[PathName[Forbidden]] = path.descent
      def separator(path: Path): Text = t"\\"
    
    given creator: PathCreator[Path, Forbidden, Drive] = Path(_, _)
    
    given rootParser: RootParser[Path, Drive] = text => text.only:
      case r"$letter([a-zA-Z]):\\.*" => (Drive(unsafely(letter(0)).toUpper), text.drop(3))

    given show: Show[Path] = _.render
    given encoder: Encoder[Path] = _.render
    given debug: Debug[Path] = _.render

  case class Path(drive: Drive, descent: List[PathName[Forbidden]]) extends galilei.Path:
    def root: Drive = drive
    def name: Text = if descent.isEmpty then drive.name else descent.head.show
    
    def fullname: Text =
      t"${Path.reachable.prefix(drive)}${descent.reverse.map(_.render).join(t"\\")}"

  class SafePath(drive: Drive, val safeDescent: List[PathName[GeneralForbidden]])
  extends Path(drive, safeDescent.map(_.widen[Forbidden]))
  
  object Link: 
    given creator: PathCreator[Link, Forbidden, Int] = Link(_, _)
    
    given followable: Followable[Link, Forbidden, "..", "."] with
      val separators: Set[Char] = Set('\\')
      def separator(path: Link): Text = t"\\"
      def ascent(path: Link): Int = path.ascent
      def descent(path: Link): List[PathName[Forbidden]] = path.descent
  
    inline given decoder(using CanThrow[PathError]): Decoder[Link] = Followable.decoder[Link]
    given show: Show[Link] = _.render
    given encoder: Encoder[Link] = _.render
    given debug: Debug[Link] = _.render

  case class Drive(letter: Char):
    def name: Text = t"$letter:"
    
    @targetName("child")
    def /(name: PathName[Forbidden]): Path = Path(this, List(name))
    
    @targetName("child2")
    inline def /(name: Text): Path throws PathError = Path(this, List(PathName(name)))
  
  case class Link(ascent: Int, descent: List[PathName[Forbidden]]) extends galilei.Link
  
  sealed trait Inode extends galilei.Inode

object Unix:
  
  type Forbidden = ".*\\/.*" | ".*[\\cA-\\cZ].*" | "\\.\\." | "\\."
  
  @targetName("child")
  def /(name: PathName[Forbidden]): Path = Path(List(name))

  @targetName("child2")
  inline def /(name: Text): Path throws PathError = Path(List(PathName(name)))

  object Path:
    given mainRoot: MainRoot[Path] = () => Path(Nil)
    
    inline given decoder(using CanThrow[PathError]): Decoder[Path] = new Decoder[Path]:
      def decode(text: Text): Path = Reachable.decode(text)
    
    given rootParser: RootParser[Path, Unset.type] = text =>
      if text.starts(t"/") then (Unset, text.drop(1)) else Unset
    
    given creator: PathCreator[Path, Forbidden, Unset.type] = (root, descent) => Path(descent)

    given reachable: Reachable[Path, Forbidden, Unset.type] with
      def separator(path: Path): Text = t"/"
      def root(path: Path): Unset.type = Unset
      def prefix(root: Unset.type): Text = t"/"
      def descent(path: Path): List[PathName[Forbidden]] = path.descent
    
    given show: Show[Path] = _.render
    given encoder: Encoder[Path] = _.render
    given debug: Debug[Path] = _.render
  
  case class Path(descent: List[PathName[Forbidden]]) extends galilei.Path:
    def root: Unset.type = Unset
    def name: Text = if descent.isEmpty then Path.reachable.prefix(Unset) else descent.head.show
    
    def fullname: Text =
      t"${Path.reachable.prefix(Unset)}${descent.reverse.map(_.render).join(t"/")}"
  

  class SafePath(val safeDescent: List[PathName[GeneralForbidden]])
  extends Path(safeDescent.map(_.widen[Forbidden]))

  object Link:
    given creator: PathCreator[Link, Forbidden, Int] = Link(_, _)
    
    given followable: Followable[Link, Forbidden, "..", "."] with
      val separators: Set[Char] = Set('/')
      def separator(path: Link): Text = t"/"
      def ascent(path: Link): Int = path.ascent
      def descent(path: Link): List[PathName[Forbidden]] = path.descent
    
    inline given decoder(using CanThrow[PathError]): Decoder[Link] = Followable.decoder[Link]
    given show: Show[Link] = _.render
    given encoder: Encoder[Link] = _.render
    given debug: Debug[Link] = _.render
  
  case class Link(ascent: Int, descent: List[PathName[Forbidden]]) extends galilei.Link
  
  sealed trait Inode extends galilei.Inode

case class Volume(name: Text, volumeType: Text)

sealed trait Inode:
  def path: Path
  def fullname: Text = path.fullname
  def stillExists(): Boolean = path.exists()
  def hidden(): Boolean throws IoError =
    try jnf.Files.isHidden(path.java) catch case error: ji.IOException => throw IoError(path)
  
  def readable(): Boolean = jnf.Files.isReadable(path.java)
  def writable(): Boolean = jnf.Files.isWritable(path.java)
  def executable(): Boolean = jnf.Files.isExecutable(path.java)

  def hardLinks()(using dereferenceSymlinks: DereferenceSymlinks, io: CanThrow[IoError]): Int =
    try jnf.Files.getAttribute(path.java, "unix:nlink", dereferenceSymlinks.options()*) match
      case count: Int => count
      case _          => throw IoError(path)
    catch case error: IllegalArgumentException => throw IoError(path)

  def volume: Volume =
    val fileStore = jnf.Files.getFileStore(path.java).nn
    Volume(fileStore.name.nn.tt, fileStore.`type`.nn.tt)

  def delete
      ()(using deleteRecursively: DeleteRecursively, io: CanThrow[IoError])
      : Path^{deleteRecursively, io} =
    
    try deleteRecursively.conditionally(path)(jnf.Files.delete(path.java))
    catch
      case error: jnf.NoSuchFileException        => throw IoError(path)
      case error: ji.FileNotFoundException       => throw IoError(path)
      case error: ji.IOException                 => throw IoError(path)
      case error: SecurityException              => throw IoError(path)
    
    path
  
  def symlinkTo
      (destination: Path)
      (using overwritePreexisting: OverwritePreexisting,
          createNonexistentParents: CreateNonexistentParents)
      (using io: CanThrow[IoError])
      : Path^{io, overwritePreexisting, createNonexistentParents} =
    
    createNonexistentParents(destination):
      overwritePreexisting(destination):
        jnf.Files.createSymbolicLink(destination.java, path.java)

    destination
  
  def hardLinkTo
      (destination: Path)
      (using overwritePreexisting: OverwritePreexisting,
          createNonexistentParents: CreateNonexistentParents)
      (using io: CanThrow[IoError])
      : Path^{io, overwritePreexisting, createNonexistentParents} =
    
    createNonexistentParents(destination):
      overwritePreexisting(destination):
        jnf.Files.createLink(path.java, destination.java)

    destination
  
  def copyInto
      (destination: Directory)
      (using overwritePreexisting: OverwritePreexisting, dereferenceSymlinks: DereferenceSymlinks)
      (using io: CanThrow[IoError])
      : Path^{io, overwritePreexisting, dereferenceSymlinks} =
    given CreateNonexistentParents = filesystemOptions.createNonexistentParents
    copyTo(destination / path.descent.head)

  def copyTo
      (destination: Path)
      (using overwritePreexisting: OverwritePreexisting, dereferenceSymlinks: DereferenceSymlinks,
          createNonexistentParents: CreateNonexistentParents)
      (using io: CanThrow[IoError])
      : Path^{io, overwritePreexisting, createNonexistentParents, dereferenceSymlinks} =

    createNonexistentParents(destination):
      overwritePreexisting(destination):
        jnf.Files.copy(path.java, destination.java, dereferenceSymlinks.options()*)

    destination
      
  def moveInto
      (destination: Directory)
      (using overwritePreexisting: OverwritePreexisting, moveAtomically: MoveAtomically,
          dereferenceSymlinks: DereferenceSymlinks)
      (using io: CanThrow[IoError])
      : Path^{io, overwritePreexisting, moveAtomically, dereferenceSymlinks} =
    given CreateNonexistentParents = filesystemOptions.createNonexistentParents
    moveTo(destination / path.descent.head)

  def moveTo
      (destination: Path)
      (using overwritePreexisting: OverwritePreexisting, moveAtomically: MoveAtomically,
          dereferenceSymlinks: DereferenceSymlinks,
          createNonexistentParents: CreateNonexistentParents)
      (using io: CanThrow[IoError])
      : Path^{io, overwritePreexisting, createNonexistentParents, moveAtomically,
          dereferenceSymlinks} =

    val options: Seq[jnf.CopyOption] = dereferenceSymlinks.options() ++ moveAtomically.options()

    createNonexistentParents(destination):
      overwritePreexisting(destination):
        jnf.Files.move(path.java, destination.java, options*)

    destination
      
object PathResolver:
  given inode
      (using dereferenceSymlinks: DereferenceSymlinks, io: CanThrow[IoError],
          notFound: CanThrow[NotFoundError])
      : PathResolver[Inode, Path] =
    new PathResolver[Inode, Path]:
      def apply(path: Path): Inode =
        if path.exists() then path.inodeType() match
          case InodeType.Directory => Directory(path)
          case _                   => File(path)
        else throw NotFoundError(path)

  given file
      (using createNonexistent: CreateNonexistent, dereferenceSymlinks: DereferenceSymlinks,
          io: CanThrow[IoError])
      : PathResolver[File, Path] = path =>
    if path.exists() && path.inodeType() == InodeType.File then File(path)
    else createNonexistent(path):
      jnf.Files.createFile(path.java)
    
    File(path)
  
  given directory
      (using createNonexistent: CreateNonexistent, dereferenceSymlinks: DereferenceSymlinks,
          io: CanThrow[IoError])
      : PathResolver[Directory, Path] = path =>
    if path.exists() && path.inodeType() == InodeType.Directory then Directory(path)
    else createNonexistent(path):
      jnf.Files.createDirectory(path.java)
    
    Directory(path)

@capability
trait PathResolver[InodeType <: Inode, -PathType <: Path]:
  def apply(value: PathType): InodeType

object InodeMaker:
  given directory
      (using createNonexistentParents: CreateNonexistentParents,
          overwritePreexisting: OverwritePreexisting)
      (using io: CanThrow[IoError])
      : InodeMaker[Directory, Path] = path =>
    createNonexistentParents(path):
      overwritePreexisting(path):
        jnf.Files.createDirectory(path.java)
    
    Directory(path)
  
  given socket
      (using createNonexistentParents: CreateNonexistentParents,
          overwritePreexisting: OverwritePreexisting)
      (using io: CanThrow[IoError])
      : InodeMaker[Socket, Unix.Path] = path =>
    createNonexistentParents(path):
      overwritePreexisting(path):
        val address = java.net.UnixDomainSocketAddress.of(path.java).nn
        val channel = jnc.ServerSocketChannel.open(java.net.StandardProtocolFamily.UNIX).nn
        channel.bind(address)
        Socket(path, channel)
  
  given file
      (using createNonexistentParents: CreateNonexistentParents,
          overwritePreexisting: OverwritePreexisting)
      : InodeMaker[File, Path] =
    path => createNonexistentParents(path):
      overwritePreexisting(path):
        jnf.Files.createFile(path.java)
    
    File(path)
  
  given fifo
      (using createNonexistentParents: CreateNonexistentParents,
          overwritePreexisting: OverwritePreexisting, properties: SystemProperties, systemProperty: CanThrow[SystemPropertyError], log: Log, io: CanThrow[IoError])
      : InodeMaker[Fifo, Unix.Path] =
    path => createNonexistentParents(path):
      overwritePreexisting(path):
        sh"mkfifo $path"() match
          case ExitStatus.Ok => ()
          case _             => throw IoError(path)
    
    Fifo(path)
  
@capability
trait InodeMaker[+InodeType <: Inode, -PathType <: Path]:
  def apply(value: PathType): InodeType

object Directory:
  given GenericWatchService[Directory] = () =>
    jnf.Path.of("/").nn.getFileSystem.nn.newWatchService().nn

case class Directory(path: Path) extends Unix.Inode, Windows.Inode:
  def children: LazyList[Path] = jnf.Files.list(path.java).nn.toScala(LazyList).map: child =>
    path / PathName.unsafe(child.getFileName.nn.toString.nn.tt)
    
  @targetName("child")
  def /(name: PathName[GeneralForbidden]): Path = path / name
  
  @targetName("child2")
  inline def /(name: Text): Path throws PathError = path / PathName(name)

object File:
  given readableBytes(using streamCut: CanThrow[StreamCutError], io: CanThrow[IoError]): Readable[File, Bytes] =
    Readable.inputStream.contraMap: file =>
      ji.BufferedInputStream(jnf.Files.newInputStream(file.path.java))
  
case class File(path: Path) extends Unix.Inode, Windows.Inode:
  def size(): ByteSize = jnf.Files.size(path.java).b

case class Socket(path: Unix.Path, channel: jnc.ServerSocketChannel) extends Unix.Inode
case class Fifo(path: Unix.Path) extends Unix.Inode
case class Symlink(path: Unix.Path) extends Unix.Inode
case class BlockDevice(path: Unix.Path) extends Unix.Inode
case class CharDevice(path: Unix.Path) extends Unix.Inode

enum InodeType:
  case Fifo, CharDevice, Directory, BlockDevice, File, Symlink, Socket

@capability
trait DereferenceSymlinks:
  def options(): List[jnf.LinkOption]

@capability
trait MoveAtomically:
  def options(): List[jnf.CopyOption]

@capability
trait CopyAttributes:
  def options(): List[jnf.CopyOption]

@capability
trait DeleteRecursively:
  def conditionally[ResultType](path: Path)(operation: => ResultType): ResultType

@capability
trait OverwritePreexisting:
  def apply[ResultType](path: Path)(operation: => ResultType): ResultType

@capability
trait CreateNonexistentParents:
  def apply[ResultType](path: Path)(operation: => ResultType): ResultType

@capability
trait CreateNonexistent:
  def apply(path: Path)(operation: => Unit): Unit

@capability
trait WriteSynchronously:
  def options(): List[jnf.StandardOpenOption]

package filesystemOptions:
  given dereferenceSymlinks: DereferenceSymlinks = new DereferenceSymlinks:
    def options(): List[jnf.LinkOption] = Nil
  
  given doNotDereferenceSymlinks: DereferenceSymlinks = new DereferenceSymlinks:
    def options(): List[jnf.LinkOption] = List(jnf.LinkOption.NOFOLLOW_LINKS)
  
  given moveAtomically: MoveAtomically with
    def options(): List[jnf.CopyOption] = List(jnf.StandardCopyOption.ATOMIC_MOVE)
  
  given doNotMoveAtomically: MoveAtomically with
    def options(): List[jnf.CopyOption] = Nil
  
  given copyAttributes: CopyAttributes with
    def options(): List[jnf.CopyOption] = List(jnf.StandardCopyOption.COPY_ATTRIBUTES)

  given doNotCopyAttributes: CopyAttributes with
    def options(): List[jnf.CopyOption] = Nil

  given deleteRecursively
      (using io: CanThrow[IoError], notFound: CanThrow[NotFoundError])
      : DeleteRecursively =
    new DeleteRecursively:
      def conditionally[ResultType](path: Path)(operation: => ResultType): ResultType =
        given symlinks: DereferenceSymlinks = doNotDereferenceSymlinks
        given creation: CreateNonexistent = doNotCreateNonexistent
        
        if path.is[Directory] then path.as[Directory].children.foreach(conditionally(_)(()))
        jnf.Files.delete(path.java)
        operation
          
  given doNotDeleteRecursively
      (using unemptyDirectory: CanThrow[UnemptyDirectoryError])
      : DeleteRecursively =
    new DeleteRecursively:
      def conditionally[ResultType](path: Path)(operation: => ResultType): ResultType =
        try operation
        catch case error: jnf.DirectoryNotEmptyException => throw UnemptyDirectoryError(path)
      
  given overwritePreexisting(using deleteRecursively: DeleteRecursively): OverwritePreexisting =
    new OverwritePreexisting:
      def apply[ResultType](path: Path)(operation: => ResultType): ResultType =
        deleteRecursively.conditionally(path)(operation)
      
  given doNotOverwritePreexisting(using overwrite: CanThrow[OverwriteError]): OverwritePreexisting =
    new OverwritePreexisting:
      def apply[ResultType](path: Path)(operation: => ResultType): ResultType =
        try operation catch case error: jnf.FileAlreadyExistsException => throw OverwriteError(path)
      
  given createNonexistentParents(using CanThrow[IoError]): CreateNonexistentParents =
    new CreateNonexistentParents:
      def apply[ResultType](path: Path)(operation: => ResultType): ResultType =
        path.parent.mm: parent =>
          given DereferenceSymlinks = filesystemOptions.doNotDereferenceSymlinks
         
          if !parent.exists() || !parent.is[Directory]
          then jnf.Files.createDirectories(parent.java)
      
        operation

  given doNotCreateNonexistentParents
      (using notFound: CanThrow[NotFoundError])
      : CreateNonexistentParents =
    new CreateNonexistentParents:
      def apply[ResultType](path: Path)(operation: => ResultType): ResultType =
        try operation catch case error: ji.FileNotFoundException => throw NotFoundError(path)

  given createNonexistent
      (using createNonexistentParents: CreateNonexistentParents)
      : CreateNonexistent =
    new CreateNonexistent:
      def apply(path: Path)(operation: => Unit): Unit =
        if !path.exists() then createNonexistentParents(path)(operation)

  given doNotCreateNonexistent: CreateNonexistent =
    new CreateNonexistent:
      def apply(path: Path)(operation: => Unit): Unit = ()

  given writeSynchronously: WriteSynchronously with
    def options(): List[jnf.StandardOpenOption] = List(jnf.StandardOpenOption.SYNC)

  given doNotWriteSynchronously: WriteSynchronously with
    def options(): List[jnf.StandardOpenOption] = Nil


case class IoError(path: Path) extends Error(msg"an I/O error occurred")

case class NotFoundError(path: Path)
extends Error(msg"no filesystem node was found at the path $path")

case class OverwriteError(path: Path)
extends Error(msg"cannot overwrite a pre-existing filesystem node $path")

case class UnemptyDirectoryError(path: Path)
extends Error(msg"the directory is not empty")

case class ForbiddenOperationError(path: Path)
extends Error(msg"insufficient permissions to modify $path")

case class InodeTypeError(path: Path)
extends Error(msg"the filesystem node at $path was expected to be a different type")

case class SafeLink(ascent: Int, descent: List[PathName[GeneralForbidden]]) extends Link

object SafeLink:
  given creator: PathCreator[SafeLink, GeneralForbidden, Int] = SafeLink(_, _)
  given show: Show[SafeLink] = _.render
  given encoder: Encoder[SafeLink] = _.render
  given debug: Debug[SafeLink] = _.render
  
  given followable
      (using creator: PathCreator[SafeLink, GeneralForbidden, Int])
      : Followable[SafeLink, GeneralForbidden, "..", "."] =
    new Followable[SafeLink, GeneralForbidden, "..", "."]:
      val separators: Set[Char] = Set('/', '\\')
      def separator(link: SafeLink): Text = t"/"
      def ascent(link: SafeLink): Int = link.ascent
      def descent(link: SafeLink): List[PathName[GeneralForbidden]] = link.descent

  inline given decoder(using CanThrow[PathError]): Decoder[SafeLink] =
    Followable.decoder[SafeLink]
