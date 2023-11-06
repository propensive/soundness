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
import fulminate.*
import eucalyptus.*
import turbulence.*
import perforate.*
import galilei.*
import serpentine.*
import guillotine.*
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
      (using path: Raises[PathError], followable: Followable[Link, GeneralForbidden, ?, ?]): Operator["+", Path, Link] with
    type Result = Path
    def apply(left: Path, right: Link): Path = left.append(right)
  
  inline given add2
      (using path: Raises[PathError], followable: Followable[SafeLink, GeneralForbidden, ?, ?]): Operator["+", Path, SafeLink] with
    type Result = Path
    def apply(left: Path, right: SafeLink): Path = left.append(right)
  
  given Insertion[Sh.Params, Path] = path => Sh.Params(path.fullname)
  
  given writableBytes
      (using io: Raises[IoError], streamCut: Raises[StreamCutError])
      : Writable[Path, Bytes] =
    Writable.outputStreamBytes.contraMap: path =>
      if !path.java.toFile.nn.canWrite then abort(IoError(path))
      ji.BufferedOutputStream(ji.FileOutputStream(path.java.toFile, false))

  given reachable: Reachable[Path, GeneralForbidden, Maybe[Windows.Drive]] with
    def root(path: Path): Maybe[Windows.Drive] = path match
      case path: Windows.SafePath => path.drive
      case path: Windows.Path     => path.drive
      case _                      => Unset
    
    def prefix(root: Maybe[Windows.Drive]): Text =
      root.mm(Windows.Path.reachable.prefix(_)).or(Unix.Path.reachable.prefix(Unset))
    
    def descent(path: Path): List[PathName[GeneralForbidden]] =
      // FIXME: This is a bit of a hack
      import errorHandlers.throwUnsafely
      path match
        case path: Unix.SafePath    => path.safeDescent
        case path: Windows.SafePath => path.safeDescent
        case path: Unix.Path        => path.descent.map(_.narrow[GeneralForbidden])
        case path: Windows.Path     => path.descent.map(_.narrow[GeneralForbidden])
    
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

  given Communicable[Path] = path => Message(path.render)

  inline given decoder(using Raises[PathError]): Decoder[Path] = new Decoder[Path]:
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
  
  def touch()(using Raises[IoError]): Unit = jnf.Files.write(java, Array[Byte]())
  
  def wipe()(using deleteRecursively: DeleteRecursively)(using io: Raises[IoError]): Path =
    deleteRecursively.conditionally(this):
      jnf.Files.deleteIfExists(java)
    
    this
    
  def entryType
      ()(using dereferenceSymlinks: DereferenceSymlinks)(using io: Raises[IoError])
      : PathStatus =
    
    try (jnf.Files.getAttribute(java, "unix:mode", dereferenceSymlinks.options()*): @unchecked) match
      case mode: Int => (mode & 61440) match
        case  4096 => PathStatus.Fifo
        case  8192 => PathStatus.CharDevice
        case 16384 => PathStatus.Directory
        case 24576 => PathStatus.BlockDevice
        case 32768 => PathStatus.File
        case 40960 => PathStatus.Symlink
        case 49152 => PathStatus.Socket
        case _     => throw Mistake(msg"an unexpected POSIX mode value was returned")
    
    catch
      case error: UnsupportedOperationException =>
        throw Mistake(msg"the file attribute unix:mode could not be accessed")
      
      case error: ji.FileNotFoundException =>
        raise(IoError(this))(PathStatus.File)
      
      case error: ji.IOException =>
        raise(IoError(this))(PathStatus.File)

  def as[EntryType <: Entry](using resolver: PathResolver[EntryType, this.type]): EntryType =
    resolver(this)
  
  inline def is
      [EntryType <: Entry]
      (using DereferenceSymlinks, Raises[IoError])
      : Boolean =
    inline erasedValue[EntryType] match
      case _: Directory   => entryType() == PathStatus.Directory
      case _: File        => entryType() == PathStatus.File
      case _: Symlink     => entryType() == PathStatus.Symlink
      case _: Socket      => entryType() == PathStatus.Socket
      case _: Fifo        => entryType() == PathStatus.Fifo
      case _: BlockDevice => entryType() == PathStatus.BlockDevice
      case _: CharDevice  => entryType() == PathStatus.CharDevice
  
  def make[EntryType <: Entry]()(using maker: EntryMaker[EntryType, this.type]): EntryType =
    maker(this)

object Link:
  given creator: PathCreator[Link, GeneralForbidden, Int] with
    def path(ascent: Int, descent: List[PathName[GeneralForbidden]]): SafeLink =
      SafeLink(ascent, descent)
  
  inline given decoder(using Raises[PathError]): Decoder[Link] = new Decoder[Link]:
    def decode(text: Text): Link =
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
    inline given decoder(using Raises[PathError]): Decoder[Path] = new Decoder[Path]:
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

  class SafePath(initDrive: Drive, val safeDescent: List[PathName[GeneralForbidden]])
  extends Path(initDrive, safeDescent.map(_.widen[Forbidden]))
  
  object Link: 
    given creator: PathCreator[Link, Forbidden, Int] = Link(_, _)
    
    given followable: Followable[Link, Forbidden, "..", "."] with
      val separators: Set[Char] = Set('\\')
      def separator(path: Link): Text = t"\\"
      def ascent(path: Link): Int = path.ascent
      def descent(path: Link): List[PathName[Forbidden]] = path.descent
  
    inline given decoder(using Raises[PathError]): Decoder[Link] = Followable.decoder[Link]
    given show: Show[Link] = _.render
    given encoder: Encoder[Link] = _.render
    given debug: Debug[Link] = _.render

  object Drive:
    given Default[Drive](Drive('C'))

  case class Drive(letter: Char):
    def name: Text = t"$letter:"
    
    @targetName("child")
    def /(name: PathName[Forbidden]): Path = Path(this, List(name))
    
    @targetName("child2")
    inline def /(name: Text)(using Raises[PathError]): Path = Path(this, List(PathName(name)))
  
  case class Link(ascent: Int, descent: List[PathName[Forbidden]]) extends galilei.Link
  
  sealed trait Entry extends galilei.Entry

object Unix:
  
  type Forbidden = ".*\\/.*" | ".*[\\cA-\\cZ].*" | "\\.\\." | "\\."
  
  @targetName("child")
  def /(name: PathName[Forbidden]): Path = Path(List(name))

  @targetName("child2")
  inline def /(name: Text)(using Raises[PathError]): Path = Path(List(PathName(name)))

  object Path:
    given mainRoot: MainRoot[Path] = () => Path(Nil)
    
    inline given decoder(using Raises[PathError]): Decoder[Path] = new Decoder[Path]:
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
    
    inline given decoder(using Raises[PathError]): Decoder[Link] = Followable.decoder[Link]
    given show: Show[Link] = _.render
    given encoder: Encoder[Link] = _.render
    given debug: Debug[Link] = _.render
  
  case class Link(ascent: Int, descent: List[PathName[Forbidden]]) extends galilei.Link
  
  sealed trait Entry extends galilei.Entry

case class Volume(name: Text, volumeType: Text)

sealed trait Entry:
  def path: Path
  def fullname: Text = path.fullname
  def stillExists(): Boolean = path.exists()
  def hidden()(using Raises[IoError]): Boolean =
    try jnf.Files.isHidden(path.java) catch case error: ji.IOException => raise(IoError(path))(false)
  
  def readable(): Boolean = jnf.Files.isReadable(path.java)
  def writable(): Boolean = jnf.Files.isWritable(path.java)
  def executable(): Boolean = jnf.Files.isExecutable(path.java)

  def hardLinks()(using dereferenceSymlinks: DereferenceSymlinks, io: Raises[IoError]): Int =
    try jnf.Files.getAttribute(path.java, "unix:nlink", dereferenceSymlinks.options()*) match
      case count: Int => count
      case _          => raise(IoError(path))(1)
    catch case error: IllegalArgumentException => raise(IoError(path))(1)

  def volume: Volume =
    val fileStore = jnf.Files.getFileStore(path.java).nn
    Volume(fileStore.name.nn.tt, fileStore.`type`.nn.tt)

  def delete
      ()(using deleteRecursively: DeleteRecursively, io: Raises[IoError])
      : Path^{deleteRecursively, io} =
    
    try deleteRecursively.conditionally(path)(jnf.Files.delete(path.java))
    catch
      case error: jnf.NoSuchFileException        => raise(IoError(path))(())
      case error: ji.FileNotFoundException       => raise(IoError(path))(())
      case error: ji.IOException                 => raise(IoError(path))(())
      case error: SecurityException              => raise(IoError(path))(())
    
    path
  
  def symlinkTo
      (destination: Path)
      (using overwritePreexisting: OverwritePreexisting,
          createNonexistentParents: CreateNonexistentParents)
      (using io: Raises[IoError])
      : Path^{io, overwritePreexisting, createNonexistentParents} =
    
    createNonexistentParents(destination):
      overwritePreexisting(destination):
        jnf.Files.createSymbolicLink(destination.java, path.java)

    destination
  
  def copyInto
      (destination: Directory)
      (using overwritePreexisting: OverwritePreexisting, dereferenceSymlinks: DereferenceSymlinks)
      (using io: Raises[IoError])
      : Path^{io, overwritePreexisting, dereferenceSymlinks} =
    given CreateNonexistentParents = filesystemOptions.createNonexistentParents
    copyTo(destination / path.descent.head)

  def copyTo
      (destination: Path)
      (using overwritePreexisting: OverwritePreexisting, dereferenceSymlinks: DereferenceSymlinks,
          createNonexistentParents: CreateNonexistentParents)
      (using io: Raises[IoError])
      : Path^{io, overwritePreexisting, createNonexistentParents, dereferenceSymlinks} =

    createNonexistentParents(destination):
      overwritePreexisting(destination):
        jnf.Files.copy(path.java, destination.java, dereferenceSymlinks.options()*)

    destination
      
  def moveInto
      (destination: Directory)
      (using overwritePreexisting: OverwritePreexisting, moveAtomically: MoveAtomically,
          dereferenceSymlinks: DereferenceSymlinks)
      (using io: Raises[IoError])
      : Path^{io, overwritePreexisting, moveAtomically, dereferenceSymlinks} =
    given CreateNonexistentParents = filesystemOptions.createNonexistentParents
    moveTo(destination / path.descent.head)

  def moveTo
      (destination: Path)
      (using overwritePreexisting: OverwritePreexisting, moveAtomically: MoveAtomically,
          dereferenceSymlinks: DereferenceSymlinks,
          createNonexistentParents: CreateNonexistentParents)
      (using io: Raises[IoError])
      : Path^{io, overwritePreexisting, createNonexistentParents, moveAtomically,
          dereferenceSymlinks} =

    val options: Seq[jnf.CopyOption] = dereferenceSymlinks.options() ++ moveAtomically.options()

    createNonexistentParents(destination):
      overwritePreexisting(destination):
        jnf.Files.move(path.java, destination.java, options*)

    destination
      
object PathResolver:
  // given entry
  //     (using dereferenceSymlinks: DereferenceSymlinks, io: Raises[IoError], notFound: Raises[NotFoundError])
  //     : PathResolver[Entry, Path] =
  //   new PathResolver[Entry, Path]:
  //     def apply(path: Path): Entry =
  //       if path.exists() then path.entryType() match
  //         case PathStatus.Directory => Directory(path)
  //         case _                    => File(path)
  //       else raise(NotFoundError(path))(Directory(path))

  given file
      (using createNonexistent: CreateNonexistent, dereferenceSymlinks: DereferenceSymlinks,
          io: Raises[IoError])
      : PathResolver[File, Path] = path =>
    if path.exists() && path.entryType() == PathStatus.File then File(path)
    else createNonexistent(path):
      jnf.Files.createFile(path.java)
    
    File(path)
  
  given directory
      (using createNonexistent: CreateNonexistent, dereferenceSymlinks: DereferenceSymlinks,
          io: Raises[IoError])
      : PathResolver[Directory, Path] = path =>
    if path.exists() && path.entryType() == PathStatus.Directory then Directory(path)
    else createNonexistent(path):
      jnf.Files.createDirectory(path.java)
    
    Directory(path)

@capability
trait PathResolver[sealed +EntryType <: Entry, -PathType <: Path]:
  def apply(value: PathType): EntryType

object EntryMaker:
  given directory
      (using createNonexistentParents: CreateNonexistentParents,
          overwritePreexisting: OverwritePreexisting)
      (using io: Raises[IoError])
      : EntryMaker[Directory, Path] = path =>
    createNonexistentParents(path):
      overwritePreexisting(path):
        jnf.Files.createDirectory(path.java)
    
    Directory(path)
  
  given socket
      (using createNonexistentParents: CreateNonexistentParents,
          overwritePreexisting: OverwritePreexisting)
      (using io: Raises[IoError])
      : EntryMaker[Socket, Unix.Path] = path =>
    createNonexistentParents(path):
      overwritePreexisting(path):
        val address = java.net.UnixDomainSocketAddress.of(path.java).nn
        val channel = jnc.ServerSocketChannel.open(java.net.StandardProtocolFamily.UNIX).nn
        channel.bind(address)
        Socket(path, channel)
  
  given file
      (using createNonexistentParents: CreateNonexistentParents,
          overwritePreexisting: OverwritePreexisting)
      : EntryMaker[File, Path] =
    path => createNonexistentParents(path):
      overwritePreexisting(path):
        jnf.Files.createFile(path.java)
    
    File(path)
  
  given fifo
      (using createNonexistentParents: CreateNonexistentParents,
          overwritePreexisting: OverwritePreexisting, working: WorkingDirectory, log: Log, io: Raises[IoError], exec: Raises[ExecError])
      : EntryMaker[Fifo, Unix.Path] =
    path => createNonexistentParents(path):
      overwritePreexisting(path):
        sh"mkfifo $path"() match
          case ExitStatus.Ok => ()
          case _             => raise(IoError(path))(())
    
    Fifo(path)
  
@capability
trait EntryMaker[+EntryType <: Entry, -PathType <: Path]:
  def apply(value: PathType): EntryType

object Directory:
  given GenericWatchService[Directory] = () =>
    jnf.Path.of("/").nn.getFileSystem.nn.newWatchService().nn

case class Directory(path: Path) extends Unix.Entry, Windows.Entry:
  def children: LazyList[Path] = jnf.Files.list(path.java).nn.toScala(LazyList).map: child =>
    path / PathName.unsafe(child.getFileName.nn.toString.nn.tt)

  def descendants(using DereferenceSymlinks, Raises[IoError], PathResolver[Directory, Path]): LazyList[Path] =
    children #::: children.filter(_.is[Directory]).map(_.as[Directory]).flatMap(_.descendants)
    
  @targetName("child")
  def /(name: PathName[GeneralForbidden]): Path = path / name
  
  @targetName("child2")
  inline def /(name: Text)(using Raises[PathError]): Path = path / PathName(name)

object File:
  given readableBytes(using streamCut: Raises[StreamCutError], io: Raises[IoError]): Readable[File, Bytes] =
    Readable.inputStream.contraMap: file =>
      ji.BufferedInputStream(jnf.Files.newInputStream(file.path.java))
  
  given writableBytes
      (using io: Raises[IoError], streamCut: Raises[StreamCutError])
      : Writable[File, Bytes] =
    Appendable.outputStreamBytes.asWritable.contraMap: file =>
      if !file.writable() then abort(IoError(file.path))
      ji.BufferedOutputStream(ji.FileOutputStream(file.path.java.toFile, false))

  given appendableBytes
      (using io: Raises[IoError], streamCut: Raises[StreamCutError])
      : Appendable[File, Bytes] =
    Appendable.outputStreamBytes.contraMap: file =>
      if !file.writable() then abort(IoError(file.path))
      ji.BufferedOutputStream(ji.FileOutputStream(file.path.java.toFile, true))

case class File(path: Path) extends Unix.Entry, Windows.Entry:
  def size(): ByteSize = jnf.Files.size(path.java).b
  
  def hardLinkTo
      (destination: Path)
      (using overwritePreexisting: OverwritePreexisting,
          createNonexistentParents: CreateNonexistentParents)
      (using io: Raises[IoError])
      : Path^{io, overwritePreexisting, createNonexistentParents} =
    
    createNonexistentParents(destination):
      overwritePreexisting(destination):
        jnf.Files.createLink(destination.java, path.java)

    destination

case class Socket(path: Unix.Path, channel: jnc.ServerSocketChannel) extends Unix.Entry
case class Fifo(path: Unix.Path) extends Unix.Entry
case class Symlink(path: Unix.Path) extends Unix.Entry
case class BlockDevice(path: Unix.Path) extends Unix.Entry
case class CharDevice(path: Unix.Path) extends Unix.Entry

enum PathStatus:
  case Fifo, CharDevice, Directory, BlockDevice, File, Symlink, Socket

object DereferenceSymlinks:
  given default(using Quickstart): DereferenceSymlinks = filesystemOptions.dereferenceSymlinks

@capability
trait DereferenceSymlinks:
  def options(): List[jnf.LinkOption]

object MoveAtomically:
  given default(using Quickstart): MoveAtomically = filesystemOptions.doNotMoveAtomically

@capability
trait MoveAtomically:
  def options(): List[jnf.CopyOption]

object CopyAttributes:
  given default(using Quickstart): CopyAttributes = filesystemOptions.doNotCopyAttributes

@capability
trait CopyAttributes:
  def options(): List[jnf.CopyOption]

object DeleteRecursively:
  given default(using Quickstart, Raises[UnemptyDirectoryError]): DeleteRecursively =
    filesystemOptions.doNotDeleteRecursively

@capability
trait DeleteRecursively:
  def conditionally[sealed ResultType](path: Path)(operation: => ResultType): ResultType

object OverwritePreexisting:
  given default(using Quickstart, Raises[OverwriteError]): OverwritePreexisting =
    filesystemOptions.doNotOverwritePreexisting

@capability
trait OverwritePreexisting:
  def apply[sealed ResultType](path: Path)(operation: => ResultType): ResultType

object CreateNonexistentParents:
  given default(using Quickstart, Raises[IoError]): CreateNonexistentParents =
    filesystemOptions.createNonexistentParents

@capability
trait CreateNonexistentParents:
  def apply[sealed ResultType](path: Path)(operation: => ResultType): ResultType

object CreateNonexistent:
  given default(using Quickstart, Raises[IoError]): CreateNonexistent =
    filesystemOptions.createNonexistent(using filesystemOptions.createNonexistentParents)

@capability
trait CreateNonexistent:
  def apply(path: Path)(operation: => Unit): Unit

object WriteSynchronously:
  given default(using Quickstart): WriteSynchronously = filesystemOptions.writeSynchronously

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
      (using io: Raises[IoError], notFound: Raises[NotFoundError])
      : DeleteRecursively =
    new DeleteRecursively:
      def conditionally[sealed ResultType](path: Path)(operation: => ResultType): ResultType =
        given symlinks: DereferenceSymlinks = doNotDereferenceSymlinks
        given creation: CreateNonexistent = doNotCreateNonexistent
        
        if path.exists() then
          if path.is[Directory] then path.as[Directory].children.foreach(conditionally(_)(()))
          jnf.Files.delete(path.java)
        
        operation
          
  given doNotDeleteRecursively
      (using unemptyDirectory: Raises[UnemptyDirectoryError])
      : DeleteRecursively =
    new DeleteRecursively:
      def conditionally[sealed ResultType](path: Path)(operation: => ResultType): ResultType =
        try operation
        catch case error: jnf.DirectoryNotEmptyException => abort(UnemptyDirectoryError(path))
      
  given overwritePreexisting(using deleteRecursively: DeleteRecursively): OverwritePreexisting =
    new OverwritePreexisting:
      def apply[sealed ResultType](path: Path)(operation: => ResultType): ResultType =
        deleteRecursively.conditionally(path)(operation)
      
  given doNotOverwritePreexisting(using overwrite: Raises[OverwriteError]): OverwritePreexisting =
    new OverwritePreexisting:
      def apply[sealed ResultType](path: Path)(operation: => ResultType): ResultType =
        try operation catch case error: jnf.FileAlreadyExistsException => abort(OverwriteError(path))
      
  given createNonexistentParents(using Raises[IoError]): CreateNonexistentParents =
    new CreateNonexistentParents:
      def apply[sealed ResultType](path: Path)(operation: => ResultType): ResultType =
        path.parent.mm: parent =>
          given DereferenceSymlinks = filesystemOptions.doNotDereferenceSymlinks
         
          if !parent.exists() || !parent.is[Directory]
          then jnf.Files.createDirectories(parent.java)
      
        operation

  given doNotCreateNonexistentParents
      (using notFound: Raises[NotFoundError])
      : CreateNonexistentParents =
    new CreateNonexistentParents:
      def apply[sealed ResultType](path: Path)(operation: => ResultType): ResultType =
        try operation catch case error: ji.FileNotFoundException => abort(NotFoundError(path))

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

case class PathStatusError(path: Path)
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

  inline given decoder(using Raises[PathError]): Decoder[SafeLink] =
    Followable.decoder[SafeLink]
