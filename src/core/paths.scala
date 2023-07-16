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
import serpentine.*
import spectacular.*
import kaleidoscope.*
import gossamer.*

import scala.compiletime.*

import java.io as ji
import java.nio as jn
import java.nio.file as jnf

import language.experimental.captureChecking

object Path:
  type Forbidden = Windows.Forbidden | Unix.Forbidden
  
  given reachable: Reachable[Path, Forbidden, Maybe[Windows.Drive]] with
    def root(path: Path): Maybe[Windows.Drive] = path match
      case path: Unix.SafePath    => Unset
      case path: Windows.SafePath => path.drive
    
    def prefix(root: Maybe[Windows.Drive]): Text =
      root.mm(Windows.Path.reachable.prefix(_)).or(Unix.Path.reachable.prefix(Unset))
    
    def descent(path: Path): List[PathName[Forbidden]] = (path: @unchecked) match
      case path: Unix.SafePath    => path.safeDescent
      case path: Windows.SafePath => path.safeDescent
    
    def separator(path: Path): Text = path match
      case path: Unix.SafePath    => t"/"
      case path: Windows.SafePath => t"\\"
  
  given rootParser: RootParser[Path, Maybe[Windows.Drive]] = text =>
    Windows.Path.rootParser.parse(text).or(Unix.Path.rootParser.parse(text))
  
  given PathCreator[Path, Forbidden, Maybe[Windows.Drive]] with
    def path(root: Maybe[Windows.Drive], descent: List[PathName[Forbidden]]) = root match
      case drive@Windows.Drive(_) => Windows.SafePath(drive, descent)
      case _                      => Unix.SafePath(descent)

  given AsMessage[Path] = path => Message(path.render)

  inline given decoder(using CanThrow[PathError]): Decoder[Path] = new Decoder[Path]:
    def decode(text: Text): Path = Reachable.decode(text)

trait Link

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

  case class Path(drive: Drive, descent: List[PathName[Forbidden]]) extends galilei.Path:
    def root: Drive = drive
    def name: Text = if descent.isEmpty then drive.name else descent.head.show
    
    def fullname: Text =
      t"${Path.reachable.prefix(drive)}${descent.reverse.map(_.render).join(t"\\")}"

  class SafePath(drive: Drive, val safeDescent: List[PathName[galilei.Path.Forbidden]])
  extends Path(drive, safeDescent.map(_.widen[Forbidden]))
  
  object Link: 
    given creator: PathCreator[Link, Forbidden, Int] = Link(_, _)
    
    given followable: Followable[Link, Forbidden, "..", "."] with
      val separators: Set[Char] = Set('\\')
      def separator(path: Link): Text = t"\\"
      def ascent(path: Link): Int = path.ascent
      def descent(path: Link): List[PathName[Forbidden]] = path.descent
      def make(ascent: Int, descent: List[PathName[Forbidden]]): Link = Link(ascent, descent)

  case class Drive(letter: Char):
    def name: Text = t"$letter:"
    def /(name: PathName[Forbidden]): Path = Path(this, List(name))
  
  case class Link(ascent: Int, descent: List[PathName[Forbidden]]) extends galilei.Link

  sealed trait Inode extends galilei.Inode

object Unix:
  type Forbidden = ".*\\/.*" | ".*[\\cA-\\cZ].*" | "\\.\\." | "\\."

  object Path:
    inline given decoder(using CanThrow[PathError]): Decoder[Path] = new Decoder[Path]:
      def decode(text: Text): Path = Reachable.decode(text)
    
    given rootParser: RootParser[Path, Unset.type] = text => (Unset, text.drop(1))
    given creator: PathCreator[Path, Forbidden, Unset.type] = (root, descent) => Path(descent)

    given reachable: Reachable[Path, Forbidden, Unset.type] with
      def separator(path: Path): Text = t"/"
      def root(path: Path): Unset.type = Unset
      def prefix(root: Unset.type): Text = t"/"
      def descent(path: Path): List[PathName[Forbidden]] = path.descent
    
  case class Path(descent: List[PathName[Forbidden]]) extends galilei.Path:
    def root: Unset.type = Unset
    def name: Text = if descent.isEmpty then Path.reachable.prefix(Unset) else descent.head.show
    
    def fullname: Text =
      t"${Path.reachable.prefix(Unset)}${descent.reverse.map(_.render).join(t"/")}"
  
    def socket(): Socket = Socket(this)
    def symlink(): Symlink = Symlink(this)
    def fifo(): Fifo = Fifo(this)
    def blockDevice(): BlockDevice = BlockDevice(this)
    def charDevice(): CharDevice = CharDevice(this)

  class SafePath(val safeDescent: List[PathName[galilei.Path.Forbidden]])
  extends Path(safeDescent.map(_.widen[Forbidden]))

  object Link:
    given creator: PathCreator[Link, Forbidden, Int] = Link(_, _)
    
    given followable: Followable[Link, Forbidden, "..", "."] with
      val separators: Set[Char] = Set('/')
      def separator(path: Link): Text = t"/"
      def ascent(path: Link): Int = path.ascent
      def descent(path: Link): List[PathName[Forbidden]] = path.descent
  
  case class Link(ascent: Int, descent: List[PathName[Forbidden]]) extends galilei.Link
      
  sealed trait Inode extends galilei.Inode

case class Volume(name: Text, volumeType: Text)

sealed trait Inode:
  def path: Path
  def fullname: Text = path.fullname
  def stillExists(): Boolean = path.exists()
  
  def volume: Volume =
    val fileStore = jnf.Files.getFileStore(jnf.Path.of(path.render.s)).nn
    Volume(fileStore.name.nn.tt, fileStore.`type`.nn.tt)
  
  def delete(): Path throws IoError =
    try
      jnf.Files.delete(jnf.Path.of(path.render.s))
      path
    
    catch
      case error: jnf.NoSuchFileException        => throw IoError(path)
      case error: jnf.DirectoryNotEmptyException => throw IoError(path)
      case error: ji.FileNotFoundException       => throw IoError(path)
      case error: ji.IOException                 => throw IoError(path)
      case error: SecurityException              => throw IoError(path)
  
  def moveTo
      (destination: Path)
      (using overwritePreexisting: OverwritePreexisting, moveAtomically: MoveAtomically,
          dereferenceSymlinks: DereferenceSymlinks,
          createNonexistentParents: CreateNonexistentParents)
      (using io: CanThrow[IoError])
      : Path^{io, overwritePreexisting, moveAtomically, dereferenceSymlinks} =

    val javaPath = jnf.Path.of(destination.render.s).nn
    val options: Seq[jnf.CopyOption] = dereferenceSymlinks() ++ moveAtomically()

    if overwritePreexisting() then jnf.Files.deleteIfExists(javaPath)
    
    def op() = jnf.Files.move(jnf.Path.of(fullname.s), javaPath, options*)

    overwritePreexisting(op(), destination)
    destination
      
trait Path:
  this: Path =>
  def fullname: Text
  def name: Text
  def exists(): Boolean = jnf.Files.exists(jnf.Path.of(fullname.s))
  
  def wipe
      ()
      (using dereferenceSymlinks: DereferenceSymlinks, deleteRecursively: DeleteRecursively)
      (using io: CanThrow[IoError]): Path =
    ???
    

  inline def inodeType
      ()(using dereferenceSymlinks: DereferenceSymlinks)(using io: CanThrow[IoError])
      : InodeType^{io} =
    
    val options = dereferenceSymlinks()
    
    try jnf.Files.getAttribute(jnf.Path.of(fullname.s), "unix:mode", options*) match
      case mode: Int => (mode & 61440) match
        case  4096 => InodeType.Fifo
        case  8192 => InodeType.CharDevice
        case 16384 => InodeType.Directory
        case 24576 => InodeType.BlockDevice
        case 32768 => InodeType.File
        case 40960 => InodeType.Symlink
        case 49152 => InodeType.Socket
        case _     => throw IoError(this)
    
    catch
      case error: UnsupportedOperationException =>
        throw Mistake(msg"the file attribute unix:mode could not be obtained")
      
      case error: ji.IOException =>
        throw IoError(this)


  def as[InodeType <: Inode](using resolver: PathResolver[InodeType, this.type]): InodeType =
    resolver(this)
  
  def make[InodeType <: Inode]()(using maker: InodeMaker[InodeType, this.type]): InodeType =
    maker(this)
  
  def directory(): Directory = Directory(this)
  
  inline def file
      ()
      (using inline dereferenceSymlinks: DereferenceSymlinks)
      (using CanThrow[IoError], CanThrow[NotFoundError], CanThrow[InodeTypeError])
      : File =
    
    if !exists() then throw NotFoundError(this)
    if inodeType() != InodeType.File then throw InodeTypeError(this)
    
    File(this)

object PathResolver:
  given directory
      (using createNonexistent: CreateNonexistent)
      : PathResolver[Directory, Path] = path => Directory(path)
    
trait PathResolver[+InodeType <: Inode, -PathType <: Path]:
  def apply(value: PathType): InodeType

object InodeMaker:
  given makeDirectory
      (using createNonexistentParents: CreateNonexistentParents,
          overwritePreexisting: OverwritePreexisting)
      : InodeMaker[Directory, Path]^{createNonexistentParents, overwritePreexisting} = path =>
    
    val javaPath = jnf.Path.of(path.render.s).nn

    def op() =
      if createNonexistentParents()
      then path.parent.mm { path => jnf.Files.createDirectories(jnf.Path.of(path.render.s).nn) }

      if overwritePreexisting() then jnf.Files.deleteIfExists(javaPath)
      
      jnf.Files.createDirectory(javaPath)
    
    createNonexistentParents(overwritePreexisting(op(), path), path)
    
    Directory(path)
  
  given makeFile
      (using createNonexistentParents: CreateNonexistentParents,
          overwritePreexisting: OverwritePreexisting)
      : InodeMaker[File, Path]^{createNonexistentParents, overwritePreexisting} = path =>
    def op() = jnf.Files.createFile(jnf.Path.of(path.render.s).nn)
    
    createNonexistentParents(overwritePreexisting(op(), path), path)
    File(path)
      
trait InodeMaker[+InodeType <: Inode, -PathType <: Path]:
  def apply(value: PathType): InodeType

case class Directory(path: Path) extends Unix.Inode, Windows.Inode

case class File(path: Path) extends Unix.Inode, Windows.Inode:
  def size(): ByteSize = jnf.Files.size(jnf.Path.of(path.render.s)).b

case class Socket(path: Unix.Path) extends Unix.Inode
case class Fifo(path: Unix.Path) extends Unix.Inode
case class Symlink(path: Unix.Path) extends Unix.Inode
case class BlockDevice(path: Unix.Path) extends Unix.Inode
case class CharDevice(path: Unix.Path) extends Unix.Inode

enum InodeType:
  case Fifo, CharDevice, Directory, BlockDevice, File, Symlink, Socket

@capability
trait DereferenceSymlinks:
  def apply(): List[jnf.LinkOption]

@capability
trait MoveAtomically:
  def apply(): List[jnf.CopyOption]

@capability
trait CopyAttributes:
  def apply(): List[jnf.CopyOption]

@capability
trait DeleteRecursively

@capability
trait OverwritePreexisting:
  def apply(op: => Unit, path: Path): Unit
  def apply(): Boolean

@capability
trait CreateNonexistentParents:
  def apply(value: => Unit, path: Path): Unit
  def apply(): Boolean

@capability
trait CreateNonexistent

@capability
trait WriteSynchronously

package filesystemOptions:
  given dereferenceSymlinks: DereferenceSymlinks = new DereferenceSymlinks:
    def apply(): List[jnf.LinkOption] = Nil
  
  given doNotDereferenceSymlinks: DereferenceSymlinks = new DereferenceSymlinks:
    def apply(): List[jnf.LinkOption] = List(jnf.LinkOption.NOFOLLOW_LINKS)
  
  given moveAtomically: MoveAtomically with
    def apply(): List[jnf.CopyOption] = List(jnf.StandardCopyOption.ATOMIC_MOVE)
  
  given doNotMoveAtomically: MoveAtomically with
    def apply(): List[jnf.CopyOption] = Nil
  
  given copyAttributes: CopyAttributes with
    def apply(): List[jnf.CopyOption] = List(jnf.StandardCopyOption.COPY_ATTRIBUTES)

  given doNotCopyAttributes: CopyAttributes with
    def apply(): List[jnf.CopyOption] = Nil

  given deleteRecursively: DeleteRecursively with
    ()

  given doNotDeleteRecursively: DeleteRecursively with
    ()

  given overwritePreexisting: OverwritePreexisting with
    def apply(op: => Unit, path: Path): Unit = op
    def apply(): Boolean = true

  given doNotOverwritePreexisting(using overwrite: CanThrow[OverwriteError]): OverwritePreexisting =
    new OverwritePreexisting:
      def apply(op: => Unit, path: Path): Unit =
        try op catch case error: jnf.FileAlreadyExistsException => throw OverwriteError(path)

      def apply(): Boolean = false

  given createNonexistentParents: CreateNonexistentParents with
    def apply(op: => Unit, path: Path): Unit = op
    def apply(): Boolean = true

  given doNotCreateNonexistentParents
      (using notFound: CanThrow[NotFoundError])
      : CreateNonexistentParents =
    new CreateNonexistentParents:
      def apply(): Boolean = false
      
      def apply(op: => Unit, path: Path): Unit =
        try op catch case error: ji.FileNotFoundException => throw NotFoundError(path)

  given createNonexistent: CreateNonexistent with
    ()

  given doNotCreateNonexistent: CreateNonexistent with
    ()

  given writeSynchronously: WriteSynchronously with
    ()

  given doNotWriteSynchronously: WriteSynchronously with
    ()


case class IoError(path: Path) extends Error(msg"an I/O error occurred")

case class NotFoundError(path: Path)
extends Error(msg"no filesystem node was found at the path $path")

case class OverwriteError(path: Path)
extends Error(msg"cannot overwrite a pre-existing filesystem node $path")

case class ForbiddenOperationError(path: Path)
extends Error(msg"insufficient permissions to modify $path")

case class InodeTypeError(path: Path)
extends Error(msg"the filesystem node at $path was expected to be a different type")