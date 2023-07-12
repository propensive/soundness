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

sealed trait Inode:
  def path: Path
  def fullname: Text = path.fullname
  
  def delete(): Path throws IoError =
    jnf.Files.delete(jnf.Path.of(path.render.s))
    path
  
  inline def moveTo
      (destination: Path)
      (using overwritePreexisting: OverwritePreexisting, moveAtomically: MoveAtomically,
          dereferenceSymlinks: DereferenceSymlinks,
          createNonexistentParents: CreateNonexistentParents)
      (using io: CanThrow[IoError])
      : Unit^{io, overwritePreexisting, moveAtomically, dereferenceSymlinks} =

    // inline if erasedValue[createNonexistentParents.Flag] then
    //   path.parent.directory()

    val options: Seq[jnf.CopyOption] = Iterable(
      inline if erasedValue[overwritePreexisting.Flag] then jnf.StandardCopyOption.REPLACE_EXISTING
      else Unset,
      inline if erasedValue[moveAtomically.Flag] then jnf.StandardCopyOption.ATOMIC_MOVE
      else Unset,
      inline if !erasedValue[dereferenceSymlinks.Flag] then jnf.LinkOption.NOFOLLOW_LINKS
      else Unset
    ).sift[jnf.StandardCopyOption].to(Seq)
    
    try jnf.Files.move(jnf.Path.of(fullname.s), jnf.Path.of(destination.render.s), options*)
    catch
      case error: UnsupportedOperationException   => throw IoError(path)
      case error: jnf.FileAlreadyExistsException      => throw IoError(path)
      case error: jnf.DirectoryNotEmptyException      => throw IoError(path)
      case error: jnf.AtomicMoveNotSupportedException => throw IoError(path)
      case error: ji.IOException                      => throw IoError(path)
      case error: SecurityException               => throw IoError(path)
  
trait Path:
  this: Path =>
  def fullname: Text
  def name: Text
  def exists(): Boolean = jnf.Files.exists(jnf.Path.of(fullname.s))

  inline def inodeType
      ()
      (using dereferenceSymlinks: DereferenceSymlinks)
      (using io: CanThrow[IoError], notFound: CanThrow[NotFoundError])
      : InodeType^{dereferenceSymlinks, io, notFound} =
    val options =
      inline if erasedValue[dereferenceSymlinks.Flag] then Seq()
      else Seq(jnf.LinkOption.NOFOLLOW_LINKS)
    
    try jnf.Files.getAttribute(jnf.Path.of(fullname.s), "unix:mode", options*) match
      case mode: Int => (mode & 61440) match
        case 40960 => InodeType.Symlink
        case 32768 => InodeType.File
        case 16384 => InodeType.Directory
        case  8192 => InodeType.CharDevice
        case  4096 => InodeType.Fifo
        case 24576 => InodeType.BlockDevice
        case 49152 => InodeType.Socket
        case _     => throw IoError(this)
    
    catch
      case error: UnsupportedOperationException =>
        throw Mistake(msg"the file attribute unix:mode could not be obtained")
      
      case error: IllegalArgumentException =>
        throw Mistake(msg"the file attribute unix:mode could not be obtained")
      
      case error: SecurityException =>
        throw Mistake(msg"a security exception occurred")
      
      case error: ji.FileNotFoundException =>
        throw NotFoundError(this)
      
      case error: ji.IOException =>
        throw IoError(this)


  def as[InodeType <: Inode](using resolver: PathResolver[InodeType, this.type]): InodeType =
    resolver(this)
  
  def make[InodeType <: Inode](using maker: PathMaker[InodeType, this.type]): InodeType =
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

trait PathMaker[+InodeType <: Inode, -PathType <: Path]:
  def apply(value: PathType): InodeType

case class Directory(path: Path) extends Unix.Inode, Windows.Inode

case class File(path: Path) extends Unix.Inode, Windows.Inode
  // def open[ResultType]()(action: FileHandle[this.type] ?-> ResultType): ResultType =
  //   val handle = new FileHandle[ji.BufferedInputStream, this.type](
  //   try action(using handle) finally handle.close()

case class Socket(path: Unix.Path) extends Unix.Inode
case class Fifo(path: Unix.Path) extends Unix.Inode
case class Symlink(path: Unix.Path) extends Unix.Inode
case class BlockDevice(path: Unix.Path) extends Unix.Inode
case class CharDevice(path: Unix.Path) extends Unix.Inode

enum InodeType:
  case Fifo, CharDevice, Directory, BlockDevice, File, Symlink, Socket

@capability
erased trait DereferenceSymlinks:
  type Flag <: Boolean & Singleton

@capability
erased trait MoveAtomically:
  type Flag <: Boolean & Singleton

@capability
erased trait CopyAttributes:
  type Flag <: Boolean & Singleton

@capability
erased trait RecursiveDeletion:
  type Flag <: Boolean & Singleton

@capability
erased trait OverwritePreexisting:
  type Flag <: Boolean & Singleton

@capability
erased trait CreateNonexistentParents:
  type Flag <: Boolean & Singleton

@capability
erased trait CreateNonexistent:
  type Flag <: Boolean & Singleton

@capability
erased trait SynchronousWrites:
  type Flag <: Boolean & Singleton

package filesystemOptions:
  object dereferenceSymlinks:
    erased given yes: DereferenceSymlinks { type Flag = true } = ###
    erased given no: DereferenceSymlinks { type Flag = false } = ###
  
  object moveAtomically:
    erased given yes: MoveAtomically { type Flag = true } = ###
    erased given no: MoveAtomically { type Flag = false } = ###

  object copyAttributes:
    erased given yes: CopyAttributes { type Flag = true } = ###
    erased given no: CopyAttributes { type Flag = false } = ###
  
  object recursiveDeletion:
    erased given yes: RecursiveDeletion { type Flag = true } = ###
    erased given no: RecursiveDeletion { type Flag = false } = ###
  
  object overwriteExisting:
    erased given yes: OverwritePreexisting { type Flag = true } = ###
    erased given no: OverwritePreexisting { type Flag = false } = ###
  
  object createNonexistentParents:
    erased given yes: CreateNonexistentParents { type Flag = true } = ###
    erased given no: CreateNonexistentParents { type Flag = false } = ###
  
  object createNonexistent:
    erased given yes: CreateNonexistent { type Flag = true } = ###
    erased given no: CreateNonexistent { type Flag = false } = ###
  
  object synchronousWrites:
    erased given yes: SynchronousWrites { type Flag = true } = ###
    erased given no: SynchronousWrites { type Flag = false } = ###


case class IoError(path: Path) extends Error(msg"an I/O error occurred")

case class PathConflictError(path: Path)
extends Error(msg"cannot overwrite a pre-existing filesystem node")

case class NotFoundError(path: Path)
extends Error(msg"no filesystem node was found at the path $path")

case class OverwriteError(path: Path)
extends Error(msg"cannot overwrite a pre-existing directory")

case class ForbiddenOperationError(path: Path)
extends Error(msg"insufficient access rights for this operation")

case class SymlinkError(path: Path)
extends Error(msg"the symlink at $path did not link to a valid filesystem node")

case class InodeTypeError(path: Path)
extends Error(msg"the filesystem node at $path was expected to be a different type")