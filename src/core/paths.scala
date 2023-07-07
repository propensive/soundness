package galilei

import rudiments.*
import digression.*
import serpentine.*
import spectacular.*
import kaleidoscope.*
import gossamer.*

import java.io as ji

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

trait Path:
  this: Path =>
  def fullname: Text
  def name: Text
  
  def directory(creation: Creation = Ensure): Directory = Directory(this)
  def file(creation: Creation = Ensure): File = File(this)

// enum InodeType:
//   case Directory, File, Socket, Fifo, Symlink, BlockDevice, CharDevice

case class Directory(path: Path) extends Unix.Inode, Windows.Inode

// object File:
//   given [FileType <: File & Singleton](using handle: FileHandle[FileType]): Readable[FileType, Bytes] =
    

// trait HandleMaker[ItemType]:
//   type Value
//   def make(file: File): FileHandle[file.type]

// object HandleMaker:
//   given HandleMaker[Byte] with
//     type Handle = BufferedInputStream
    
//     def make[FileType <: File](file: FileType) =
//       FileHandle[file.type](ji.BufferedInputStream(ji.FileInputStream(ji.File(file.fullname.s))))
    
//     def close(value: Value): Unit

case class File(path: Path) extends Unix.Inode, Windows.Inode
  // def open[ResultType]()(action: FileHandle[this.type] ?-> ResultType): ResultType =
  //   val handle = new FileHandle[ji.BufferedInputStream, this.type](
  //   try action(using handle) finally handle.close()

case class Socket(path: Unix.Path) extends Unix.Inode
case class Fifo(path: Unix.Path) extends Unix.Inode
case class Symlink(path: Unix.Path) extends Unix.Inode
case class BlockDevice(path: Unix.Path) extends Unix.Inode
case class CharDevice(path: Unix.Path) extends Unix.Inode

// abstract class FileHandle[FileType <: File & Singleton]:
//   type Handle
//   val handle: Handle
//   def close(): Unit

enum Creation:
  case Expect, Ensure, Create

export Creation.{Expect, Ensure, Create}

// object AccessScheduler:
//   private val waiting: HashMap[Text, List[() -> Any]] = HashMap()
//   private val active: HashSet[Text] = HashSet()

//   def process[FileType <: File & Singleton](key: Text)(action: FileHandle[FileType] ?-> ResultType)