package bitumen

import turbulence.*
import anticipation.*
import rudiments.*
import vacuous.*
import gossamer.*
import hieroglyph.*, charEncoders.ascii, textMetrics.uniform
import hypotenuse.*

case class UnixMode
    (setUid:     Boolean = false,
     setGid:     Boolean = false,
     ownerRead:  Boolean = true,
     ownerWrite: Boolean = true,
     ownerExec:  Boolean = false,
     groupRead:  Boolean = true,
     groupWrite: Boolean = false,
     groupExec:  Boolean = false,
     otherRead:  Boolean = true,
     otherWrite: Boolean = false,
     otherExec:  Boolean = false):
  
  def int: Int =
    var sum: Int = 0
    if setUid then sum += 2048
    if setGid then sum += 1024
    if ownerRead then sum += 256
    if ownerWrite then sum += 128
    if ownerExec then sum += 64
    if groupRead then sum += 32
    if groupWrite then sum += 16
    if groupExec then sum += 8
    if otherRead then sum += 4
    if otherWrite then sum += 2
    if otherExec then sum += 1
    sum
  
  def bytes: Bytes = int.octal.pad(8, Rtl, '0').bytes

case class Uid(value: Int):
  def bytes: Bytes = value.octal.pad(8, Rtl, '0').bytes

case class Gid(value: Int):
  def bytes: Bytes = value.octal.pad(8, Rtl, '0').bytes

enum TypeFlag:
  case File
  case AFile
  case Link
  case Symlink
  case CharSpecial
  case BlockSpecial
  case Directory
  case Fifo
  case Contiguous
  case NextFile
  case GlobalExtension

  def id: Char = this match
    case File            => '0'
    case AFile           => '\u0000'
    case Link            => '1'
    case Symlink         => '2'
    case CharSpecial     => '3'
    case BlockSpecial    => '4'
    case Directory       => '5'
    case Fifo            => '6'
    case Contiguous      => '7'
    case NextFile        => 'x'
    case GlobalExtension => 'g'

object TarEntry:
  def apply[DataType, InstantType: GenericInstant]
      (name:  Text,
       data:  DataType,
       mode:  UnixMode              = UnixMode(),
       uid:   Uid                   = Uid(0),
       gid:   Gid                   = Gid(0),
       mtime: Optional[InstantType] = Unset)
      (using Readable[DataType, Bytes])
          : TarEntry =
    
    val mtimeLong: Long = mtime.let(_.millisecondsSinceEpoch).or(System.currentTimeMillis)/1000
    TarEntry.File(name, mode, uid, gid, mtimeLong, data.stream[Bytes])

enum TarEntry(name: Text, mode: UnixMode, uid: Uid, gid: Gid, mtime: Long):
  case File(name: Text, mode: UnixMode, uid: Uid, gid: Gid, mtime: Long, data: LazyList[Bytes])
  extends TarEntry(name, mode, uid, gid, mtime)

  case Directory(name: Text, mode: UnixMode, uid: Uid, gid: Gid, mtime: Long)
  extends TarEntry(name, mode, uid, gid, mtime)

  def size: Int = this match
    case file: File => file.data.sumBy(_.length)
    case directory  => 0
  
  def dataBlocks: LazyList[Bytes] = this match
    case file: File => file.data.chunked(512)
    case directory  => LazyList()

  def typeFlag: TypeFlag = this match
    case file: File           => TypeFlag.File
    case directory: Directory => TypeFlag.Directory

  lazy val header: Bytes = Bytes.construct(512): array =>
    array.place(name.bytes, 0)
    array.place(mode.bytes, 100)
    array.place(uid.bytes, 108)
    array.place(gid.bytes, 116)
    array.place(size.octal.pad(12, Rtl, '0').bytes, 124)
    array.place(mtime.octal.pad(12, Rtl, '0').bytes, 136)
    array.place(t"        ".bytes, 148)
    array(156) = typeFlag.id.toByte
    array.place(t"ustar\u0000".bytes, 257)
    array.place(t"00".bytes, 263)
    val total = array.sumBy(java.lang.Byte.toUnsignedInt(_))
    array.place(total.octal.pad(8, Rtl, '0').bytes, 148)

  def serialize: LazyList[Bytes] = header #:: dataBlocks

object Tar:
  val zeroBlock: Bytes = IArray.fill[Byte](512)(0)

case class Tar(entries: LazyList[TarEntry]):
  def serialize: LazyList[Bytes] = entries.flatMap(_.serialize) #::: LazyList(Tar.zeroBlock, Tar.zeroBlock)
  