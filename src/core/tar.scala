/*
    Bitumen, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package bitumen

import anticipation.*
import contingency.*
import denominative.*
import gossamer.*
import hieroglyph.*, charEncoders.ascii, textMetrics.uniform
import hypotenuse.*, arithmeticOptions.overflow.unchecked
import prepositional.*
import rudiments.*
import serpentine.*
import spectacular.*
import turbulence.*
import vacuous.*

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

  def bytes: Bytes = int.octal.pad(7, Rtl, '0').bytes

case class UnixUser(value: Int, name: Optional[Text] = Unset):
  def bytes: Bytes = value.octal.pad(7, Rtl, '0').bytes

case class UnixGroup(value: Int, name: Optional[Text] = Unset):
  def bytes: Bytes = value.octal.pad(7, Rtl, '0').bytes

type InvalidTarNames = ".*[\u0000-\u0019].*" | ".*\u007f-\uffff.*" | ".*\\/.*" | ".*\\\\.*"

case class TarPath(tarFile: Tar, ref: TarRef):
  def entry: TarEntry = ???

case class TarRef(descent: List[Name[InvalidTarNames]]):
  def parent: Optional[TarRef] = descent match
    case Nil       => Unset
    case _ :: tail => TarRef(tail)

object TarRef:
  def apply(text: Text)
     (using pathError:  Tactic[PathError],
            navigable:  TarRef is Navigable[InvalidTarNames, Unset.type],
            rootParser: RootParser[TarRef, Unset.type],
            creator:    PathCreator[TarRef, InvalidTarNames, Unset.type])
          : TarRef =
    Navigable.decode[TarRef](text)

  @targetName("child")
  infix def / (name: Name[InvalidTarNames]): TarRef = TarRef(List(name))

  given TarRef is Navigable[InvalidTarNames, Unset.type] as navigable:
    def root(path: TarRef): Unset.type = Unset
    def descent(path: TarRef): List[Name[InvalidTarNames]] = path.descent
    def prefix(ref: Unset.type): Text = t""
    def separator(path: TarRef): Text = t"/"

  given RootParser[TarRef, Unset.type] as rootParser:
    def parse(text: Text): (Unset.type, Text) =
      (Unset, if text.at(Prim) == '/' then text.skip(1) else text)

  given PathCreator[TarRef, InvalidTarNames, Unset.type] as pathCreator = (root, descent) => TarRef(descent)
  given TarRef is Showable as showable = _.descent.reverse.map(_.render).join(t"/")

enum TypeFlag:
  case File
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
  def apply[DataType: Readable by Bytes, InstantType: GenericInstant]
     (name:  TarRef,
      data:  DataType,
      mode:  UnixMode              = UnixMode(),
      user:  UnixUser              = UnixUser(0),
      group: UnixGroup             = UnixGroup(0),
      mtime: Optional[InstantType] = Unset)
          : TarEntry =

    val mtimeU32: U32 = (mtime.let(_.millisecondsSinceEpoch).or(System.currentTimeMillis)/1000).toInt.bits.u32
    TarEntry.File(name, mode, user, group, mtimeU32, data.stream[Bytes])

enum TarEntry(path: TarRef, mode: UnixMode, user: UnixUser, group: UnixGroup, mtime: U32):
  case File(path: TarRef, mode: UnixMode, user: UnixUser, group: UnixGroup, mtime: U32, data: LazyList[Bytes])
  extends TarEntry(path, mode, user, group, mtime)

  case Directory(path: TarRef, mode: UnixMode, user: UnixUser, group: UnixGroup, mtime: U32)
  extends TarEntry(path, mode, user, group, mtime)

  case Link(path: TarRef, mode: UnixMode, user: UnixUser, group: UnixGroup, mtime: U32, target: Text)
  extends TarEntry(path, mode, user, group, mtime)

  case Symlink(path: TarRef, mode: UnixMode, user: UnixUser, group: UnixGroup, mtime: U32, target: Text)
  extends TarEntry(path, mode, user, group, mtime)

  case CharSpecial
     (path: TarRef, mode: UnixMode, user: UnixUser, group: UnixGroup, mtime: U32, device: (U32, U32))
  extends TarEntry(path, mode, user, group, mtime)

  case BlockSpecial
     (path: TarRef, mode: UnixMode, user: UnixUser, group: UnixGroup, mtime: U32, device: (U32, U32))
  extends TarEntry(path, mode, user, group, mtime)

  case Fifo(path: TarRef, mode: UnixMode, user: UnixUser, group: UnixGroup, mtime: U32)
  extends TarEntry(path, mode, user, group, mtime)


  def size: U32 = this match
    case file: File => file.data.sumBy(_.length).bits.u32
    case _          => 0

  def dataBlocks: LazyList[Bytes] = this match
    case file: File => file.data.chunked(512)
    case directory  => LazyList()

  def typeFlag: TypeFlag = this match
    case _: File         => TypeFlag.File
    case _: Link         => TypeFlag.Link
    case _: Symlink      => TypeFlag.Symlink
    case _: CharSpecial  => TypeFlag.CharSpecial
    case _: BlockSpecial => TypeFlag.BlockSpecial
    case _: Directory    => TypeFlag.Directory
    case _: Fifo         => TypeFlag.Fifo

  def entryName: Text = this match
    case directory: Directory => t"${directory.path}/"
    case other                => this.path.show

  def link: Optional[Text] = this.only:
    case link: Link       => link.target
    case symlink: Symlink => symlink.target

  def deviceNumbers: Optional[(U32, U32)] = this.only:
    case special: CharSpecial  => special.device
    case special: BlockSpecial => special.device

  def format(number: U32, width: Int): Bytes =
    number.octal.pad(width - 1).bytes

  lazy val header: Bytes = Bytes.construct(512): array =>
    array.place(entryName.bytes, 0)
    array.place(mode.bytes, 100)
    array.place(user.bytes, 108)
    array.place(group.bytes, 116)
    array.place(format(size, 12), 124)
    array.place(format(mtime, 12), 136)
    array.place(t"        ".bytes, 148)
    array(156) = typeFlag.id.toByte

    link.let { link => array.place(link.bytes, 157) }

    deviceNumbers.let: (devMajor, devMinor) =>
      array.place(format(devMajor, 8), 329)
      array.place(format(devMinor, 8), 337)

    user.name.let { name => array.place(name.bytes, 265) }
    group.name.let { name => array.place(name.bytes, 297) }

    array.place(t"ustar\u0000".bytes, 257)
    array.place(t"00".bytes, 263)

    val total = array.map(_.bits.u8.u32).reduce(_ + _)
    array.place(format(total, 8), 148)

  def serialize: LazyList[Bytes] = header #:: dataBlocks

object Tar:
  val zeroBlock: Bytes = IArray.fill[Byte](512)(0)

  given Tar is Readable by Bytes as readable = _.serialize

case class Tar(entries: LazyList[TarEntry]):
  def serialize: LazyList[Bytes] = entries.flatMap(_.serialize) #::: LazyList(Tar.zeroBlock, Tar.zeroBlock)
