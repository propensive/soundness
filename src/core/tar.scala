/*
    Bitumen, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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
  
  def bytes: Bytes = int.octal.pad(7, Rtl, '0').bytes

case class UnixUser(value: Int, name: Optional[Text] = Unset):
  def bytes: Bytes = value.octal.pad(7, Rtl, '0').bytes

case class UnixGroup(value: Int, name: Optional[Text] = Unset):
  def bytes: Bytes = value.octal.pad(7, Rtl, '0').bytes

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
  def apply[DataType, InstantType: GenericInstant]
      (name:  Text,
       data:  DataType,
       mode:  UnixMode              = UnixMode(),
       user:  UnixUser              = UnixUser(0),
       group: UnixGroup             = UnixGroup(0),
       mtime: Optional[InstantType] = Unset)
      (using Readable[DataType, Bytes])
          : TarEntry =
    
    val mtimeLong: Long = mtime.let(_.millisecondsSinceEpoch).or(System.currentTimeMillis)/1000
    TarEntry.File(name, mode, user, group, mtimeLong, data.stream[Bytes])

enum TarEntry(name: Text, mode: UnixMode, user: UnixUser, group: UnixGroup, mtime: Long):
  case File(name: Text, mode: UnixMode, user: UnixUser, group: UnixGroup, mtime: Long, data: LazyList[Bytes])
  extends TarEntry(name, mode, user, group, mtime)

  case Directory(name: Text, mode: UnixMode, user: UnixUser, group: UnixGroup, mtime: Long)
  extends TarEntry(name, mode, user, group, mtime)
  
  case Link(name: Text, mode: UnixMode, user: UnixUser, group: UnixGroup, mtime: Long, target: Text)
  extends TarEntry(name, mode, user, group, mtime)
  
  case Symlink(name: Text, mode: UnixMode, user: UnixUser, group: UnixGroup, mtime: Long, target: Text)
  extends TarEntry(name, mode, user, group, mtime)
  
  case CharSpecial
      (name: Text, mode: UnixMode, user: UnixUser, group: UnixGroup, mtime: Long, device: (Int, Int))
  extends TarEntry(name, mode, user, group, mtime)
  
  case BlockSpecial
      (name: Text, mode: UnixMode, user: UnixUser, group: UnixGroup, mtime: Long, device: (Int, Int))
  extends TarEntry(name, mode, user, group, mtime)
  
  case Fifo(name: Text, mode: UnixMode, user: UnixUser, group: UnixGroup, mtime: Long)
  extends TarEntry(name, mode, user, group, mtime)


  def size: Int = this match
    case file: File => file.data.sumBy(_.length)
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

  
  def link: Optional[Text] = this match
    case Link(_, _, _, _, _, target)    => target
    case Symlink(_, _, _, _, _, target) => target
    case _                              => Unset
  
  def deviceNumbers: Optional[(Int, Int)] = this match
    case special: CharSpecial  => special.device
    case special: BlockSpecial => special.device
    case _                     => Unset

  lazy val header: Bytes = Bytes.construct(512): array =>
    array.place(name.bytes, 0)
    array.place(mode.bytes, 100)
    array.place(user.bytes, 108)
    array.place(group.bytes, 116)
    array.place(size.octal.pad(11, Rtl, '0').bytes, 124)
    array.place(mtime.octal.pad(11, Rtl, '0').bytes, 136)
    array.place(t"        ".bytes, 148)
    array(156) = typeFlag.id.toByte
    
    link.let { link => array.place(link.bytes, 157) }
    
    deviceNumbers.let: (devMajor, devMinor) =>
      array.place(devMajor.octal.pad(7, Rtl, '0').bytes, 329)
      array.place(devMinor.octal.pad(7, Rtl, '0').bytes, 337)

    user.name.let { name => array.place(name.bytes, 265) }
    group.name.let { name => array.place(name.bytes, 297) }
    
    array.place(t"ustar\u0000".bytes, 257)
    array.place(t"00".bytes, 263)
    

    val total = array.sumBy(java.lang.Byte.toUnsignedInt(_))
    array.place(total.octal.pad(7, Rtl, '0').bytes, 148)

  def serialize: LazyList[Bytes] = header #:: dataBlocks

object Tar:
  val zeroBlock: Bytes = IArray.fill[Byte](512)(0)

  given readable: Readable[Tar, Bytes] = _.serialize

case class Tar(entries: LazyList[TarEntry]):
  def serialize: LazyList[Bytes] = entries.flatMap(_.serialize) #::: LazyList(Tar.zeroBlock, Tar.zeroBlock)