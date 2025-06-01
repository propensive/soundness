                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.32.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package bitumen

import anticipation.*
import contingency.*
import denominative.*
import gossamer.*
import hieroglyph.*, charEncoders.ascii, textMetrics.uniform
import hypotenuse.*, arithmeticOptions.overflow.unchecked
import nomenclature.*
import prepositional.*
import rudiments.*
import serpentine.*
import spectacular.*
import turbulence.*
import vacuous.*

object TarEntry:
  def apply[data: Readable by Bytes, instant: Abstractable across Instants]
        ( name:  TarRef,
          data:  data,
          mode:  UnixMode              = UnixMode(),
          user:  UnixUser              = UnixUser(0),
          group: UnixGroup             = UnixGroup(0),
          mtime: Optional[instant]     = Unset )
  : TarEntry =

      val mtimeU32: U32 =
        (mtime.let(_.milliseconds).or(System.currentTimeMillis)/1000).toInt.bits.u32

      TarEntry.File(name, mode, user, group, mtimeU32, data.stream[Bytes])

enum TarEntry(path: TarRef, mode: UnixMode, user: UnixUser, group: UnixGroup, mtime: U32):
  case File
         ( path:  TarRef,
           mode:  UnixMode,
           user:  UnixUser,
           group: UnixGroup,
           mtime: U32,
           data:  LazyList[Bytes] )
  extends TarEntry(path, mode, user, group, mtime)

  case Directory(path: TarRef, mode: UnixMode, user: UnixUser, group: UnixGroup, mtime: U32)
  extends TarEntry(path, mode, user, group, mtime)

  case Link
         ( path: TarRef, mode: UnixMode, user: UnixUser, group: UnixGroup, mtime: U32, target: Text )
  extends TarEntry(path, mode, user, group, mtime)

  case Symlink
         ( path: TarRef, mode: UnixMode, user: UnixUser, group: UnixGroup, mtime: U32, target: Text )
  extends TarEntry(path, mode, user, group, mtime)

  case CharSpecial
         ( path:   TarRef,
           mode:   UnixMode,
           user:   UnixUser,
           group:  UnixGroup,
           mtime:  U32,
           device: (U32, U32) )
  extends TarEntry(path, mode, user, group, mtime)

  case BlockSpecial
         ( path:   TarRef,
           mode:   UnixMode,
           user:   UnixUser,
           group:  UnixGroup,
           mtime:  U32,
           device: (U32, U32) )
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
    array.place(entryName.bytes, Prim)
    array.place(mode.bytes, 100.z)
    array.place(user.bytes, 108.z)
    array.place(group.bytes, 116.z)
    array.place(format(size, 12.z), 124.z)
    array.place(format(mtime, 12.z), 136.z)
    array.place(t"        ".bytes, 148.z)
    array(156) = typeFlag.id.toByte

    link.let { link => array.place(link.bytes, 157.z) }

    deviceNumbers.let: (devMajor, devMinor) =>
      array.place(format(devMajor, 8), 329.z)
      array.place(format(devMinor, 8), 337.z)

    user.name.let { name => array.place(name.bytes, 265.z) }
    group.name.let { name => array.place(name.bytes, 297.z) }

    array.place(t"ustar\u0000".bytes, 257.z)
    array.place(t"00".bytes, 263.z)

    val total = array.map(_.bits.u8.u32).reduce(_ + _)
    array.place(format(total, 8), 148.z)

  def serialize: LazyList[Bytes] = header #:: dataBlocks
