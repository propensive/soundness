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
┃    Soundness, version 0.64.0.                                                                    ┃
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
import denominative.*
import contingency.*
import gossamer.*
import hypotenuse.*
import rudiments.*
import vacuous.*

object TarHeader:
  val blockSize: Int = 512
  val checksumOffset: Int = 148
  val checksumLength: Int = 8

  def parse(block: Data): TarHeader raises Tar.Error =
    if block.length < blockSize
    then raise(Tar.Error(Tar.Error.Reason.TruncatedStream(blockSize, block.length)))

    TarHeader
      ( name     = block.segment((0).z till (100).z),
        mode     = block.segment((100).z till (108).z),
        uid      = block.segment((108).z till (116).z),
        gid      = block.segment((116).z till (124).z),
        size     = block.segment((124).z till (136).z),
        mtime    = block.segment((136).z till (148).z),
        checksum = block.segment((148).z till (156).z),
        // Total: after a raised `TruncatedStream` the header is parsed best-effort, so a
        // short block yields the default (regular-file) flag rather than an overrun.
        typeFlag = block.at((156).z).or(0),
        linkName = block.segment((157).z till (257).z),
        magic    = block.segment((257).z till (263).z),
        version  = block.segment((263).z till (265).z),
        uname    = block.segment((265).z till (297).z),
        gname    = block.segment((297).z till (329).z),
        devMajor = block.segment((329).z till (337).z),
        devMinor = block.segment((337).z till (345).z),
        prefix   = block.segment((345).z till (500).z) )

  def verifyChecksum(block: Data, recorded: U32): Unit raises Tar.Error =
    var sum: Long = 0L

    // A short block sums fewer bytes and fails the comparison below, rather than overrunning.
    block.iterate(block.extent.capped(blockSize)): index =>
      val i: Int = (index: Ordinal).n0

      val byte: Int =
        if i >= checksumOffset && i < checksumOffset + checksumLength then 0x20
        else block.at(index) & 0xff

      sum = sum + byte

    val actual: U32 = sum.toInt.bits.u32

    if actual != recorded then raise(Tar.Error(Tar.Error.Reason.BadChecksum(recorded, actual)))

  def decodeOctal(data: Data, field: Text): U32 raises Tar.Error =
    // Two-stage scan: leading spaces, then the octal digit run. `prefix(after)` is
    // cumulative, so `digits` spans both stages and its limit is the digit run's end.
    val spaces = data.prefix { index => data.at(index) == ' '.toByte }

    val digits = data.prefix(spaces): index =>
      val byte: Byte = data.at(index)
      byte >= '0'.toByte && byte <= '7'.toByte

    if (digits: Interval).size == (spaces: Interval).size
    then raise(Tar.Error(Tar.Error.Reason.BadOctal(field, data)))

    // The one genuinely checked access: the terminator position is one past the digit run,
    // which may be one past the end of the field, so `at` returning `Unset` means a run
    // flush to the field's end — valid, with nothing to check.
    data.at((digits: Interval).limit).let: byte =>
      if byte != 0 && byte != ' '.toByte
      then raise(Tar.Error(Tar.Error.Reason.BadOctal(field, data)))

    var n: Long = 0L

    // `digits` includes the leading spaces (cumulative), which are skipped by value.
    data.iterate(digits): index =>
      val byte: Byte = data.at(index)
      if byte != ' '.toByte then n = n*8L + (byte - '0'.toByte).toLong

    n.toInt.bits.u32

  def decodeNulText(data: Data): Text =
    data.segment(data.prefix { index => data.at(index) != 0 }).utf8

  def isZeroBlock(block: Data): Boolean =
    (block.prefix { index => block.at(index) == 0.toByte }: Interval).size
    >= block.length.min(blockSize)

case class TarHeader
  ( name:     Data,
    mode:     Data,
    uid:      Data,
    gid:      Data,
    size:     Data,
    mtime:    Data,
    checksum: Data,
    typeFlag: Byte,
    linkName: Data,
    magic:    Data,
    version:  Data,
    uname:    Data,
    gname:    Data,
    devMajor: Data,
    devMinor: Data,
    prefix:   Data )
