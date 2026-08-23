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


// Residue: this header parser is all byte subscripts, and the frozen-array `apply` is
// partial; it awaits the partial-operations tranche.
import proscenium.compat.apply

import anticipation.*
import denominative.*
import contingency.*
import gossamer.*
import hypotenuse.*
import rudiments.*

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
        typeFlag = block(156),
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
    var i = 0

    while i < blockSize do
      val byte: Int =
        if i >= checksumOffset && i < checksumOffset + checksumLength then 0x20 else block(i) & 0xff

      sum = sum + byte
      i = i + 1

    val actual: U32 = sum.toInt.bits.u32

    if actual != recorded then raise(Tar.Error(Tar.Error.Reason.BadChecksum(recorded, actual)))

  def decodeOctal(data: Data, field: Text): U32 raises Tar.Error =
    var i = 0
    while i < data.length && data(i) == ' '.toByte do i = i + 1

    var sawDigit = false
    var n: Long = 0L
    var done = false

    while !done && i < data.length do
      val byte: Byte = data(i)

      if byte == 0 || byte == ' '.toByte then done = true
      else if byte >= '0'.toByte && byte <= '7'.toByte then
        n = n*8L + (byte - '0'.toByte).toLong
        sawDigit = true
        i = i + 1
      else
        raise(Tar.Error(Tar.Error.Reason.BadOctal(field, data)))
        done = true

    if !sawDigit then raise(Tar.Error(Tar.Error.Reason.BadOctal(field, data)))

    n.toInt.bits.u32

  def decodeNulText(data: Data): Text =
    var i = 0
    while i < data.length && data(i) != 0 do i = i + 1
    data.segment((0).z till (i).z).utf8

  def isZeroBlock(block: Data): Boolean =
    var i = 0
    val n = block.length.min(blockSize)
    while i < n && block(i) == 0.toByte do i = i + 1
    i == n

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
