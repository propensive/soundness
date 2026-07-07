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
┃    Soundness, version 0.63.0.                                                                    ┃
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
import gossamer.*
import hypotenuse.*

object TarHeader:
  val blockSize: Int = 512
  val checksumOffset: Int = 148
  val checksumLength: Int = 8

  def parse(block: Data): TarHeader raises TarError =
    if block.length < blockSize
    then raise(TarError(TarError.Reason.TruncatedStream(blockSize, block.length)))

    TarHeader
      ( name     = block.slice(0, 100),
        mode     = block.slice(100, 108),
        uid      = block.slice(108, 116),
        gid      = block.slice(116, 124),
        size     = block.slice(124, 136),
        mtime    = block.slice(136, 148),
        checksum = block.slice(148, 156),
        typeFlag = block(156),
        linkName = block.slice(157, 257),
        magic    = block.slice(257, 263),
        version  = block.slice(263, 265),
        uname    = block.slice(265, 297),
        gname    = block.slice(297, 329),
        devMajor = block.slice(329, 337),
        devMinor = block.slice(337, 345),
        prefix   = block.slice(345, 500) )

  def verifyChecksum(block: Data, recorded: U32): Unit raises TarError =
    var sum: Long = 0L
    var i = 0

    while i < blockSize do
      val byte: Int =
        if i >= checksumOffset && i < checksumOffset + checksumLength then 0x20
        else block(i) & 0xff

      sum = sum + byte
      i = i + 1

    val actual: U32 = sum.toInt.bits.u32

    if actual != recorded
    then raise(TarError(TarError.Reason.BadChecksum(recorded, actual)))

  def decodeOctal(data: Data, field: Text): U32 raises TarError =
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
        raise(TarError(TarError.Reason.BadOctal(field, data)))
        done = true

    if !sawDigit then raise(TarError(TarError.Reason.BadOctal(field, data)))

    n.toInt.bits.u32

  def decodeNulText(data: Data): Text =
    var i = 0
    while i < data.length && data(i) != 0 do i = i + 1
    data.slice(0, i).utf8

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
