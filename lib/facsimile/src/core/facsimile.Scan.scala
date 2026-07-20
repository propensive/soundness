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
package facsimile

import proscenium.compat.*
import rudiments.*

import anticipation.*

private[facsimile] object Scan:
  private val chunkSize: Int = 8192

  def apply(data: Data): Scan = new Scan(DataSource(data), 0L)

// A buffered forward cursor over a `ByteSource`, starting at an arbitrary offset: the single
// input abstraction for the lexer. Refills its window on demand, so lexemes spanning chunk
// boundaries need no special handling; `-1` signals the end of the source.
private[facsimile] class Scan(source: ByteSource, start: Long):
  private var base: Long = start
  private var chunk: Data = IArray.empty[Byte]
  private var cursor: Int = 0

  def offset: Long = base + cursor

  // Slides the window forward so that at least `count` bytes follow the cursor, if the source
  // has them; returns the number actually available.
  private def ensure(count: Int): Int =
    if cursor + count <= chunk.length then count else
      val absolute = offset
      val length = (source.size - absolute).min(count.max(Scan.chunkSize).toLong).toInt

      chunk = if length <= 0 then IArray.empty[Byte] else source.read(absolute, length)
      base = absolute
      cursor = 0

      count.min(chunk.length)

  def peek: Int = if ensure(1) < 1 then -1 else chunk(cursor) & 0xff

  def peek(ahead: Int): Int =
    if ensure(ahead + 1) < ahead + 1 then -1 else chunk(cursor + ahead) & 0xff

  def take(): Int =
    if ensure(1) < 1 then -1 else
      val byte = chunk(cursor) & 0xff
      cursor += 1
      byte

  def skip(count: Long): Unit =
    if count > 0 then
      if cursor + count <= chunk.length then cursor += count.toInt else
        base = offset + count
        chunk = IArray.empty[Byte]
        cursor = 0

  // An exact-range binary read from the current position, advancing past it: stream payloads
  // and other raw sections are consumed without passing through the lexical window.
  def read(length: Int): Data =
    if length <= 0 then IArray.empty[Byte]
    else if cursor + length <= chunk.length then
      val data = chunk.slice(cursor, cursor + length)
      cursor += length
      data
    else
      val data = source.read(offset, length)
      base = offset + data.length
      chunk = IArray.empty[Byte]
      cursor = 0
      data
