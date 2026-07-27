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
package pneumatic

import scala.language.adhocExtensions

import scala.annotation.*

import anticipation.*
import prepositional.*
import turbulence.*
import zephyrine.*

extension (data: Data)
  // Compress/decompress a whole value through the format's own duct, driven directly over it as
  // a single window (`Duct.feed`): the whole-value counterparts of the stream stages, sharing
  // their implementation.
  @targetName("compressWholeData")
  def compress[format <: Compressor](using compression: format is Compression, buffering: Buffering)
  :   Data =

    Duct.feed(data, compression.compressor())

  @targetName("decompressWholeData")
  def decompress[format <: Compressor]
    ( using compression: format is Compression, buffering: Buffering )
  :   Data =

    Duct.feed(data, compression.decompressor())

extension (consume stream: (Stream[Data] over Credit)^)
  def compress[format <: Compressor](using compression: format is Compression, buffering: Buffering)
  :   (Stream[Data] over Credit)^ =

    stream.via(compression.compressor()).asInstanceOf[(Stream[Data] over Credit)^]

  def decompress[format <: Compressor]
    ( using compression: format is Compression, buffering: Buffering )
  :   (Stream[Data] over Credit)^ =

    stream.via(compression.decompressor()).asInstanceOf[(Stream[Data] over Credit)^]

extension (stream: Progression[Data])
  def compress[compression <: Compressor: Compression]: Progression[Data] =
    compression.compress(stream)

  def decompress[compression <: Compressor: Compression]: Progression[Data] =
    compression.decompress(stream)

// An exclusive, writable view of an array held in an untracked (`untrackedCaptures`) field:
// reading such a field yields a read-only capture, so update sites route through this
// assume-pure rebind. Sound because each engine reaches its buffers only through `this`.
private[pneumatic] def writable[element](array: Array[element]): Array[element]^ =
  array.asInstanceOf[Array[element]]
