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

import anticipation.*
import rudiments.*
import turbulence.*
import vacuous.*
import zephyrine.*

// LZW, the compression of TIFF and PDF streams, implemented natively (the JDK offers none)
// and therefore available on every platform. `earlyChange` — both sides widening their
// codes one table entry sooner — is the TIFF/PDF default, and what every known encoder
// produces; the parameterized factories serve formats which state it explicitly.
object Lzw:
  def compressor(earlyChange: Boolean)(using Buffering): Duct[Data, Data] {
    type Transport = Credit
    type Upstream = Credit } =

    LzwStage(LzwEncoder(earlyChange))

  def decompressor(earlyChange: Boolean)(using Buffering): Duct[Data, Data] {
    type Transport = Credit
    type Upstream = Credit } =

    LzwStage(LzwDecoder(earlyChange))

  def compress(stream: LazyList[Data], earlyChange: Boolean = true): LazyList[Data] =
    drive(LzwEncoder(earlyChange), stream)

  def decompress(stream: LazyList[Data], earlyChange: Boolean = true): LazyList[Data] =
    drive(LzwDecoder(earlyChange), stream)

  given compression: Lzw is Compression:
    def compressor()(using Buffering): Duct[Data, Data] {
      type Transport = Credit
      type Upstream = Credit } =

      LzwStage(LzwEncoder(true))

    def decompressor()(using Buffering): Duct[Data, Data] {
      type Transport = Credit
      type Upstream = Credit } =

      LzwStage(LzwDecoder(true))

    override def compress(stream: LazyList[Data]): LazyList[Data] = Lzw.compress(stream)
    override def decompress(stream: LazyList[Data]): LazyList[Data] = Lzw.decompress(stream)

  // Drives an engine over a lazy stream chunk by chunk, then collects its finished tail.
  private def drive(engine: LzwEngine, stream: LazyList[Data]): LazyList[Data] =
    def recur(stream: LazyList[Data]): LazyList[Data] = stream match
      case head #:: tail =>
        engine.accept(head.mutable(using Unsafe), 0, head.length)
        val data = engine.gather()
        if data.length > 0 then data #:: recur(tail) else recur(tail)

      case _ =>
        engine.finish()
        val data = engine.gather()
        if data.length > 0 then LazyList(data) else LazyList.empty

    LazyList.defer(recur(stream))

sealed trait Lzw extends Compressor
