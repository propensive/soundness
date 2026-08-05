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
import proscenium.compat.*
import rudiments.*
import turbulence.*
import vacuous.*
import zephyrine.*

// LZW, the compression of TIFF and PDF streams, implemented natively (the JDK offers none)
// and therefore available on every platform. `earlyChange` — both sides widening their
// codes one table entry sooner — is the TIFF/PDF default, and what every known encoder
// produces; the parameterized factories serve formats which state it explicitly.
object Lzw:
  def compressor(earlyChange: Boolean)(using Buffering): (Duct[Data, Data] {
    type Transport = Credit
    type Upstream = Credit })^ =

    LzwStage(LzwEncoder(earlyChange))

  def decompressor(earlyChange: Boolean)(using Buffering): (Duct[Data, Data] {
    type Transport = Credit
    type Upstream = Credit })^ =

    LzwStage(LzwDecoder(earlyChange))

  def compress(stream: Chain[Data], earlyChange: Boolean = true): Chain[Data] =
    drive(LzwEncoder(earlyChange), stream)

  def decompress(stream: Chain[Data], earlyChange: Boolean = true): Chain[Data] =
    drive(LzwDecoder(earlyChange), stream)

  given compression: Lzw is Compression:
    def compressor()(using Buffering): (Duct[Data, Data] {
      type Transport = Credit
      type Upstream = Credit })^ =

      LzwStage(LzwEncoder(true))

    def decompressor()(using Buffering): (Duct[Data, Data] {
      type Transport = Credit
      type Upstream = Credit })^ =

      LzwStage(LzwDecoder(true))

    override def compress(stream: Chain[Data]): Chain[Data] = Lzw.compress(stream)
    override def decompress(stream: Chain[Data]): Chain[Data] = Lzw.decompress(stream)

  // Drives an engine over a lazy stream chunk by chunk, emitting each chunk's output as it
  // is produced. The engine argument is by-name, so the (exclusive, mutable) engine is
  // minted inside the deferred block and threaded through the recursion.
  private def drive(engine0: => LzwEngine^, stream: Chain[Data]): Chain[Data] =
    def recur(engine: LzwEngine^, stream: Chain[Data]): Chain[Data] = stream match
      case head #:: tail =>
        engine.accept(head, 0, head.length)
        val data = engine.gather()
        if data.length > 0 then data #:: recur(engine, tail) else recur(engine, tail)

      case _ =>
        engine.finish()
        val data = engine.gather()
        if data.length > 0 then Chain(data) else Chain.empty

    Chain.defer(recur(engine0, stream))

sealed trait Lzw extends Compressor
