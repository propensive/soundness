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
import zephyrine.*

object Zlib:
  given compression: Zlib is Compression:
    def compressor()(using Buffering): (Duct[Data, Data] {
      type Transport = Credit
      type Upstream = Credit })^ =

      Deflation(gzip = false, nowrap = false)

    def decompressor()(using Buffering): (Duct[Data, Data] {
      type Transport = Credit
      type Upstream = Credit })^ =

      Inflation(gzip = false, nowrap = false)

  // Inflates the one zlib stream which begins at `offset` in `data`, returning what it produced
  // and how many compressed bytes it consumed: for formats which concatenate zlib streams
  // without framing them, as git's packfiles do, so that the next one can be found.
  def inflate(data: Data, offset: Int): (Data, Int) =
    val input = data.asInstanceOf[scala.Array[Byte]]
    val available = input.length - offset
    val output = java.io.ByteArrayOutputStream()

    // The engine and the window travel through the recursion rather than being captured by it,
    // which separation checking would reject.
    def recur(engine: InflateEngine^, buffer: scala.Array[Byte]^): Int =
      if engine.finished then
        val consumed = available - engine.getRemaining
        engine.end()
        consumed
      else
        val produced = engine.inflate(buffer, 0, buffer.length)
        if produced > 0 then output.write(buffer, 0, produced)
        else if !engine.finished then throw IllegalStateException("the compressed data is truncated")
        recur(engine, buffer)

    val engine: InflateEngine^ = FlateBackend.inflater(false)
    engine.setInput(input, offset, available)
    val consumed = recur(engine, new scala.Array[Byte](65536))
    (output.toByteArray.nn.asInstanceOf[Data], consumed)

sealed trait Zlib extends Compressor
