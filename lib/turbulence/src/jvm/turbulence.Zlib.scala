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
package turbulence

import java.io as ji
import java.util.zip as juz

import anticipation.*
import denominative.*
import rudiments.*
import vacuous.*

object Zlib:
  given compression: Zlib is Compression:
    def compress(stream: LazyList[Data]): LazyList[Data] =
      val deflater = juz.Deflater()
      val buffer: Array[Byte] = new Array(4096)
      val out = new ji.ByteArrayOutputStream()

      def recur(stream: LazyList[Data]): LazyList[Data] = stream match
        case head #:: tail =>
          deflater.setInput(head.mutable(using Unsafe))
          var count = deflater.deflate(buffer, 0, buffer.length, juz.Deflater.SYNC_FLUSH)

          while count > 0 do
            out.write(buffer, 0, count)
            count = deflater.deflate(buffer, 0, buffer.length, juz.Deflater.SYNC_FLUSH)

          val data = out.toByteArray.nn.immutable(using Unsafe)
          out.reset()

          if !data.nil then data #:: recur(tail) else recur(tail)

        case _ =>
          deflater.finish()

          while !deflater.finished() do
            val count = deflater.deflate(buffer, 0, buffer.length)
            if count > 0 then out.write(buffer, 0, count)

          val data = out.toByteArray.nn.immutable(using Unsafe)
          out.reset()
          deflater.end()
          if !data.nil then LazyList(data) else LazyList.empty

      recur(stream)

    def decompress(stream: LazyList[Data]): LazyList[Data] =
      val inflater = juz.Inflater(false)
      val buffer: Array[Byte] = new Array(4096)

      def recur(stream: LazyList[Data]): LazyList[Data] = stream match
        case head #:: tail =>
          inflater.setInput(head.mutable(using Unsafe))
          val out = new ji.ByteArrayOutputStream()
          var count = inflater.inflate(buffer)

          while count > 0 do
            out.write(buffer, 0, count)
            count = inflater.inflate(buffer)

          val data = out.toByteArray.nn.immutable(using Unsafe)
          if !data.nil then data #:: recur(tail) else recur(tail)

        case _ =>
          val finished = inflater.finished()
          inflater.end()
          LazyList.empty

      recur(stream)

sealed trait Zlib extends Compressor
