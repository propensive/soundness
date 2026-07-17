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

import anticipation.*
import rudiments.*
import vacuous.*
import zephyrine.*

object Gzip:
  given compression: Gzip is Compression:
    def compressor()(using Buffering): (Duct[Data, Data] {
      type Transport = Credit
      type Upstream = Credit })^ =

      Deflation(gzip = true, nowrap = true)

    def decompressor()(using Buffering): (Duct[Data, Data] {
      type Transport = Credit
      type Upstream = Credit })^ =

      Inflation(gzip = true, nowrap = true)

    def compress(stream: LazyList[Data]): LazyList[Data] =
      val deflater = Deflater(-1, true)
      val crc = Crc32()
      val buffer: Array[Byte] = new Array(4096)
      val header: Data = Data(31, -117, 8, 0, 0, 0, 0, 0, 0, -1)
      var size: Long = 0

      // Drain with no forced flush, exactly as `GZIPOutputStream` would: output appears only as
      // the deflater's own buffers fill, so the byte stream is identical to the JDK's.
      def drain(): Data =
        val out = scala.collection.mutable.ArrayBuffer[Byte]()
        var count = deflater.deflate(buffer, 0, buffer.length, Flate.ZNoFlush)

        while count > 0 do
          var i = 0
          while i < count do { out += buffer(i); i += 1 }
          count = deflater.deflate(buffer, 0, buffer.length, Flate.ZNoFlush)

        val result = new Array[Byte](out.length)
        var i = 0
        while i < out.length do { result(i) = out(i); i += 1 }
        result.immutable(using Unsafe)

      def recur(stream: LazyList[Data]): LazyList[Data] = stream match
        case head #:: tail =>
          val bytes = head.mutable(using Unsafe)
          crc.update(bytes, 0, bytes.length)
          size += bytes.length
          deflater.setInput(bytes)
          val data = drain()
          if data.length > 0 then data #:: recur(tail) else recur(tail)

        case _ =>
          deflater.finish()
          val out = scala.collection.mutable.ArrayBuffer[Byte]()

          while !deflater.finished do
            val count = deflater.deflate(buffer, 0, buffer.length)
            var i = 0
            while i < count do { out += buffer(i); i += 1 }

          // CRC-32 and size trailer, little-endian
          val value = crc.value
          var index = 0

          while index < 4 do
            out += ((value >>> (index*8)) & 0xff).toByte
            index += 1

          index = 0

          while index < 4 do
            out += ((size >>> (index*8)) & 0xff).toByte
            index += 1

          val result = new Array[Byte](out.length)
          var i = 0
          while i < out.length do { result(i) = out(i); i += 1 }
          LazyList(result.immutable(using Unsafe))

      header #:: LazyList.defer(recur(stream))

    def decompress(stream: LazyList[Data]): LazyList[Data] =
      val inflater = Inflater(true)
      val buffer: Array[Byte] = new Array(4096)
      // Consume the gzip header from the start of the chunked stream, returning the number of
      // bytes consumed from this chunk: the fixed 10-byte part, then any optional extra, name,
      // comment and header-checksum fields selected by the FLG byte, in that order.
      var flags: Int = 0
      var fixedRemaining: Int = 10
      var position: Int = 0
      var extraLow: Int = -1
      var extraRemaining: Int = -1
      var checksumRemaining: Int = 2
      var headerDone: Boolean = false

      def afterExtra(): Unit =
        flags &= ~4
        if (flags & (8 | 16 | 2)) == 0 then headerDone = true

      def consumeHeader(bytes: Array[Byte], offset: Int, length: Int): Int =
        var index = 0

        while index < length && !headerDone do
          val byte = bytes(offset + index) & 0xff

          if fixedRemaining > 0 then
            if position == 3 then flags = byte
            position += 1
            fixedRemaining -= 1
            if fixedRemaining == 0 && (flags & (4 | 8 | 16 | 2)) == 0 then headerDone = true
          else if (flags & 4) != 0 then
            if extraLow == -1 then extraLow = byte
            else if extraRemaining == -1 then
              extraRemaining = (byte << 8) | extraLow
              if extraRemaining == 0 then afterExtra()
            else
              extraRemaining -= 1
              if extraRemaining == 0 then afterExtra()
          else if (flags & 8) != 0 then
            if byte == 0 then
              flags &= ~8
              if (flags & (16 | 2)) == 0 then headerDone = true
          else if (flags & 16) != 0 then
            if byte == 0 then
              flags &= ~16
              if (flags & 2) == 0 then headerDone = true
          else
            checksumRemaining -= 1
            if checksumRemaining == 0 then headerDone = true

          index += 1

        index

      def recur(stream: LazyList[Data]): LazyList[Data] = stream match
        case head #:: tail =>
          val bytes = head.mutable(using Unsafe)
          val skip = if headerDone then 0 else consumeHeader(bytes, 0, bytes.length)
          val out = scala.collection.mutable.ArrayBuffer[Byte]()

          if headerDone && skip < bytes.length && !inflater.finished then
            inflater.setInput(bytes, skip, bytes.length - skip)
            var count = inflater.inflate(buffer)

            while count > 0 do
              var i = 0
              while i < count do { out += buffer(i); i += 1 }
              count = if inflater.finished then 0 else inflater.inflate(buffer)

          if out.isEmpty then recur(tail) else
            val result = new Array[Byte](out.length)
            var i = 0
            while i < out.length do { result(i) = out(i); i += 1 }
            result.immutable(using Unsafe) #:: recur(tail)

        case _ =>
          LazyList()

      LazyList.defer(recur(stream))

sealed trait Gzip extends Compressor
