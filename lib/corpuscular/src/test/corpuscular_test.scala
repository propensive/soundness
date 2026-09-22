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
package corpuscular

import soundness.*

import strategies.throwUnsafely
import errorDiagnostics.emptyDiagnostics
import charEncoders.utf8Encoder


object Tests extends Suite(m"Corpuscular tests"):
  // The published check values for each algorithm, plus the edge cases the donor
  // implementations in pneumatic, hallucination and zeppelin had to get right: the empty
  // input, a single byte, and a segmented input whose parts must accumulate as one stream.
  def run(): Unit =
    suite(m"CRC-32"):
      test(m"The empty input checksums to zero"):
        Crc32.checksum()
      . assert(_ == 0)

      test(m"The empty segment checksums to zero"):
        Crc32.checksum(t"".in[Data])
      . assert(_ == 0)

      // The standard check value: "123456789" has CRC-32 0xcbf43926.
      test(m"The standard check vector"):
        Crc32.checksum(t"123456789".in[Data])
      . assert(_ == 0xcbf43926)

      test(m"A single byte"):
        Crc32.checksum(t"a".in[Data])
      . assert(_ == 0xe8b7be43)

      // PNG chunks and ZIP entries checksum several segments as one stream.
      test(m"Segments accumulate as a single stream"):
        Crc32.checksum(t"1234".in[Data], t"56789".in[Data])
      . assert(_ == Crc32.checksum(t"123456789".in[Data]))

      test(m"The running form agrees with the one-shot form"):
        val bytes = t"123456789".in[Data]
        val crc = Crc32()
        crc.update(bytes.unsafeMutable(using Unsafe), 0, bytes.length)
        crc.value == (Crc32.checksum(bytes).toLong & 0xffffffffL)
      . assert(_ == true)

      test(m"The running form accumulates across updates"):
        val whole = t"123456789".in[Data]
        val first = t"1234".in[Data]
        val rest = t"56789".in[Data]
        val crc = Crc32()
        crc.update(first.unsafeMutable(using Unsafe), 0, first.length)
        crc.update(rest.unsafeMutable(using Unsafe), 0, rest.length)
        crc.value == (Crc32.checksum(whole).toLong & 0xffffffffL)
      . assert(_ == true)

      test(m"Reset returns the running form to its initial value"):
        val bytes = t"123456789".in[Data]
        val crc = Crc32()
        crc.update(bytes.unsafeMutable(using Unsafe), 0, bytes.length)
        crc.reset()
        crc.value
      . assert(_ == 0L)

    suite(m"CRC-64"):
      // The ECMA-182 check value for "123456789".
      test(m"The standard check vector"):
        val bytes = t"123456789".in[Data]
        val crc = Crc64()
        crc.update(bytes.unsafeMutable(using Unsafe), 0, bytes.length)
        crc.value
      . assert(_ == 0x995dc9bbdf1939faL)

      test(m"The empty input checksums to zero"):
        Crc64().value
      . assert(_ == 0L)

      // The sliced eight-byte path must agree with the bytewise definition on every alignment,
      // so a 1000-byte input is checked whole and in every prefix length under sixteen.
      test(m"The sliced path agrees with the bytewise definition"):
        val bytes: Data = Data.fill(1000)(i => ((i*131 + (i >> 3)) & 0xff).toByte)

        def bytewise(length: Int): Long =
          var c = -1L
          var i = 0

          while i < length do
            c = Crc64.table.readable(((c ^ bytes.readable(i)) & 0xff).toInt) ^ (c >>> 8)
            i += 1

          ~c

        def sliced(length: Int): Long =
          val crc = Crc64()
          crc.update(bytes.unsafeMutable(using Unsafe), 0, length)
          crc.value

        (0 to 16).forall(n => sliced(n) == bytewise(n)) && sliced(1000) == bytewise(1000)
      . assert(_ == true)

      test(m"Segmented updates accumulate as one stream"):
        val bytes: Data = Data.fill(1000)(i => ((i*131 + (i >> 3)) & 0xff).toByte)
        val whole = Crc64()
        whole.update(bytes.unsafeMutable(using Unsafe), 0, 1000)
        val parts = Crc64()
        parts.update(bytes.unsafeMutable(using Unsafe), 0, 13)
        parts.update(bytes.unsafeMutable(using Unsafe), 13, 987)
        parts.value == whole.value
      . assert(_ == true)

    suite(m"Adler-32"):
      test(m"The empty input checksums to one"):
        Adler32().value
      . assert(_ == 1L)

      // The standard check value for "123456789".
      test(m"The standard check vector"):
        val bytes = t"123456789".in[Data]
        val adler = Adler32()
        adler.update(bytes.unsafeMutable(using Unsafe), 0, bytes.length)
        adler.value
      . assert(_ == 0x091e01deL)

      test(m"Accumulates across updates"):
        val first = t"1234".in[Data]
        val rest = t"56789".in[Data]
        val split = Adler32()
        split.update(first.unsafeMutable(using Unsafe), 0, first.length)
        split.update(rest.unsafeMutable(using Unsafe), 0, rest.length)
        val whole = t"123456789".in[Data]
        val once = Adler32()
        once.update(whole.unsafeMutable(using Unsafe), 0, whole.length)
        split.value == once.value
      . assert(_ == true)
