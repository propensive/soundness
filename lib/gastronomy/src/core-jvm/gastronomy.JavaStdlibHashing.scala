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
package gastronomy

import scala.caps

import java.security as js
import java.util.zip as juz

import anticipation.*
import corpuscular.*
import gossamer.*

// The default hashing provider, backed by the JDK: `MessageDigest` for the
// cryptographic hashes and `java.util.zip.CRC32` for the checksum. This is the
// single home of `java.security`/`java.util.zip` usage in gastronomy. It does not
// offer BLAKE3 (the JDK has no implementation) — use the Soundness provider.
object JavaStdlibHashing extends Hashing:
  def md5:  Hashing.Function = messageDigest(t"MD5")
  def sha1: Hashing.Function = messageDigest(t"SHA1")
  def sha2(bits: Int): Hashing.Function = messageDigest(t"SHA-$bits")

  // The JDK has `java.util.zip` CRC-32 and Adler-32 (both intrinsified) but no CRC-64, so this
  // provider simply does not offer `crc64`; the structural refinement means a CRC-64 digest
  // resolves only under the Soundness provider, exactly as BLAKE3 does.
  def adler32: Hashing.Function = new Hashing.Function:
    def digestion(): Digestion^ = new Digestion:
      private val state: juz.Adler32 = juz.Adler32()
      update def append(bytes: Data): Unit = state.update(Array.unsafeJvm(bytes))

      override update def append(array: Array[Byte]^{caps.any.rd}, start: Int, count: Int): Unit =
        state.update(Array.unsafeJvm(array), start, count)

      update def digest(): Data =
        val v = state.getValue
        Array(((v >>> 24) & 0xff).toByte, ((v >>> 16) & 0xff).toByte, ((v >>> 8) & 0xff).toByte,
              (v & 0xff).toByte)

  def crc32: Hashing.Function = new Hashing.Function:
    def digestion(): Digestion^ = new Digestion:
      private val state: juz.CRC32 = juz.CRC32()
      update def append(bytes: Data): Unit = state.update(Array.unsafeJvm(bytes))

      override update def append(array: Array[Byte]^{caps.any.rd}, start: Int, count: Int): Unit =
        state.update(Array.unsafeJvm(array), start, count)

      update def digest(): Data =
        val value = state.getValue()
        Array[Byte]((value >> 24).toByte, (value >> 16).toByte, (value >> 8).toByte, value.toByte)

  private def messageDigest(name: Text): Hashing.Function = new Hashing.Function:
    def digestion(): Digestion^ = new Digestion:
      private val md: js.MessageDigest = js.MessageDigest.getInstance(name.s).nn
      update def append(bytes: Data): Unit = md.update(Array.unsafeJvm(bytes))

      override update def append(array: Array[Byte]^{caps.any.rd}, start: Int, count: Int): Unit =
        md.update(Array.unsafeJvm(array), start, count)

      update def digest(): Data = Array.unsafeFrozen(md.digest.nn)
