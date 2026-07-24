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

import anticipation.*
import rudiments.*
import vacuous.*

// Pure-Scala implementations of the MD5, SHA-1 and SHA-2 hash families and the CRC-32 checksum,
// so that hashing works on every platform (Scala.js and WASI, where `java.security.MessageDigest`
// does not exist) and not only the JVM. They follow FIPS 180-4 (SHA), RFC 1321 (MD5) and RFC 1952
// (CRC-32), and are validated byte-for-byte against `MessageDigest`/`java.util.zip.CRC32` and the
// NIST test vectors. On the JVM the JDK's native implementations remain the default.
private[gastronomy] object PureHashes:
  import HashConstants.*

  // SHA-256 and SHA-224 (a truncation of SHA-256 with a different initial state).
  final class Sha256(initial: Array[Int], outputBytes: Int) extends BlockDigestion(64):
    private val h: Array[Int] = initial.clone
    private val w: Array[Int] = new Array[Int](64)

    protected def bitLengthBytes: Int = 8

    protected def writeLength(target: Array[Byte], offset: Int, bits: Long): Unit =
      var i = 0
      while i < 8 do { target(offset + i) = (bits >>> ((7 - i)*8)).toByte; i += 1 }

    protected def compress(data: Array[Byte], start: Int): Unit =
      var i = 0

      while i < 16 do
        w(i) = ((data(start + i*4) & 0xff) << 24) | ((data(start + i*4 + 1) & 0xff) << 16) |
          ((data(start + i*4 + 2) & 0xff) << 8) | (data(start + i*4 + 3) & 0xff)

        i += 1

      while i < 64 do
        val s0 = rotr(w(i - 15), 7) ^ rotr(w(i - 15), 18) ^ (w(i - 15) >>> 3)
        val s1 = rotr(w(i - 2), 17) ^ rotr(w(i - 2), 19) ^ (w(i - 2) >>> 10)
        w(i) = w(i - 16) + s0 + w(i - 7) + s1
        i += 1

      var a = h(0); var b = h(1); var c = h(2); var d = h(3)
      var e = h(4); var f = h(5); var g = h(6); var hh = h(7)
      i = 0

      while i < 64 do
        val s1 = rotr(e, 6) ^ rotr(e, 11) ^ rotr(e, 25)
        val ch = (e & f) ^ (~e & g)
        val t1 = hh + s1 + ch + sha256K(i) + w(i)
        val s0 = rotr(a, 2) ^ rotr(a, 13) ^ rotr(a, 22)
        val maj = (a & b) ^ (a & c) ^ (b & c)
        val t2 = s0 + maj
        hh = g; g = f; f = e; e = d + t1; d = c; c = b; b = a; a = t1 + t2
        i += 1

      h(0) += a; h(1) += b; h(2) += c; h(3) += d
      h(4) += e; h(5) += f; h(6) += g; h(7) += hh

    protected def result(): Array[Byte] =
      val out = new Array[Byte](outputBytes)
      var i = 0

      while i < outputBytes do { out(i) = (h(i/4) >>> ((3 - i%4)*8)).toByte; i += 1 }

      out

  // SHA-512 and SHA-384 (a truncation of SHA-512 with a different initial state).
  final class Sha512(initial: Array[Long], outputBytes: Int) extends BlockDigestion(128):
    private val h: Array[Long] = initial.clone
    private val w: Array[Long] = new Array[Long](80)

    // The message length is a 128-bit big-endian count of bits; inputs never approach 2^64 bytes,
    // so the high 64 bits are always zero.
    protected def bitLengthBytes: Int = 16

    protected def writeLength(target: Array[Byte], offset: Int, bits: Long): Unit =
      var i = 0
      while i < 8 do { target(offset + i) = 0; i += 1 }
      while i < 16 do { target(offset + i) = (bits >>> ((15 - i)*8)).toByte; i += 1 }

    protected def compress(data: Array[Byte], start: Int): Unit =
      var i = 0

      while i < 16 do
        var word = 0L
        var j = 0
        while j < 8 do { word = (word << 8) | (data(start + i*8 + j) & 0xffL); j += 1 }
        w(i) = word
        i += 1

      while i < 80 do
        val s0 = rotrL(w(i - 15), 1) ^ rotrL(w(i - 15), 8) ^ (w(i - 15) >>> 7)
        val s1 = rotrL(w(i - 2), 19) ^ rotrL(w(i - 2), 61) ^ (w(i - 2) >>> 6)
        w(i) = w(i - 16) + s0 + w(i - 7) + s1
        i += 1

      var a = h(0); var b = h(1); var c = h(2); var d = h(3)
      var e = h(4); var f = h(5); var g = h(6); var hh = h(7)
      i = 0

      while i < 80 do
        val s1 = rotrL(e, 14) ^ rotrL(e, 18) ^ rotrL(e, 41)
        val ch = (e & f) ^ (~e & g)
        val t1 = hh + s1 + ch + sha512K(i) + w(i)
        val s0 = rotrL(a, 28) ^ rotrL(a, 34) ^ rotrL(a, 39)
        val maj = (a & b) ^ (a & c) ^ (b & c)
        val t2 = s0 + maj
        hh = g; g = f; f = e; e = d + t1; d = c; c = b; b = a; a = t1 + t2
        i += 1

      h(0) += a; h(1) += b; h(2) += c; h(3) += d
      h(4) += e; h(5) += f; h(6) += g; h(7) += hh

    protected def result(): Array[Byte] =
      val out = new Array[Byte](outputBytes)
      var i = 0

      while i < outputBytes do { out(i) = (h(i/8) >>> ((7 - i%8)*8)).toByte; i += 1 }

      out

  // SHA-1 (RFC 3174).
  final class Sha1 extends BlockDigestion(64):
    private var h0 = 0x67452301; private var h1 = 0xefcdab89; private var h2 = 0x98badcfe
    private var h3 = 0x10325476; private var h4 = 0xc3d2e1f0
    private val w: Array[Int] = new Array[Int](80)

    protected def bitLengthBytes: Int = 8

    protected def writeLength(target: Array[Byte], offset: Int, bits: Long): Unit =
      var i = 0
      while i < 8 do { target(offset + i) = (bits >>> ((7 - i)*8)).toByte; i += 1 }

    protected def compress(data: Array[Byte], start: Int): Unit =
      var i = 0

      while i < 16 do
        w(i) = ((data(start + i*4) & 0xff) << 24) | ((data(start + i*4 + 1) & 0xff) << 16) |
          ((data(start + i*4 + 2) & 0xff) << 8) | (data(start + i*4 + 3) & 0xff)

        i += 1

      while i < 80 do { w(i) = rotl(w(i - 3) ^ w(i - 8) ^ w(i - 14) ^ w(i - 16), 1); i += 1 }

      var a = h0; var b = h1; var c = h2; var d = h3; var e = h4
      i = 0

      while i < 80 do
        val (f, k) =
          if i < 20 then ((b & c) | (~b & d), 0x5a827999)
          else if i < 40 then (b ^ c ^ d, 0x6ed9eba1)
          else if i < 60 then ((b & c) | (b & d) | (c & d), 0x8f1bbcdc)
          else (b ^ c ^ d, 0xca62c1d6)

        val t = rotl(a, 5) + f + e + k + w(i)
        e = d; d = c; c = rotl(b, 30); b = a; a = t
        i += 1

      h0 += a; h1 += b; h2 += c; h3 += d; h4 += e

    protected def result(): Array[Byte] =
      val h = Array(h0, h1, h2, h3, h4)
      val out = new Array[Byte](20)
      var i = 0

      while i < 20 do { out(i) = (h(i/4) >>> ((3 - i%4)*8)).toByte; i += 1 }

      out

  // MD5 (RFC 1321). Little-endian throughout, unlike the SHA family.
  final class Md5 extends BlockDigestion(64):
    private var a0 = 0x67452301; private var b0 = 0xefcdab89
    private var c0 = 0x98badcfe; private var d0 = 0x10325476
    private val m: Array[Int] = new Array[Int](16)

    private val shifts: Array[Int] = Array(
      7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22,
      5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20,
      4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23,
      6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21)

    protected def bitLengthBytes: Int = 8

    // The bit-length trailer is little-endian for MD5.
    protected def writeLength(target: Array[Byte], offset: Int, bits: Long): Unit =
      var i = 0
      while i < 8 do { target(offset + i) = (bits >>> (i*8)).toByte; i += 1 }

    protected def compress(data: Array[Byte], start: Int): Unit =
      var i = 0

      while i < 16 do
        m(i) = (data(start + i*4) & 0xff) | ((data(start + i*4 + 1) & 0xff) << 8) |
          ((data(start + i*4 + 2) & 0xff) << 16) | ((data(start + i*4 + 3) & 0xff) << 24)

        i += 1

      var a = a0; var b = b0; var c = c0; var d = d0
      i = 0

      while i < 64 do
        val (f, g) =
          if i < 16 then ((b & c) | (~b & d), i)
          else if i < 32 then ((d & b) | (~d & c), (5*i + 1)%16)
          else if i < 48 then (b ^ c ^ d, (3*i + 5)%16)
          else (c ^ (b | ~d), (7*i)%16)

        val tmp = d
        d = c; c = b
        b = b + rotl(a + f + md5T(i) + m(g), shifts(i))
        a = tmp
        i += 1

      a0 += a; b0 += b; c0 += c; d0 += d

    protected def result(): Array[Byte] =
      val h = Array(a0, b0, c0, d0)
      val out = new Array[Byte](16)
      var i = 0

      while i < 16 do { out(i) = (h(i/4) >>> ((i%4)*8)).toByte; i += 1 }

      out

  object Crc32:
    val table: Array[Int] =
      val result = new Array[Int](256)
      var n = 0

      while n < 256 do
        var c = n
        var k = 8
        while k > 0 do { k -= 1; c = if (c & 1) != 0 then 0xedb88320 ^ (c >>> 1) else c >>> 1 }
        result(n) = c
        n += 1

      result

  // CRC-32 (RFC 1952), the checksum used by gzip and zip.
  final class Crc32 extends Digestion:
    private var value: Int = 0

    def append(bytes: Data): Unit = append(bytes.mutable(using Unsafe), 0, bytes.length)

    override def append(data: Array[Byte], start: Int, count: Int): Unit =
      val end = start + count
      var c = ~value
      var i = start
      while i < end do { c = Crc32.table((c ^ data(i)) & 0xff) ^ (c >>> 8); i += 1 }
      value = ~c

    def digest(): Data =
      IArray[Byte]((value >>> 24).toByte, (value >>> 16).toByte, (value >>> 8).toByte, value.toByte)

  private def rotr(x: Int, n: Int): Int = (x >>> n) | (x << (32 - n))
  private def rotl(x: Int, n: Int): Int = (x << n) | (x >>> (32 - n))
  private def rotrL(x: Long, n: Int): Long = (x >>> n) | (x << (64 - n))

  def sha1: Digestion = Sha1()
  def md5: Digestion = Md5()
  def crc32: Digestion = Crc32()

  def sha2(bits: Int): Digestion = bits match
    case 224 => Sha256(sha224H, 28)
    case 256 => Sha256(sha256H, 32)
    case 384 => Sha512(sha384H, 48)
    case _   => Sha512(sha512H, 64)

// A block-oriented incremental hash: buffers input into `blockSize`-byte blocks, running
// `compress` on each, and applies the standard Merkle–Damgård padding (a `0x80` byte, zero
// padding, then the message bit-length) on `digest`.
private[gastronomy] abstract class BlockDigestion(blockSize: Int) extends Digestion:
  private val block: Array[Byte] = new Array[Byte](blockSize)
  private var filled: Int = 0
  private var totalBytes: Long = 0

  protected def compress(data: Array[Byte], start: Int): Unit
  protected def result(): Array[Byte]
  protected def bitLengthBytes: Int
  protected def writeLength(target: Array[Byte], offset: Int, bits: Long): Unit

  def append(bytes: Data): Unit = append(bytes.mutable(using Unsafe), 0, bytes.length)

  override def append(data: Array[Byte], start: Int, count: Int): Unit =
    var offset = start
    val end = start + count
    totalBytes += count

    // Complete a partially-filled block first.
    if filled > 0 then
      val take = Math.min(blockSize - filled, end - offset)
      System.arraycopy(data, offset, block, filled, take)
      filled += take
      offset += take
      if filled == blockSize then { compress(block, 0); filled = 0 }

    // Process whole blocks straight from the input.
    while end - offset >= blockSize do
      compress(data, offset)
      offset += blockSize

    // Retain the remainder.
    if offset < end then
      System.arraycopy(data, offset, block, 0, end - offset)
      filled = end - offset

  def digest(): Data =
    val bits = totalBytes*8

    // The 0x80 marker and the length trailer need `filled + 1 + bitLengthBytes` bytes; if that
    // does not fit in the final block, the padding spills into a second block. The remaining
    // bytes stay zero (a freshly-allocated array).
    val twoBlocks = filled + 1 + bitLengthBytes > blockSize
    val padded = if twoBlocks then blockSize*2 else blockSize
    val pad = new Array[Byte](padded)
    var i = 0

    while i < filled do { pad(i) = block(i); i += 1 }
    pad(filled) = 0x80.toByte
    writeLength(pad, padded - bitLengthBytes, bits)

    compress(pad, 0)
    if twoBlocks then compress(pad, blockSize)

    result().immutable(using Unsafe)
