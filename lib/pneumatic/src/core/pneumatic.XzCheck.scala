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

import scala.caps

import proscenium.compat.*

import vacuous.*

// The integrity checks an XZ stream may carry over each block's uncompressed data. The stream flags
// name one type for the whole stream: 0x00 none, 0x01 CRC-32, 0x04 CRC-64, 0x0A SHA-256. We compute
// and verify none/CRC-32/CRC-64; SHA-256-checked streams still decode, but the check is skipped
// rather than verified. Check values are written little-endian in the block trailer.
private[pneumatic] object XzCheck:
  inline val None = 0x00
  inline val Crc32Type = 0x01
  inline val Crc64Type = 0x04
  inline val Sha256Type = 0x0a

  def size(checkType: Int): Int = checkType match
    case None       => 0
    case Crc32Type  => 4
    case Crc64Type  => 8
    case Sha256Type => 32
    case _          => throw IllegalStateException("the XZ data is corrupt: unknown check type")

  // A fresh running check for the given type, used by both the encoder (to emit the trailer) and
  // the decoder (to verify it). Every standard check type is supported.
  def checker(checkType: Int): XzChecker^ = checkType match
    case None       => NoChecker()
    case Crc32Type  => Crc32Checker()
    case Crc64Type  => Crc64Checker()
    case Sha256Type => Sha256Checker()
    case _          => throw IllegalStateException("the XZ data uses an unknown check type")

  def encoder(checkType: Int): XzChecker^ = checker(checkType)

private[pneumatic] object Crc64:
  val table: IArray[Long] =
    val result = Array[Long](256)
    val poly = 0xc96c5795d7870f42L
    var n = 0

    while n < 256 do
      var c = n.toLong
      var k = 8
      while k > 0 do { k -= 1; c = if (c & 1L) != 0 then (c >>> 1) ^ poly else c >>> 1 }
      result(n) = c
      n += 1

    IArray.freeze(result)

// A check that accumulates over the uncompressed bytes and yields its little-endian trailer
// bytes. Checkers are mutable running state, so each use instantiates a fresh one (including
// `NoChecker`, whose instances are trivially cheap).
private[pneumatic] trait XzChecker extends caps.Mutable:
  def size: Int
  update def absorb(buffer: scala.Array[Byte]^{caps.any.rd}, offset: Int, length: Int): Unit
  update def bytes: scala.Array[Byte]^

private[pneumatic] final class NoChecker extends XzChecker:
  def size: Int = 0
  update def absorb(buffer: scala.Array[Byte]^{caps.any.rd}, offset: Int, length: Int): Unit = ()
  update def bytes: scala.Array[Byte]^ = new scala.Array[Byte](0)

private[pneumatic] final class Crc32Checker extends XzChecker:
  private val crc = Crc32()
  def size: Int = 4

  update def absorb(buffer: scala.Array[Byte]^{caps.any.rd}, offset: Int, length: Int): Unit =
    crc.update(buffer, offset, length)

  update def bytes: scala.Array[Byte]^ =
    val value = crc.value
    val out: scala.Array[Byte]^ = new scala.Array[Byte](4)
    var i = 0
    while i < 4 do { out(i) = ((value >>> (i*8)) & 0xff).toByte; i += 1 }
    out

private[pneumatic] final class Crc64Checker extends XzChecker:
  private var v: Long = -1L
  def size: Int = 8

  update def absorb(buffer: scala.Array[Byte]^{caps.any.rd}, offset: Int, length: Int): Unit =
    var i = offset
    val end = offset + length

    while i < end do
      v = Crc64.table(((v ^ buffer(i)) & 0xff).toInt) ^ (v >>> 8)
      i += 1

  update def bytes: scala.Array[Byte]^ =
    val value = v ^ -1L
    val out: scala.Array[Byte]^ = new scala.Array[Byte](8)
    var i = 0
    while i < 8 do { out(i) = ((value >>> (i*8)) & 0xff).toByte; i += 1 }
    out

// SHA-256 (FIPS 180-4): the 64 round constants are the fractional parts of the cube roots of the
// first 64 primes; the initial hash words are the fractional parts of the square roots of the first
// eight. Big-endian digest, written to the block trailer in order.
private[pneumatic] object Sha256:
  val roundConstants: IArray[Int] =
    IArray.unsafeFromArray:
      scala.Array(
        0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1,
        0x923f82a4, 0xab1c5ed5, 0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
        0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174, 0xe49b69c1, 0xefbe4786,
        0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
        0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147,
        0x06ca6351, 0x14292967, 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
        0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85, 0xa2bfe8a1, 0xa81a664b,
        0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
        0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a,
        0x5b9cca4f, 0x682e6ff3, 0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
        0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2)

private[pneumatic] final class Sha256Checker extends XzChecker:
  private val h: scala.Array[Int]^ = new scala.Array[Int](8)
  private val block: scala.Array[Byte]^ = new scala.Array[Byte](64)
  private val w: scala.Array[Int]^ = new scala.Array[Int](64)
  private var blockLength = 0
  private var totalLength = 0L

  def size: Int = 32

  private update def reset(): Unit =
    h(0) = 0x6a09e667; h(1) = 0xbb67ae85; h(2) = 0x3c6ef372; h(3) = 0xa54ff53a
    h(4) = 0x510e527f; h(5) = 0x9b05688c; h(6) = 0x1f83d9ab; h(7) = 0x5be0cd19
    blockLength = 0
    totalLength = 0

  reset()

  private inline def rotr(x: Int, n: Int): Int = (x >>> n) | (x << (32 - n))

  private update def processBlock(): Unit =
    var i = 0

    while i < 16 do
      w(i) = ((block(i*4) & 0xff) << 24) | ((block(i*4 + 1) & 0xff) << 16) |
        ((block(i*4 + 2) & 0xff) << 8) | (block(i*4 + 3) & 0xff)

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
      val big1 = rotr(e, 6) ^ rotr(e, 11) ^ rotr(e, 25)
      val ch = (e & f) ^ ((~e) & g)
      val t1 = hh + big1 + ch + Sha256.roundConstants(i) + w(i)
      val big0 = rotr(a, 2) ^ rotr(a, 13) ^ rotr(a, 22)
      val maj = (a & b) ^ (a & c) ^ (b & c)
      val t2 = big0 + maj
      hh = g; g = f; f = e; e = d + t1; d = c; c = b; b = a; a = t1 + t2
      i += 1

    h(0) += a; h(1) += b; h(2) += c; h(3) += d
    h(4) += e; h(5) += f; h(6) += g; h(7) += hh

  update def absorb(buffer: scala.Array[Byte]^{caps.any.rd}, offset: Int, length: Int): Unit =
    var i = offset
    val end = offset + length
    totalLength += length

    while i < end do
      block(blockLength) = buffer(i)
      blockLength += 1
      if blockLength == 64 then { processBlock(); blockLength = 0 }
      i += 1

  update def bytes: scala.Array[Byte]^ =
    val bitLength = totalLength*8
    block(blockLength) = 0x80.toByte
    blockLength += 1

    if blockLength > 56 then
      while blockLength < 64 do { block(blockLength) = 0; blockLength += 1 }
      processBlock()
      blockLength = 0

    while blockLength < 56 do { block(blockLength) = 0; blockLength += 1 }

    var i = 0
    while i < 8 do { block(56 + i) = ((bitLength >>> (56 - i*8)) & 0xff).toByte; i += 1 }
    processBlock()

    val out: scala.Array[Byte]^ = new scala.Array[Byte](32)
    var j = 0

    while j < 8 do
      out(j*4) = (h(j) >>> 24).toByte
      out(j*4 + 1) = (h(j) >>> 16).toByte
      out(j*4 + 2) = (h(j) >>> 8).toByte
      out(j*4 + 3) = h(j).toByte
      j += 1

    out
