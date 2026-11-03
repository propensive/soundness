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
package enigmatic

import anticipation.*

// Poly1305 (RFC 8439 §2.5), the one-time authenticator: a 16-byte tag over a message under a
// 32-byte key that must be used once, which is why a protocol derives it from a keystream per
// message. It is written here in Scala rather than taken from a provider because the JDK offers
// it only fused with ChaCha20, and OpenSSH's `chacha20-poly1305@openssh.com` needs it alone.
//
// The arithmetic is the 26-bit-limb schoolbook of poly1305-donna: the accumulator and `r` are
// five limbs of 26 bits, so a product of two fits a `Long` with room for the five-term sums, and
// the multiplier `r` is clamped at the key as the RFC requires. Everything runs in constant time
// with respect to the message content, as an authenticator must; the only data-dependent branch
// is the length.
object Poly1305:
  private val Mask: Long = 0x3ffffffL

  def mac(key: Data, message: Data): Data =
    val k = Array.unsafeJvm(key)
    val m = Array.unsafeJvm(message)

    // `r` clamped: the 128-bit `r` is read in 26-bit limbs at bit offsets 0, 26, 52, 78 and 104,
    // with the clamp (`0ffffffc0ffffffc0ffffffc0fffffff`) folded into each limb's mask.
    val r0 = le32(k, 0) & 0x3ffffffL
    val r1 = (le32(k, 3) >>> 2) & 0x3ffff03L
    val r2 = (le32(k, 6) >>> 4) & 0x3ffc0ffL
    val r3 = (le32(k, 9) >>> 6) & 0x3f03fffL
    val r4 = (le32(k, 12) >>> 8) & 0x00fffffL
    val s1 = r1*5
    val s2 = r2*5
    val s3 = r3*5
    val s4 = r4*5

    // One block: `h += block; h *= r; h %= p` (lazily reduced). A partial final block is padded
    // with a single 1 bit then zeros, so its high bit is in the padding rather than at 2^128.
    def absorb(h0: Long, h1: Long, h2: Long, h3: Long, h4: Long, offset: Int)
    :   (Long, Long, Long, Long, Long) =

      if offset >= m.length then (h0, h1, h2, h3, h4) else
        val remaining = m.length - offset
        val block = Array.scratch[Byte](17)

        if remaining >= 16 then
          java.lang.System.arraycopy(m, offset, block, 0, 16)
          block(16) = 1
        else
          java.lang.System.arraycopy(m, offset, block, 0, remaining)
          block(remaining) = 1

        val a0 = h0 + (le32(block, 0) & Mask)
        val a1 = h1 + ((le32(block, 3) >>> 2) & Mask)
        val a2 = h2 + ((le32(block, 6) >>> 4) & Mask)
        val a3 = h3 + ((le32(block, 9) >>> 6) & Mask)
        val a4 = h4 + ((le32(block, 12) >>> 8) | (block(16).toLong << 24))

        val d0 = a0*r0 + a1*s4 + a2*s3 + a3*s2 + a4*s1
        val d1 = a0*r1 + a1*r0 + a2*s4 + a3*s3 + a4*s2
        val d2 = a0*r2 + a1*r1 + a2*r0 + a3*s4 + a4*s3
        val d3 = a0*r3 + a1*r2 + a2*r1 + a3*r0 + a4*s4
        val d4 = a0*r4 + a1*r3 + a2*r2 + a3*r1 + a4*r0

        val e1 = d1 + (d0 >>> 26)
        val e2 = d2 + (e1 >>> 26)
        val e3 = d3 + (e2 >>> 26)
        val e4 = d4 + (e3 >>> 26)
        val f0 = (d0 & Mask) + (e4 >>> 26)*5
        val f1 = (e1 & Mask) + (f0 >>> 26)

        absorb(f0 & Mask, f1, e2 & Mask, e3 & Mask, e4 & Mask, offset + 16)

    val (h0, h1, h2, h3, h4) = absorb(0, 0, 0, 0, 0, 0)

    // Full carry, then `h mod p` by computing `g = h + 5 - 2^130` and taking it when it does not
    // borrow — selected by mask, not by branch.
    val c1 = h1 >>> 26
    val i1 = h1 & Mask
    val i2 = (h2 + c1)
    val c2 = i2 >>> 26
    val j2 = i2 & Mask
    val i3 = h3 + c2
    val c3 = i3 >>> 26
    val j3 = i3 & Mask
    val i4 = h4 + c3
    val c4 = i4 >>> 26
    val j4 = i4 & Mask
    val i0 = h0 + c4*5
    val c0 = i0 >>> 26
    val j0 = i0 & Mask
    val j1 = i1 + c0

    val g0 = j0 + 5
    val g1 = j1 + (g0 >>> 26)
    val g2 = j2 + (g1 >>> 26)
    val g3 = j3 + (g2 >>> 26)
    val g4 = j4 + (g3 >>> 26) - (1L << 26)

    // `g4` is negative exactly when `h < p`; the arithmetic shift spreads its sign over the mask.
    val take = (g4 >> 63) ^ -1L
    val keep = ~take
    val k0 = (j0 & keep) | (g0 & Mask & take)
    val k1 = (j1 & keep) | (g1 & Mask & take)
    val k2 = (j2 & keep) | (g2 & Mask & take)
    val k3 = (j3 & keep) | (g3 & Mask & take)
    val k4 = (j4 & keep) | (g4 & Mask & take)

    // Back to 32-bit words, then `(h + s) mod 2^128` with `s` the key's second half.
    val w0 = (k0 | (k1 << 26)) & 0xffffffffL
    val w1 = ((k1 >>> 6) | (k2 << 20)) & 0xffffffffL
    val w2 = ((k2 >>> 12) | (k3 << 14)) & 0xffffffffL
    val w3 = ((k3 >>> 18) | (k4 << 8)) & 0xffffffffL

    val t0 = w0 + le32(k, 16)
    val t1 = w1 + le32(k, 20) + (t0 >>> 32)
    val t2 = w2 + le32(k, 24) + (t1 >>> 32)
    val t3 = w3 + le32(k, 28) + (t2 >>> 32)

    val tag = Array.allocate[Byte](16)
    put32(tag.raw, 0, t0)
    put32(tag.raw, 4, t1)
    put32(tag.raw, 8, t2)
    put32(tag.raw, 12, t3)
    Array.freeze(tag)

  // A little-endian 32-bit word at `offset`, as an unsigned `Long`.
  private def le32(bytes: scala.Array[Byte], offset: Int): Long =
    (bytes(offset) & 0xffL) |
      ((bytes(offset + 1) & 0xffL) << 8) |
      ((bytes(offset + 2) & 0xffL) << 16) |
      ((bytes(offset + 3) & 0xffL) << 24)

  private def put32(bytes: scala.Array[Byte]^, offset: Int, value: Long): Unit =
    bytes(offset) = value.toByte
    bytes(offset + 1) = (value >>> 8).toByte
    bytes(offset + 2) = (value >>> 16).toByte
    bytes(offset + 3) = (value >>> 24).toByte
