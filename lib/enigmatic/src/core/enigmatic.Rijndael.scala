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

// AES (FIPS 197) as a bare block function, written in Scala so that the `SoundnessCrypto`
// provider can run wherever Scala does: Scala.js and WASI have no `javax.crypto`, and Scala
// Native only has it through OpenSSL. One instance holds the expanded key schedule for one key
// (128, 192 or 256 bits) and transforms single 16-byte blocks between caller's buffers; the
// modes of operation live in `SoundnessBlockCipher`, GCM in `Gcm`.
//
// The state is the byte-oriented one of the standard — sixteen bytes, column-major, so byte `i`
// of a block is state row `i % 4`, column `i / 4` — and the S-boxes are computed at start-up from
// the field arithmetic rather than tabulated. This is a straightforward, not a T-table,
// implementation, and like every software AES without hardware support it is not immune to
// cache-timing attacks: see the caveat on `SoundnessCrypto`.
//
// Every loop is a recursion whose exclusive arrays are parameters, not captures: separation
// checking forbids a local function closing over a fresh exclusive array.
private[enigmatic] object Rijndael:
  val BlockSize: Int = 16

  // Multiplication in GF(2^8) modulo the AES polynomial x^8 + x^4 + x^3 + x + 1 (0x11b), by
  // shift-and-add over the eight bits of `b`.
  private def multiply(a: Int, b: Int, product: Int = 0): Int =
    if b == 0 then product
    else multiply(xtime(a), b >>> 1, if (b & 1) != 0 then product ^ a else product)

  private def xtime(a: Int): Int = ((a << 1) ^ (if (a & 0x80) != 0 then 0x1b else 0)) & 0xff

  private def power(base: Int, exponent: Int, result: Int = 1): Int =
    if exponent == 0 then result
    else
      val next = if (exponent & 1) != 0 then multiply(result, base) else result
      power(multiply(base, base), exponent >>> 1, next)

  private def rotateLeft(value: Int, bits: Int): Int =
    ((value << bits) | (value >>> (8 - bits))) & 0xff

  // The forward S-box: the multiplicative inverse (as x^254, since x^255 = 1 for x ≠ 0, and 0
  // maps to 0) followed by the affine transformation; the inverse S-box is its inversion.
  private def tabulate(forward: Array[Int]^, backward: Array[Int]^, index: Int): Unit =
    if index < 256 then
      val inverse = power(index, 254)

      val substituted =
        inverse ^ rotateLeft(inverse, 1) ^ rotateLeft(inverse, 2) ^ rotateLeft(inverse, 3) ^
          rotateLeft(inverse, 4) ^ 0x63

      forward(index) = substituted
      backward(substituted) = index
      tabulate(forward, backward, index + 1)

  private val boxes: (Array[Int]^{}, Array[Int]^{}) =
    val forward = Array.allocate[Int](256)
    val backward = Array.allocate[Int](256)
    tabulate(forward, backward, 0)
    (Array.freeze(forward), Array.freeze(backward))

  private val sbox: Array[Int]^{} = boxes(0)
  private val inverseSbox: Array[Int]^{} = boxes(1)

  // The expanded key schedule, as `(rounds + 1)*16` bytes: FIPS 197 §5.2. `RotWord` then
  // `SubWord` then the round constant at every `words`th word; `SubWord` alone at the fourth
  // word of each 256-bit key schedule.
  private def expand
    ( schedule: scala.Array[Byte]^,
      box:      scala.Array[Int],
      words:    Int,
      rounds:   Int,
      word:     Int,
      rcon:     Int )
  :   Unit =

    if word < (rounds + 1)*4 then
      val previous = (word - 1)*4
      val t0 = schedule(previous) & 0xff
      val t1 = schedule(previous + 1) & 0xff
      val t2 = schedule(previous + 2) & 0xff
      val t3 = schedule(previous + 3) & 0xff

      val (u0, u1, u2, u3, nextRcon) =
        if word%words == 0 then (box(t1) ^ rcon, box(t2), box(t3), box(t0), xtime(rcon))
        else if words > 6 && word%words == 4 then (box(t0), box(t1), box(t2), box(t3), rcon)
        else (t0, t1, t2, t3, rcon)

      val base = (word - words)*4
      val target = word*4
      schedule(target) = (schedule(base) ^ u0).toByte
      schedule(target + 1) = (schedule(base + 1) ^ u1).toByte
      schedule(target + 2) = (schedule(base + 2) ^ u2).toByte
      schedule(target + 3) = (schedule(base + 3) ^ u3).toByte
      expand(schedule, box, words, rounds, word + 1, nextRcon)

  private def schedule(key: scala.Array[Byte]): Data =
    val words = key.length/4
    val rounds = words + 6
    val schedule = Array.allocate[Byte]((rounds + 1)*16)
    java.lang.System.arraycopy(key, 0, schedule.raw, 0, key.length)
    // [java-boundary] a read-only view of the S-box, computed once at start-up
    expand(schedule.raw, Array.unsafeJvm(sbox), words, rounds, words, 1)
    Array.freeze(schedule)

  def apply(key: Data): Rijndael =
    if key.length != 16 && key.length != 24 && key.length != 32
    then CryptoFailure.invalidKey("an AES key must be 16, 24 or 32 bytes")

    // [java-boundary] a read-only view of the key, copied into the schedule and never written
    new Rijndael(key.length/4 + 6, schedule(Array.unsafeJvm(key)))

  private def addRoundKey
    ( state: scala.Array[Byte]^, schedule: scala.Array[Byte], round: Int, index: Int )
  :   Unit =

    if index < 16 then
      state(index) = (state(index) ^ schedule(round*16 + index)).toByte
      addRoundKey(state, schedule, round, index + 1)

  // `SubBytes` and `ShiftRows` together: byte `(row, column)` of the new state is the
  // substitution of byte `(row, column + row)` of the old, so one pass from a copy does both. The
  // inverse shifts the other way and uses the inverse box.
  private def substituteAndShift
    ( state:   scala.Array[Byte]^,
      copy:    scala.Array[Byte],
      box:     scala.Array[Int],
      inverse: Boolean,
      index:   Int )
  :   Unit =

    if index < 16 then
      val row = index%4
      val column = index/4
      val source = if inverse then (column - row + 4)%4 else (column + row)%4
      state(index) = box(copy(source*4 + row) & 0xff).toByte
      substituteAndShift(state, copy, box, inverse, index + 1)

  private def mixColumns(state: scala.Array[Byte]^, column: Int): Unit =
    if column < 4 then
      val offset = column*4
      val a0 = state(offset) & 0xff
      val a1 = state(offset + 1) & 0xff
      val a2 = state(offset + 2) & 0xff
      val a3 = state(offset + 3) & 0xff
      state(offset) = (xtime(a0) ^ (xtime(a1) ^ a1) ^ a2 ^ a3).toByte
      state(offset + 1) = (a0 ^ xtime(a1) ^ (xtime(a2) ^ a2) ^ a3).toByte
      state(offset + 2) = (a0 ^ a1 ^ xtime(a2) ^ (xtime(a3) ^ a3)).toByte
      state(offset + 3) = ((xtime(a0) ^ a0) ^ a1 ^ a2 ^ xtime(a3)).toByte
      mixColumns(state, column + 1)

  private def inverseMixColumns(state: scala.Array[Byte]^, column: Int): Unit =
    if column < 4 then
      val offset = column*4
      val a0 = state(offset) & 0xff
      val a1 = state(offset + 1) & 0xff
      val a2 = state(offset + 2) & 0xff
      val a3 = state(offset + 3) & 0xff
      val b0 = multiply(a0, 14) ^ multiply(a1, 11) ^ multiply(a2, 13) ^ multiply(a3, 9)
      val b1 = multiply(a0, 9) ^ multiply(a1, 14) ^ multiply(a2, 11) ^ multiply(a3, 13)
      val b2 = multiply(a0, 13) ^ multiply(a1, 9) ^ multiply(a2, 14) ^ multiply(a3, 11)
      val b3 = multiply(a0, 11) ^ multiply(a1, 13) ^ multiply(a2, 9) ^ multiply(a3, 14)
      state(offset) = b0.toByte
      state(offset + 1) = b1.toByte
      state(offset + 2) = b2.toByte
      state(offset + 3) = b3.toByte
      inverseMixColumns(state, column + 1)

  private def encryptRounds
    ( state:    scala.Array[Byte]^,
      copy:     scala.Array[Byte]^,
      schedule: scala.Array[Byte],
      box:      scala.Array[Int],
      round: Int, rounds: Int )
  :   Unit =

    if round < rounds then
      java.lang.System.arraycopy(state, 0, copy, 0, 16)
      substituteAndShift(state, copy, box, false, 0)
      mixColumns(state, 0)
      addRoundKey(state, schedule, round, 0)
      encryptRounds(state, copy, schedule, box, round + 1, rounds)

  private def decryptRounds
    ( state:    scala.Array[Byte]^,
      copy:     scala.Array[Byte]^,
      schedule: scala.Array[Byte],
      box:      scala.Array[Int],
      round: Int )
  :   Unit =

    if round > 0 then
      java.lang.System.arraycopy(state, 0, copy, 0, 16)
      substituteAndShift(state, copy, box, true, 0)
      addRoundKey(state, schedule, round, 0)
      inverseMixColumns(state, 0)
      decryptRounds(state, copy, schedule, box, round - 1)

private[enigmatic] final class Rijndael private (rounds: Int, expanded: Data):
  import Rijndael.*

  // Encrypts the block at `offset` in `input` into `output` at `target`, via a scratch state, so
  // the two may be different arrays or the same one.
  def encrypt(input: scala.Array[Byte], offset: Int, output: scala.Array[Byte]^, target: Int)
  :   Unit =

    // [java-boundary] read-only views of the frozen key schedule and S-box
    val schedule = Array.unsafeJvm(expanded)
    val box = Array.unsafeJvm(sbox)
    val state: scala.Array[Byte]^ = Array.scratch[Byte](16)
    val copy: scala.Array[Byte]^ = Array.scratch[Byte](16)
    java.lang.System.arraycopy(input, offset, state, 0, 16)
    addRoundKey(state, schedule, 0, 0)
    encryptRounds(state, copy, schedule, box, 1, rounds)
    java.lang.System.arraycopy(state, 0, copy, 0, 16)
    substituteAndShift(state, copy, box, false, 0)
    addRoundKey(state, schedule, rounds, 0)
    java.lang.System.arraycopy(state, 0, output, target, 16)

  def decrypt(input: scala.Array[Byte], offset: Int, output: scala.Array[Byte]^, target: Int)
  :   Unit =

    // [java-boundary] read-only views of the frozen key schedule and inverse S-box
    val schedule = Array.unsafeJvm(expanded)
    val box = Array.unsafeJvm(inverseSbox)
    val state: scala.Array[Byte]^ = Array.scratch[Byte](16)
    val copy: scala.Array[Byte]^ = Array.scratch[Byte](16)
    java.lang.System.arraycopy(input, offset, state, 0, 16)
    addRoundKey(state, schedule, rounds, 0)
    decryptRounds(state, copy, schedule, box, rounds - 1)
    java.lang.System.arraycopy(state, 0, copy, 0, 16)
    substituteAndShift(state, copy, box, true, 0)
    addRoundKey(state, schedule, 0, 0)
    java.lang.System.arraycopy(state, 0, output, target, 16)
