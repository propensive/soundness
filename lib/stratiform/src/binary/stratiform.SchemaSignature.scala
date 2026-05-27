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
┃    Soundness, version 0.54.0.                                                                    ┃
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
package stratiform

import scala.language.unsafeNulls

import anticipation.*
import contingency.*

// §8.2 of the BinTEL spec — palimpsest schema-signature construction
// at byte cadence k = 2. Given n 32-byte component hashes, the signature
// is a `30 + 2n`-byte sequence that uniquely identifies the ordered
// component sequence (under the assumption that no two candidate
// component hashes share the same final 16 bits).

object SchemaSignature:

  // The constant component-hash size in bytes (SHA-256 width).
  final val HashSize: Int = 32

  // §8.2 encoding. Given an ordered sequence of n component hashes
  // (each `HashSize` bytes), compute S = 0 then for each h_i in order
  // S = (S << 16) XOR h_i, and emit S as `30 + 2n` bytes big-endian.
  def encode(hashes: List[Data]): Data raises BintelError =
    if hashes.isEmpty then abort(BintelError(BintelError.Reason.BadSignatureLength))

    var bad = false
    val it = hashes.iterator

    while it.hasNext && !bad do if it.next().length != HashSize then bad = true
    if bad then abort(BintelError(BintelError.Reason.BadSignatureLength))

    val n = hashes.length
    val outputBits = 256 + (n - 1) * 16
    val outputBytes = (outputBits + 7) / 8

    var acc: BigInt = BigInt(0)
    hashes.foreach: h =>
      acc = (acc << 16) ^ bytesToBigInt(h)

    val raw = acc.toByteArray
    val out = new Array[Byte](outputBytes)

    if raw.length >= outputBytes then
      System.arraycopy(raw, raw.length - outputBytes, out, 0, outputBytes)
    else
      System.arraycopy(raw, 0, out, outputBytes - raw.length, raw.length)

    out.asInstanceOf[IArray[Byte]]

  // §8.2 decoding. Given a signature of length L = `30 + 2n` for some
  // n ≥ 1 and a library of candidate component hashes, recover the
  // ordered sequence (h_0, …, h_{n-1}). The library MUST cover every
  // component used by the signature, otherwise raises `BadSignature`.
  // For a malformed length raises `BadSignatureLength`.
  def decode(signature: Data, library: List[Data]): List[Data] raises BintelError =
    val L = signature.length
    if L < 32 || (L - 30) % 2 != 0
    then abort(BintelError(BintelError.Reason.BadSignatureLength))

    val n = (L - 30) / 2
    val s0 = bytesToBigInt(signature)

    // Build a quick lookup table keyed by the lowest 16 bits of each
    // candidate hash. We only need the last 16 bits of each hash to
    // identify it as the next component.
    val byLowBits =
      library.groupBy: h =>
        ((h(h.length - 2) & 0xff) << 8) | (h(h.length - 1) & 0xff)

    // Backwards-decode by repeatedly peeling off the trailing 16 bits.
    // Per the spec, a unique decoding is guaranteed when no two
    // library candidates share the same low 16 bits.
    def recur(s: BigInt, remaining: Int, acc: List[Data]): List[Data] raises BintelError =
      if remaining == 0 then
        if s == BigInt(0) then acc
        else abort(BintelError(BintelError.Reason.BadSignature))
      else
        val low = (s & BigInt(0xffff)).toInt

        byLowBits.getOrElse(low, Nil) match
          case h :: Nil =>
            val nextS = (s ^ bytesToBigInt(h)) >> 16
            recur(nextS, remaining - 1, h :: acc)

          case Nil =>
            abort(BintelError(BintelError.Reason.BadSignature))

          case multiple =>
            // For now we don't backtrack; report ambiguity as bad
            // signature. Per the spec collision is computationally
            // infeasible for SHA-256, so this should never trigger
            // in practice for genuine schema libraries.
            abort(BintelError(BintelError.Reason.BadSignature))

    recur(s0, n, Nil)

  private def bytesToBigInt(bytes: Data): BigInt =
    val arr = new Array[Byte](bytes.length + 1)
    arr(0) = 0
    var i = 0

    while i < bytes.length do
      arr(i + 1) = bytes(i)
      i += 1

    BigInt(arr)
