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
package ulysses

import fulminate.*
import rudiments.*
import vacuous.*

object Cadence:
  // §3.1 hash-size table — `s` indexes byte lengths.
  val hashSizes: Array[Int]^{} = Array(8, 10, 12, 16, 20, 24, 28, 32, 48, 64)

  // Default user choice: 96-bit hash, 4-byte initial cadence, 1-byte regular cadence.
  // §10: hash sizes of 8, 10 and 12 bytes leave a realistic chance that two
  // distinct hashes share a prefix, so this default suits a small closed
  // library only; where decoding ambiguity has security consequences, or
  // the library is open, choose `hashSize = 32` (as BinTEL pins).
  given default: Cadence = Cadence(initial = 4, regular = 1, hashSize = 12)

  // Build a `Cadence` from the trailing cadence byte (§5.2 parameter recovery).
  // Returns `Unset` if the byte names a reserved hash-size index (`s ∈ {10..15}`).
  def unpack(byte: Byte): Optional[Cadence] =
    val b = byte & 0xff
    val regular = (b & 0x03) + 1
    val initial = regular + ((b >>> 2) & 0x03)
    val s = (b >>> 4) & 0x0f
    if s >= hashSizes.length then Unset else Cadence(initial, regular, hashSizes.readable(s))

case class Cadence(initial: Int, regular: Int, hashSize: Int):
  if regular < 1 || regular > 4 then panic(m"regular cadence must be in 1..4 (got $regular)")

  if initial < regular || initial > regular + 3
  then panic(m"initial cadence must be in $regular..${regular + 3} (got $initial)")

  val hashSizeIndex: Int =
    Cadence.hashSizes.where(_ == hashSize).lay:
      panic(m"hash size $hashSize is not one of ${Cadence.hashSizes.readable.toList.toString}")
    . apply(_.n0)

  // Packed cadence byte per §3.1: bits 4-7 = s, bits 2-3 = k_i - k_r, bits 0-1 = k_r - 1.
  val byte: Byte =
    ((hashSizeIndex << 4) | ((initial - regular) << 2) | (regular - 1)).toByte

  // Byte offset of the i-th hash within the palimpsest body.
  def offset(i: Int): Int = if i == 0 then 0 else initial + (i - 1)*regular

  // Body length for `n` hashes.
  def bodyLength(n: Int): Int = if n == 1 then hashSize else hashSize + initial + regular*(n - 2)

  // Total palimpsest length including the trailing cadence byte.
  def totalLength(n: Int): Int = bodyLength(n) + 1

  // Recover the number of hashes from a palimpsest body length, or `Unset` if the
  // length is inconsistent with this cadence.
  def hashCount(bodyLen: Int): Optional[Int] =
    if bodyLen == hashSize then 1
    else if bodyLen >= hashSize + initial && (bodyLen - hashSize - initial) % regular == 0
    then 2 + (bodyLen - hashSize - initial)/regular
    else Unset
