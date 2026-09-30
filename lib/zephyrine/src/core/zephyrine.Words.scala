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
package zephyrine

import java.lang as jl

// SWAR primitives over a byte buffer: eight bytes are read as one little-endian `Long`
// (`load`, through the platform's `WordLoad`) and classified lane by lane with the classic
// has-zero-byte arithmetic, so a scan for a stop byte advances eight bytes per step. Every
// mask has the high bit set in each lane that satisfies the test; the borrow from one flagged
// lane can spill into the lanes above it, so only the *lowest* flagged lane is exact — which
// is the one `first` reports, and the only one a scan needs. The tests are valid for ASCII
// lanes; a lane with its own high bit set (a UTF-8 byte) is flagged by `nonAscii` and must be
// handled separately by any scan that cares.
object Words:
  inline val HighBits  = 0x8080808080808080L
  inline val EveryByte = 0x0101010101010101L

  inline def load(bytes: scala.Array[Byte], index: Int): Long = WordLoad.get(bytes, index)

  // A byte value replicated into all eight lanes, the comparand for `matches` and `below`.
  inline def replicate(byte: Int): Long = (byte & 0xffL)*EveryByte

  // The lanes of `word` that are zero.
  inline def zeroes(word: Long): Long = (word - EveryByte) & ~word & HighBits

  // The lanes of `word` equal to the replicated byte.
  inline def matches(word: Long, replicated: Long): Long = zeroes(word ^ replicated)

  // The lanes of `word` below the replicated byte (both ASCII).
  inline def below(word: Long, replicated: Long): Long = (word - replicated) & ~word & HighBits

  // The lanes of `word` holding a non-ASCII byte.
  inline def nonAscii(word: Long): Long = word & HighBits

  // The offset within the word of the lowest flagged lane of a non-zero mask.
  inline def first(mask: Long): Int = jl.Long.numberOfTrailingZeros(mask) >> 3
