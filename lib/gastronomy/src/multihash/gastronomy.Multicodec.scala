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
import rudiments.at
import corpuscular.*
import gossamer.*
import prepositional.*
import vacuous.*

// The multicodec identifier of a hash function, as registered in the multiformats table
// (https://github.com/multiformats/multicodec/blob/master/table.csv), which is the normative
// source — the codes below are transcribed from it, not from memory.
//
// The typeclass *is* the mapping, and it is deliberately partial: an algorithm with no
// registered code simply has no instance, so enveloping its digest does not compile. That is
// the whole of the algorithm-code question. Note in particular:
//
//   - Adler-32 has no multicodec code at all, so it can never be enveloped;
//   - CRC-32 (`0x0132`) and CRC-64/ECMA (`0x0164`) are registered with the tag `hash`, not
//     `multihash`. Multihash's own specification says it is intended for "well-established
//     cryptographic hash functions", because "non-cryptographic hash functions are not suitable
//     for content addressing systems", and reserves the `hash` tag for the cases where naming
//     one is nevertheless wanted. Since content addressing is the reason multihash exists, no
//     instance is provided for either: a checksum is not a content address, and a caller who
//     genuinely wants that envelope can construct the `Multihash` directly from its code.
//
// `Sha384`/`Sha512` share the codes of `Sha2[384]`/`Sha2[512]`: they are the same functions,
// and the table has one entry apiece.
object Multicodec:
  given sha1: Sha1 is Multicodec = Multicodec(0x11, t"sha1")
  given sha2_256: Sha2[256] is Multicodec = Multicodec(0x12, t"sha2-256")
  given sha2_512: Sha2[512] is Multicodec = Multicodec(0x13, t"sha2-512")
  given sha2_384: Sha2[384] is Multicodec = Multicodec(0x20, t"sha2-384")
  given sha2_224: Sha2[224] is Multicodec = Multicodec(0x1013, t"sha2-224")
  given sha384: Sha384 is Multicodec = Multicodec(0x20, t"sha2-384")
  given sha512: Sha512 is Multicodec = Multicodec(0x13, t"sha2-512")

  // Draft entries in the table, but unambiguous and widely implemented.
  given blake3: Blake3 is Multicodec = Multicodec(0x1e, t"blake3")
  given md5: Md5 is Multicodec = Multicodec(0xd5, t"md5")

  // The registered names of every code above, so a decoded envelope can be described even
  // though its algorithm is not recoverable as a type. Codes outside this set are legal and
  // representable; they simply have no name here.
  private val names: Map[Int, Text] =
    Map(0x11 -> t"sha1", 0x12 -> t"sha2-256", 0x13 -> t"sha2-512", 0x20 -> t"sha2-384",
        0x1013 -> t"sha2-224", 0x1e -> t"blake3", 0xd5 -> t"md5", 0x00 -> t"identity")

  def name(code: Int): Optional[Text] = names.at(code)

  def apply[algorithm <: Algorithm](code0: Int, name0: Text): algorithm is Multicodec =
    new Multicodec:
      type Self = algorithm
      val code: Int = code0
      val name: Text = name0

trait Multicodec extends Typeclass:
  def code: Int
  def name: Text
