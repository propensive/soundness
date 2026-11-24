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
import corpuscular.*
import gastronomy.*
import prepositional.*
import rudiments.*

// Key-derivation functions: how a protocol turns a secret that is not itself a key — the output
// of a key agreement, or a password — into key material of the length a cipher needs. All three
// are defined over a hash, named as a type, and all are plain compositions of HMAC or the hash
// itself, so they run under any `Crypto` provider.
//
// - `hkdf` is RFC 5869's extract-then-expand, used by TLS 1.3, Noise and most modern protocols;
// - `concat` is the single-step "Concat KDF" of NIST SP 800-56A §5.8.1, which JOSE's ECDH-ES
//   derives its keys with (RFC 7518 §4.6);
// - `pbkdf2` is RFC 8018's password-based derivation, deliberately slow, used by PBES2.
//
// The results are frozen `Data`, as every provider's outputs are; a caller that adopts one as a
// `SymmetricKey` copies it into a cloak.
object KeyDerivation:
  private def be32(value: Int): Data =
    Array.tabulate[Byte](4): index => (value >>> (24 - 8*index)).toByte

  private def xor(left: Data, right: Data): Data =
    val leftBytes = Array.unsafeJvm(left)
    val rightBytes = Array.unsafeJvm(right)
    Array.tabulate[Byte](leftBytes.length): index => (leftBytes(index) ^ rightBytes(index)).toByte

  // HKDF-Extract: a pseudorandom key from the input keying material. An empty salt stands for
  // a string of zeros as long as the hash's output (RFC 5869 §2.2).
  def extract[algorithm <: Algorithm](material: Data, salt: Data)
    ( using hash: Hash in algorithm, crypto: Crypto )
  :   Data =

    val mac = crypto.hmac(hash.hmacName)
    // The JDK refuses an empty HMAC key, so the width is measured with a one-byte one.
    val probe: Data = Array.fill[Byte](1)(0)
    val size = mac.mac(probe, Data()).length
    val zeros: Data = Array.fill[Byte](size)(0)
    mac.mac(if salt.length == 0 then zeros else salt, material)

  // HKDF-Expand: `length` bytes of output keying material from a pseudorandom key.
  def expand[algorithm <: Algorithm](key: Data, info: Data, length: Int)
    ( using hash: Hash in algorithm, crypto: Crypto )
  :   Data =

    val mac = crypto.hmac(hash.hmacName)

    def recur(previous: Data, counter: Int, output: Data): Data =
      if output.length >= length then output.keep(length) else
        val index: Data = Array.fill[Byte](1)(counter.toByte)
        val block = mac.mac(key, Array.concat(previous, info, index))
        recur(block, counter + 1, Array.concat(output, block))

    recur(Data(), 1, Data())

  def hkdf[algorithm <: Algorithm](material: Data, salt: Data, info: Data, length: Int)
    ( using hash: Hash in algorithm, crypto: Crypto )
  :   Data =

    expand[algorithm](extract[algorithm](material, salt), info, length)

  // The hash of a 32-bit counter, the shared secret and the caller's `OtherInfo`, repeated with
  // the counter incremented until there is enough output.
  def concat[algorithm <: Algorithm](secret: Data, info: Data, length: Int)
    ( using hash: Hash in algorithm, erased weakness: Permit[HashWeakness[algorithm]] )
  :   Data =

    def recur(counter: Int, output: Data): Data =
      if output.length >= length then output.keep(length) else
        val round: Data = Array.concat(be32(counter), secret, info)
        recur(counter + 1, Array.concat(output, round.digest[algorithm].data))

    recur(1, Data())

  // PBKDF2 with HMAC over the named hash as its pseudorandom function. Each output block is the
  // xor of `iterations` chained HMACs, which is the whole of its cost.
  def pbkdf2[algorithm <: Algorithm](password: Data, salt: Data, iterations: Int, length: Int)
    ( using hash: Hash in algorithm, crypto: Crypto )
  :   Data =

    val mac = crypto.hmac(hash.hmacName)

    def iterate(remaining: Int, previous: Data, accumulator: Data): Data =
      if remaining <= 0 then accumulator else
        val next = mac.mac(password, previous)
        iterate(remaining - 1, next, xor(accumulator, next))

    def block(index: Int): Data =
      val first = mac.mac(password, Array.concat(salt, be32(index)))
      iterate(iterations - 1, first, first)

    def recur(index: Int, output: Data): Data =
      if output.length >= length then output.keep(length)
      else recur(index + 1, Array.concat(output, block(index)))

    recur(1, Data())

  // PBKDF2 from a `Password`, whose UTF-8 bytes are lent by its cloak for the derivation alone.
  def pbkdf2[algorithm <: Algorithm](password: Password^, salt: Data, iterations: Int, length: Int)
    ( using hash: Hash in algorithm, crypto: Crypto )
  :   Data =

    password.secret.uncloak: bytes =>
      pbkdf2[algorithm](Array.unsafeFrozen(bytes), salt, iterations, length)
