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
┃    Soundness, version 0.63.0.                                                                    ┃
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
import gossamer.*
import vacuous.*

// A block cipher's type-level *policy* (which mode and padding apply, whether an
// IV is needed and how input length is checked) lives here; the byte-level
// *mechanics* are delegated to the in-scope `Crypto` provider's
// `Crypto.SymmetricCipher`, captured at construction by the algorithm's given.
abstract class BlockCipher
  ( val algorithm: Text,
    mode:          BlockCipherMode,
    padding:       BlockCipherPadding,
    cipher:        Crypto.SymmetricCipher )
extends Cipher, Encryption, Symmetric:
  type Transport
  type Contrast

  private def transformation: Text = t"$algorithm/${mode.name}/${padding.name}"

  // The initialization vector is supplied explicitly by the caller; modes that
  // don't use one (ECB) ignore it.
  def encrypt(bytes: Data, key: Data, vector: InitializationVector): Data =
    val blockSize = cipher.blockSize(transformation)
    padding.verify(bytes.length, blockSize, mode.blockAligned)
    val iv: Optional[Data] = if mode.usesIv then vector(blockSize) else Unset
    cipher.encrypt(transformation, key, iv, bytes)

  // Streaming encryption feeds each chunk through the provider's `CipherSession`,
  // mirroring `turbulence.Compression`. The IV (if any) is emitted as the first
  // chunk; the `NoPadding` alignment check happens at end-of-stream, where the
  // total length is finally known.
  def encryptStream(stream: LazyList[Data], key: Data, vector: InitializationVector)
  :   LazyList[Data] =
    val blockSize = cipher.blockSize(transformation)
    val iv: Optional[Data] = if mode.usesIv then vector(blockSize) else Unset
    val session = cipher.stream(transformation, key, iv)
    val prefix: LazyList[Data] = iv.lay(LazyList())(LazyList(_))

    def recur(stream: LazyList[Data], total: Int): LazyList[Data] = stream match
      case head #:: tail =>
        val updated = session.update(head)
        val rest = recur(tail, total + head.length)
        if updated.length > 0 then updated #:: rest else rest

      case _ =>
        padding.verify(total, blockSize, mode.blockAligned)
        val last = session.finish()
        if last.length > 0 then LazyList(last) else LazyList()

    prefix #::: recur(stream, 0)

  def decrypt(bytes: Data, key: Data): Data =
    val ivSize: Optional[Int] = if mode.usesIv then cipher.blockSize(transformation) else Unset
    cipher.decrypt(transformation, key, ivSize, bytes)

  def genKey(): Data = cipher.generateKey(keySize)

  def privateToPublic(key: Data): Data = key
