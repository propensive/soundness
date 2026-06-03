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
package enigmatic

import javax.crypto as jc, javax.crypto.spec.*

import anticipation.*
import gossamer.*
import rudiments.*
import vacuous.*

abstract class BlockCipher
  ( val algorithm: Text,
    mode:          BlockCipherMode,
    padding:       BlockCipherPadding,
    vector:        InitializationVector )
extends Cipher, Encryption, Symmetric:
  type Transport
  type Contrast

  private def transformation: Text = t"$algorithm/${mode.name}/${padding.name}"
  private def initialize(): jc.Cipher = jc.Cipher.getInstance(transformation.s).nn

  private def makeKey(key: Data): SecretKeySpec =
    SecretKeySpec(key.mutable(using Unsafe), algorithm.s)

  def encrypt(bytes: Data, key: Data): Data =
    val cipher = initialize()
    padding.verify(bytes.length, cipher.getBlockSize, mode.blockAligned)

    if mode.usesIv then
      val iv = vector(cipher.getBlockSize).mutable(using Unsafe)
      cipher.init(jc.Cipher.ENCRYPT_MODE, makeKey(key), IvParameterSpec(iv))
      (iv ++ cipher.doFinal(bytes.mutable(using Unsafe)).nn).immutable(using Unsafe)
    else
      cipher.init(jc.Cipher.ENCRYPT_MODE, makeKey(key))
      cipher.doFinal(bytes.mutable(using Unsafe)).nn.immutable(using Unsafe)

  // Streaming encryption feeds each chunk through `Cipher.update` and flushes with
  // `doFinal`, mirroring `turbulence.Compression`. The IV (if any) is emitted as
  // the first chunk; the `NoPadding` alignment check happens at end-of-stream,
  // where the total length is finally known.
  def encryptStream(stream: Stream[Data], key: Data): Stream[Data] =
    val cipher = initialize()

    val prefix: Stream[Data] =
      if mode.usesIv then
        val iv = vector(cipher.getBlockSize).mutable(using Unsafe)
        cipher.init(jc.Cipher.ENCRYPT_MODE, makeKey(key), IvParameterSpec(iv))
        Stream(iv.immutable(using Unsafe))
      else
        cipher.init(jc.Cipher.ENCRYPT_MODE, makeKey(key))
        Stream()

    def recur(stream: Stream[Data], total: Int): Stream[Data] = stream match
      case head #:: tail =>
        val updated = cipher.update(head.mutable(using Unsafe)).nn.immutable(using Unsafe)
        val rest = recur(tail, total + head.length)
        if updated.length > 0 then updated #:: rest else rest

      case _ =>
        padding.verify(total, cipher.getBlockSize, mode.blockAligned)
        val last = cipher.doFinal().nn.immutable(using Unsafe)
        if last.length > 0 then Stream(last) else Stream()

    prefix #::: recur(stream, 0)

  def decrypt(bytes: Data, key: Data): Data =
    val cipher = initialize()
    val input = bytes.mutable(using Unsafe)

    if mode.usesIv then
      val blockSize = cipher.getBlockSize
      cipher.init(jc.Cipher.DECRYPT_MODE, makeKey(key), IvParameterSpec(input.take(blockSize)))
      cipher.doFinal(input.drop(blockSize)).nn.immutable(using Unsafe)
    else
      cipher.init(jc.Cipher.DECRYPT_MODE, makeKey(key))
      cipher.doFinal(input).nn.immutable(using Unsafe)

  def genKey(): Data =
    val keyGen = jc.KeyGenerator.getInstance(algorithm.s).nn
    keyGen.init(keySize)
    keyGen.generateKey().nn.getEncoded.nn.immutable(using Unsafe)

  def privateToPublic(key: Data): Data = key
