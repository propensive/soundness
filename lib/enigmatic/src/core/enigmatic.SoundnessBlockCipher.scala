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

import java.util.concurrent.atomic as juca

import anticipation.*
import gossamer.*
import vacuous.*

// A failure raised by the pure-Scala provider: the counterpart of the `javax.crypto` exceptions
// the JDK provider throws, carrying the `Crypto.Error.Reason` directly so that the
// platform-neutral `decrypt` paths can map it without matching class names.
private[enigmatic] object CryptoFailure:
  def invalidKey(message: String): Nothing =
    throw CryptoFailure(Crypto.Error.Reason.InvalidKey, message)

  def badPadding(message: String): Nothing =
    throw CryptoFailure(Crypto.Error.Reason.BadPadding, message)

  def ioFailure(message: String): Nothing =
    throw CryptoFailure(Crypto.Error.Reason.IoFailure, message)

  def invalidAlgorithm(message: String): Nothing =
    throw CryptoFailure(Crypto.Error.Reason.InvalidAlgorithm, message)

  def illegalBlockSize(message: String): Nothing =
    throw CryptoFailure(Crypto.Error.Reason.IllegalBlockSize, message)

private[enigmatic] class CryptoFailure(val reason: Crypto.Error.Reason, message: String)
extends RuntimeException(message)

// AES in the block-cipher modes of operation (NIST SP 800-38A) with the JCE's paddings, as a
// `Crypto.SymmetricCipher` over `Rijndael`: the `SoundnessCrypto` provider's `aes`. The
// transformation is the JCE string the typed API builds, `AES/<mode>/<padding>`, and the
// semantics follow the JDK provider's so that a ciphertext made by one decrypts under the other:
// the IV is the leading block of a whole-value ciphertext; CFB and OFB are their full-block
// (128-bit feedback) variants; CTR increments the whole 128-bit counter block; padding, when
// asked for, applies to every mode, and a decrypting session withholds the last full block until
// `finish` so the padding can be stripped.
private[enigmatic] object SoundnessBlockCipher:
  private val Block: Int = Rijndael.BlockSize

  private enum Mode:
    case Ecb, Cbc, Ctr, Cfb, Ofb

    // ECB and CBC consume whole blocks only; the feedback modes produce a keystream, so they
    // accept a partial final block.
    def blockAligned: Boolean = this == Ecb || this == Cbc

  private enum Padding:
    case Pkcs7, Iso10126, None

  private def parse(transformation: Text): (Mode, Padding) =
    transformation.cut(t"/") match
      case List(algorithm, mode, padding) if algorithm == t"AES" =>
        val mode0 =
          if mode == t"CBC" then Mode.Cbc else if mode == t"ECB" then Mode.Ecb
          else if mode == t"CTR" then Mode.Ctr else if mode == t"CFB" then Mode.Cfb
          else if mode == t"OFB" then Mode.Ofb
          else CryptoFailure.invalidAlgorithm(s"unsupported mode $mode")

        val padding0 =
          if padding == t"PKCS5Padding" then Padding.Pkcs7
          else if padding == t"ISO10126Padding" then Padding.Iso10126
          else if padding == t"NoPadding" then Padding.None
          else CryptoFailure.invalidAlgorithm(s"unsupported padding $padding")

        // The JDK refuses to pad a counter-mode ciphertext, so a padded CTR transformation would
        // make a ciphertext no JDK could read; refusing it too keeps the providers interchangeable.
        if mode0 == Mode.Ctr && padding0 != Padding.None
        then CryptoFailure.invalidAlgorithm("CTR mode must be used with NoPadding")

        (mode0, padding0)

      case _ =>
        CryptoFailure.invalidAlgorithm(s"unsupported transformation $transformation")

  // A fresh frozen array holding `count` bytes of `data` from `start`.
  private[enigmatic] def slice(data: Data, start: Int, count: Int): Data =
    val result = Array.allocate[Byte](count)
    result.place(data, start, 0, count)
    Array.freeze(result)

  // The concatenation of the parts, as one fresh frozen array.
  private[enigmatic] def join(parts: Data*): Data =
    val length = parts.foldLeft(0)(_ + _.length)
    val result = Array.allocate[Byte](length)
    concatenate(result, parts, 0, 0)
    Array.freeze(result)

  private def concatenate
    ( result: Array[Byte]^, parts: scala.collection.immutable.Seq[Data], index: Int, offset: Int )
  :   Unit =

    if index < parts.length then
      val part = parts(index)
      result.place(part, 0, offset, part.length)
      concatenate(result, parts, index + 1, offset + part.length)

  // The whole-value form of a session: everything through `update`, then `finish`.
  private def run(session: Cipher.Session, data: Data): Data =
    join(session.update(data), session.finish())

  def aes: Crypto.SymmetricCipher = new Crypto.SymmetricCipher:
    def blockSize(transformation: Text): Int = Block
    def generateKey(bits: Int): Data = Entropy.bytes(bits/8)

    def encrypt(transformation: Text, key: Data, iv: Optional[Data], data: Data): Data =
      iv.lay(run(stream(transformation, key, Unset), data)): iv =>
        join(iv, run(stream(transformation, key, iv), data))

    def decrypt(transformation: Text, key: Data, ivSize: Optional[Int], data: Data): Data =
      ivSize.lay(run(decryptStream(transformation, key, Unset), data)): size =>
        if data.length < size
        then CryptoFailure.illegalBlockSize("the ciphertext is shorter than its IV")

        val iv = slice(data, 0, size)
        run(decryptStream(transformation, key, iv), slice(data, size, data.length - size))

    def stream(transformation: Text, key: Data, iv: Optional[Data]): Cipher.Session =
      val (mode, padding) = parse(transformation)
      Session(Rijndael(key), mode, padding, iv, true)

    def decryptStream(transformation: Text, key: Data, iv: Optional[Data]): Cipher.Session =
      val (mode, padding) = parse(transformation)
      Session(Rijndael(key), mode, padding, iv, false)

  // The chaining state a mode carries from block to block: the previous ciphertext block (CBC,
  // CFB), the previous keystream block (OFB) or the counter (CTR), starting from the IV. It is
  // held between calls as a frozen array, and worked on within a call in exclusive scratch: one
  // allocation per `update`, not per block. The two pieces of state live in Java references,
  // as the JDK provider's live in its `Cipher` object: `Cipher.Session` is a plain trait, so a
  // session has no `update` methods through which tracked state could be written.
  private final class Session
    ( cipher: Rijndael, mode: Mode, padding: Padding, iv: Optional[Data], encrypting: Boolean )
  extends Cipher.Session:

    private val feedback: juca.AtomicReference[Data] =
      juca.AtomicReference:
        iv.lay(Array.fill[Byte](Block)(0)): iv =>
          if iv.length != Block then CryptoFailure.invalidKey(s"an AES IV must be $Block bytes")

          iv

    // Input not yet transformed: fewer than a block's worth, or — when decrypting padded data —
    // up to one whole block, withheld until `finish` can strip the padding from it.
    private val pending: juca.AtomicReference[Data] = juca.AtomicReference(Data())
    private val withholding: Boolean = !encrypting && padding != Padding.None

    // A working copy of the chaining state.
    private def chain(): scala.Array[Byte]^ =
      val copy: scala.Array[Byte]^ = Array.scratch[Byte](Block)
      // [java-boundary] the frozen chaining state, read into exclusive scratch
      java.lang.System.arraycopy(Array.unsafeJvm(feedback.get().nn), 0, copy, 0, Block)
      copy

    private def retain(chain: scala.Array[Byte]): Unit =
      val copy = Array.allocate[Byte](Block)
      java.lang.System.arraycopy(chain, 0, copy.raw, 0, Block)
      feedback.set(Array.freeze(copy))

    private def xor
      ( input:     scala.Array[Byte],
        offset:    Int,
        keystream: scala.Array[Byte],
        output:    scala.Array[Byte]^,
        target:    Int,
        count:     Int,
        index:     Int = 0 )
    :   Unit =

      if index < count then
        output(target + index) = (input(offset + index) ^ keystream(index)).toByte
        xor(input, offset, keystream, output, target, count, index + 1)

    // Big-endian increment of the whole counter block, as the JDK's CTR does.
    private def increment(counter: scala.Array[Byte]^, index: Int): Unit =
      if index >= 0 then
        val next = ((counter(index) & 0xff) + 1).toByte
        counter(index) = next
        if next == 0 then increment(counter, index - 1)

    // One whole block: `input` at `offset` to `output` at `target`, advancing `chain`.
    private def transform
      ( chain:     scala.Array[Byte]^,
        keystream: scala.Array[Byte]^,
        input:     scala.Array[Byte],
        offset:    Int,
        output:    scala.Array[Byte]^,
        target:    Int )
    :   Unit =

      mode match
        case Mode.Ecb =>
          if encrypting then cipher.encrypt(input, offset, output, target)
          else cipher.decrypt(input, offset, output, target)

        case Mode.Cbc =>
          if encrypting then
            xor(input, offset, chain, keystream, 0, Block)
            cipher.encrypt(keystream, 0, output, target)
            java.lang.System.arraycopy(output, target, chain, 0, Block)
          else
            cipher.decrypt(input, offset, keystream, 0)
            xor(keystream, 0, chain, output, target, Block)
            java.lang.System.arraycopy(input, offset, chain, 0, Block)

        case Mode.Ctr =>
          cipher.encrypt(chain, 0, keystream, 0)
          xor(input, offset, keystream, output, target, Block)
          increment(chain, Block - 1)

        case Mode.Cfb =>
          cipher.encrypt(chain, 0, keystream, 0)
          xor(input, offset, keystream, output, target, Block)

          if encrypting
          then java.lang.System.arraycopy(output, target, chain, 0, Block)
          else java.lang.System.arraycopy(input, offset, chain, 0, Block)

        case Mode.Ofb =>
          cipher.encrypt(chain, 0, keystream, 0)
          java.lang.System.arraycopy(keystream, 0, chain, 0, Block)
          xor(input, offset, keystream, output, target, Block)

    // A final partial block in a feedback mode: the next keystream block, applied to `count` bytes.
    private def tail(chain: scala.Array[Byte]^, input: scala.Array[Byte], count: Int): Data =
      val keystream: scala.Array[Byte]^ = Array.scratch[Byte](Block)
      cipher.encrypt(chain, 0, keystream, 0)
      val output = Array.allocate[Byte](count)
      xor(input, 0, keystream, output.raw, 0, count)
      Array.freeze(output)

    private def blocks
      ( chaining: scala.Array[Byte]^, keystream: scala.Array[Byte]^, combined: scala.Array[Byte],
        output: scala.Array[Byte]^, offset: Int, emitting: Int )
    :   Unit =

      if offset < emitting then
        transform(chaining, keystream, combined, offset, output, offset)
        blocks(chaining, keystream, combined, output, offset + Block, emitting)

    def update(chunk: Data): Data =
      val held = pending.get().nn
      val total = held.length + chunk.length
      val count = if withholding && total%Block == 0 then total/Block - 1 else total/Block
      val emitting = count.max(0)*Block

      val combined: scala.Array[Byte]^ = Array.scratch[Byte](total)
      // [java-boundary] the frozen pending bytes and chunk, read into exclusive scratch
      java.lang.System.arraycopy(Array.unsafeJvm(held), 0, combined, 0, held.length)
      // [java-boundary]
      java.lang.System.arraycopy(Array.unsafeJvm(chunk), 0, combined, held.length, chunk.length)

      val chaining = chain()
      val keystream: scala.Array[Byte]^ = Array.scratch[Byte](Block)
      val output = Array.allocate[Byte](emitting)
      blocks(chaining, keystream, combined, output.raw, 0, emitting)
      retain(chaining)
      val rest = Array.allocate[Byte](total - emitting)
      java.lang.System.arraycopy(combined, emitting, rest.raw, 0, total - emitting)
      pending.set(Array.freeze(rest))
      Array.freeze(output)

    // The pending bytes in exclusive scratch of a block's length, zero beyond their length.
    private def padded(): scala.Array[Byte]^ =
      val held = pending.get().nn
      val block: scala.Array[Byte]^ = Array.scratch[Byte](Block)
      // [java-boundary] the frozen pending bytes, read into exclusive scratch
      java.lang.System.arraycopy(Array.unsafeJvm(held), 0, block, 0, held.length)
      block

    private def fill(block: scala.Array[Byte]^, value: Byte, index: Int): Unit =
      if index < Block then
        block(index) = value
        fill(block, value, index + 1)

    private def check(bytes: scala.Array[Byte], count: Int, index: Int): Boolean =
      index >= Block || ((bytes(index) & 0xff) == count && check(bytes, count, index + 1))

    private def finalBlock(block: scala.Array[Byte]): Data =
      val chaining = chain()
      val keystream: scala.Array[Byte]^ = Array.scratch[Byte](Block)
      val output = Array.allocate[Byte](Block)
      transform(chaining, keystream, block, 0, output.raw, 0)
      retain(chaining)
      Array.freeze(output)

    def finish(): Data =
      val filled = pending.get().nn.length

      if encrypting then padding match
        case Padding.None =>
          if filled == 0 then Data()
          else if mode.blockAligned
          then CryptoFailure.illegalBlockSize("the input is not a whole number of blocks")
          else tail(chain(), padded(), filled)

        case Padding.Pkcs7 =>
          val count = Block - filled
          val block = padded()
          fill(block, count.toByte, filled)
          finalBlock(block)

        case Padding.Iso10126 =>
          val count = Block - filled
          val block = padded()
          // [java-boundary] fresh random bytes, read once into the padding
          java.lang.System.arraycopy(Array.unsafeJvm(Entropy.bytes(count)), 0, block, filled, count)
          block(Block - 1) = count.toByte
          finalBlock(block)

      else padding match
        case Padding.None =>
          if filled == 0 then Data()
          else if mode.blockAligned
          then CryptoFailure.illegalBlockSize("the ciphertext is not a whole number of blocks")
          else tail(chain(), padded(), filled)

        case _ =>
          if filled != Block
          then CryptoFailure.illegalBlockSize("the ciphertext is not a whole number of blocks")

          val plain = finalBlock(padded())
          // [java-boundary] the decrypted block, read to validate and strip its padding
          val bytes = Array.unsafeJvm(plain)
          val count = bytes(Block - 1) & 0xff

          if count < 1 || count > Block then CryptoFailure.badPadding("the padding is malformed")

          // PKCS#7 pads with `count` copies of `count`; ISO 10126 pads with random bytes before it.
          if padding == Padding.Pkcs7 && !check(bytes, count, Block - count)
          then CryptoFailure.badPadding("the padding is malformed")

          slice(plain, 0, Block - count)
