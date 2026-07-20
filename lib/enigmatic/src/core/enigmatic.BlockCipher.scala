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

import scala.caps

import anticipation.*
import contingency.*
import gossamer.*
import prepositional.*
import proscenium.compat.*
import rudiments.*
import vacuous.*
import zephyrine.*

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
  def encryptStream(stream: Progression[Data], key: Data, vector: InitializationVector)
  :   Progression[Data] =
    val blockSize = cipher.blockSize(transformation)
    val iv: Optional[Data] = if mode.usesIv then vector(blockSize) else Unset
    val session = cipher.stream(transformation, key, iv)
    val prefix: Progression[Data] = iv.lay(Progression())(Progression(_))

    def recur(stream: Progression[Data], total: Int): Progression[Data] = stream match
      case head #:: tail =>
        val updated = session.update(head)
        val rest = recur(tail, total + head.length)
        // `Progression.cons` (asInstanceOf-based) rather than `#::` here: the extension cons
        // trips separation checking on the captured `rest`.
        if updated.length > 0 then Progression.cons(updated, rest) else rest

      case _ =>
        padding.verify(total, blockSize, mode.blockAligned)
        val last = session.finish()
        if last.length > 0 then Progression(last) else Progression()

    prefix #::: recur(stream, 0)

  // Kernel-native streaming encryption: the same session-driven
  // transformation as `encryptStream`, as a pipeline stage. The IV (if any)
  // is the stage's first output, and the `NoPadding` alignment check runs at
  // end-of-stream, exactly as in the whole-value and `Progression` forms.
  def encrypt
    ( stream: (Stream[Data] over Credit)^, key: Data, vector: InitializationVector )
    ( using Buffering )
  :   (Stream[Data] over Credit)^ =

    val blockSize = cipher.blockSize(transformation)
    val iv: Optional[Data] = if mode.usesIv then vector(blockSize) else Unset
    val session = cipher.stream(transformation, key, iv)
    val finalise: Int => Unit = total => padding.verify(total, blockSize, mode.blockAligned)

    stream.via(CipherDuct(session, iv, finalise)).asInstanceOf[(Stream[Data] over Credit)^]

  // Kernel-native streaming decryption. The leading IV block (for modes that
  // use one) is consumed off the stream before the session begins. NOTE: an
  // AEAD mode (GCM) releases no plaintext until its tag verifies at
  // end-of-stream — the provider buffers the whole message internally — so
  // streaming decryption of AEAD input bounds no memory; it is offered for
  // API uniformity only.
  def decrypt(stream: (Stream[Data] over Credit)^, key: Data)
    (using buffering: Buffering, tactic: Tactic[CryptoError])
  :   (Stream[Data] over Credit)^ =

    val ivSize = if mode.usesIv then cipher.blockSize(transformation) else 0

    val start: Data => CipherSession = iv =>
      cipher.decryptStream(transformation, key, if mode.usesIv then iv else Unset)

    stream.via(DecipherDuct(start, ivSize, tactic)).asInstanceOf[(Stream[Data] over Credit)^]

  def decrypt(bytes: Data, key: Data): Data =
    val ivSize: Optional[Int] = if mode.usesIv then cipher.blockSize(transformation) else Unset
    cipher.decrypt(transformation, key, ivSize, bytes)

  def genKey(): Data = cipher.generateKey(keySize)

  def privateToPublic(key: Data): Data = key

// The session-driven cipher transformation as a `Duct`: input windows feed
// `CipherSession.update`, whose output (of arbitrary size — a block cipher
// may buffer or release several blocks) stages in `pending` and delivers as
// the target window allows. `prefix` (the IV) delivers before any ciphertext;
// `finalise` sees the total input length at end-of-stream, before the final
// block is flushed.
private[enigmatic] final class CipherDuct
  ( session: CipherSession, prefix: Optional[Data], finalise: Int => Unit )
extends Duct[Data, Data]:
  type Transport = Credit
  type Upstream = Credit

  private var pending: Data = prefix.or(Data())
  private var offset: Int = 0
  private var total: Int = 0
  private var finished: Boolean = false

  def regulation: Credit is Regulation = summon[Credit is Regulation]
  def translate(demand: Credit): Credit = demand

  private update def deliver(target: Array[Byte], targetOffset: Int, targetSpace: Int): Int =
    val count = targetSpace.min(pending.length - offset)

    if count > 0 then
      System.arraycopy(pending.mutable(using Unsafe), offset, target, targetOffset, count)
      offset += count

    count

  update def step
    ( source: input.Storage,
      sourceOffset: Int,
      sourceLength: Int,
      target: output.Storage,
      targetOffset: Int,
      targetSpace: Int )
  :   Duct.Progress =

    val bytes = target.asInstanceOf[Array[Byte]]

    if offset < pending.length then Duct.Progress(0, deliver(bytes, targetOffset, targetSpace))
    else
      val chunk = input.materialize(source, sourceOffset, sourceLength)
      total += sourceLength
      pending = session.update(chunk)
      offset = 0
      Duct.Progress(sourceLength, deliver(bytes, targetOffset, targetSpace))

  override update def flush(target: output.Storage, targetOffset: Int, targetSpace: Int): Int =
    val bytes = target.asInstanceOf[Array[Byte]]

    if offset < pending.length then deliver(bytes, targetOffset, targetSpace)
    else if !finished then
      finished = true
      finalise(total)
      pending = session.finish()
      offset = 0
      deliver(bytes, targetOffset, targetSpace)
    else 0

// Streaming decryption's IV-prefix state machine (the `Inflation` header
// precedent): the leading `ivSize` bytes accumulate before the session can
// exist; thereafter this delegates to a `CipherDuct` around the started
// session. A stream that ends before a whole IV arrives starts the session
// with the truncated prefix, and the provider raises its own (accurate)
// invalid-parameter error.
private[enigmatic] final class DecipherDuct
  ( start: Data => CipherSession, ivSize: Int, tactic: Tactic[CryptoError] )
extends Duct[Data, Data]:
  type Transport = Credit
  type Upstream = Credit

  // Turn the provider's end-of-stream verification failure (an AEAD tag
  // mismatch, or corrupt padding on a block mode) into a typed `CryptoError`
  // as the final window is pulled, rather than leaking the JCE exception.
  private def detail(error: Throwable): Optional[Text] = error.getMessage match
    case null         => Unset
    case text: String => text.tt

  private val header: Array[Byte] = new Array[Byte](ivSize)
  private var headerFilled: Int = 0

  // Untracked, cast-erased: the inner duct is reached only through this
  // stage's own exclusive methods.
  @caps.unsafe.untrackedCaptures
  private var inner: CipherDuct | Null = null

  private update def begin(): CipherDuct =
    CipherDuct(start(header.take(headerFilled).immutable(using Unsafe)), Unset, _ => ())

  def regulation: Credit is Regulation = summon[Credit is Regulation]
  def translate(demand: Credit): Credit = demand

  update def step
    ( source: input.Storage,
      sourceOffset: Int,
      sourceLength: Int,
      target: output.Storage,
      targetOffset: Int,
      targetSpace: Int )
  :   Duct.Progress =

    inner match
      case duct: CipherDuct =>
        // `Addressable` instances are unique per medium, so the two ducts'
        // `Storage` paths coincide at erasure.
        duct.step
          ( source.asInstanceOf[duct.input.Storage],
            sourceOffset,
            sourceLength,
            target.asInstanceOf[duct.output.Storage],
            targetOffset,
            targetSpace )

      case null =>
        val take = sourceLength.min(ivSize - headerFilled)
        System.arraycopy(source.asInstanceOf[Array[Byte]], sourceOffset, header, headerFilled, take)
        headerFilled += take
        if headerFilled == ivSize then inner = begin().asInstanceOf[CipherDuct]
        Duct.Progress(take, 0)

  override update def flush(target: output.Storage, targetOffset: Int, targetSpace: Int): Int =
    inner match
      case duct: CipherDuct =>
        try duct.flush(target.asInstanceOf[duct.output.Storage], targetOffset, targetSpace)
        catch case error: Exception =>
          // Matched by class name (see `securityException`): `javax.crypto` types cannot be
          // referenced from this platform-neutral file. (`canThrowAny` only relicenses the
          // rethrow of the exceptions this handler does not match.)
          import unsafeExceptions.canThrowAny
          if securityException(error, "javax.crypto.BadPaddingException")
          then tactic.abort(CryptoError(CryptoError.Reason.BadPadding, detail(error)))
          else if securityException(error, "javax.crypto.IllegalBlockSizeException")
          then tactic.abort(CryptoError(CryptoError.Reason.IllegalBlockSize, detail(error)))
          else throw error

      case null =>
        inner = begin().asInstanceOf[CipherDuct]
        flush(target, targetOffset, targetSpace)
