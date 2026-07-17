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

import java.security as js
import javax.crypto as jc

import anticipation.*
import contingency.*
import distillate.*
import gastronomy.{Permit, ProcessingPermit}
import prepositional.*
import vacuous.*

// Encryption is total: a valid transformation is guaranteed by the static types
// (see `Permits`), so `encrypt` cannot fail. Only `decrypt` can fail at runtime —
// from a wrong key, corrupted ciphertext, or malformed input — and those JCE
// failures are surfaced as a `CryptoError`.

extension [value: Encodable in Data](value: value)
  def encrypt[cipher <: Cipher](iv: InitializationVector)
    ( using encryptor: Encryptor[cipher],
            algorithm: cipher & Encryption,
            erased weakness: Permit[Weakness[cipher]],
            erased authentication: Permit[Authentication[cipher]] )
  :   Data =

    algorithm.encrypt(value.bytestream, encryptor.bytes, iv)

// Streaming encryption (block ciphers only) lazily transforms a `LazyList`, driving
// the JCE cipher through update/doFinal. The IV is emitted as the leading chunk
// and the `NoPadding` alignment check runs at end-of-stream. Drain it within the
// `expose` block — only the fixed ciphertext of `stream` could otherwise leak.

extension (stream: LazyList[Data])
  def encrypt[cipher <: BlockCipher](iv: InitializationVector)
    ( using encryptor: Encryptor[cipher],
            algorithm: cipher & Encryption,
            erased weakness: Permit[Weakness[cipher]],
            erased authentication: Permit[Authentication[cipher]] )
  :   LazyList[Data] =

    algorithm.encryptStream(stream, encryptor.bytes, iv)

extension (stream: (zephyrine.Stream[Data] over zephyrine.Credit)^)
  // Kernel-native streaming encryption: the pipeline-stage counterpart of the
  // `LazyList` form above, with the same IV-prefix framing.
  def encrypt[cipher <: BlockCipher](iv: InitializationVector)
    ( using encryptor:  Encryptor[cipher],
            algorithm:  cipher & Encryption,
            buffering:  zephyrine.Buffering,
            erased weakness: Permit[Weakness[cipher]],
            erased authentication: Permit[Authentication[cipher]] )
  :   (zephyrine.Stream[Data] over zephyrine.Credit)^ =

    algorithm.encrypt(stream, encryptor.bytes, iv)

  // Kernel-native streaming decryption of `iv ++ ciphertext` framing,
  // yielding plaintext bytes as they become available. An AEAD mode releases
  // nothing until its tag verifies at end-of-stream (the provider buffers the
  // whole message), so this bounds memory only for non-AEAD modes.
  def decrypt[cipher <: BlockCipher]
    ( using decryptor:  Decryptor[cipher],
            algorithm:  cipher & Encryption,
            buffering:  zephyrine.Buffering,
            tactic:     Tactic[CryptoError],
            erased weakness: ProcessingPermit[Weakness[cipher]],
            erased authentication: ProcessingPermit[Authentication[cipher]] )
  :   (zephyrine.Stream[Data] over zephyrine.Credit)^ =

    algorithm.decrypt(stream, decryptor.bytes)

extension (data: Data)
  def decrypt[decodable: Decodable in Data, cipher <: Cipher]
    ( using decryptor: Decryptor[cipher],
            algorithm: cipher & Encryption,
            erased weakness: ProcessingPermit[Weakness[cipher]],
            erased authentication: ProcessingPermit[Authentication[cipher]] )
  :   decodable raises CryptoError =

    def detail(error: Throwable): Optional[Text] = error.getMessage match
      case null         => Unset
      case text: String => text.tt

    val plaintext =
      try algorithm.decrypt(data, decryptor.bytes) catch
        case error: jc.AEADBadTagException =>
          abort(CryptoError(CryptoError.Reason.BadPadding, detail(error)))

        case error: jc.BadPaddingException =>
          abort(CryptoError(CryptoError.Reason.BadPadding, detail(error)))

        case error: jc.IllegalBlockSizeException =>
          abort(CryptoError(CryptoError.Reason.IllegalBlockSize, detail(error)))

        case error: js.InvalidKeyException =>
          abort(CryptoError(CryptoError.Reason.InvalidKey, detail(error)))

        case error: js.GeneralSecurityException =>
          abort(CryptoError(CryptoError.Reason.IoFailure, detail(error)))

    decodable.decoded(plaintext)

// `expose` lends the key to the block as an `Encryptor`/`Decryptor` capability,
// and capture checking confines it there: `result` is instantiated at the call
// site, so returning the capability — or any closure over it — is a compile
// error ("leaks into outer capture set"). The capabilities are `SharedCapability`
// because they are stateless key wrappers, freely aliasable within the block
// (the symmetric variant lends two at once), so separation checking is
// deliberately not used. The streaming caveat above stands: a lazily-encrypted
// `LazyList[Data]` is pure (the key bytes are already baked in) and escapes
// tracking by design. Confinement is regression-tested in `CaptureTests`.

extension [cipher <: Cipher](key: PublicKey[cipher])
  def expose[result](block: Encryptor[cipher]^ ?=> result): result =
    block(using Encryptor(key.bytes))

extension [cipher <: Cipher](key: PrivateKey[cipher])
  def expose[result](block: Decryptor[cipher]^ ?=> result): result =
    block(using Decryptor(key.privateData))

extension [cipher <: Cipher](key: SymmetricKey[cipher])
  def expose[result](block: (Encryptor[cipher]^, Decryptor[cipher]^) ?=> result): result =
    block(using Encryptor(key.bytes), Decryptor(key.bytes))
