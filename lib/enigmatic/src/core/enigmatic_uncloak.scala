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
import contingency.*
import distillate.*
import gastronomy.{Permit, ProcessingPermit}
import prepositional.*
import rudiments.*
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
// `uncloak` block: beyond leaking, a stream drained after the block ends would
// read key bytes the cloak has already zeroed.

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
      // Matched by class name (see `securityException`): `javax.crypto`/`java.security` types
      // cannot be referenced from this platform-neutral file. `AEADBadTagException` is a
      // `BadPaddingException`, so the superclass-walking match preserves the original order.
      try algorithm.decrypt(data, decryptor.bytes) catch case error: Exception =>
        // (`canThrowAny` only relicenses the rethrow of the exceptions this handler does not
        // match.)
        import unsafeExceptions.canThrowAny
        if securityException(error, "javax.crypto.BadPaddingException")
        then abort(CryptoError(CryptoError.Reason.BadPadding, detail(error)))
        else if securityException(error, "javax.crypto.IllegalBlockSizeException")
        then abort(CryptoError(CryptoError.Reason.IllegalBlockSize, detail(error)))
        else if securityException(error, "java.security.InvalidKeyException")
        then abort(CryptoError(CryptoError.Reason.InvalidKey, detail(error)))
        else if securityException(error, "java.security.GeneralSecurityException")
        then abort(CryptoError(CryptoError.Reason.IoFailure, detail(error)))
        else throw error

    decodable.decoded(plaintext)

// `uncloak` lends the key to the block as an `Encryptor`/`Decryptor` capability,
// and capture checking confines it there: `result` is instantiated at the call
// site, so returning the capability — or any closure over it — is a compile
// error ("leaks into outer capture set"). The capabilities are `SharedCapability`
// because they are stateless key wrappers, freely aliasable within the block
// (the symmetric variant lends two at once), so separation checking is
// deliberately not used. The key material is materialized through the key's
// cloak for the duration of the block and zeroed when it exits — the lent
// capabilities view the live transient array, so their view dies with it. The
// streaming requirement above is therefore hard: a lazily-encrypted stream
// drained after the block reads zeroes. Confinement is regression-tested in
// `CaptureTests`.

extension [cipher <: Cipher](key: PublicKey[cipher])
  def uncloak[result](block: Encryptor[cipher]^ ?=> result): result =
    block(using Encryptor(key.bytes))

extension [cipher <: Cipher](key: PrivateKey[cipher]^)
  def uncloak[result](block: Decryptor[cipher]^ ?=> result): result =
    key.secret.uncloak: bytes =>
      block(using Decryptor(bytes.immutable(using Unsafe)))

extension [cipher <: Cipher](key: SymmetricKey[cipher]^)
  def uncloak[result](block: (Encryptor[cipher]^, Decryptor[cipher]^) ?=> result): result =
    key.secret.uncloak: bytes =>
      val data = bytes.immutable(using Unsafe)
      block(using Encryptor(data), Decryptor(data))
