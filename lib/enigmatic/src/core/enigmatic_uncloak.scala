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
import fulminate.*
import gastronomy.{Permit, ProcessingPermit}
import prepositional.*
import rudiments.*
import vacuous.*

// Encryption is total: a valid transformation is guaranteed by the static types
// (see `Permits`), so `encrypt` cannot fail. Only `decrypt` can fail at runtime —
// from a wrong key, corrupted ciphertext, or malformed input — and those JCE
// failures are surfaced as a `Crypto.Error`.

extension [value: Encodable in Data](value: value)
  def encrypt[cipher <: Cipher](iv: InitializationVector)
    ( using encryptor: Encryptor[cipher],
            algorithm: cipher & Encryption,
            erased weakness: Permit[Weakness[cipher]],
            erased authentication: Permit[Authentication[cipher]] )
  :   Data =

    algorithm.encrypt(value.bytestream, encryptor.bytes, iv)

// Streaming encryption (block ciphers only) lazily transforms a `Chain`, driving
// the JCE cipher through update/doFinal. The IV is emitted as the leading chunk
// and the `NoPadding` alignment check runs at end-of-stream. Drain it within the
// `uncloak` block: beyond leaking, a stream drained after the block ends would
// read key bytes the cloak has already zeroed.

extension (stream: Chain[Data])
  def encrypt[cipher <: BlockCipher](iv: InitializationVector)
    ( using encryptor: Encryptor[cipher],
            algorithm: cipher & Encryption,
            erased weakness: Permit[Weakness[cipher]],
            erased authentication: Permit[Authentication[cipher]] )
  :   Chain[Data] =

    algorithm.encryptStream(stream, encryptor.bytes, iv)

extension (consume stream: (zephyrine.Stream[Data] over zephyrine.Credit)^)
  // Kernel-native streaming encryption: the pipeline-stage counterpart of the
  // `Chain` form above, with the same IV-prefix framing.
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
            tactic:     Tactic[Crypto.Error],
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
  :   decodable raises Crypto.Error =

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
        then abort(Crypto.Error(Crypto.Error.Reason.BadPadding, detail(error)))
        else if securityException(error, "javax.crypto.IllegalBlockSizeException")
        then abort(Crypto.Error(Crypto.Error.Reason.IllegalBlockSize, detail(error)))
        else if securityException(error, "java.security.InvalidKeyException")
        then abort(Crypto.Error(Crypto.Error.Reason.InvalidKey, detail(error)))
        else if securityException(error, "java.security.GeneralSecurityException")
        then abort(Crypto.Error(Crypto.Error.Reason.IoFailure, detail(error)))
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
    key.secret.uncloak: bytes => block(using Decryptor(Array.unsafeFrozen(bytes)))

extension [cipher <: Cipher](key: SymmetricKey[cipher]^)
  def uncloak[result](block: (Encryptor[cipher]^, Decryptor[cipher]^) ?=> result): result =
    key.secret.uncloak: bytes =>
      val data = Array.unsafeFrozen(bytes)
      block(using Encryptor(data), Decryptor(data))

// Key agreement lends the shared secret to the block as a transient `Data` view of an array
// that is zeroed when the block exits, as `uncloak` lends key material: a protocol hashes the
// secret into its session keys and has no use for it afterwards, so nothing of it should
// outlive the block.
extension [cipher <: Cipher](key: PrivateKey[cipher]^)
  def agree[result](peer: PublicKey[cipher])(using algorithm: cipher & Agreeing)
    ( block: Data => result )
  :   result =

    key.secret.uncloak: bytes =>
      val shared = Array.unsafeJvm(algorithm.agree(Array.unsafeFrozen(bytes), peer.bytes))
      try block(Array.unsafeFrozen(shared)) finally java.util.Arrays.fill(shared, 0.toByte)

// Authenticated encryption needs no `Permit`: an AEAD cipher is not `Unauthenticated`, and the
// nonce discipline it does need — never repeating one under a key — is the caller's, which is
// why the nonce is a plain argument rather than an `InitializationVector` strategy.
extension [cipher <: Cipher](key: SymmetricKey[cipher]^)
  def seal(nonce: Data, associated: Data, plaintext: Data)(using algorithm: cipher & Authenticated)
  :   Data =

    key.secret.uncloak: bytes =>
      algorithm.seal(Array.unsafeFrozen(bytes), nonce, associated, plaintext)

  // A real `using` clause rather than the `raises` sugar: a context-function result hides the
  // `key` parameter from the separation checker.
  def unseal(nonce: Data, associated: Data, ciphertext: Data)
    ( using algorithm:   cipher & Authenticated,
            diagnostics: Diagnostics,
            tactic:      Tactic[Crypto.Error] )
  :   Data =

    key.secret.uncloak: bytes =>
      algorithm.unseal(Array.unsafeFrozen(bytes), nonce, associated, ciphertext).or:
        abort(Crypto.Error(Crypto.Error.Reason.BadTag))

// A keystream is applied through `xor`, which encrypts and decrypts alike; the counter lets a
// protocol start partway through the stream, as OpenSSH does to keep block zero for the Poly1305
// key.
extension [cipher <: Cipher](key: SymmetricKey[cipher]^)
  def xor(nonce: Data, counter: Int, data: Data)(using algorithm: cipher & Keystream): Data =
    key.secret.uncloak(bytes => algorithm.xor(Array.unsafeFrozen(bytes), nonce, counter, data))
