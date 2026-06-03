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

import java.security as js
import javax.crypto as jc

import anticipation.*
import contingency.*
import distillate.*
import gossamer.*
import prepositional.*
import vacuous.*

// Encryption is total: a valid transformation is guaranteed by the static types
// (see `Permits`), so `encrypt` cannot fail. Only `decrypt` can fail at runtime —
// from a wrong key, corrupted ciphertext, or malformed input — and those JCE
// failures are surfaced as a `CryptoError`.

extension [value: Encodable in Data](value: value)
  def encrypt[cipher <: Cipher]
    ( using encryptor: Encryptor[cipher], algorithm: cipher & Encryption )
  :   Data =

    algorithm.encrypt(value.bytestream, encryptor.bytes)

// Streaming encryption (block ciphers only) lazily transforms a `Stream`, driving
// the JCE cipher through update/doFinal. The IV is emitted as the leading chunk
// and the `NoPadding` alignment check runs at end-of-stream. Drain it within the
// `expose` block — only the fixed ciphertext of `stream` could otherwise leak.

extension (stream: Stream[Data])
  def encrypt[cipher <: BlockCipher]
    ( using encryptor: Encryptor[cipher], algorithm: cipher & Encryption )
  :   Stream[Data] =

    algorithm.encryptStream(stream, encryptor.bytes)

extension (data: Data)
  def decrypt[decodable: Decodable in Data, cipher <: Cipher]
    ( using decryptor: Decryptor[cipher], algorithm: cipher & Encryption )
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

// `expose` lends the key to the block as an `Encryptor`/`Decryptor` capability.
// Capture checking (which would confine the capability to this scope) is not yet
// enabled; the capability types are kept so it can be turned on as an enhancement.

extension [cipher <: Cipher](key: PublicKey[cipher])
  def expose[result](block: Encryptor[cipher] ?=> result): result =
    block(using Encryptor(key.bytes))

extension [cipher <: Cipher](key: PrivateKey[cipher])
  def expose[result](block: Decryptor[cipher] ?=> result): result =
    block(using Decryptor(key.privateData))

extension [cipher <: Cipher](key: SymmetricKey[cipher])
  def expose[result](block: (Encryptor[cipher], Decryptor[cipher]) ?=> result): result =
    block(using Encryptor(key.bytes), Decryptor(key.bytes))
