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

// Capture checking is enabled per-file: the rest of the test module compiles
// without it (its `NoPadding` tests summon a tactic-capturing padding given that
// pure `using cipher` parameters cannot yet accept). The import is part of the
// source text, so larceny's sub-compilation of `demilitarize` blocks sees it too.
import language.experimental.captureChecking

import soundness.*

import strategies.throwUnsafely
import charDecoders.utf8Decoder, charEncoders.utf8Encoder, textSanitizers.skipSanitizer
import gossamer.textDecodable
import providers.javaStdlibProvider
import crypto.permitDisallowedCrypto   // RSA-1024 below is weak; AES-CBC is unauthenticated

// `expose` lends a key to its block as an `Encryptor`/`Decryptor` capability, and
// capture checking confines the capability to that block: these tests demonstrate
// that key material cannot be smuggled out — neither directly, nor closed over —
// while legitimate use within the block remains unaffected.
object CaptureTests extends Suite(m"Capability confinement tests"):
  def run(): Unit =
    test(m"expose still works as normal under capture checking"):
      val key = SymmetricKey.generate[Aes[256] over Cbc against Pkcs7]()
      key.expose:
        t"Hello world".encrypt(InitializationVector.random).decrypt.as[Text]
    . assert(_ == t"Hello world")

    test(m"a roundtrip within expose compiles without errors"):
      demilitarize:
        val key = SymmetricKey.generate[Aes[256] over Cbc against Pkcs7]()
        key.expose:
          t"Hello world".encrypt(InitializationVector.random).decrypt.as[Text]
    . assert(_ == Nil)

    test(m"the Encryptor capability cannot be returned from expose"):
      demilitarize:
        val key = PrivateKey.generate[Rsa[1024]]()
        val stolen = key.public.expose(summon[Encryptor[Rsa[1024]]])
      . map(_.message)
    . assert(_.exists(_.contains("outlives its scope")))

    test(m"the Decryptor capability cannot be returned from expose"):
      demilitarize:
        val key = PrivateKey.generate[Rsa[1024]]()
        val stolen = key.expose(summon[Decryptor[Rsa[1024]]])
    . assert(_.nonEmpty)

    test(m"a closure encrypting later cannot escape expose"):
      demilitarize:
        val key = SymmetricKey.generate[Aes[256] over Cbc against Pkcs7]()
        val later = key.expose:
          () => t"secret".encrypt(InitializationVector.random)
    . assert(_.nonEmpty)

    test(m"a closure decrypting later cannot escape expose"):
      demilitarize:
        val key = SymmetricKey.generate[Aes[256] over Cbc against Pkcs7]()
        val ciphertext = key.expose(t"secret".encrypt(InitializationVector.random))
        val later = key.expose(() => ciphertext.decrypt.as[Text])
    . assert(_.nonEmpty)

    test(m"the capability cannot be stashed in an outer variable"):
      demilitarize:
        val key = SymmetricKey.generate[Aes[256] over Cbc against Pkcs7]()
        var stash: () => Unit = () => ()
        key.expose:
          stash = () => { t"secret".encrypt(InitializationVector.random); () }
      . map(_.message)
    . assert(_.exists(_.contains("is not included in the allowed capture set")))

    // A lazily-encrypted stream may leave the block: the ciphertext `LazyList` is
    // pure (the key bytes are baked into the deferred JCE cipher, beyond the reach
    // of capture checking), which is why `encrypt`'s documentation says to drain
    // streams within the `expose` block. This is an executable record of that
    // caveat; if it ever starts failing, the documentation should be updated.
    test(m"caveat: a lazily-encrypted stream escapes by design"):
      demilitarize:
        val key = SymmetricKey.generate[Aes[256] over Cbc against Pkcs7]()
        val ciphertext = key.expose:
          LazyList(t"Hello world".in[Data]).encrypt(InitializationVector.random)
    . assert(_ == Nil)

    test(m"a password's cleartext is available within expose"):
      Password(t"hunter2").expose(cleartext.text)
    . assert(_ == t"hunter2")

    test(m"the Cleartext capability cannot be returned from expose"):
      demilitarize:
        val password = Password(t"hunter2")
        val stolen = password.expose(summon[Cleartext])
      . map(_.message)
    . assert(_.exists(_.contains("outlives its scope")))

    test(m"a closure reading the cleartext later cannot escape expose"):
      demilitarize:
        val password = Password(t"hunter2")
        val later = password.expose(() => cleartext.text)
    . assert(_.nonEmpty)

    test(m"a password never renders its secret"):
      Password(t"hunter2").show
    . assert(_ == t"Password(•••)")
