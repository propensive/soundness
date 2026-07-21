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
import cloaks.cloakHeap

// `uncloak` lends a key to its block as an `Encryptor`/`Decryptor` capability, and
// capture checking confines the capability to that block: these tests demonstrate
// that key material cannot be smuggled out — neither directly, nor closed over —
// while legitimate use within the block remains unaffected.
object CaptureTests extends Suite(m"Capability confinement tests"):
  def run(): Unit =
    test(m"uncloak still works as normal under capture checking"):
      val key = SymmetricKey.generate[Aes[256] over Cbc against Pkcs7]()
      key.uncloak:
        t"Hello world".encrypt(InitializationVector.random).decrypt.as[Text]
    . assert(_ == t"Hello world")

    test(m"a roundtrip within uncloak compiles without errors"):
      demilitarize:
        val key = SymmetricKey.generate[Aes[256] over Cbc against Pkcs7]()
        key.uncloak:
          t"Hello world".encrypt(InitializationVector.random).decrypt.as[Text]
    . assert(_ == Nil)

    test(m"the Encryptor capability cannot be returned from uncloak"):
      demilitarize:
        val key = PrivateKey.generate[Rsa[1024]]()
        val stolen = key.public.uncloak(summon[Encryptor[Rsa[1024]]])
      . map(_.message)
    . assert(_.exists(_.contains("outlives its scope")))

    test(m"the Decryptor capability cannot be returned from uncloak"):
      demilitarize:
        val key = PrivateKey.generate[Rsa[1024]]()
        val stolen = key.uncloak(summon[Decryptor[Rsa[1024]]])
    . assert(_.nonEmpty)

    test(m"a closure encrypting later cannot escape uncloak"):
      demilitarize:
        val key = SymmetricKey.generate[Aes[256] over Cbc against Pkcs7]()
        val later = key.uncloak:
          () => t"secret".encrypt(InitializationVector.random)
    . assert(_.nonEmpty)

    test(m"a closure decrypting later cannot escape uncloak"):
      demilitarize:
        val key = SymmetricKey.generate[Aes[256] over Cbc against Pkcs7]()
        val ciphertext = key.uncloak(t"secret".encrypt(InitializationVector.random))
        val later = key.uncloak(() => ciphertext.decrypt.as[Text])
    . assert(_.nonEmpty)

    test(m"the capability cannot be stashed in an outer variable"):
      demilitarize:
        val key = SymmetricKey.generate[Aes[256] over Cbc against Pkcs7]()
        var stash: () => Unit = () => ()
        key.uncloak:
          stash = () => { t"secret".encrypt(InitializationVector.random); () }
      . map(_.message)
    . assert(_.exists(_.contains("is not included in the allowed capture set")))

    // A lazily-encrypted stream may leave the block: the ciphertext `LazyList` is
    // pure (the key bytes are baked into the deferred JCE cipher, beyond the reach
    // of capture checking), which is why `encrypt`'s documentation says to drain
    // streams within the `uncloak` block. This is an executable record of that
    // caveat; if it ever starts failing, the documentation should be updated.
    test(m"caveat: a lazily-encrypted stream escapes by design"):
      demilitarize:
        val key = SymmetricKey.generate[Aes[256] over Cbc against Pkcs7]()
        val ciphertext = key.uncloak:
          LazyList(t"Hello world".in[Data]).encrypt(InitializationVector.random)
    . assert(_ == Nil)

    test(m"a password's cleartext is available within uncloak"):
      Password(t"hunter2").uncloak(String(cleartext.chars).tt)
    . assert(_ == t"hunter2")

    test(m"the Cleartext capability cannot be returned from uncloak"):
      demilitarize:
        val password = Password(t"hunter2")
        val stolen = password.uncloak(summon[Cleartext])
      . map(_.message)
    . assert(_.exists(_.contains("outlives its scope")))

    test(m"a closure reading the cleartext later cannot escape uncloak"):
      demilitarize:
        val password = Password(t"hunter2")
        val later = password.uncloak(() => cleartext.chars)
    . assert(_.nonEmpty)

    test(m"a password never renders its secret"):
      Password(t"hunter2").show
    . assert(_ == t"Password(•••)")

    test(m"an off-heap-cloaked key round-trips under capture checking"):
      import cloaks.cloakOffHeap
      val key = SymmetricKey.generate[Aes[256] over Cbc against Pkcs7]()
      key.uncloak:
        t"Hello world".encrypt(InitializationVector.random).decrypt.as[Text]
    . assert(_ == t"Hello world")

    test(m"an off-heap-cloaked key cannot be ascribed a pure type"):
      demilitarize:
        import cloaks.cloakOffHeap
        val key: SymmetricKey[Aes[256] over Cbc against Pkcs7] =
          SymmetricKey.generate[Aes[256] over Cbc against Pkcs7]()
    . assert(_.nonEmpty)
