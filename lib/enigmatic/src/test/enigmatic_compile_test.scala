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

import soundness.*

import charEncoders.utf8Encoder
import blockCipherModes.cbc, blockCipherPaddings.pkcs7
import providers.javaBaseProvider
import cryptoPermits.permitUnauthenticatedCrypto   // AES-CBC is unauthenticated
import cloaks.heapCloak

// Compile-time regressions for the cipher API. (Capture-checking confinement of
// the lent `Encryptor`/`Decryptor` capability is enforced and regression-tested
// in `CaptureTests`.)
object CompileChecks:
  val key: SymmetricKey[Aes[256]] = SymmetricKey.generate[Aes[256]]()

  val ciphertext: Data = key.uncloak:
    t"Hello world".encrypt(InitializationVector.random)

  // Validity regression: only cipher/mode/padding triples the JDK supports have a
  // `given`, so an invalid combination does not compile. CTR permits only
  // `NoPadding`, so the line below fails with "no implicit values were found that
  // match type Ctr Permits Pkcs7" (verified manually — uncomment to re-check).
  //
  //   val invalid = SymmetricKey.generate[Aes[256] over Ctr against Pkcs7]()
  val valid = SymmetricKey.generate[Aes[256] over Cbc against Pkcs7]()

  // Totality regression: `NoPadding` can fail on misaligned input, so its `given`
  // demands a `Tactic[Crypto.Error]`. With no error-handling strategy in scope,
  // summoning a `NoPadding` cipher (and hence encrypting with one) does not
  // compile, whereas every padded cipher is total (verified manually — uncomment).
  //
  //   val noTactic = SymmetricKey.generate[Aes[256] over Cbc against NoPadding]()

  // Permission regression: only `cryptoPermits.permitUnauthenticatedCrypto` is imported
  // here, so AES (above) compiles, but reaching a "disallowed" algorithm without
  // `cryptoPermits.permitDisallowedCrypto` does not — encrypting with DES fails with the
  // `Permit` "no given instance" diagnostic (verified manually — uncomment). Note
  // that key generation is *not* gated; only the encryption operation is.
  //
  //   val desKey = SymmetricKey.generate[Des over Cbc against Pkcs7]()
  //   val desText = desKey.uncloak(t"Hello world".encrypt(InitializationVector.random))
