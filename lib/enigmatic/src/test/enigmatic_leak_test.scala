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

import language.experimental.captureChecking

import soundness.*

import charEncoders.utf8
import blockCipherMode.cbc, blockCipherPadding.pkcs7

// Capture-checking regression for `expose`.
//
// The exposed `Encryptor`/`Decryptor` capability must not escape its scope; only
// pure values (e.g. the ciphertext `Data`) may leave the block. The commented-out
// line fails to compile with a capture error of the form
//
//   Capability `contextual$1` outlives its scope: it leaks into outer capture set
//   's1 which is owned by value escaped.
//
// (verified manually — uncomment to re-check).
object LeakCheck:
  val key: SymmetricKey[Aes[256]] = SymmetricKey.generate[Aes[256]]()

  // Legitimate: only the pure `Data` ciphertext leaves the scope.
  val ciphertext: Data = key.expose:
    t"Hello world".encrypt

  // LEAK (must NOT compile): smuggling the capability out directly.
  //
  //   val escaped = key.expose:
  //     summon[Encryptor[Aes[256]]]

  // Validity regression: only cipher/mode/padding triples the JDK supports have a
  // `given`, so an invalid combination does not compile. CTR permits only
  // `NoPadding`, so the line below fails with "no implicit values were found that
  // match type Ctr Permits Pkcs7" (verified manually — uncomment to re-check).
  //
  //   val invalid = SymmetricKey.generate[Aes[256] over Ctr against Pkcs7]()
  val valid = SymmetricKey.generate[Aes[256] over Cbc against Pkcs7]()

  // Totality regression: `NoPadding` can fail on misaligned input, so its `given`
  // demands a `Tactic[CryptoError]`. With no error-handling strategy in scope,
  // summoning a `NoPadding` cipher (and hence encrypting with one) does not
  // compile, whereas every padded cipher is total (verified manually — uncomment).
  //
  //   val noTactic = SymmetricKey.generate[Aes[256] over Cbc against NoPadding]()
