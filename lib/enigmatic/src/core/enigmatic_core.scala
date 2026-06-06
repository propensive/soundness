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

import anticipation.*
import gastronomy.*
import prepositional.*
import rudiments.*
import vacuous.*

extension [encodable: Encodable in Data](value: encodable)
  def hmac[algorithm <: Algorithm](key: Data)
      (using hash: Hash in algorithm, crypto: Crypto, erased weakness: Permit[HashWeakness[algorithm]])
  :   Hmac in algorithm =

    Hmac(crypto.hmac(hash.hmacName).mac(key, encodable.encode(value)))

package blockCipherMode:
  export Cbc.mode as cbc
  export Ctr.mode as ctr
  export Cfb.mode as cfb
  export Ofb.mode as ofb
  // `Ecb.mode` is not re-exported: it is now a context-function given (it requires
  // an erased `Permit[Concession.Ecb]`), and re-exporting such a given trips a
  // compiler assertion. ECB is summoned from its own companion; use `over Ecb`.

package blockCipherPadding:
  export Pkcs7.padding as pkcs7
  export Iso10126.padding as iso10126
  // `NoPadding` is not re-exported for import-based inference: its `given` takes a
  // `Tactic[CryptoError]`, and re-exporting a context-function given trips a
  // compiler assertion ("bad adapt"). Use `against NoPadding` explicitly instead.

package initializationVector:
  // `random` is the default `InitializationVector` (in `InitializationVector`'s
  // companion). It is not re-exported here: it is now a context-function given
  // over `Crypto`, and re-exporting such a given trips a compiler assertion
  // ("bad adapt") — see the note in `blockCipherPadding` above.
  given zero: InitializationVector = size => Array.fill[Byte](size)(0).immutable(using Unsafe)

// Crypto providers select the algorithmic backend. Pick one with an explicit
// import, e.g. `import cryptoProviders.javaStdlibCrypto`. The given's type is the
// provider object's singleton type, so the optional (structurally-typed)
// algorithms it offers remain visible to consumers that require them.
package cryptoProviders:
  given javaStdlibCrypto: JavaStdlibCrypto.type = JavaStdlibCrypto

// The `Permit`/`Concession` machinery and the `crypto.permit…Crypto` aggregates
// live in gastronomy (shared with hashing); the cipher concessions they cover are
// mapped from cipher types by `Weakness`/`Authentication` in `enigmatic.Weakness`.
