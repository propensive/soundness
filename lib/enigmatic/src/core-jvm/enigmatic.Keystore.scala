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

import proscenium.compat.*

import java.io as ji
import java.security as js
import java.util as ju

import anticipation.*
import aperture.*
import contingency.*
import prepositional.*
import rudiments.*
import vacuous.*

// The form for PKCS#12 keystores: `path.open[Keystore](Password(t"..."))`. The password is
// passed as a flag -- enigmatic's opaque `Password`, so the secret neither appears in the call
// nor renders in diagnostics -- and the handle serves the store's aliases and DER-encoded
// certificates for the duration of the scope.
trait Keystore

// The contextual keystore handle within an `open[Keystore]` block, in the manner of galilei's
// `file`. Transparent inline so the handle's precise (grant-refined, capturing) type is
// preserved.
transparent inline def keystore(using handle: Keystore.KeystoreHandle^): handle.type = handle

object Keystore:
  class KeystoreHandle private[enigmatic] (keystore: js.KeyStore)
  extends caps.ExclusiveCapability:

    def aliases: List[Text] =
      val enumeration = keystore.aliases.nn
      val builder = scala.collection.immutable.List.newBuilder[Text]
      while enumeration.hasMoreElements do builder += enumeration.nextElement.nn.tt
      List.of(builder.result())

    // The DER-encoded (X.509) certificate stored under `alias`, if any.
    def certificate(alias: Text): Optional[Data] =
      keystore.getCertificate(alias.s) match
        case null        => Unset
        case certificate => certificate.getEncoded.nn.immutable(using Unsafe)

  // A named class rather than an anonymous given instance, for the reasons documented on
  // galilei's `FileOpenable`. Read-only until staged keystore writing lands.
  class KeystoreOpenable[path: Abstractable across Paths to Text]
    ( using keystoreError: Tactic[KeystoreError] )
  extends Openable:

    type Self = path
    type Form = Keystore
    type Operand = Password
    type Result = KeystoreHandle

    def open[grants <: Grant, result]
      ( value: path, mode: Mode granting grants, flags: List[Password] )
      ( block: ((KeystoreHandle & Granting[grants])^) ?=> result )
    :   result =

      if mode.atoms.has(Write)
      then abort(KeystoreError(KeystoreError.Reason.WriteUnsupported))

      val in = ji.BufferedInputStream(ji.FileInputStream(value.generic.s))

      try
        val keystore = js.KeyStore.getInstance("PKCS12").nn

        // A missing password loads without an integrity check, per `KeyStore.load`. The
        // cleartext is lent as a mutable `Char` array, zeroed when the block exits.
        flags.prim.lay(loadKeystore(keystore, in, null)): password =>
          password.uncloak(loadKeystore(keystore, in, cleartext.chars))

        block(using new KeystoreHandle(keystore) with Granting[grants] {})
      finally in.close()

    // Public, and failure-wrapping: any of the JDK's load-time exceptions (bad password, bad
    // format, truncation) becomes `Unreadable`, which deliberately does not distinguish a
    // wrong password from a corrupt store.
    def loadKeystore(keystore: js.KeyStore, in: ji.InputStream, password: Array[Char] | Null)
    :   Unit =
      try keystore.load(in, password)
      catch case error: Exception => abort(KeystoreError(KeystoreError.Reason.Unreadable))

  given openable: [path: Abstractable across Paths to Text]
  =>  Tactic[KeystoreError]
  =>  ( KeystoreOpenable[path]^ ) =
    KeystoreOpenable[path]
