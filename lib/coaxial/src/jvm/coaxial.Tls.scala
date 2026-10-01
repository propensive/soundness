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
package coaxial

import java.io as ji
import java.security as js
import javax.net.ssl as jns
import javax.net.ssl.SSLContext

import anticipation.*
import gastronomy.*
import gossamer.*
import rudiments.*
import vacuous.*

import providers.javaBaseProvider

// Configuration for a TLS client connection (a `SecureEndpoint`). `context` supplies the
// trust and key material — `Unset` means the JVM default `SSLContext` (the system trust
// store), which is what you want for a public `wss`/HTTPS peer. `verify` toggles hostname
// verification (RFC 2818 endpoint identification); it is ON by default and should only be
// turned off for a self-signed peer whose certificate you already trust out of band.
// `protocols` lists the ALPN application protocols to offer, in preference order (e.g.
// `h2`, `http/1.1`); empty means no ALPN is offered, preserving the plain-TLS handshake a
// `wss` peer expects. `versions` restricts the TLS protocol versions (e.g. `TLSv1.3`);
// empty accepts the context's defaults. `mutual` makes a `SecurePort` listening with this
// configuration demand a certificate of every client (and a client's context, see
// `TlsAcceptance#keyed`, must then present one), so that both peers authenticate. The default
// `given` is fully secure and offers no ALPN. A `TlsAcceptance` (the richer, permit-gated
// trust policy) is presented in this form via its `tls(...)` and `keyed(...)` extensions.
object Tls:
  given Tls = Tls()

  // The key material of a PKCS#12 keystore, as a `Tls` whose context PRESENTS it: the
  // configuration a `SecurePort` listens with, or a client authenticating itself to a peer.
  // Trust is the platform's default; `TlsAcceptance#keyed` presents the same material under
  // any other acceptance. The store's bytes are passed rather than a path, so the caller
  // decides where secrets live.
  def keyed(keystore: Data, password: Text): Tls = TlsAcceptance().keyed(keystore, password)

  // The DER encoding of the first certificate the keystore holds under a private key: the
  // certificate a `SecurePort` bound with `keyed(keystore, password)` presents, whose
  // `fingerprint` a client pins with `TlsAcceptance.pinning`.
  def certificate(keystore: Data, password: Text): Optional[Data] =
    val store = load(keystore, password)
    val aliases = store.aliases.nn
    var found: Optional[Data] = Unset

    while found.absent && aliases.hasMoreElements do
      val alias = aliases.nextElement.nn

      if store.isKeyEntry(alias) then
        store.getCertificate(alias) match
          case null        => ()
          case certificate => found = Array.unsafeFrozen(certificate.getEncoded.nn)

    found

  // The SHA-256 digest of a certificate's DER encoding: the identity a `TlsAcceptance.pinning`
  // compares, and what `openssl x509 -fingerprint -sha256` prints.
  def fingerprint(certificate: Data): Data = certificate.digest[Sha2[256]].data

  private[coaxial] def load(keystore: Data, password: Text): js.KeyStore =
    val store = js.KeyStore.getInstance("PKCS12").nn
    val in = ji.ByteArrayInputStream(keystore.unsafeMutable(using Unsafe))
    try store.load(in, password.s.toCharArray) finally in.close()
    store

case class Tls
  ( context:   Optional[SSLContext] = Unset,
    verify:    Boolean = true,
    protocols: List[Text] = Nil,
    versions:  List[Text] = Nil,
    mutual:    Boolean = false )
