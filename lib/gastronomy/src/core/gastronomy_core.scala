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
package gastronomy

import anticipation.*
import prepositional.*
import turbulence.*

// Hashing providers supply the per-algorithm implementations. Pick one (or both)
// with an explicit import, e.g. `import hashProviders.javaStdlibHashing` for the
// JDK hashes and `import hashProviders.soundnessHashing` for BLAKE3. The given's
// type is the provider object's singleton type, so the structurally-typed
// algorithms it offers stay visible to the per-algorithm `Hash` givens.
package hashProviders:
  given javaStdlibHashing: JavaStdlibHashing.type = JavaStdlibHashing
  given soundnessHashing: SoundnessHashing.type = SoundnessHashing

// Opt-in permits for sub-optimal cryptography, named after NIST SP 800-131A's
// algorithm statuses. Each is an (erased) intersection of fine-grained `Permit`s,
// covering both hashes (here) and ciphers (added downstream by enigmatic). The
// levels nest by inclusion: `permitDisallowedCrypto` contains every other permit,
// and since `Permit <: ProcessingPermit` it also covers "legacy use".
package crypto:
  // Unauthenticated (non-AEAD) encryption — every block-cipher mode (enigmatic).
  erased given permitUnauthenticatedCrypto: Permit[Concession.Unauthenticated] =
    caps.unsafe.unsafeErasedValue

  // "Deprecated": usable but transitional — Triple-DES, and SHA-1 (still common in
  // legacy formats such as Git).
  erased given permitDeprecatedCrypto: Permit[Concession.TripleDes] & Permit[Concession.Sha1] =
    caps.unsafe.unsafeErasedValue

  // "Legacy use": processing already-protected data only (decrypt/verify).
  erased given permitLegacyCrypto
      :   ProcessingPermit[Concession.TripleDes] & ProcessingPermit[Concession.Dsa] =

    caps.unsafe.unsafeErasedValue

  // "Disallowed": broken or non-approved algorithms, key lengths and modes (incl.
  // MD5 and SHA-1); subsumes every weaker permit above.
  erased given permitDisallowedCrypto
  : Permit[Concession.Des]
    & Permit[Concession.Rc2]
    & Permit[Concession.Blowfish]
    & Permit[Concession.TripleDes]
    & Permit[Concession.Dsa]
    & Permit[Concession.SmallRsa]
    & Permit[Concession.Ecb]
    & Permit[Concession.Unauthenticated]
    & Permit[Concession.Md5]
    & Permit[Concession.Sha1] =

    caps.unsafe.unsafeErasedValue

  // Year-based permits: an alternative to the named levels, composed from the same
  // fine-grained `Permit`s. `permitCryptoThrough<year>` permits every algorithm
  // NIST disallowed on or before that year, so a later year is strictly more
  // permissive. (Unauthenticated/ECB are not date-based and are not included.)

  // Through 2014: DES, the never-approved MD5/RC2/Blowfish, and RSA/DSA key
  // lengths below 2048 bits (disallowed at the end of 2013).
  erased given permitCryptoThrough2014
      : Permit[Concession.Des]
      & Permit[Concession.Md5]
      & Permit[Concession.Rc2]
      & Permit[Concession.Blowfish]
      & Permit[Concession.SmallRsa] =
    caps.unsafe.unsafeErasedValue

  // Through 2024: also Triple-DES (encryption disallowed after 2023) and DSA
  // signatures (removed in FIPS 186-5).
  erased given permitCryptoThrough2024
      : Permit[Concession.Des]
      & Permit[Concession.Md5]
      & Permit[Concession.Rc2]
      & Permit[Concession.Blowfish]
      & Permit[Concession.SmallRsa]
      & Permit[Concession.TripleDes]
      & Permit[Concession.Dsa] =
    caps.unsafe.unsafeErasedValue

  // Through 2030: also SHA-1 (NIST's planned phase-out by 2030).
  erased given permitCryptoThrough2030
      : Permit[Concession.Des]
      & Permit[Concession.Md5]
      & Permit[Concession.Rc2]
      & Permit[Concession.Blowfish]
      & Permit[Concession.SmallRsa]
      & Permit[Concession.TripleDes]
      & Permit[Concession.Dsa]
      & Permit[Concession.Sha1] =
    caps.unsafe.unsafeErasedValue

extension [digestible: Digestible](value: digestible)
  def digest[hash <: Algorithm]
    ( using hashed: Hash in hash, erased weakness: Permit[HashWeakness[hash]] )
  :   Digest in hash =

    val digester = Digester(digestible.digest(_, value))
    digester.apply

extension [source: Streamable by Data](source: source)
  def checksum[hash <: Algorithm]
    ( using hashed: Hash in hash, erased weakness: Permit[HashWeakness[hash]] )
  :   Digest in hash =

    source.stream[Data].digest[hash]


// The concession of a hash algorithm: MD5 and SHA-1 are weak; everything else
// (SHA-2, BLAKE3, CRC-32) is `Acceptable` and needs no permission.
type HashWeakness[algorithm] = algorithm match
  case Md5  => Concession.Md5
  case Sha1 => Concession.Sha1
  case _    => Concession.Acceptable
