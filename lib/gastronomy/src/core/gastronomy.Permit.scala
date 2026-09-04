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
package gastronomy

import scala.caps

import scala.annotation.implicitNotFound

// Capability evidence that a `concession` (a weak algorithm, key length or mode)
// is permitted. Following NIST SP 800-131A's apply-vs-process distinction:
// `ProcessingPermit` allows *processing already-protected* data (decrypt/verify),
// while `Permit` (a subtype) additionally allows *applying new protection*
// (encrypt/sign/hash). Both are erased — purely compile-time gates with no runtime
// cost. Bring them into scope with a `cryptoPermits.permit…Crypto` import. Defined here in
// gastronomy so both gastronomy (weak hashes) and enigmatic (weak ciphers) share
// one permit vocabulary.

@implicitNotFound("this operation uses an algorithm whose status is \"legacy use\" or worse; "+
    "import a permit (e.g. `cryptoPermits.permitLegacyCrypto` to process existing data, or "+
    "`cryptoPermits.permitDisallowedCrypto`) to allow it")
object ProcessingPermit:
  erased given acceptable: ProcessingPermit[Concession.Acceptable] = caps.unsafe.unsafeErasedValue

trait ProcessingPermit[concession]

@implicitNotFound("this operation uses a sub-optimal algorithm, key length or mode — or a "+
    "checksum in place of a hash; import the matching permit "+
    "(`cryptoPermits.permitNonCryptographicHashes` for CRC-32/CRC-64/Adler-32, or "+
    "`cryptoPermits.permitUnauthenticatedCrypto`, `cryptoPermits.permitDeprecatedCrypto` or "+
    "`cryptoPermits.permitDisallowedCrypto`) to allow it")
object Permit:
  // `Acceptable` crypto needs no permission, so this permit is always available.
  erased given acceptable: Permit[Concession.Acceptable] = caps.unsafe.unsafeErasedValue

trait Permit[concession] extends ProcessingPermit[concession]
