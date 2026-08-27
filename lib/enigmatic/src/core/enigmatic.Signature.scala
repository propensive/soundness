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

import anticipation.*
import gossamer.*
import monotonous.*
import prepositional.*
import spectacular.*

object Signature:
  given showable: [signature <: Cipher] => Signature[signature] is Showable = sig =>
    import alphabets.base64Standard
    t"Signature(${sig.bytes.serialize[Base64]})"

  given encodable: [cipher <: Cipher] => Signature[cipher] is Encodable in Data = _.bytes

  // Hexadecimal rather than `showable`'s Base64, which needs an `Alphabet` in scope; the whole
  // signature is shown, since a signature which differs anywhere is a different signature.
  given inspectable: [signature <: Signature[?]] => signature is Inspectable = signature =>
    t"Signature(${Inspection.hex(signature.bytes)})"

  // SignatureDigest → Signature.Digest
  // The digest an asymmetric signature is taken over. RSA and ECDSA sign a hash of the message
  // rather than the message itself, and the choice of hash is part of the signature algorithm's
  // identity — `SHA256withRSA` is a different algorithm from `SHA384withRSA`, with its own object
  // identifier — so it is fixed when the cipher is summoned rather than chosen per signature.
  //
  // SHA-256 is the default, since it is what every certificate profile in current use mandates.
  // Import `signatureDigests.sha384Signature` or `signatureDigests.sha512Signature` to override it;
  // an imported given outranks the one below.
  object Digest:
    given sha256Signature: Signature.Digest = Signature.Digest(t"SHA256")

  // `token` is the digest's name as it appears in a JCE transformation, e.g. `SHA256` in
  // `SHA256withRSA`. It has no hyphen, unlike the same digest's name as a `Hash`.
  case class Digest(token: Text)

case class Signature[+cipher <: Cipher](bytes: Data)
