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
import corpuscular.*
import gossamer.*
import vacuous.*

// The `AlgorithmIdentifier` that names a cipher-and-digest pair in a certificate. It is a property
// of the pair, not of either alone: `sha256WithRSAEncryption` and `sha384WithRSAEncryption` are
// different object identifiers over the same key type.
object SignatureAlgorithm:
  // RSASSA-PKCS1-v1_5 identifiers live under the PKCS#1 arc, and RFC 3279 requires the parameters
  // field to be present and NULL — not absent.
  given rsa: [bits <: 1024 | 2048 | 3072 | 4096] => Rsa[bits] is SignatureAlgorithm = digest =>
    arc(digest, t"SHA256" -> 11, t"SHA384" -> 12, t"SHA512" -> 13).let: last =>
      Asn1.Sequence(List(Asn1.ObjectId(List(1, 2, 840, 113549, 1, 1, last)), Asn1.Null))

  // ECDSA identifiers live under the ANSI X9.62 arc, and RFC 5758 requires the parameters field to
  // be absent, since the curve is already named by the public key.
  given ecdsa: [bits <: 256 | 384 | 521] => Ecdsa[bits] is SignatureAlgorithm = digest =>
    arc(digest, t"SHA256" -> 2, t"SHA384" -> 3, t"SHA512" -> 4).let: last =>
      Asn1.Sequence(List(Asn1.ObjectId(List(1, 2, 840, 10045, 4, 3, last))))

  private def arc(digest: Signature.Digest, entries: (Text, Int)*): Optional[Int] =
    entries.find(_(0) == digest.token).map(_(1)).getOrElse(Unset)

trait SignatureAlgorithm:
  type Self

  // `Unset` when the cipher and digest have no object identifier between them, which a caller
  // turns into a `Certificate.Error` rather than an encoding that no verifier would recognize.
  def identifier(digest: Signature.Digest): Optional[Asn1]
