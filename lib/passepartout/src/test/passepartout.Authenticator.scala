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
package passepartout

import soundness.*

import strategies.throwUnsafely
import charsets.utf8Charset, codepages.utf8Codepage, textSanitizers.skipSanitizer
import errorDiagnostics.stackTracesDiagnostics
import providers.javaBaseProvider
import cloaks.heapCloak
import alphabets.base64Url
import formatting.compactJsonFormatting
import dysasymptotics.linearSize

// CBOR written by hand, a byte at a time, so that what the tests feed the relying party owes
// nothing to the encoder the relying party's own dependencies use.
object Wire:
  def concat(parts: List[Data]): Data = parts match
    case first :: rest => Array.concat(first, concat(rest))
    case _             => Data()

  def join(parts: Data*): Data = concat(parts)
  def byte(value: Int): Data = Array.fill[Byte](1)(value.toByte)

  def be(value: Long, size: Int): Data =
    Array.tabulate[Byte](size)(index => (value >>> (8*(size - 1 - index))).toByte)

  private def head(major: Int, argument: Long): Data =
    if argument < 24 then byte((major << 5) | argument.toInt)
    else if argument < 256 then join(byte((major << 5) | 24), be(argument, 1))
    else join(byte((major << 5) | 25), be(argument, 2))

  def integer(value: Long): Data = if value >= 0 then head(0, value) else head(1, -1 - value)
  def bytes(value: Data): Data = join(head(2, value.length), value)
  def text(value: Text): Data = join(head(3, value.in[Data].length), value.in[Data])
  def array(items: List[Data]): Data = concat(head(4, items.size) :: items)

  def map(entries: (Data, Data)*): Data =
    concat(head(5, entries.size) :: entries.map(entry => Array.concat(entry(0), entry(1))))

// How an authenticator attests the credential it makes.
enum Conveyance:
  case None, Self, Certified, Unknown

// A software authenticator, and the browser in front of it: it answers the two ceremonies'
// options with the JSON a page would send back. The credential's key is `sign`'s to use and
// `publicKey`'s to describe, so that one class serves every algorithm.
abstract class Authenticator(origin: Text):
  val aaguid: Data = Array.tabulate[Byte](16)(index => (0xa0 + index).toByte)
  val credential: Data = Array.tabulate[Byte](24)(index => (index*7 + 3).toByte)

  // [test-harness] the signature counter an authenticator keeps
  @scala.caps.unsafe.untrackedCaptures
  var counter: Long = 0L

  // What the user does, and where the page thinks it is: each can be changed to misbehave.
  // [test-harness] whether the user is verified
  @scala.caps.unsafe.untrackedCaptures
  var verifies: Boolean = false

  // [test-harness] whether the user is present
  @scala.caps.unsafe.untrackedCaptures
  var presents: Boolean = true

  // [test-harness] the origin the page reports
  @scala.caps.unsafe.untrackedCaptures
  var location: Text = origin

  def algorithm: Long
  def publicKey: Data
  def sign(message: Data): Data

  // The attestation key and its certificate chain, for `Conveyance.Certified`.
  def attest(message: Data): Data = Data()
  def chain: List[Certificate] = Nil

  def encode(data: Data): Text = data.serialize[Base64]

  private def clientData(ceremony: Text, challenge: Text): Data =
    t"""{"type":"$ceremony","challenge":"$challenge","origin":"$location","crossOrigin":false}""".in[Data]

  private def flags(attested: Boolean): Int =
    (if presents then 0x01 else 0) | (if verifies then 0x04 else 0) | (if attested then 0x40 else 0)

  private def authenticatorData(party: Text, attested: Boolean): Data =
    counter += 1
    val base = Wire.join(party.in[Data].digest[Sha2[256]].data, Wire.byte(flags(attested)), Wire.be(counter, 4))

    if !attested then base
    else Wire.join(base, aaguid, Wire.be(credential.length, 2), credential, publicKey)

  // The answer to `navigator.credentials.create`.
  def create(options: Json, conveyance: Conveyance = Conveyance.None): Json =
    val client = clientData(t"webauthn.create", options(t"challenge").as[Text])
    val data = authenticatorData(options(t"rp")(t"id").as[Text], true)
    val signed = Wire.join(data, client.digest[Sha2[256]].data)

    val (format, statement) = conveyance match
      case Conveyance.None =>
        (t"none", Wire.map())

      case Conveyance.Unknown =>
        (t"fido-u2f", Wire.map())

      case Conveyance.Self =>
        val entries = Wire.map
          ( Wire.text(t"alg") -> Wire.integer(algorithm),
            Wire.text(t"sig") -> Wire.bytes(sign(signed)) )

        (t"packed", entries)

      case Conveyance.Certified =>
        val certificates = chain.map(certificate => Wire.bytes(certificate.asn1.in[Der].data))

        val entries = Wire.map
          ( Wire.text(t"alg") -> Wire.integer(-7L),
            Wire.text(t"sig") -> Wire.bytes(attest(signed)),
            Wire.text(t"x5c") -> Wire.array(certificates) )

        (t"packed", entries)

    val attestation = Wire.map
      ( Wire.text(t"fmt")      -> Wire.text(format),
        Wire.text(t"attStmt")  -> statement,
        Wire.text(t"authData") -> Wire.bytes(data) )

    t"""{"id":"${encode(credential)}","rawId":"${encode(credential)}","type":"public-key",
         "response":{"clientDataJSON":"${encode(client)}",
                     "attestationObject":"${encode(attestation)}"},
         "clientExtensionResults":{}}""".read[Json]

  // The answer to `navigator.credentials.get`.
  def get(options: Json): Json =
    val client = clientData(t"webauthn.get", options(t"challenge").as[Text])
    val data = authenticatorData(options(t"rpId").as[Text], false)
    val signature = sign(Wire.join(data, client.digest[Sha2[256]].data))

    t"""{"id":"${encode(credential)}","rawId":"${encode(credential)}","type":"public-key",
         "response":{"clientDataJSON":"${encode(client)}",
                     "authenticatorData":"${encode(data)}",
                     "signature":"${encode(signature)}"},
         "clientExtensionResults":{}}""".read[Json]
