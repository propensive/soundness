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

import java.io as ji
import java.math as jm
import java.nio.charset as jnc

import scala.caps

import anticipation.*
import contingency.*
import distillate.*
import fulminate.*
import gossamer.*
import monotonous.*, alphabets.base64Standard
import prepositional.*
import rudiments.*
import vacuous.*

// OpenSSH's two key formats: the `openssh-key-v1` container that `ssh-keygen` has written private
// keys in since OpenSSH 7.8 (armored as `OPENSSH PRIVATE KEY`), and the single line — type, base64
// blob, optional comment — that `id_*.pub`, `authorized_keys` and `known_hosts` hold a public key
// as. Both are built from the SSH wire encoding (RFC 4251 §5): big-endian `uint32`s, length-
// prefixed `string`s and two's-complement `mpint`s. Neither is ASN.1, so each is converted to the
// PKCS#8 or SubjectPublicKeyInfo document the providers take.
//
// Only unencrypted private keys are read: a passphrase-protected key (cipher and KDF other than
// `none`) needs bcrypt's KDF, which no provider offers, and is reported as `Encrypted`.
object OpenSsh:
  private val Magic: Text = t"openssh-key-v1\u0000"

  private val Ed25519Name: Text = t"ssh-ed25519"
  private val RsaName: Text = t"ssh-rsa"
  private val EcdsaPrefix: Text = t"ecdsa-sha2-"

  // The PKCS#8 form of the one private key in an `openssh-key-v1` container.
  def privateKey(container: Data)(using Tactic[PrivateKey.Error], Diagnostics): Data =
    import PrivateKey.Error.Reason

    def malformed(detail: Text): Nothing = abort(PrivateKey.Error(Reason.Malformed(detail)))
    val reader = Reader(container)(detail => PrivateKey.Error(Reason.Malformed(detail)))

    if reader.remaining < Magic.length || reader.text(Magic.length) != Magic
    then malformed(t"the document is not an openssh-key-v1 container")

    val cipher = reader.text()
    val kdf = reader.text()
    reader.string() // KDF options
    val count = reader.uint32()

    if cipher != t"none" || kdf != t"none" then abort(PrivateKey.Error(Reason.Encrypted))
    if count != 1 then abort(PrivateKey.Error(Reason.Unsupported(t"container holding $count keys")))

    reader.string() // the public key, repeated in the private section
    val section = Reader(reader.string())(detail => PrivateKey.Error(Reason.Malformed(detail)))
    val check = section.uint32()
    if section.uint32() != check then malformed(t"the check integers differ")

    val keyType = section.text()

    if keyType == Ed25519Name then
      section.string() // the public key again
      val secret = section.string() // the 32-byte seed followed by the 32-byte public key
      if secret.length != 64 then malformed(t"the Ed25519 key is not 64 bytes")

      // RFC 8410's `CurvePrivateKey`, an OCTET STRING of the seed, inside the PrivateKeyInfo's
      // own OCTET STRING, with the public key in the top-level `[1]` field in the explicit form
      // `JavaBaseCrypto.extractTopLevel` reads.
      val seed: Asn1 = Asn1.OctetString(secret.keep(32))
      val public = Asn1.Tagged(1, true, Asn1.BitString(secret.skip(32), 0))
      val identifier = Asn1.Sequence(List(Asn1.ObjectId(KeyAlgorithm.Ed25519)))

      val octets = Asn1.OctetString(seed.in[Der].data)
      val info: Asn1 = Asn1.Sequence(List(Asn1.Integer(0), identifier, octets, public))

      info.in[Der].data

    else if keyType.starts(EcdsaPrefix) then
      val bits = curveBits(section.text()).or:
        abort(PrivateKey.Error(Reason.Unsupported(t"curve in $keyType")))

      val point = section.string()
      val scalar = section.mpint()
      val curve = Asn1.ObjectId(KeyAlgorithm.curve(bits))

      val sec1: Asn1 = Asn1.Sequence:
        List
          ( Asn1.Integer(1),
            Asn1.OctetString(unsigned(scalar, (bits + 7)/8)),
            Asn1.Tagged(0, true, curve),
            Asn1.Tagged(1, true, Asn1.BitString(point, 0)) )

      Pkcs8.wrap(Asn1.Sequence(List(Asn1.ObjectId(KeyAlgorithm.Ec), curve)), sec1.in[Der].data)

    else if keyType == RsaName then
      val n = section.mpint()
      val e = section.mpint()
      val d = section.mpint()
      val qInv = section.mpint()
      val p = section.mpint()
      val q = section.mpint()

      // PKCS#1 carries the two CRT exponents OpenSSH leaves out, and the JDK's RSA private key
      // needs them (`privateToPublic` matches an `RSAPrivateCrtKey`), so they are computed here.
      val pkcs1: Asn1 = Asn1.Sequence:
        List(BigInt(0), n, e, d, p, q, d.mod(p - 1), d.mod(q - 1), qInv).map(Asn1.Integer(_))

      Pkcs8.wrap(rsaIdentifier, pkcs1.in[Der].data)

    else abort(PrivateKey.Error(Reason.Unsupported(t"key type $keyType")))

  // A public key from its single-line form, `<type> <base64> [comment]`, checked against the
  // cipher the caller named.
  def publicKey[cipher <: Cipher: KeyAlgorithm](line: Text)
    ( using Tactic[PublicKey.Error], Diagnostics )
  :   PublicKey[cipher] =

    import PublicKey.Error.Reason

    line.trim.cut(t" ").filter(_.length > 0) match
      case keyType :: encoded :: _ =>
        val blob: Data =
          mitigate:
            case Serialization.Error(_, _) =>
              PublicKey.Error(Reason.Malformed(t"the key is not base64"))

          . protect[Data](encoded.deserialize[Base64])

        PublicKey.adopt[cipher](spki(blob))

      case _ => abort(PublicKey.Error(Reason.Malformed(t"the line is not a public key")))

  // The SubjectPublicKeyInfo form of a wire-encoded public key, as a line's blob or a container's
  // public section holds it.
  def spki(blob: Data)(using Tactic[PublicKey.Error], Diagnostics): Data =
    import PublicKey.Error.Reason

    val reader = Reader(blob)(detail => PublicKey.Error(Reason.Malformed(detail)))
    val keyType = reader.text()

    if keyType == Ed25519Name then
      Spki.wrap(Asn1.Sequence(List(Asn1.ObjectId(KeyAlgorithm.Ed25519))), reader.string())

    else if keyType.starts(EcdsaPrefix) then
      val bits = curveBits(reader.text()).or:
        abort(PublicKey.Error(Reason.Unsupported(t"curve in $keyType")))

      val curve = Asn1.ObjectId(KeyAlgorithm.curve(bits))
      Spki.wrap(Asn1.Sequence(List(Asn1.ObjectId(KeyAlgorithm.Ec), curve)), reader.string())

    else if keyType == RsaName then
      val e = reader.mpint()
      val n = reader.mpint()
      val pkcs1: Asn1 = Asn1.Sequence(List(Asn1.Integer(n), Asn1.Integer(e)))
      Spki.wrap(rsaIdentifier, pkcs1.in[Der].data)

    else abort(PublicKey.Error(Reason.Unsupported(t"key type $keyType")))

  // A public key in its single-line form, with the comment if one is given. The key's algorithm
  // must be one OpenSSH names; the result is `Unset` otherwise.
  def line[cipher <: Cipher](key: PublicKey[cipher], comment: Optional[Text] = Unset)
    ( using algorithm: cipher is KeyAlgorithm )
  :   Optional[Text] =

    algorithm.sshName.let: keyType =>
      safely(Der(key.bytes).as[Asn1]).let:
        case Asn1.Sequence(List(identifier, Asn1.BitString(bits, 0))) =>
          KeyAlgorithm.identifier(identifier).let: (arcs, parameters) =>
            val writer = Writer()
            writer.text(keyType)

            val encoded: Boolean =
              if arcs == KeyAlgorithm.Ed25519 then
                writer.string(bits)
                true
              else if arcs == KeyAlgorithm.Ec then
                val curve = parameters match
                  case Asn1.ObjectId(KeyAlgorithm.P256) => t"nistp256"
                  case Asn1.ObjectId(KeyAlgorithm.P384) => t"nistp384"
                  case Asn1.ObjectId(KeyAlgorithm.P521) => t"nistp521"
                  case _                                => t""

                writer.text(curve)
                writer.string(bits)
                curve.length > 0
              else if arcs == KeyAlgorithm.Rsa then
                safely(Der(bits).as[Asn1]) match
                  case Asn1.Sequence(List(Asn1.Integer(n), Asn1.Integer(e))) =>
                    writer.mpint(e)
                    writer.mpint(n)
                    true

                  case _ => false
              else false

            if encoded then
              val blob = writer.data.serialize[Base64]
              comment.lay(t"$keyType $blob")(comment => t"$keyType $blob $comment")
            else Unset

        case _ => Unset

  private val rsaIdentifier: Asn1 =
    Asn1.Sequence(List(Asn1.ObjectId(KeyAlgorithm.Rsa), Asn1.Null))

  private def curveBits(name: Text): Optional[Int] = name match
    case t"nistp256" => 256
    case t"nistp384" => 384
    case t"nistp521" => 521
    case _           => Unset

  // The big-endian unsigned bytes of a non-negative integer, left-padded to `length`, for an EC
  // scalar, whose SEC1 encoding is the curve order's width.
  private def unsigned(value: BigInt, length: Int): Data =
    val raw = value.toByteArray
    val start = if raw.length > 0 && raw(0) == 0 then 1 else 0
    val significant = raw.length - start
    val output = Array.allocate[Byte](length)

    if significant <= length
    then java.lang.System.arraycopy(raw, start, output.raw, length - significant, significant)

    Array.freeze(output)

  // A cursor over SSH wire-encoded data: a truncated or oversized field is reported as the
  // `malformed` error, through the tactic each read takes, so the reader itself captures no
  // capability. Positions are checked before every read, so the JVM array behind the frozen data
  // is indexed within its bounds.
  class Reader[error <: fulminate.Error](data: Data)(malformed: Text -> error)
  extends caps.Mutable:
    private val bytes: scala.Array[Byte] = Array.unsafeJvm(data)
    private var position: Int = 0

    def remaining: Int = bytes.length - position

    private def need(count: Int)(using Tactic[error]): Unit =
      if count < 0 || remaining < count then abort(malformed(t"the document is truncated"))

    private update def next(): Int =
      val byte = bytes(position) & 0xff
      position += 1
      byte

    update def uint32()(using Tactic[error]): Int =
      need(4)
      (next() << 24) | (next() << 16) | (next() << 8) | next()

    update def string()(using Tactic[error]): Data =
      val length = uint32()
      need(length)
      val output = Array.allocate[Byte](length)
      java.lang.System.arraycopy(bytes, position, output.raw, 0, length)
      position += length
      Array.freeze(output)

    update def text()(using Tactic[error]): Text = utf8(string())

    update def text(length: Int)(using Tactic[error]): Text =
      need(length)
      val output = Array.allocate[Byte](length)
      java.lang.System.arraycopy(bytes, position, output.raw, 0, length)
      position += length
      utf8(Array.freeze(output))

    update def mpint()(using Tactic[error]): BigInt =
      val raw = Array.unsafeJvm(string())
      if raw.length == 0 then BigInt(0) else BigInt(jm.BigInteger(raw))

    private def utf8(data: Data): Text =
      String(Array.unsafeJvm(data), jnc.StandardCharsets.UTF_8).tt

  // The writing counterpart, for a public key's blob.
  class Writer() extends caps.Mutable:
    private val buffer: ji.ByteArrayOutputStream = ji.ByteArrayOutputStream()

    update def uint32(value: Int): Unit =
      buffer.write(value >>> 24)
      buffer.write(value >>> 16)
      buffer.write(value >>> 8)
      buffer.write(value)

    update def string(data: Data): Unit =
      uint32(data.length)
      buffer.write(Array.unsafeJvm(data))

    update def text(text: Text): Unit =
      // [java-boundary] fresh array returned by the JDK, retained by nobody else
      string(Array.unsafeFrozen(text.s.getBytes(jnc.StandardCharsets.UTF_8).nn))

    // [java-boundary] fresh array returned by the JDK, retained by nobody else
    update def mpint(value: BigInt): Unit = string(Array.unsafeFrozen(value.toByteArray))

    // [java-boundary] fresh array returned by the JDK, retained by nobody else
    def data: Data = Array.unsafeFrozen(buffer.toByteArray.nn)
