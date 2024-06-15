/*
    Gastronomy, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package gastronomy

import rudiments.*
import gossamer.{take as _, *}
import anticipation.*
import contingency.*
import spectacular.*
import fulminate.*
import kaleidoscope.*
import vacuous.*

object PemLabel:
  lazy val index: Map[Text, PemLabel] =
    (0 to 17).map(fromOrdinal(_)).indexBy(_.toString.tt.uncamel.map(_.upper).join(t" "))

  given PemLabel is Showable =
    case Proprietary(label) => label
    case other              => other.toString.tt.uncamel.map(_.upper).join(t" ")

  def unapply(text: Text): Some[PemLabel] = Some(index.get(text).getOrElse(Proprietary(text)))

enum PemLabel:
  case Certificate, CertificateRequest, NewCertificateRequest, PrivateKey, RsaPrivateKey, DsaPrivateKey,
      EcPrivateKey, EncryptedPrivateKey, PublicKey, Pkcs7, Cms, DhParameters, X509Crl, AttributeCertificate,
      EncryptedMessage, SignedMessage, RsaPublicKey, DsaPublicKey

  case Proprietary(label: Text)

case class Pem(label: PemLabel, data: Bytes):
  def serialize: Text =
    import alphabets.base64.standard
    Seq
     (Seq(t"-----BEGIN $label-----"),
      data.grouped(48).to(Seq).map(_.serialize[Base64]),
      Seq(t"-----END $label-----")).flatten.join(t"\n")

object Pem:
  def parse(text: Text): Pem raises PemError =
    val lines = text.trim.cut(t"\n")

    val label = lines.prim match
      case Unset =>
        abort(PemError(PemError.Reason.EmptyFile))

      case r"-----* *BEGIN ${PemLabel(label)}([ A-Z]+) *-----*" =>
        label

      case _ =>
        abort(PemError(PemError.Reason.BeginMissing))

    lines.tail.indexWhere:
      case r"-----* *END $label([ A-Z]+) *-----*" => true
      case _                                     => false
    .match
      case -1  => abort(PemError(PemError.Reason.EndMissing))
      case index =>
        val joined: Text = lines.tail.take(index).join
        tend(Pem(label, joined.decode[Base64])).remedy:
          case CryptoError(_) => abort(PemError(PemError.Reason.BadBase64))

object PemError:
  given Reason is Communicable =
    case Reason.BadBase64    => msg"could not parse the BASE-64 PEM message"
    case Reason.BeginMissing => msg"the BEGIN line could not be found"
    case Reason.EndMissing   => msg"the END line could not be found"
    case Reason.EmptyFile    => msg"the file was empty"

  enum Reason:
    case BeginMissing, EndMissing, BadBase64, EmptyFile

case class PemError(reason: PemError.Reason)
extends Error(msg"could not parse PEM content because $reason")
