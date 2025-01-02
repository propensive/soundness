/*
    Enigmatic, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package enigmatic

import anticipation.*
import gossamer.*
import rudiments.*
import spectacular.*

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
