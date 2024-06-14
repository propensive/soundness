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

case class Pem(kind: Text, data: Bytes):
  def serialize: Text =
    import alphabets.base64.standard
    Seq
     (Seq(t"-----BEGIN $kind-----"),
      data.grouped(48).to(Seq).map(_.encodeAs[Base64]),
      Seq(t"-----END $kind-----")).flatten.join(t"\n")

object Pem:
  def parse(text: Text): Pem raises PemError =
    val lines = text.trim.cut(t"\n")

    val label = lines.head match
      case r"-----* *BEGIN $label([A-Z]+) *-----*" => label.show
      case _                         => abort(PemError(PemError.Reason.BeginMissing))

    lines.tail.indexWhere:
      case r"-----* *END $label([A-Z]+) *-----*" => true
      case _                                     => false
    match
      case -1  =>
        abort(PemError(PemError.Reason.EndMissing))
      case idx =>
        val joined: Text = lines.tail.take(idx).join
        tend(Pem(label, joined.decode[Base64])).remedy:
          case CryptoError(_) => abort(PemError(PemError.Reason.BadBase64))

object PemError:
  given Reason is Communicable =
    case Reason.BadBase64    => msg"could not parse the BASE-64 PEM message"
    case Reason.BeginMissing => msg"the BEGIN line could not be found"
    case Reason.EndMissing   => msg"the END line could not be found"

  enum Reason:
    case BeginMissing, EndMissing, BadBase64

case class PemError(reason: PemError.Reason)
extends Error(msg"could not parse PEM content because $reason")
