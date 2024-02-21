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
import gossamer.*
import anticipation.*
import contingency.*
import spectacular.*
import kaleidoscope.*

case class Pem(kind: Text, data: Bytes):
  def serialize: Text = Seq(
    Seq(t"-----BEGIN $kind-----"),
    data.grouped(48).to(Seq).map(_.encodeAs[Base64]),
    Seq(t"-----END $kind-----")
  ).flatten.join(t"\n")

object Pem:
  def parse(string: Text): Pem raises PemError =
    given (PemError fixes DecodeError) = error => PemError(error.detail)
    val lines = string.trim.nn.cut(t"\n")
    
    val label = lines.head match
      case r"-----* *BEGIN $label([A-Z]+) *-----*" => label.show
      case _                         => abort(PemError(t"the BEGIN line could not be found"))
    
    lines.tail.indexWhere:
      case r"-----* *END $label([A-Z]+) *-----*" => true
      case _                                     => false
    match
      case -1  =>
        abort(PemError(t"the message's END line could not be found"))
      case idx =>
        val joined: Text = lines.tail.take(idx).join
        try Pem(label, joined.decode[Base64])
        catch Exception => abort(PemError(t"could not parse Base64 PEM message"))
