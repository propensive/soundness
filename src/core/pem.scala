/*
    Gastronomy, version 0.5.0. Copyright 2018-21 Jon Pretty, Propensive OÜ.

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

case class Pem(kind: String, data: Bytes):
  def serialize: String = Seq(
    Seq(s"-----BEGIN $kind-----"),
    data.grouped(48).to(Seq).map(_.encode[Base64]),
    Seq(s"-----END $kind-----")
  ).flatten.join("\n")

object Pem:
  def parse(string: String): Pem throws PemParseError =
    val lines = string.trim.nn.cut("\n")
    
    val label = lines.head match
      case s"-----BEGIN $label-----" => label
      case _                         => throw PemParseError("the BEGIN line could not be found")
    
    lines.tail.indexWhere {
      case s"-----END $label-----" => true
      case _                       => false
    } match
      case -1  =>
        throw PemParseError("the message's END line could not be found")
      case idx =>
        try Pem(label, lines.tail.take(idx).join.decode[Base64])
        catch Exception => throw PemParseError("could not parse Base64 PEM message")