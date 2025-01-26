/*
    Hieroglyph, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package hieroglyph

import anticipation.*
import proscenium.*
import rudiments.*
import vacuous.*

import java.nio as jn, jn.charset as jnc

import language.experimental.captureChecking

object CharEncoder:
  given default: Quickstart => CharEncoder = charEncoders.utf8
  def system: CharEncoder = unapply(jnc.Charset.defaultCharset.nn.displayName.nn.tt).get

  def unapply(name: Text): Option[CharEncoder] =
    Encoding.codecs.get(name.s.toLowerCase.nn.tt).map(CharEncoder(_))

class CharEncoder(val encoding: Encoding { type CanEncode = true })
extends Encodable:
  type Self = Text
  type Format = Bytes
  def encode(text: Text): Bytes = text.s.getBytes(encoding.name.s).nn.immutable(using Unsafe)
  def encode(stream: Stream[Text]): Stream[Bytes] = stream.map(encode)
