/*
    Coaxial, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package coaxial

import anticipation.*
import gossamer.*
import hieroglyph.*
import prepositional.*
import proscenium.*
import spectacular.*

trait Transmissible:
  type Self
  def serialize(message: Self): Stream[Bytes]

object Transmissible:
  given bytes: [BytesType <: Bytes] => BytesType is Transmissible = Stream(_)
  given stream: [StreamType <: Stream[Bytes]] => StreamType is Transmissible = identity(_)

  given text: [TextType <: Text] => CharEncoder => TextType is Transmissible =
    text => Stream(text.bytes)

  given encoder: [MessageType: Encodable in Text] => CharEncoder => MessageType is Transmissible =
    value => Stream(value.encode.bytes)
