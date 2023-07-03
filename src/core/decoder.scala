/*
    Spectacular, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package spectacular

import rudiments.*

import language.experimental.captureChecking

@capability
trait Decoder[+ValueType]:
  def decode(text: Text): ValueType

@capability
trait Encoder[-ValueType]:
  def encode(text: ValueType): Text

object Codec:
  given codec[ValueType]
      (using encoder: Encoder[ValueType], decoder: Decoder[ValueType])
      : Codec[ValueType]^{decoder, encoder} =
    Codec(encoder, decoder)

case class Codec[ValueType](encoder: Encoder[ValueType], decoder: Decoder[ValueType]):
  def decode(text: Text): ValueType = decoder.decode(text)
  def encode(value: ValueType): Text = encoder.encode(value)

extension (text: Text)
  def decodeAs[ValueType](using decoder: Decoder[ValueType]): ValueType^{decoder} =
    decoder.decode(text)