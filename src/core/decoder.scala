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
import anticipation.*

import language.experimental.captureChecking

object Decoder:
  given Decoder[Int] = text => Integer.parseInt(text.s)
  given Decoder[Byte] = text => Integer.parseInt(text.s).toByte
  given Decoder[Short] = text => Integer.parseInt(text.s).toShort
  given Decoder[Long] = text => java.lang.Long.parseLong(text.s)
  given Decoder[Text] = identity(_)

@capability
trait Decoder[+ValueType]:
  def decode(text: Text): ValueType

object Encoder:
  given Encoder[Int] = _.toString.tt
  given Encoder[Double] = _.toString.tt
  given Encoder[Char] = _.toString.tt
  given Encoder[Byte] = _.toString.tt
  given Encoder[Short] = _.toString.tt
  given Encoder[Long] = _.toString.tt
  given Encoder[Float] = _.toString.tt
  given Encoder[Text] = identity(_)

@capability
trait Encoder[-ValueType]:
  def encode(text: ValueType): Text

extension (text: Text)
  def decodeAs[ValueType](using decoder: Decoder[ValueType]): ValueType^{decoder} =
    decoder.decode(text)

extension [ValueType](value: ValueType)
  def encode(using encoder: Encoder[ValueType]): Text^{encoder} = encoder.encode(value)
