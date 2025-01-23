/*
    Spectacular, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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

import anticipation.*
import contingency.*
import denominative.*
import digression.*
import inimitable.*
import rudiments.*
import vacuous.*
import wisteria.*

import language.experimental.pureFunctions

object Decoder:
  given int: (number: Tactic[NumberError]) => Decoder[Int] = text =>
    try Integer.parseInt(text.s) catch case _: NumberFormatException =>
      raise(NumberError(text, Int), 0)

  given fqcn: Tactic[FqcnError] => Decoder[Fqcn] = Fqcn(_)
  given uuid: Tactic[UuidError] => Decoder[Uuid] = Uuid.parse(_)

  given byte: Decoder[Byte] raises NumberError = text =>
    val int = try Integer.parseInt(text.s) catch case _: NumberFormatException =>
      raise(NumberError(text, Byte), 0)

    if int < Byte.MinValue || int > Byte.MaxValue then raise(NumberError(text, Byte), 0.toByte)
    else int.toByte

  given short: Tactic[NumberError] => Decoder[Short] = text =>
    val int = try Integer.parseInt(text.s) catch case _: NumberFormatException =>
      raise(NumberError(text, Short), 0)

    if int < Short.MinValue || int > Short.MaxValue then raise(NumberError(text, Short), 0.toShort)
    else int.toShort

  given long: Tactic[NumberError] => Decoder[Long] = text =>
    try java.lang.Long.parseLong(text.s) catch case _: NumberFormatException =>
      raise(NumberError(text, Long), 0L)

  given double: Tactic[NumberError] => Decoder[Double] = text =>
    try java.lang.Double.parseDouble(text.s) catch case _: NumberFormatException =>
      raise(NumberError(text, Double), 0.0)

  given float: Tactic[NumberError] => Decoder[Float] = text =>
    try java.lang.Float.parseFloat(text.s) catch case _: NumberFormatException =>
      raise(NumberError(text, Float), 0.0F)

  given char: Decoder[Char] = _.s(0)
  given text: Decoder[Text] = identity(_)
  given string: Decoder[String] = _.s
  given pid: (number: Tactic[NumberError]) => Decoder[Pid] = long.map(Pid(_))

  given [EnumType <: reflect.Enum: {Enumerable, Identifiable}] => Tactic[VariantError]
      => Decoder[EnumType] = value =>
    EnumType.value(EnumType.decode(value)).or:
      val names = EnumType.values.to(List).map(EnumType.name(_)).map(EnumType.encode(_))
      raise(VariantError(value, EnumType.name, names), EnumType.value(Prim).vouch(using Unsafe))

trait Decoder[ValueType] extends Extractable:
  type Self = Text
  type Result = ValueType

  def extract(text: Text): Optional[ValueType] =
    try decode(text) catch case error: Exception => Unset

  def decode(text: Text): ValueType

  def map[ValueType2](lambda: ValueType => ValueType2): Decoder[ValueType2] =
    text => lambda(decode(text))
