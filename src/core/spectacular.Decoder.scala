/*
    Spectacular, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

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
  given (using number: Tactic[NumberError]) => Decoder[Int] as int = text =>
    try Integer.parseInt(text.s) catch case _: NumberFormatException =>
      raise(NumberError(text, Int), 0)

  given (using Tactic[FqcnError]) => Decoder[Fqcn] as fqcn = Fqcn(_)
  given (using Tactic[UuidError]) => Decoder[Uuid] as uuid = Uuid.parse(_)

  given (using Tactic[NumberError]) => Decoder[Byte] as byte = text =>
    val int = try Integer.parseInt(text.s) catch case _: NumberFormatException =>
      raise(NumberError(text, Byte), 0)

    if int < Byte.MinValue || int > Byte.MaxValue then raise(NumberError(text, Byte), 0.toByte)
    else int.toByte

  given (using Tactic[NumberError]) => Decoder[Short] as short = text =>
    val int = try Integer.parseInt(text.s) catch case _: NumberFormatException =>
      raise(NumberError(text, Short), 0)

    if int < Short.MinValue || int > Short.MaxValue then raise(NumberError(text, Short), 0.toShort)
    else int.toShort

  given (using Tactic[NumberError]) => Decoder[Long] as long = text =>
    try java.lang.Long.parseLong(text.s) catch case _: NumberFormatException =>
      raise(NumberError(text, Long), 0L)

  given (using Tactic[NumberError]) => Decoder[Double] as double = text =>
    try java.lang.Double.parseDouble(text.s) catch case _: NumberFormatException =>
      raise(NumberError(text, Double), 0.0)

  given (using Tactic[NumberError]) => Decoder[Float] = text =>
    try java.lang.Float.parseFloat(text.s) catch case _: NumberFormatException =>
      raise(NumberError(text, Float), 0.0F)

  given Decoder[Char] as char = _.s(0)
  given Decoder[Text] as text = identity(_)
  given Decoder[String] as string = _.s
  given (using number: Tactic[NumberError]) => Decoder[Pid] as pid = long.map(Pid(_))

  given [EnumType <: reflect.Enum: {Enumerable, Identifiable}](using Tactic[VariantError])
      => Decoder[EnumType] = value =>
    EnumType.value(EnumType.decode(value)).or:
      val names = EnumType.values.to(List).map(EnumType.name(_)).map(EnumType.encode(_))
      raise(VariantError(value, EnumType.name, names), EnumType.value(Prim).vouch(using Unsafe))

trait Decoder[+ValueType] extends Unapply[Text, ValueType]:
  def unapply(text: Text): Option[ValueType] = try Some(decode(text)) catch case error: Exception => None
  def decode(text: Text): ValueType
  def map[ValueType2](lambda: ValueType => ValueType2): Decoder[ValueType2] = text => lambda(decode(text))
