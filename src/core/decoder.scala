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
import fulminate.*
import perforate.*
import anticipation.*
import inimitable.*

import language.experimental.captureChecking

case class NumberError(text: Text, specializable: Specializable)
extends Error(msg"$text is not a valid ${specializable.show}")

object Decoder:
  given (using number: Raises[NumberError]): Decoder[Int] = text =>
    try Integer.parseInt(text.s) catch case _: NumberFormatException =>
      raise(NumberError(text, Int))(0)

  given (using uuid: Raises[UuidError]): Decoder[Uuid] = Uuid.parse(_)

  given (using number: Raises[NumberError]): Decoder[Byte] = text =>
    val int = try Integer.parseInt(text.s) catch case _: NumberFormatException =>
      raise(NumberError(text, Byte))(0)
    
    if int < Byte.MinValue || int > Byte.MaxValue then raise(NumberError(text, Byte))(0.toByte)
    else int.toByte
  
  given (using number: Raises[NumberError]): Decoder[Short] = text =>
    val int = try Integer.parseInt(text.s) catch case _: NumberFormatException =>
      raise(NumberError(text, Short))(0)
    
    if int < Short.MinValue || int > Short.MaxValue then raise(NumberError(text, Short))(0.toShort)
    else int.toShort
  
  given (using number: Raises[NumberError]): Decoder[Long] = text =>
    try java.lang.Long.parseLong(text.s) catch case _: NumberFormatException =>
      raise(NumberError(text, Long))(0L)

  given Decoder[Char] = _.s(0)
  given Decoder[Text] = identity(_)
  given Decoder[String] = _.s

@capability
trait Decoder[+ValueType]:
  def decode(text: Text): ValueType
  def map[ValueType2](fn: ValueType => ValueType2): Decoder[ValueType2] = text => fn(decode(text))

object Encoder:
  given Encoder[Int] = _.toString.tt
  given Encoder[Double] = _.toString.tt
  given Encoder[Byte] = _.toString.tt
  given Encoder[Short] = _.toString.tt
  given Encoder[Long] = _.toString.tt
  given Encoder[Float] = _.toString.tt
  given Encoder[Text] = identity(_)
  given Encoder[Char] = _.toString.tt
  given Encoder[Uuid] = _.text

@capability
trait Encoder[-ValueType]:
  def encode(value: ValueType): Text
  def contraMap[ValueType2](fn: ValueType2 => ValueType): Encoder[ValueType2] = value => encode(fn(value))

extension (text: Text)
  def decodeAs[ValueType](using decoder: Decoder[ValueType]): ValueType^{decoder} =
    decoder.decode(text)

extension [ValueType](value: ValueType)
  def encode(using encoder: Encoder[ValueType]): Text^{encoder} = encoder.encode(value)
