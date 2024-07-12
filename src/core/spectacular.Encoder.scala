/*
    Spectacular, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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
import inimitable.*
import digression.*

import language.experimental.pureFunctions

object Encoder:
  given Encoder[Int] as int = _.toString.tt
  given Encoder[Double] as double = _.toString.tt
  given Encoder[Byte] as byte = _.toString.tt
  given Encoder[Short] as short = _.toString.tt
  given Encoder[Long] as long = _.toString.tt
  given Encoder[Float] as float = _.toString.tt
  given Encoder[Text] as text = identity(_)
  given Encoder[Char] as char = _.toString.tt
  given Encoder[Uuid] as uuid = _.text
  given Encoder[Pid] as pid = long.contramap(_.value)
  given Encoder[Fqcn] as fqcn = _.text

@capability
trait Encoder[-ValueType] extends Irrefutable[ValueType, Text]:
  def unapply(value: ValueType): Text = encode(value)
  def encode(value: ValueType): Text
  def contramap[ValueType2](lambda: ValueType2 => ValueType): Encoder[ValueType2] = value => encode(lambda(value))
