/*
    Inimitable, version 0.24.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package inimitable

import language.experimental.captureChecking

import java.util as ju

import anticipation.*
import contingency.*
import fulminate.*
import prepositional.*
import rudiments.*
import vacuous.*

object Uuid extends Extractor[Text, Uuid]:
  def parse(text: Text): Uuid raises UuidError = extract(text).or(raise(UuidError(text), Uuid(0L, 0L)))

  def extract(text: Text): Optional[Uuid] = safely:
    ju.UUID.fromString(text.s).nn.pipe: uuid =>
      Uuid(uuid.getMostSignificantBits, uuid.getLeastSignificantBits)

  def apply(): Uuid = ju.UUID.randomUUID().nn.pipe: uuid =>
    Uuid(uuid.getMostSignificantBits, uuid.getLeastSignificantBits)

  given Uuid is Communicable = uuid => Message(uuid.text)
  given Uuid is Encodable in Text = _.text

case class Uuid(msb: Long, lsb: Long):
  def java: ju.UUID = ju.UUID(msb, lsb)
  def text: Text = this.java.toString.tt
  def bytes: Bytes = unsafely((msb.bytes.mutable ++ lsb.bytes.mutable).immutable)

  @targetName("invert")
  def `unary_~`: Uuid = Uuid(~msb, ~lsb)

  @targetName("xor")
  infix def ^ (right: Uuid): Uuid = Uuid(msb ^ right.msb, lsb ^ right.lsb)
