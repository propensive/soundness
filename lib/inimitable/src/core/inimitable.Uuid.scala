                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.64.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package inimitable

import java.util as ju

import anticipation.*
import contingency.*
import fulminate.*
import prepositional.*
import rudiments.*
import spectacular.*
import vacuous.*

object Uuid extends Extractor[Text, Uuid]:
  // In `Uuid`'s own companion rather than `Showable`'s, so that `spectacular` need not depend on
  // `inimitable`; being companion-to-companion, this is the same implicit scope as before.
  given showable: Uuid is Showable = _.text

  def parse(text: Text): Uuid raises UuidError =
    extract(text).lest(UuidError(text))

  def extract(text: Text): Optional[Uuid] = safely:
    ju.UUID.fromString(text.s).nn.pipe: uuid =>
      Uuid(uuid.getMostSignificantBits, uuid.getLeastSignificantBits)

  def apply(): Uuid = ju.UUID.randomUUID().nn.pipe: uuid =>
    Uuid(uuid.getMostSignificantBits, uuid.getLeastSignificantBits)

  given communicable: Uuid is Communicable = uuid => Message(uuid.text)
  given encodable: Uuid is Encodable in Text = _.text

case class Uuid(msb: Long, lsb: Long):
  def java: ju.UUID = ju.UUID(msb, lsb)
  def text: Text = this.java.toString.tt

  def bytes: Data =
    val high = msb.bytestream
    val low = lsb.bytestream
    val buffer = Array[Byte](high.length + low.length)
    buffer.copyFrom(high, 0, 0, high.length)
    buffer.copyFrom(low, 0, high.length, low.length)
    Array.freeze(buffer)

  @targetName("invert")
  def `unary_~`: Uuid = Uuid(~msb, ~lsb)

  @targetName("xor")
  infix def ^ (right: Uuid): Uuid = Uuid(msb ^ right.msb, lsb ^ right.lsb)
