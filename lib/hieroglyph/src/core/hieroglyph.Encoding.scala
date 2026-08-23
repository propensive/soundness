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
package hieroglyph

import java.nio as jn, jn.charset as jnc

import scala.jdk.CollectionConverters.SetHasAsScala

import anticipation.*
import fulminate.*

object Encoding:
  given textualizable: Encoding is Textualizable = _.name
  given communicable: Encoding is Communicable = encoding => Message(encoding.name)

  private val allCharsets: Set[jnc.Charset] =
    Set.of(jnc.Charset.availableCharsets.nn.asScala.toMap.values.toSet)

  private[hieroglyph] val codecs: Map[Text, Encoding { type CanEncode = true }] =
    Map.from:
      allCharsets.stdlib.filter(_.canEncode).flatMap: charset =>
        (charset.aliases.nn.asScala.toSet + charset.displayName.nn).map: name =>
          name.toLowerCase.nn.tt -> Encoding(name.tt, true)

  private[hieroglyph] val decodeOnly: Map[Text, Encoding { type CanEncode = false }] =
    Map.from:
      allCharsets.stdlib.filter(!_.canEncode).flatMap: charset =>
        (charset.aliases.nn.asScala.toSet + charset.displayName.nn).map: name =>
          name.toLowerCase.nn.tt -> Encoding(name.tt, false)

  def unapply(name: Text): Option[Encoding] =
    codecs.stdlib.get(name.s.toLowerCase.nn.tt).orElse(decodeOnly.stdlib.get(name.s.toLowerCase.nn.tt))

  def apply(name: Text, canEncode: Boolean): Encoding { type CanEncode = canEncode.type } =
    new Encoding(name) { type CanEncode = canEncode.type }

  extension (encoding: Encoding { type CanEncode = true })
    def encoder: CharEncoder = CharEncoder(encoding)

class Encoding(name0: Text):
  def name: Text = charset.displayName.nn.tt

  type CanEncode <: Boolean

  def decoder(using TextSanitizer): CharDecoder = CharDecoder(this)
  lazy val charset: jnc.Charset = jnc.Charset.forName(name0.s).nn

  override def toString: String = s"enc\"${charset.displayName}\""
