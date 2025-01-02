/*
    Hieroglyph, version 0.24.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package hieroglyph

import anticipation.*
import fulminate.*
import rudiments.*

import scala.jdk.CollectionConverters.SetHasAsScala

import java.nio as jn, jn.charset as jnc

import language.experimental.captureChecking

object Encoding:
  given Encoding is Textualizer = _.name
  given Encoding is Communicable = encoding => Message(encoding.name)

  private val allCharsets: Set[jnc.Charset] =
    jnc.Charset.availableCharsets.nn.asScala.to(Map).values.to(Set)

  private[hieroglyph] val codecs: Map[Text, Encoding { type CanEncode = true }] =
    allCharsets.filter(_.canEncode).flatMap: charset =>
      (charset.aliases.nn.asScala.to(Set) + charset.displayName.nn).map: name =>
        name.toLowerCase.nn.tt -> Encoding(name.tt, true)

    . to(Map)

  private[hieroglyph] val decodeOnly: Map[Text, Encoding { type CanEncode = false }] =
    allCharsets.filter(!_.canEncode).flatMap: charset =>
      (charset.aliases.nn.asScala.to(Set) + charset.displayName.nn).map: name =>
        name.toLowerCase.nn.tt -> Encoding(name.tt, false)

    . to(Map)

  def unapply(name: Text): Option[Encoding] =
    codecs.get(name.s.toLowerCase.nn.tt).orElse(decodeOnly.get(name.s.toLowerCase.nn.tt))

  def apply(name: Text, canEncode: Boolean): Encoding { type CanEncode = canEncode.type } =
    new Encoding(name) { type CanEncode = canEncode.type }

class Encoding(name0: Text):
  def name: Text = charset.displayName.nn.tt
  type CanEncode <: Boolean
  def decoder(using TextSanitizer): CharDecoder = CharDecoder(this)
  lazy val charset: jnc.Charset = jnc.Charset.forName(name0.s).nn

  override def toString: String = s"enc\"${charset.displayName}\""
