/*
    Revolution, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package revolution

import java.util.jar as juj

import anticipation.*
import denominative.*
import gossamer.*
import hieroglyph.*, charEncoders.utf8
import rudiments.*
import prepositional.*
import turbulence.*
import vacuous.*

object Manifest:
  protected def parse[SourceType: Readable by Bytes](source: SourceType): Manifest =
    val java = juj.Manifest(LazyListInputStream(source.read[LazyList[Bytes]]))

    Manifest:
      java.getMainAttributes.nn.asScala.to(List).map: (key, value) =>
        (key.toString.tt, value.toString.tt)
      .to(Map)

  given Manifest is Readable by Bytes as readable = manifest => LazyList(manifest.serialize)
  given [SourceType: Readable by Bytes] => Manifest is Aggregable by Bytes as aggregable = parse(_)

  def apply(entries: ManifestEntry*): Manifest = Manifest:
    entries.map: entry =>
      (entry.key, entry.value)
    .to(Map)

case class Manifest(entries: Map[Text, Text]):
  def apply[KeyType <: Label: DecodableManifest](attribute: ManifestAttribute[KeyType])
          : Optional[KeyType.Subject] =

    if entries.contains(attribute.key) then KeyType.decode(entries(attribute.key)) else Unset

  def serialize: Bytes =
    Text.construct:
      entries.each: (key, value) =>
        buffer.append(key)
        buffer.append(t": ")
        val used = key.length + 2

        def putValue(index: Int, space: Int): Unit =
          buffer.append(value.slice(Ordinal.zerary(index) ~ Ordinal.natural(index + space)))
          buffer.append(t"\r\n")

          if index + space > 70 then
            buffer.append(t" ")
            putValue(index + space, 69)

        putValue(0, 68 - key.length)
    .bytes
