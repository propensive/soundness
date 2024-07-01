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

import anticipation.*
import rudiments.*
import turbulence.*
import vacuous.*
import gossamer.*
import contingency.*
import hieroglyph.*, charEncoders.utf8
import digression.*
import spectacular.*
import symbolism.*

import java.util.jar as juj

infix type of [Type, ValueType] = Type { type Value = ValueType }

object DecodableManifest:
  given (using Errant[FqcnError]) => ("Main-Class" is DecodableManifest of Fqcn) as mainClass = Fqcn(_)
  given ("Created-By" is DecodableManifest of Text) as createdBy = identity(_)

trait DecodableManifest:
  type Self <: Label
  type Value
  def decode(text: Text): Value

object EncodableManifest:
  given ("Main-Class" is EncodableManifest of Fqcn) as mainClass = _.text
  given ("Manifest-Version" is EncodableManifest of VersionNumber) as manifestVersion = _.text
  given ("Created-By" is EncodableManifest of Text) as createdBy = identity(_)

trait EncodableManifest:
  type Self <: Label
  type Value
  def encode(value: Value): Text

abstract class ManifestAttribute[KeyType <: Label: ValueOf]():
  val key: Text = valueOf[KeyType].tt
  def parse(value: Text)(using decoder: KeyType is DecodableManifest): decoder.Value = decoder.decode(value)

  def apply(using encoder: KeyType is EncodableManifest)(value: encoder.Value): ManifestEntry =
    ManifestEntry(valueOf[KeyType].tt, encoder.encode(value))

object VersionNumber:
  def apply(value: Text)(using Decoder[Int]): VersionNumber =
    VersionNumber(value.cut(t".").to(List).map(_.decodeAs[Int])*)

case class VersionNumber(digits: Int*):
  def text: Text = digits.map(_.toString.tt).join(".".tt)

case class ManifestEntry(key: Text, value: Text)

package manifestAttributes:
  object ManifestVersion       extends ManifestAttribute["Manifest-Version"]()
  object MainClass             extends ManifestAttribute["Main-Class"]()
  object CreatedBy             extends ManifestAttribute["Created-By"]()
  object ClassPath             extends ManifestAttribute["Class-Path"]()
  object ContentType           extends ManifestAttribute["Content-Type"]()
  object ExtensionList         extends ManifestAttribute["Extension-List"]()
  object ExtensionName         extends ManifestAttribute["Extension-Name"]()
  object ImplementationTitle   extends ManifestAttribute["Implementation-Title"]()
  object ImplementationVendor  extends ManifestAttribute["Implementation-Vendor"]()
  object ImplementationVersion extends ManifestAttribute["Implementation-Version"]()
  object Sealed                extends ManifestAttribute["Sealed"]()
  object SignatureVersion      extends ManifestAttribute["Signature-Version"]()
  object SpecificationTitle    extends ManifestAttribute["Specification-Title"]()
  object SpecifacationVendor   extends ManifestAttribute["Specification-Vendor"]()
  object SpecifacationVersion  extends ManifestAttribute["Specification-Version"]()

object Manifest:
  protected def parse[SourceType: Readable by Bytes](source: SourceType): Manifest =
    val java = juj.Manifest(LazyListInputStream(source.read[LazyList[Bytes]]))

    Manifest:
      java.getMainAttributes.nn.asScala.to(List).map: (key, value) =>
        (key.toString.tt, value.toString.tt)
      .to(Map)

  given Manifest is Readable by Bytes as readable = manifest => LazyList(manifest.serialize)

  def apply(entries: ManifestEntry*): Manifest = Manifest:
    entries.map: entry =>
      (entry.key, entry.value)
    .to(Map)

case class Manifest(entries: Map[Text, Text]):
  def apply[KeyType <: Label: DecodableManifest](attribute: ManifestAttribute[KeyType])
          : Optional[KeyType.Value] =

    if entries.contains(attribute.key) then KeyType.decode(entries(attribute.key)) else Unset

  def serialize: Bytes =
    Text.construct:
      entries.each: (key, value) =>
        buffer.append(key)
        buffer.append(t": ")
        val used = key.length + 2

        def putValue(index: Int, space: Int): Unit =
          buffer.append(value.slice(index, index + space))
          buffer.append(t"\r\n")
          if index + space > 70 then
            buffer.append(t" ")
            putValue(index + space, 69)

        putValue(0, 68 - key.length)
    .bytes
