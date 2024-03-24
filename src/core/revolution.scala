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

import java.util.jar as juj

object ManifestDecoder:
  given mainClass(using Raises[FqcnError]): ManifestDecoder["Main-Class"] { type Value = Fqcn } =
    new ManifestDecoder["Main-Class"]:
      type Value = Fqcn
      def decode(text: Text): Fqcn = Fqcn(text)
  
  given createdBy: ManifestDecoder["Created-By"] { type Value = Text } =
    new ManifestDecoder["Created-By"]:
      type Value = Text
      def decode(creator: Text): Text = creator
  
trait ManifestDecoder[KeyType <: Label]:
  type Value
  def decode(text: Text): Value

object ManifestEncoder:
  given mainClass: ManifestEncoder["Main-Class"] with
    type Value = Fqcn
    def encode(fqcn: Fqcn): Text = fqcn.text
  
  given manifestVersion: ManifestEncoder["Manifest-Version"] with
    type Value = VersionNumber
    def encode(version: VersionNumber): Text = version.text
  
  given createdBy: ManifestEncoder["Created-By"] with
    type Value = Text
    def encode(creator: Text): Text = creator

trait ManifestEncoder[KeyType <: Label]:
  type Value
  def encode(value: Value): Text

abstract class ManifestAttribute[KeyType <: Label: ValueOf]():
  val key: Text = valueOf[KeyType].tt
  def parse(value: Text)(using decoder: ManifestDecoder[KeyType]): decoder.Value = decoder.decode(value)

  def apply(using encoder: ManifestEncoder[KeyType])(value: encoder.Value): ManifestEntry =
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
  protected def parse[SourceType](source: SourceType)(using readable: Readable[SourceType, Bytes]): Manifest =
    val java = juj.Manifest(LazyListInputStream(source.readAs[LazyList[Bytes]]))
    
    Manifest:
      java.getMainAttributes.nn.asScala.to(List).map: (key, value) =>
        (key.toString.tt, value.toString.tt)
      .to(Map)

  given readable: Readable[Manifest, Bytes] = manifest => LazyList(manifest.serialize)

  def apply(entries: ManifestEntry*): Manifest = Manifest:
    entries.map: entry =>
      (entry.key, entry.value)
    .to(Map)

case class Manifest(entries: Map[Text, Text]):
  def apply[KeyType <: Label](attribute: ManifestAttribute[KeyType])(using decoder: ManifestDecoder[KeyType])
          : Optional[decoder.Value] =
    
    if entries.contains(attribute.key) then decoder.decode(entries(attribute.key)) else Unset
  
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
  