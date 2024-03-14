package revolution

import anticipation.*
import rudiments.*
import turbulence.*
import vacuous.*
import serpentine.*
import gossamer.*
import spectacular.*

import java.util.jar as juj

trait ManifestDecoder[KeyType <: Label]:
  type Value
  def decode(text: Text): Value

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
  object ManifestVersion extends ManifestAttribute["Manifest-Version"]()
  object MainClass extends ManifestAttribute["Main-Class"]()
  object CreatedBy extends ManifestAttribute["Created-By"]()
  object SignatureVersion extends ManifestAttribute["Signature-Version"]()
  object ClassPath extends ManifestAttribute["Class-Path"]()

object Manifest:
  protected def parse[SourceType](source: SourceType)(using readable: Readable[SourceType, Bytes]): Manifest =
    val java = juj.Manifest(LazyListInputStream(source.readAs[LazyList[Bytes]]))
    
    Manifest:
      java.getMainAttributes.nn.asScala.to(List).map:
        case (key: String, value: String) => (key.tt, value.tt)
      .to(Map)
  
case class Manifest(entries: Map[Text, Text]):
  def apply[KeyType <: Label](attribute: ManifestAttribute[KeyType])(using decoder: ManifestDecoder[KeyType])
          : Optional[decoder.Value] =
    
    if entries.contains(attribute.key) then decoder.decode(entries(attribute.key)) else Unset