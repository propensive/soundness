package spectacular

import rudiments.*

import language.experimental.captureChecking

@capability
trait Decoder[+ValueType]:
  def decode(text: Text): ValueType

trait Encoder[-ValueType]:
  def encode(text: ValueType): Text

object Codec:
  given codec[ValueType]
      (using encoder: Encoder[ValueType], decoder: Decoder[ValueType])
      : Codec[ValueType]^{decoder} =
    Codec(encoder, decoder)

case class Codec[ValueType](encoder: Encoder[ValueType], decoder: Decoder[ValueType]):
  def decode(text: Text): ValueType = decoder.decode(text)
  def encode(value: ValueType): Text = encoder.encode(value)