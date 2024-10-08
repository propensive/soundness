package spectacular

import anticipation.*

object Identifiable:
  def apply[IdentType](encoder: Text => Text, decoder: Text => Text): IdentType is Identifiable =
    new Identifiable:
      type Self = IdentType
      def encode(text: Text): Text = encoder(text)
      def decode(text: Text): Text = decoder(text)

trait Identifiable:
  type Self
  def encode(text: Text): Text
  def decode(text: Text): Text
