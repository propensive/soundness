package anticipation

infix type in[CodingType, CodecType] = CodingType { type Codec = CodecType }

trait Encodable:
  inline def encodable: this.type = this
  type Self
  type Codec
  def encode(value: Self): Codec
  def omit(value: Self): Boolean = false

  def contramap[Self2](lambda: Self2 => Self): Self2 is Encodable in Codec = value =>
    encodable.encode(lambda(value))

trait Decodable:
  inline def decodable: this.type = this
  type Self
  type Codec
  def decode(value: Codec, omit: Boolean): Self

  def map[Self2](lambda: Self => Self2): Self2 is Decodable in Codec =
    (value, omit) => lambda(decodable.decode(value, omit))
