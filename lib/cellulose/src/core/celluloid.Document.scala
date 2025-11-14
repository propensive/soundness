package celluloid

import soundness.*

trait Loadable extends Typeclass:
  type Self <: Loadable
  type Metadata

case class Document[content <: Loadable](root: content, metadata: root.Metadata)

object Celluloid:
  opaque type CodlRef = Long

  object CodlRef:
    final val Invalid: CodlRef = -1L

    inline def isValid(byte: Byte): Boolean =
      val c: Int = byte&0xff
      val d: Int = c|0x20
      val digit: Int = ((c - 0x30) | (0x39 - c)) >>> 31
      val alpha: Int = ((d - 0x61) | (0x7a - d)) >>> 31
      val symbol: Int = ((c - 0x2d) | (0x2e - c)) >>> 31
      ((digit & alpha & symbol) ^ 1) != 0

    inline def translate(byte: Byte) =
      val letter = (byte & 64) >>> 6
      val upper = ((byte ^ 32)&32) >>> 5
      val lower = letter ^ upper
      val digit = ((byte & 16) >>> 4) & (letter ^ 1)
      val symbol = letter ^ digit ^ 1

      byte - digit*46 - letter*53 - lower*4 - symbol*45

    def apply(value: Text): CodlRef =
      if value.length <= 10 then
        var long: Long = 0L
        var i: Int = 0
        while i < value.length do
          val next = value.s.charAt(i).toByte
          long <<= 6
          if isValid(next) then long |= translate(next) else
            i = 10
            long = Invalid

          i += 1
        long
      else Invalid


export Celluloid.CodlRef

object Codl:
  case class Metadata(indentation: Int)

  given Codl is Loadable

case class Codl(children: IArray[Codl]) extends Loadable:
  type Metadata = Codl.Metadata

extension [streamable](entity: streamable)
  inline def load[content <: Loadable]: Document[content] =
    import charEncoders.utf8
    val conduit = Conduit(entity.stream[Bytes])
    Document[Codl](Codl(IArray.from(Nil)), Codl.Metadata(2))
