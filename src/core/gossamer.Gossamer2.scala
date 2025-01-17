package gossamer

import anticipation.*
import denominative.*
import fulminate.*
import rudiments.*
import spectacular.*
import vacuous.*

import scala.quoted.*

object Gossamer2:
  given Realm = realm"gossamer"

  object opaques:
    opaque type Ascii = Bytes

    object Ascii:
      def apply(bytes: Bytes): Ascii = bytes

      given Ascii is Showable = ascii => String(ascii.mutable(using Unsafe), "ASCII").nn.tt

      extension (ascii: Ascii) def bytes: Bytes = ascii

      given Ascii is Textual:
        type Show[ValueType] = ValueType is Showable

        val empty: Ascii = IArray.from[Byte](Nil)
        val classTag: ClassTag[Ascii] = summon[ClassTag[Ascii]]

        def apply(text: Text): Ascii = text.sysBytes
        def length(ascii: Ascii): Int = ascii.size
        def text(ascii: Ascii): Text = String(ascii.mutable(using Unsafe), "ASCII").nn.tt
        def unsafeChar(ascii: Ascii, index: Ordinal): Char = ascii(index.n0).toChar
        def buffer(size: Optional[Int]): Buffer[Ascii] = AsciiBuffer(size)
        def size(ascii: Ascii): Int = ascii.length

        def map(ascii: Ascii, lambda: Char => Char): Ascii = ascii.map: byte =>
          lambda(byte.toChar).toByte

        def concat(left: Ascii, right: Ascii): Ascii =
          IArray.create[Byte](left.length + right.length): array =>
            array.place(left, Prim)
            array.place(right, left.length.z)

        def indexOf(ascii: Ascii, sub: Text, start: Ordinal): Optional[Ordinal] =
          ascii.indexOfSlice(apply(sub)).puncture(-1).let(_.z)

        def show[ValueType](value: ValueType)(using show: Show[ValueType]): Ascii =
          Ascii(show.text(value).sysBytes)

        def segment(ascii: Ascii, interval: Interval): Ascii =
          ascii.slice(interval.start.n0, interval.end.n0)
