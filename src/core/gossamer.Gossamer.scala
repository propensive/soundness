/*
    Gossamer, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package gossamer

import anticipation.*
import denominative.*
import fulminate.*
import rudiments.*
import spectacular.*
import vacuous.*

import scala.quoted.*

object Gossamer:
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

  def ascii(context: Expr[StringContext], parts: Expr[Seq[Ascii]])(using Quotes): Expr[Ascii] =
    val dynamicParts: List[Expr[Ascii]] = parts.absolve match
      case Varargs(parts) => parts.to(List)

    val staticParts: List[Expr[Ascii]] = context.value.get.parts.to(List).map: part =>
      val bytes: IArray[Expr[Byte]] = part.tt.chars.map: char =>
        if char >= 128 then halt(m"$char is not a valid ASCII character")
        Expr[Byte](char.toByte)

      '{Ascii(Bytes(${Varargs(bytes)}*))}

    def recur(first: List[Expr[Ascii]], second: List[Expr[Ascii]], expr: Expr[Ascii])
    :     Expr[Ascii] =
      first match
        case head :: tail => recur(second, tail, '{$expr+$head})
        case Nil          => expr

    recur(staticParts.tail.to(List), dynamicParts, staticParts.head)
