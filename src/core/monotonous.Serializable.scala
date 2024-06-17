/*
    Gastronomy, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package monotonous

import anticipation.*
import gossamer.*
import hieroglyph.*, textMetrics.uniform
import rudiments.*
import spectacular.*

object Serializable:
  private val HexLookup: Bytes = IArray.from(t"0123456789ABCDEF".bytes(using charEncoders.ascii))

  given (using alphabet: Alphabet[Hex]) => Serializable in Hex:
    def encode(bytes: Bytes): Text =
      val array = new Array[Char](bytes.length*2)

      bytes.indices.each: index =>
        array(2*index) = alphabet((bytes(index) >> 4) & 0xf)
        array(2*index + 1) = alphabet(bytes(index) & 0xf)

      Text(String(array))

  given (using alphabet: Alphabet[Base32]) => Serializable in Base32:
    def encode(bytes: Bytes): Text = Text.construct:
      @tailrec
      def recur(acc: Int, index: Int, unused: Int): Unit =
        append(alphabet((acc >> 11) & 0x1f))

        if index >= bytes.length then
          append(alphabet((acc >> 6) & 0x1f))
          if unused == 5 then append(alphabet((acc >> 1) & 0x1f))
          if index%8 != 0 then for i <- 0 until (5 - index%8) do append(alphabet(32))

        else
          if unused >= 8 then recur((acc << 5) + (bytes(index) << (unused - 3)), index + 1, unused - 3)
          else recur(acc << 5, index, unused + 5)

      if bytes.isEmpty then t"" else recur(bytes.head.toInt << 8, 1, 8)

  given (using alphabet: Alphabet[Base64]) => Serializable in Base64:
    def encode(bytes: Bytes): Text = Text.construct:
      @tailrec
      def recur(i: Int = 0): Int = if i >= bytes.length - 2 then i else
        val word = ((bytes(i) & 0xff) << 16) | ((bytes(i + 1) & 0xff) << 8) | (bytes(i + 2) & 0xff)
        append(alphabet((word >> 18) & 0x3f))
        append(alphabet((word >> 12) & 0x3f))
        append(alphabet((word >> 6) & 0x3f))
        append(alphabet(word & 0x3f))
        recur(i + 3)

      val last = recur()
      if last < bytes.length then
        val word = (bytes(last) & 0xff) << 16
        append(alphabet((word >> 18) & 0x3f))
        if last + 1 < bytes.length then
          val word2 = word | ((bytes(last + 1) & 0xff) << 8)
          append(alphabet((word2 >> 12) & 0x3f))
          append(alphabet((word2 >> 6) & 0x3f))
          if alphabet.padding then append(alphabet(64))
        else
          append(alphabet((word >> 12) & 0x3f))
          if alphabet.padding then
            append(alphabet(64))
            append(alphabet(64))

  given Serializable in Binary:
    def encode(bytes: Bytes): Text = Text.construct:
      bytes.each:
        byte => append(Integer.toBinaryString(byte).nn.show.fit(8, Rtl, '0'))

trait Serializable:
  type In <: Serialization
  def encode(bytes: Bytes): Text
