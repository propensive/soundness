/*
    Monotonous, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

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
import hypotenuse.*
import prepositional.*
import proscenium.*

package alphabets:
  package binary:
    given standard: Alphabet[Binary] = Alphabet(t"01", false)

  package quaternary:
    given standard: Alphabet[Quaternary] = Alphabet(t"0123", false)
    given dnaNucleotide: Alphabet[Quaternary] = Alphabet(t"ATCG", false)

  package octal:
    given standard: Alphabet[Octal] = Alphabet(t"01234567=", false)

  package hex:
    given strictUpperCase: Alphabet[Hex] = Alphabet(t"0123456789ABCDEF", false)
    given strictLowerCase: Alphabet[Hex] = Alphabet(t"0123456789abcdef", false)

    given upperCase: Alphabet[Hex] =
      Alphabet(t"0123456789ABCDEF", false, strictLowerCase.inverse)

    given lowerCase: Alphabet[Hex] =
      Alphabet(t"0123456789abcdef", false, strictUpperCase.inverse)

    given bioctal: Alphabet[Hex] = Alphabet(t"01234567cjzwfsbv", false)

  package base32:
    given strictUpperCase: Alphabet[Base32] =
      Alphabet(t"ABCDEFGHIJKLMNOPQRSTUVWXYZ234567=", true)

    given strictLowerCase: Alphabet[Base32] =
      Alphabet(t"abcdefghijklmnopqrstuvwxyz234567=", true)

    given upperCase: Alphabet[Base32] =
      Alphabet(t"ABCDEFGHIJKLMNOPQRSTUVWXYZ234567=", true, strictLowerCase.inverse)

    given lowerCase: Alphabet[Base32] =
      Alphabet(t"abcdefghijklmnopqrstuvwxyz234567=", true, strictUpperCase.inverse)

    given extendedHexUpperCase: Alphabet[Base32] =
      Alphabet(t"0123456789ABCDEFGHIJKLMNOPQRSTUV=", true, strictLowerCase.inverse)

    given extendedHexLowerCase: Alphabet[Base32] =
      Alphabet(t"0123456789abcdefghijklmnopqrstuv=", true, strictUpperCase.inverse)

    given zBase32: Alphabet[Base32] = Alphabet(t"ybndrfg8ejkmcpqxot1uwisza345h769=", true)

    given zBase32Unpadded: Alphabet[Base32] =
      Alphabet(t"ybndrfg8ejkmcpqxot1uwisza345h769", false)

    given geohash: Alphabet[Base32] = Alphabet(t"0123456789bcdefghjkmnpqrstuvwxyz", false)
    given wordSafe: Alphabet[Base32] = Alphabet(t"23456789CFGHJMPQRVWXcfghjmpqrvwx", false)

    private val crockfordAlternatives =
      Alphabet(t"0123456789abcdefghjkmnpqrstvwxyz", false).inverse ++ Map('o' -> 0, 'O' -> 0,
          'i' -> 1, 'I' -> 1, 'L' -> 1)

    given crockford: Alphabet[Base32] =
      Alphabet(t"0123456789ABCDEFGHJKMNPQRSTVWXYZ", false, crockfordAlternatives)

  package base64:
    given standard: Alphabet[Base64] =
      Alphabet(t"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=", true)

    given unpadded: Alphabet[Base64] =
      Alphabet(t"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/", false)

    given url: Alphabet[Base64] =
      Alphabet(t"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_", false)

    given xml: Alphabet[Base64] =
      Alphabet(t"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+-.", true)

    given imap: Alphabet[Base64] =
      Alphabet(t"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+,", false)

    given yui: Alphabet[Base64] =
      Alphabet(t"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789._", false)

    given radix64: Alphabet[Base64] =
      Alphabet(t"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=", true)

    given bcrypt: Alphabet[Base64] =
      Alphabet(t"./ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789", false)

    given sasl: Alphabet[Base64] =
      Alphabet(t"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+,", false)

    given uuencoding: Alphabet[Base64] =
      Alphabet(t"""!"#$$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_""", false)

  package base256:
    given modular: Alphabet[Base256] =
      def char(i: Int): Char =
        if i == 34 || i == 39 || i == 92 then (i + 8192).toChar
        else if i <= 32 || 126 < i < 161 || i == 173 then (i + 256).toChar
        else i.toChar

      Alphabet(Text(IArray.tabulate(256)(char)), false)

    given alphanumericOrBraille: Alphabet[Base256] =
      def char(i: Int): Char =
        if i <= 32 || i == 34 || i == 39 || i == 92 || i >= 127 then (i + '\u2800').toChar
        else i.toChar

      Alphabet(Text(IArray.tabulate(256)(char)), false)

    given braille: Alphabet[Base256] =
      Alphabet(Text(IArray.tabulate(256) { byte => (byte + '\u2800').toChar }), false)

extension (value: Text)
  def deserialize[SchemeType <: Serialization](using deserializable: Deserializable in SchemeType)
  :     Bytes =
    deserializable.deserialize(value)

extension (stream: Stream[Text])
  def deserialize[SchemeType <: Serialization](using deserializable: Deserializable in SchemeType)
  :     Stream[Bytes] =
    deserializable.deserialize(stream)

extension [ValueType: Encodable in Bytes](value: ValueType)
  def serialize[SchemeType <: Serialization](using encodable: Serializable in SchemeType): Text =
    encodable.encode(value.bytestream)
