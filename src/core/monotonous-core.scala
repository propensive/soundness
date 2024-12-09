/*
    Monotonous, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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
import prepositional.*
import hypotenuse.*

package alphabets:
  package binary:
    given Alphabet[Binary] as standard = Alphabet(t"01", false)

  package quaternary:
    given Alphabet[Quaternary] as standard = Alphabet(t"0123", false)
    given Alphabet[Quaternary] as dnaNucleotide = Alphabet(t"ATCG", false)

  package octal:
    given Alphabet[Octal] as standard = Alphabet(t"01234567=", false)

  package hex:
    given Alphabet[Hex] as strictUpperCase = Alphabet(t"0123456789ABCDEF", false)
    given Alphabet[Hex] as strictLowerCase = Alphabet(t"0123456789abcdef", false)

    given Alphabet[Hex] as upperCase =
      Alphabet(t"0123456789ABCDEF", false, strictLowerCase.inverse)

    given Alphabet[Hex] as lowerCase =
      Alphabet(t"0123456789abcdef", false, strictUpperCase.inverse)

    given Alphabet[Hex] as bioctal = Alphabet(t"01234567cjzwfsbv", false)

  package base32:
    given Alphabet[Base32] as strictUpperCase =
      Alphabet(t"ABCDEFGHIJKLMNOPQRSTUVWXYZ234567=", true)

    given Alphabet[Base32] as strictLowerCase =
      Alphabet(t"abcdefghijklmnopqrstuvwxyz234567=", true)

    given Alphabet[Base32] as upperCase =
      Alphabet(t"ABCDEFGHIJKLMNOPQRSTUVWXYZ234567=", true, strictLowerCase.inverse)

    given Alphabet[Base32] as lowerCase =
      Alphabet(t"abcdefghijklmnopqrstuvwxyz234567=", true, strictUpperCase.inverse)

    given Alphabet[Base32] as extendedHexUpperCase =
      Alphabet(t"0123456789ABCDEFGHIJKLMNOPQRSTUV=", true, strictLowerCase.inverse)

    given Alphabet[Base32] as extendedHexLowerCase =
      Alphabet(t"0123456789abcdefghijklmnopqrstuv=", true, strictUpperCase.inverse)

    given Alphabet[Base32] as zBase32 = Alphabet(t"ybndrfg8ejkmcpqxot1uwisza345h769=", true)

    given Alphabet[Base32] as zBase32Unpadded =
      Alphabet(t"ybndrfg8ejkmcpqxot1uwisza345h769", false)

    given Alphabet[Base32] as geohash = Alphabet(t"0123456789bcdefghjkmnpqrstuvwxyz", false)
    given Alphabet[Base32] as wordSafe = Alphabet(t"23456789CFGHJMPQRVWXcfghjmpqrvwx", false)

    private val crockfordAlternatives =
      Alphabet(t"0123456789abcdefghjkmnpqrstvwxyz", false).inverse ++ Map('o' -> 0, 'O' -> 0,
          'i' -> 1, 'I' -> 1, 'L' -> 1)

    given Alphabet[Base32] as crockford =
      Alphabet(t"0123456789ABCDEFGHJKMNPQRSTVWXYZ", false, crockfordAlternatives)

  package base64:
    given Alphabet[Base64] as standard =
      Alphabet(t"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=", true)

    given Alphabet[Base64] as unpadded =
      Alphabet(t"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/", false)

    given Alphabet[Base64] as url =
      Alphabet(t"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_", false)

    given Alphabet[Base64] as xml =
      Alphabet(t"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+-.", true)

    given Alphabet[Base64] as imap =
      Alphabet(t"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+,", false)

    given Alphabet[Base64] as yui =
      Alphabet(t"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789._", false)

    given Alphabet[Base64] as radix64 =
      Alphabet(t"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=", true)

    given Alphabet[Base64] as bcrypt =
      Alphabet(t"./ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789", false)

    given Alphabet[Base64] as sasl =
      Alphabet(t"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+,", false)

    given Alphabet[Base64] as uuencoding =
      Alphabet(t"""!"#$$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_""", false)

  package base256:
    given Alphabet[Base256] as modular =
      def char(i: Int): Char =
        if i == 34 || i == 39 || i == 92 then (i + 8192).toChar
        else if i <= 32 || 126 < i < 161 || i == 173 then (i + 256).toChar
        else i.toChar

      Alphabet(Text(IArray.tabulate(256)(char)), false)

    given Alphabet[Base256] as alphanumericOrBraille =
      def char(i: Int): Char =
        if i <= 32 || i == 34 || i == 39 || i == 92 || i >= 127 then (i + '\u2800').toChar
        else i.toChar

      Alphabet(Text(IArray.tabulate(256)(char)), false)

    given Alphabet[Base256] as braille =
      Alphabet(Text(IArray.tabulate(256) { byte => (byte + '\u2800').toChar }), false)

extension (value: Text)
  def deserialize[SchemeType <: Serialization](using deserializable: Deserializable in SchemeType)
          : Bytes =
    deserializable.deserialize(value)

extension (stream: LazyList[Text])
  def deserialize[SchemeType <: Serialization](using deserializable: Deserializable in SchemeType)
          : LazyList[Bytes] =
    deserializable.deserialize(stream)

extension [ValueType: Encodable in Bytes](value: ValueType)
  def serialize[SchemeType <: Serialization](using encodable: Serializable in SchemeType): Text =
    encodable.encode(value.bytestream)
