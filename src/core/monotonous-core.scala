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

package alphabets:
  package base32:
    given Alphabet[Base32] as default = Alphabet(t"ABCDEFGHIJKLMNOPQRSTUVWXYZ234567=", true)
    given Alphabet[Base32] as zBase32 = Alphabet(t"ybndrfg8ejkmcpqxot1uwisza345h769=", true)
    given Alphabet[Base32] as zBase32Unpadded = Alphabet(t"ybndrfg8ejkmcpqxot1uwisza345h769", false)

  package hex:
    given Alphabet[Hex] as upperCase = Alphabet(t"0123456789ABCDEF", false)
    given Alphabet[Hex] as lowerCase = Alphabet(t"0123456789abcdef", false)
    given Alphabet[Hex] as bioctal = Alphabet(t"01234567cjzwfsbv", false)

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

    given Alphabet[Base64] as Uuencoding =
      Alphabet(t"""!"#$$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_""", false)

extension (value: Text)
  def decode[SchemeType <: Serialization](using decodable: Deserializable in SchemeType): Bytes =
    decodable.decode(value)

extension [ValueType: Encodable in Bytes](value: ValueType)
  def serialize[SchemeType <: Serialization](using encodable: Serializable in SchemeType): Text =
    encodable.encode(value.binary)
