                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.54.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package monotonous

import anticipation.*
import gossamer.*
import hypotenuse.*
import prepositional.*

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

extension (value: Text)
  def deserialize[scheme <: Serialization](using deserializable: Deserializable in scheme): Data =
    deserializable.deserialize(value)


extension (stream: Stream[Text])
  def deserialize[scheme <: Serialization](using deserializable: Deserializable in scheme)
  :   Stream[Data] =

    deserializable.deserialize(stream)


extension [value: Encodable in Data](value: value)
  def serialize[scheme <: Serialization](using encodable: Serializable in scheme): Text =
    encodable.encode(value.bytestream)
