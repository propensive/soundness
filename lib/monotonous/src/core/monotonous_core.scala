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
┃    Soundness, version 0.63.0.                                                                    ┃
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
import prepositional.*

package alphabets:
  given binaryStandard: Alphabet[Binary] = Alphabet(t"01", false)

  given quaternaryStandard: Alphabet[Quaternary] = Alphabet(t"0123", false)
  given quaternaryDnaNucleotide: Alphabet[Quaternary] = Alphabet(t"ATCG", false)

  given octalStandard: Alphabet[Octal] = Alphabet(t"01234567=", false)

  given hexStrictUpperCase: Alphabet[Hex] = Alphabet(t"0123456789ABCDEF", false)
  given hexStrictLowerCase: Alphabet[Hex] = Alphabet(t"0123456789abcdef", false)

  given hexUpperCase: Alphabet[Hex] =
    Alphabet(t"0123456789ABCDEF", false, hexStrictLowerCase.inverse)

  given hexLowerCase: Alphabet[Hex] =
    Alphabet(t"0123456789abcdef", false, hexStrictUpperCase.inverse)

  given hexBioctal: Alphabet[Hex] = Alphabet(t"01234567cjzwfsbv", false)

  given base32StrictUpperCase: Alphabet[Base32] =
    Alphabet(t"ABCDEFGHIJKLMNOPQRSTUVWXYZ234567=", true)

  given base32StrictLowerCase: Alphabet[Base32] =
    Alphabet(t"abcdefghijklmnopqrstuvwxyz234567=", true)

  given base32UpperCase: Alphabet[Base32] =
    Alphabet(t"ABCDEFGHIJKLMNOPQRSTUVWXYZ234567=", true, base32StrictLowerCase.inverse)

  given base32LowerCase: Alphabet[Base32] =
    Alphabet(t"abcdefghijklmnopqrstuvwxyz234567=", true, base32StrictUpperCase.inverse)

  given base32ExtendedHexUpperCase: Alphabet[Base32] =
    Alphabet(t"0123456789ABCDEFGHIJKLMNOPQRSTUV=", true, base32StrictLowerCase.inverse)

  given base32ExtendedHexLowerCase: Alphabet[Base32] =
    Alphabet(t"0123456789abcdefghijklmnopqrstuv=", true, base32StrictUpperCase.inverse)

  given base32ZBase32: Alphabet[Base32] = Alphabet(t"ybndrfg8ejkmcpqxot1uwisza345h769=", true)

  given base32ZBase32Unpadded: Alphabet[Base32] =
    Alphabet(t"ybndrfg8ejkmcpqxot1uwisza345h769", false)

  given base32Geohash: Alphabet[Base32] = Alphabet(t"0123456789bcdefghjkmnpqrstuvwxyz", false)
  given base32WordSafe: Alphabet[Base32] = Alphabet(t"23456789CFGHJMPQRVWXcfghjmpqrvwx", false)

  private val crockfordAlternatives =
    Alphabet(t"0123456789abcdefghjkmnpqrstvwxyz", false).inverse ++ Map('o' -> 0, 'O' -> 0,
        'i' -> 1, 'I' -> 1, 'L' -> 1)

  given base32Crockford: Alphabet[Base32] =
    Alphabet(t"0123456789ABCDEFGHJKMNPQRSTVWXYZ", false, crockfordAlternatives)

  given base64Standard: Alphabet[Base64] =
    Alphabet(t"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=", true)

  given base64Unpadded: Alphabet[Base64] =
    Alphabet(t"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/", false)

  given base64Url: Alphabet[Base64] =
    Alphabet(t"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_", false)

  given base64Xml: Alphabet[Base64] =
    Alphabet(t"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+-.", true)

  given base64Imap: Alphabet[Base64] =
    Alphabet(t"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+,", false)

  given base64Yui: Alphabet[Base64] =
    Alphabet(t"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789._", false)

  given base64Radix64: Alphabet[Base64] =
    Alphabet(t"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=", true)

  given base64Bcrypt: Alphabet[Base64] =
    Alphabet(t"./ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789", false)

  given base64Sasl: Alphabet[Base64] =
    Alphabet(t"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+,", false)

  given base64Uuencoding: Alphabet[Base64] =
    Alphabet(t"""!"#$$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_""", false)

extension (value: Text)
  def deserialize[scheme <: Serialization](using deserializable: Deserializable in scheme): Data =
    deserializable.deserialize(value)


extension (stream: LazyList[Text])
  def deserialize[scheme <: Serialization](using deserializable: Deserializable in scheme)
  :   LazyList[Data] =

    deserializable.deserialize(stream)


extension [value: Encodable in Data](value: value)
  def serialize[scheme <: Serialization](using encodable: Serializable in scheme): Text =
    encodable.encode(value.bytestream)
