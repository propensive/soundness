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
package pneumatic

// Word transforms and static-dictionary geometry for Brotli, ported faithfully from Google's
// reference decoder (org.brotli.dec, MIT-licensed, Copyright 2015 Google Inc.).
// `transformDictionaryWord` writes a transformed dictionary word into `dst` and returns its length.
// The transform types are the RFC 7932 set: identity, omit-first-N, omit-last-N, and the two case
// transforms.
private[pneumatic] object BrotliDictionary:
  final val Identity = 0
  final val OmitLast1 = 1;  final val OmitLast2 = 2;  final val OmitLast3 = 3
  final val OmitLast4 = 4;  final val OmitLast5 = 5;  final val OmitLast6 = 6
  final val OmitLast7 = 7;  final val OmitLast8 = 8;  final val OmitLast9 = 9
  final val UppercaseFirst = 10; final val UppercaseAll = 11
  final val OmitFirst1 = 12; final val OmitFirst2 = 13; final val OmitFirst3 = 14
  final val OmitFirst4 = 15; final val OmitFirst5 = 16; final val OmitFirst6 = 17
  final val OmitFirst7 = 18; final val OmitFirst8 = 19; final val OmitFirst9 = 20

  final val minWordLength = 4
  final val maxWordLength = 24
  final val maxTransformedWordLength = 5 + maxWordLength + 8

  val offsetsByLength: Array[Int] = Array(
    0, 0, 0, 0, 0, 4096, 9216, 21504, 35840, 44032, 53248, 63488, 74752, 87040, 93696, 100864,
    104704, 106752, 108928, 113536, 115968, 118528, 119872, 121280, 122016)

  val sizeBitsByLength: Array[Int] = Array(
    0, 0, 0, 0, 10, 10, 11, 11, 10, 10, 10, 10, 10, 9, 9, 8, 7, 7, 8, 7, 7, 6, 6, 5, 5)

  def data: Array[Byte] = BrotliDictionaryData.data

  private def uni(s: String): Array[Byte] =
    val out = new Array[Byte](s.length)
    var i = 0
    while i < s.length do { out(i) = s.charAt(i).toByte; i += 1 }
    out

  final class Transform(prefixString: String, val kind: Int, suffixString: String):
    val prefix: Array[Byte] = uni(prefixString)
    val suffix: Array[Byte] = uni(suffixString)

  private def omitFirst(kind: Int): Int = if kind >= OmitFirst1 then kind - OmitFirst1 + 1 else 0
  private def omitLast(kind: Int): Int = if kind <= OmitLast9 then kind - OmitLast1 + 1 else 0

  val transforms: Array[Transform] = Array(
    Transform("", Identity, ""),
    Transform("", Identity, " "),
    Transform(" ", Identity, " "),
    Transform("", OmitFirst1, ""),
    Transform("", UppercaseFirst, " "),
    Transform("", Identity, " the "),
    Transform(" ", Identity, ""),
    Transform("s ", Identity, " "),
    Transform("", Identity, " of "),
    Transform("", UppercaseFirst, ""),
    Transform("", Identity, " and "),
    Transform("", OmitFirst2, ""),
    Transform("", OmitLast1, ""),
    Transform(", ", Identity, " "),
    Transform("", Identity, ", "),
    Transform(" ", UppercaseFirst, " "),
    Transform("", Identity, " in "),
    Transform("", Identity, " to "),
    Transform("e ", Identity, " "),
    Transform("", Identity, "\""),
    Transform("", Identity, "."),
    Transform("", Identity, "\">"),
    Transform("", Identity, "\n"),
    Transform("", OmitLast3, ""),
    Transform("", Identity, "]"),
    Transform("", Identity, " for "),
    Transform("", OmitFirst3, ""),
    Transform("", OmitLast2, ""),
    Transform("", Identity, " a "),
    Transform("", Identity, " that "),
    Transform(" ", UppercaseFirst, ""),
    Transform("", Identity, ". "),
    Transform(".", Identity, ""),
    Transform(" ", Identity, ", "),
    Transform("", OmitFirst4, ""),
    Transform("", Identity, " with "),
    Transform("", Identity, "'"),
    Transform("", Identity, " from "),
    Transform("", Identity, " by "),
    Transform("", OmitFirst5, ""),
    Transform("", OmitFirst6, ""),
    Transform(" the ", Identity, ""),
    Transform("", OmitLast4, ""),
    Transform("", Identity, ". The "),
    Transform("", UppercaseAll, ""),
    Transform("", Identity, " on "),
    Transform("", Identity, " as "),
    Transform("", Identity, " is "),
    Transform("", OmitLast7, ""),
    Transform("", OmitLast1, "ing "),
    Transform("", Identity, "\n\t"),
    Transform("", Identity, ":"),
    Transform(" ", Identity, ". "),
    Transform("", Identity, "ed "),
    Transform("", OmitFirst9, ""),
    Transform("", OmitFirst7, ""),
    Transform("", OmitLast6, ""),
    Transform("", Identity, "("),
    Transform("", UppercaseFirst, ", "),
    Transform("", OmitLast8, ""),
    Transform("", Identity, " at "),
    Transform("", Identity, "ly "),
    Transform(" the ", Identity, " of "),
    Transform("", OmitLast5, ""),
    Transform("", OmitLast9, ""),
    Transform(" ", UppercaseFirst, ", "),
    Transform("", UppercaseFirst, "\""),
    Transform(".", Identity, "("),
    Transform("", UppercaseAll, " "),
    Transform("", UppercaseFirst, "\">"),
    Transform("", Identity, "=\""),
    Transform(" ", Identity, "."),
    Transform(".com/", Identity, ""),
    Transform(" the ", Identity, " of the "),
    Transform("", UppercaseFirst, "'"),
    Transform("", Identity, ". This "),
    Transform("", Identity, ","),
    Transform(".", Identity, " "),
    Transform("", UppercaseFirst, "("),
    Transform("", UppercaseFirst, "."),
    Transform("", Identity, " not "),
    Transform(" ", Identity, "=\""),
    Transform("", Identity, "er "),
    Transform(" ", UppercaseAll, " "),
    Transform("", Identity, "al "),
    Transform(" ", UppercaseAll, ""),
    Transform("", Identity, "='"),
    Transform("", UppercaseAll, "\""),
    Transform("", UppercaseFirst, ". "),
    Transform(" ", Identity, "("),
    Transform("", Identity, "ful "),
    Transform(" ", UppercaseFirst, ". "),
    Transform("", Identity, "ive "),
    Transform("", Identity, "less "),
    Transform("", UppercaseAll, "'"),
    Transform("", Identity, "est "),
    Transform(" ", UppercaseFirst, "."),
    Transform("", UppercaseAll, "\">"),
    Transform(" ", Identity, "='"),
    Transform("", UppercaseFirst, ","),
    Transform("", Identity, "ize "),
    Transform("", UppercaseAll, "."),
    Transform("\u00c2\u00a0", Identity, ""),
    Transform(" ", Identity, ","),
    Transform("", UppercaseFirst, "=\""),
    Transform("", UppercaseAll, "=\""),
    Transform("", Identity, "ous "),
    Transform("", UppercaseAll, ", "),
    Transform("", UppercaseFirst, "='"),
    Transform(" ", UppercaseFirst, ","),
    Transform(" ", UppercaseAll, "=\""),
    Transform(" ", UppercaseAll, ", "),
    Transform("", UppercaseAll, ","),
    Transform("", UppercaseAll, "("),
    Transform("", UppercaseAll, ". "),
    Transform(" ", UppercaseAll, "."),
    Transform("", UppercaseAll, "='"),
    Transform(" ", UppercaseAll, ". "),
    Transform(" ", UppercaseFirst, "=\""),
    Transform(" ", UppercaseAll, "='"),
    Transform(" ", UppercaseFirst, "='")
  )

  def transformDictionaryWord
    ( dst: Array[Byte], dstOffset: Int, word: Array[Byte], wordOffset0: Int, len0: Int,
      transform: Transform )
  :   Int =

    var offset = dstOffset
    var wordOffset = wordOffset0
    var len = len0

    val prefix = transform.prefix
    var i = 0
    while i < prefix.length do { dst(offset) = prefix(i); offset += 1; i += 1 }

    val op = transform.kind
    var skip = omitFirst(op)
    if skip > len then skip = len
    wordOffset += skip
    len -= skip
    len -= omitLast(op)

    i = len
    while i > 0 do { dst(offset) = word(wordOffset); offset += 1; wordOffset += 1; i -= 1 }

    if op == UppercaseAll || op == UppercaseFirst then
      var uppercaseOffset = offset - len
      var remaining = if op == UppercaseFirst then 1 else len

      while remaining > 0 do
        val c = dst(uppercaseOffset) & 0xff

        if c < 0xc0 then
          if c >= 'a'.toInt && c <= 'z'.toInt
          then dst(uppercaseOffset) = (dst(uppercaseOffset) ^ 32).toByte

          uppercaseOffset += 1
          remaining -= 1
        else if c < 0xe0 then
          dst(uppercaseOffset + 1) = (dst(uppercaseOffset + 1) ^ 32).toByte
          uppercaseOffset += 2
          remaining -= 2
        else
          dst(uppercaseOffset + 2) = (dst(uppercaseOffset + 2) ^ 5).toByte
          uppercaseOffset += 3
          remaining -= 3

    val suffix = transform.suffix
    i = 0
    while i < suffix.length do { dst(offset) = suffix(i); offset += 1; i += 1 }

    offset - dstOffset
