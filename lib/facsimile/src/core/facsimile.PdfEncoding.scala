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
┃    Soundness, version 0.64.0.                                                                    ┃
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
package facsimile

import anticipation.*
import gossamer.*
import rudiments.*
import vacuous.*

// The simple-font encodings of ISO 32000-2 Annex D, as byte-to-Unicode tables, and the glyph
// names — a working subset of the Adobe Glyph List, plus the `uniXXXX` convention — through
// which `/Differences` arrays speak. `'\u0000'` marks an undefined code.
private[facsimile] object PdfEncoding:
  private def table(differences: (Int, Char)*): Array[Char]^{} =
    val array = Array.allocate[Char](256)
    var i = 32
    while i < 256 do
      array(i) = if i == 0x7f || (i >= 0x80 && i <= 0x9f) then '\u0000' else i.toChar
      i += 1

    // An indexed loop, not `foreach`: a closure over the exclusive buffer would alias it.
    var j = 0
    while j < differences.length do
      array(differences(j)(0)) = differences(j)(1)
      j += 1

    Array.freeze(array)

  // StandardEncoding: ASCII with typographic quotes, and its own upper half.
  val standard: Array[Char]^{} =
    table
      ( (0x27, '’'), (0x60, '‘'),
      (0xa0, '\u0000'), (0xa1, '¡'), (0xa2, '¢'), (0xa3, '£'), (0xa4, '⁄'), (0xa5, '¥'),
      (0xa6, 'ƒ'), (0xa7, '§'), (0xa8, '¤'), (0xa9, '\''), (0xaa, '“'), (0xab, '«'),
      (0xac, '‹'), (0xad, '›'), (0xae, 'ﬁ'), (0xaf, 'ﬂ'),
      (0xb0, '\u0000'), (0xb1, '–'), (0xb2, '†'), (0xb3, '‡'), (0xb4, '·'), (0xb5, '\u0000'),
      (0xb6, '¶'), (0xb7, '•'), (0xb8, '‚'), (0xb9, '„'), (0xba, '”'), (0xbb, '»'),
      (0xbc, '…'), (0xbd, '‰'), (0xbe, '\u0000'), (0xbf, '¿'),
      (0xc0, '\u0000'), (0xc1, '`'), (0xc2, '´'), (0xc3, 'ˆ'), (0xc4, '˜'), (0xc5, '¯'),
      (0xc6, '˘'), (0xc7, '˙'), (0xc8, '¨'), (0xc9, '\u0000'), (0xca, '˚'), (0xcb, '¸'),
      (0xcc, '\u0000'), (0xcd, '˝'), (0xce, '˛'), (0xcf, 'ˇ'),
      (0xd0, '—'), (0xd1, '\u0000'), (0xd2, '\u0000'), (0xd3, '\u0000'), (0xd4, '\u0000'),
      (0xd5, '\u0000'), (0xd6, '\u0000'), (0xd7, '\u0000'), (0xd8, '\u0000'), (0xd9, '\u0000'),
      (0xda, '\u0000'), (0xdb, '\u0000'), (0xdc, '\u0000'), (0xdd, '\u0000'), (0xde, '\u0000'),
      (0xdf, '\u0000'),
      (0xe0, '\u0000'), (0xe1, 'Æ'), (0xe2, '\u0000'), (0xe3, 'ª'), (0xe4, '\u0000'),
      (0xe5, '\u0000'), (0xe6, '\u0000'), (0xe7, '\u0000'), (0xe8, 'Ł'), (0xe9, 'Ø'),
      (0xea, 'Œ'), (0xeb, 'º'), (0xec, '\u0000'), (0xed, '\u0000'), (0xee, '\u0000'),
      (0xef, '\u0000'),
      (0xf0, '\u0000'), (0xf1, 'æ'), (0xf2, '\u0000'), (0xf3, '\u0000'), (0xf4, '\u0000'),
      (0xf5, 'ı'), (0xf6, '\u0000'), (0xf7, '\u0000'), (0xf8, 'ł'), (0xf9, 'ø'), (0xfa, 'œ'),
      (0xfb, 'ß'), (0xfc, '\u0000'), (0xfd, '\u0000'), (0xfe, '\u0000'), (0xff, '\u0000') )

  // WinAnsiEncoding: Windows code page 1252 — Latin-1 with publishing characters at 0x80–0x9F.
  val winAnsi: Array[Char]^{} =
    table
      ( (0x80, '€'), (0x82, '‚'), (0x83, 'ƒ'), (0x84, '„'), (0x85, '…'), (0x86, '†'),
      (0x87, '‡'), (0x88, 'ˆ'), (0x89, '‰'), (0x8a, 'Š'), (0x8b, '‹'), (0x8c, 'Œ'),
      (0x8e, 'Ž'), (0x91, '‘'), (0x92, '’'), (0x93, '“'), (0x94, '”'), (0x95, '•'),
      (0x96, '–'), (0x97, '—'), (0x98, '˜'), (0x99, '™'), (0x9a, 'š'), (0x9b, '›'),
      (0x9c, 'œ'), (0x9e, 'ž'), (0x9f, 'Ÿ') )

  // MacRomanEncoding: the classic Mac OS Roman upper half.
  val macRoman: Array[Char]^{} =
    table
      ( (0x80, 'Ä'), (0x81, 'Å'), (0x82, 'Ç'), (0x83, 'É'), (0x84, 'Ñ'), (0x85, 'Ö'),
      (0x86, 'Ü'), (0x87, 'á'), (0x88, 'à'), (0x89, 'â'), (0x8a, 'ä'), (0x8b, 'ã'),
      (0x8c, 'å'), (0x8d, 'ç'), (0x8e, 'é'), (0x8f, 'è'),
      (0x90, 'ê'), (0x91, 'ë'), (0x92, 'í'), (0x93, 'ì'), (0x94, 'î'), (0x95, 'ï'),
      (0x96, 'ñ'), (0x97, 'ó'), (0x98, 'ò'), (0x99, 'ô'), (0x9a, 'ö'), (0x9b, 'õ'),
      (0x9c, 'ú'), (0x9d, 'ù'), (0x9e, 'û'), (0x9f, 'ü'),
      (0xa0, '†'), (0xa1, '°'), (0xa2, '¢'), (0xa3, '£'), (0xa4, '§'), (0xa5, '•'),
      (0xa6, '¶'), (0xa7, 'ß'), (0xa8, '®'), (0xa9, '©'), (0xaa, '™'), (0xab, '´'),
      (0xac, '¨'), (0xad, '\u0000'), (0xae, 'Æ'), (0xaf, 'Ø'),
      (0xb0, '\u0000'), (0xb1, '±'), (0xb2, '\u0000'), (0xb3, '\u0000'), (0xb4, '¥'),
      (0xb5, 'µ'), (0xb6, '\u0000'), (0xb7, '\u0000'), (0xb8, '\u0000'), (0xb9, '\u0000'),
      (0xba, '\u0000'), (0xbb, 'ª'), (0xbc, 'º'), (0xbd, '\u0000'), (0xbe, 'æ'), (0xbf, 'ø'),
      (0xc0, '¿'), (0xc1, '¡'), (0xc2, '¬'), (0xc3, '\u0000'), (0xc4, 'ƒ'), (0xc5, '\u0000'),
      (0xc6, '\u0000'), (0xc7, '«'), (0xc8, '»'), (0xc9, '…'), (0xca, ' '), (0xcb, 'À'),
      (0xcc, 'Ã'), (0xcd, 'Õ'), (0xce, 'Œ'), (0xcf, 'œ'),
      (0xd0, '–'), (0xd1, '—'), (0xd2, '“'), (0xd3, '”'), (0xd4, '‘'), (0xd5, '’'),
      (0xd6, '÷'), (0xd7, '◊'), (0xd8, 'ÿ'), (0xd9, 'Ÿ'), (0xda, '⁄'), (0xdb, '€'),
      (0xdc, '‹'), (0xdd, '›'), (0xde, 'ﬁ'), (0xdf, 'ﬂ'),
      (0xe0, '‡'), (0xe1, '·'), (0xe2, '‚'), (0xe3, '„'), (0xe4, '‰'), (0xe5, 'Â'),
      (0xe6, 'Ê'), (0xe7, 'Á'), (0xe8, 'Ë'), (0xe9, 'È'), (0xea, 'Í'), (0xeb, 'Î'),
      (0xec, 'Ï'), (0xed, 'Ì'), (0xee, 'Ó'), (0xef, 'Ô'),
      (0xf0, '\u0000'), (0xf1, 'Ò'), (0xf2, 'Ú'), (0xf3, 'Û'), (0xf4, 'Ù'), (0xf5, 'ı'),
      (0xf6, 'ˆ'), (0xf7, '˜'), (0xf8, '¯'), (0xf9, '˘'), (0xfa, '˙'), (0xfb, '˚'),
      (0xfc, '¸'), (0xfd, '˝'), (0xfe, '˛'), (0xff, 'ˇ') )

  // Glyph names used by `/Differences`: ASCII, Latin-1 and typographic names, plus the
  // algorithmic `uniXXXX` form.
  def glyph(name: Text): Optional[Char] =
    val string = name.s

    if string.startsWith("uni") && string.length == 7 then
      var value = 0
      var i = 3
      var bad = false

      while i < 7 do
        val digit = CosLexer.hexadecimal(string.charAt(i))
        if digit < 0 then bad = true else value = value*16 + digit
        i += 1

      if bad then Unset else value.toChar
    else if string.length == 1 && (string.charAt(0).isLetterOrDigit) then string.charAt(0)
    else names(name)

  private val names: Map[Text, Char] = Map
    ( "space" -> ' ', "exclam" -> '!', "quotedbl" -> '"', "numbersign" -> '#',
      "dollar" -> '$', "percent" -> '%', "ampersand" -> '&', "quotesingle" -> '\'',
      "parenleft" -> '(', "parenright" -> ')', "asterisk" -> '*', "plus" -> '+',
      "comma" -> ',', "hyphen" -> '-', "period" -> '.', "slash" -> '/',
      "zero" -> '0', "one" -> '1', "two" -> '2', "three" -> '3', "four" -> '4',
      "five" -> '5', "six" -> '6', "seven" -> '7', "eight" -> '8', "nine" -> '9',
      "colon" -> ':', "semicolon" -> ';', "less" -> '<', "equal" -> '=', "greater" -> '>',
      "question" -> '?', "at" -> '@', "bracketleft" -> '[', "backslash" -> '\\',
      "bracketright" -> ']', "asciicircum" -> '^', "underscore" -> '_', "grave" -> '`',
      "braceleft" -> '{', "bar" -> '|', "braceright" -> '}', "asciitilde" -> '~',
      "exclamdown" -> '¡', "cent" -> '¢', "sterling" -> '£', "currency" -> '¤',
      "yen" -> '¥', "brokenbar" -> '¦', "section" -> '§', "dieresis" -> '¨',
      "copyright" -> '©', "ordfeminine" -> 'ª', "guillemotleft" -> '«',
      "logicalnot" -> '¬', "registered" -> '®', "macron" -> '¯', "degree" -> '°',
      "plusminus" -> '±', "acute" -> '´', "mu" -> 'µ', "paragraph" -> '¶',
      "periodcentered" -> '·', "cedilla" -> '¸', "ordmasculine" -> 'º',
      "guillemotright" -> '»', "onequarter" -> '¼', "onehalf" -> '½',
      "threequarters" -> '¾', "questiondown" -> '¿',
      "Agrave" -> 'À', "Aacute" -> 'Á', "Acircumflex" -> 'Â', "Atilde" -> 'Ã',
      "Adieresis" -> 'Ä', "Aring" -> 'Å', "AE" -> 'Æ', "Ccedilla" -> 'Ç',
      "Egrave" -> 'È', "Eacute" -> 'É', "Ecircumflex" -> 'Ê', "Edieresis" -> 'Ë',
      "Igrave" -> 'Ì', "Iacute" -> 'Í', "Icircumflex" -> 'Î', "Idieresis" -> 'Ï',
      "Eth" -> 'Ð', "Ntilde" -> 'Ñ', "Ograve" -> 'Ò', "Oacute" -> 'Ó',
      "Ocircumflex" -> 'Ô', "Otilde" -> 'Õ', "Odieresis" -> 'Ö', "multiply" -> '×',
      "Oslash" -> 'Ø', "Ugrave" -> 'Ù', "Uacute" -> 'Ú', "Ucircumflex" -> 'Û',
      "Udieresis" -> 'Ü', "Yacute" -> 'Ý', "Thorn" -> 'Þ', "germandbls" -> 'ß',
      "agrave" -> 'à', "aacute" -> 'á', "acircumflex" -> 'â', "atilde" -> 'ã',
      "adieresis" -> 'ä', "aring" -> 'å', "ae" -> 'æ', "ccedilla" -> 'ç',
      "egrave" -> 'è', "eacute" -> 'é', "ecircumflex" -> 'ê', "edieresis" -> 'ë',
      "igrave" -> 'ì', "iacute" -> 'í', "icircumflex" -> 'î', "idieresis" -> 'ï',
      "eth" -> 'ð', "ntilde" -> 'ñ', "ograve" -> 'ò', "oacute" -> 'ó',
      "ocircumflex" -> 'ô', "otilde" -> 'õ', "odieresis" -> 'ö', "divide" -> '÷',
      "oslash" -> 'ø', "ugrave" -> 'ù', "uacute" -> 'ú', "ucircumflex" -> 'û',
      "udieresis" -> 'ü', "yacute" -> 'ý', "thorn" -> 'þ', "ydieresis" -> 'ÿ',
      "quoteleft" -> '‘', "quoteright" -> '’', "quotedblleft" -> '“',
      "quotedblright" -> '”', "quotesinglbase" -> '‚', "quotedblbase" -> '„',
      "endash" -> '–', "emdash" -> '—', "bullet" -> '•', "dagger" -> '†',
      "daggerdbl" -> '‡', "ellipsis" -> '…', "perthousand" -> '‰',
      "guilsinglleft" -> '‹', "guilsinglright" -> '›', "fraction" -> '⁄',
      "florin" -> 'ƒ', "franc" -> '₣', "Euro" -> '€', "trademark" -> '™',
      "minus" -> '−', "fi" -> 'ﬁ', "fl" -> 'ﬂ', "dotlessi" -> 'ı', "Lslash" -> 'Ł',
      "lslash" -> 'ł', "OE" -> 'Œ', "oe" -> 'œ', "Scaron" -> 'Š', "scaron" -> 'š',
      "Ydieresis" -> 'Ÿ', "Zcaron" -> 'Ž', "zcaron" -> 'ž', "circumflex" -> 'ˆ',
      "tilde" -> '˜', "breve" -> '˘', "caron" -> 'ˇ', "dotaccent" -> '˙',
      "hungarumlaut" -> '˝', "ogonek" -> '˛', "ring" -> '˚',
      "nbspace" -> ' ', "softhyphen" -> '­' )

  // The reverse of the WinAnsi table: a character's code, for encoding show-text operands.
  private lazy val winAnsiCodes: Map[Char, Int] =
    (32 until 256).flatMap { code =>
      val char = winAnsi.readUnchecked(code)
      if char == ' ' && code != 32 then None else Some(char -> code)
    }
    . to(Map)

  // Encodes text as bytes for a simple WinAnsi font; an unrepresentable character becomes a
  // question mark, as viewers do.
  private[facsimile] def winAnsiEncode(text: Text): Data =
    val bytes = Array.allocate[Byte](text.s.length)
    var i = 0

    while i < text.s.length do
      bytes(i) = winAnsiCodes.at(text.s.charAt(i)).or('?'.toInt).toByte
      i += 1

    Array.freeze(bytes)
