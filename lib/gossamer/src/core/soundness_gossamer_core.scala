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
package soundness

export
  gossamer
  . { add, after, append, appendln, Ascii, ascii, AsciiBuilder, before, Bidi, blank, BoundsError,
      broken, build, Builder, builder, camel, capitalize, CaseSensitivity, center, chars, contains,
      count, cut, Cuttable, Decimalizer, Dictionary, ends, erase, extract, fill, fit, from,
      fuzzy, Grapheme, init, join, Joinable, kebab, keep, length, Lexicon, lines, lower,
      Ltr, Numerous, ossify, pad, pascal, plain, Proximity, proximity, Pue, pue, punycode,
      RangeError, reversibleTextual, Rtl, search, offsetOf, SimpleTExtractor, skip, slices, snake, snip,
      spaced, starts, strip, sub, subscripts, superscripts, sysData, t, tail, text, textDecodable,
      TextBuilder,
      Textual, tr, trim, txt, uncamel, uncapitalize, unkebab, unsnake, upper, upto, urlDecode,
      urlEncode, utf16, utf8, pinpoint, words, Writing, WritingBuilder, a, justify, punch }

package decimalConverters:
  export gossamer.decimalConverters.javaDecimalConverter

package enumIdentification:
  export gossamer.enumIdentification.kebabCaseIdentifiable
  export gossamer.enumIdentification.pascalCaseIdentifiable
  export gossamer.enumIdentification.snakeCaseIdentifiable
  export gossamer.enumIdentification.camelCaseIdentifiable

package proximities:
  export gossamer.proximities.jaroProximity
  export gossamer.proximities.jaroWinklerProximity
  export gossamer.proximities.prefixProximity
  export gossamer.proximities.levenshteinProximity
  export gossamer.proximities.normalizedLevenshteinProximity

package caseSensitivity:
  export gossamer.caseSensitivity.{caseInsensitive, caseSensitive, smartCase}
