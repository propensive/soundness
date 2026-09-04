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
package cosmopolite

import anticipation.*
import gossamer.*
import hieroglyph.*

object en extends Language(t"en"):
  type Code = en

  given collatable: en is Collatable:
    def collation: Collation = collations.unicodeCollation

trait en

object pl extends Language(t"pl"):
  type Code = pl

  // CLDR Polish: nine accented letters collate as letters in their own right, after their
  // bases; ż sorts after ź, so its rule chains through the previous rule's target.
  private lazy val tailored: Collation = Collation:
    CollationTable.root.tailor:
      List
        ( CollationRule(t"a", t"ą", CollationLevel.Primary),
          CollationRule(t"ą", t"Ą", CollationLevel.Tertiary),
          CollationRule(t"c", t"ć", CollationLevel.Primary),
          CollationRule(t"ć", t"Ć", CollationLevel.Tertiary),
          CollationRule(t"e", t"ę", CollationLevel.Primary),
          CollationRule(t"ę", t"Ę", CollationLevel.Tertiary),
          CollationRule(t"l", t"ł", CollationLevel.Primary),
          CollationRule(t"ł", t"Ł", CollationLevel.Tertiary),
          CollationRule(t"n", t"ń", CollationLevel.Primary),
          CollationRule(t"ń", t"Ń", CollationLevel.Tertiary),
          CollationRule(t"o", t"ó", CollationLevel.Primary),
          CollationRule(t"ó", t"Ó", CollationLevel.Tertiary),
          CollationRule(t"s", t"ś", CollationLevel.Primary),
          CollationRule(t"ś", t"Ś", CollationLevel.Tertiary),
          CollationRule(t"z", t"ź", CollationLevel.Primary),
          CollationRule(t"ź", t"Ź", CollationLevel.Tertiary),
          CollationRule(t"ź", t"ż", CollationLevel.Primary),
          CollationRule(t"ż", t"Ż", CollationLevel.Tertiary) )

  given collatable: pl is Collatable:
    def collation: Collation = tailored

trait pl

object fr extends Language(t"fr"):
  type Code = fr

  // CLDR root French is untailored (backward secondary accents are a Canadian French
  // convention, not used by `fr` since CLDR 1.9).
  given collatable: fr is Collatable:
    def collation: Collation = collations.unicodeCollation

trait fr

object de extends Language(t"de"):
  type Code = de

  // CLDR standard German is untailored (umlauts differ at the secondary level, which is
  // already the root table's behaviour; DIN phonebook order is a different collation type).
  given collatable: de is Collatable:
    def collation: Collation = collations.unicodeCollation

trait de

object es extends Language(t"es"):
  type Code = es

  // CLDR Spanish: ñ collates as a letter between n and o.
  private lazy val tailored: Collation = Collation:
    CollationTable.root.tailor:
      List
        ( CollationRule(t"n", t"ñ", CollationLevel.Primary),
          CollationRule(t"ñ", t"Ñ", CollationLevel.Tertiary) )

  given collatable: es is Collatable:
    def collation: Collation = tailored

trait es

infix type via [value, language] = Locale[language] ?=> value

// Lexically-scoped by necessity (`Collation` belongs to gossamer, the language tags to
// cosmopolite, so neither companion can host it): a contextual `Locale` confers its
// language's sort order.
given localeCollation: [language]
      =>  Locale[language]
      =>  (collatable: language is Collatable)
      =>  Collation =
  collatable.collation
