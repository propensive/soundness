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
package honeycomb

import language.dynamics

import scala.collection.mutable as scm

import anticipation.*
import beneficence.*
import gossamer.*
import vacuous.*

object Dom:
  private[honeycomb] val elements: scm.HashMap[Dom, Dictionary[Tag]] = scm.HashMap()
  private[honeycomb] val attributes: scm.HashMap[Dom, Dictionary[Attribute]] = scm.HashMap()
  private[honeycomb] val entities: scm.HashMap[Dom, Dictionary[Text]] = scm.HashMap()

  // Alphabets for the compact tries the parser walks. The parser lowercases
  // every char before stepping `compactElements` and `compactAttributes`, so
  // those alphabets carry only lowercase letters; entity names are
  // case-sensitive so `compactEntities` covers both cases plus `;`.
  val tagAlphabet: CompactTrie.Alphabet =
    CompactTrie.Alphabet.of("abcdefghijklmnopqrstuvwxyz0123456789")

  val attributeAlphabet: CompactTrie.Alphabet =
    CompactTrie.Alphabet.of("abcdefghijklmnopqrstuvwxyz-")

  val entityAlphabet: CompactTrie.Alphabet =
    CompactTrie.Alphabet.of("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789;")

trait Dom extends Findable:
  val elements: Dictionary[Tag]
  val attributes: Dictionary[Attribute]
  val entities: Dictionary[Text]

  // Dense flat-array forms used by the HTML parser's hot lookup loops. Each
  // is computed once per `Dom` instance (`lazy val`), so the materialisation
  // cost is paid the first time the parser encounters this DOM and amortised
  // across every parse afterwards. The original `Dictionary` fields remain
  // the source of truth and stay available for callers (e.g. macros) that
  // iterate over the full vocabulary.
  lazy val compactElements:   CompactTrie[Tag]       = CompactTrie.from(elements, Dom.tagAlphabet)
  lazy val compactAttributes: CompactTrie[Attribute] =
    CompactTrie.from(attributes, Dom.attributeAlphabet)
  lazy val compactEntities:   CompactTrie[Text]      =
    CompactTrie.from(entities, Dom.entityAlphabet)

  def doctype: Doctype
  def infer(parent: Tag, child: Tag): Optional[Tag]
  def generic: Tag = Tag.root(elements.iterator.map(_.label).to(Set))
