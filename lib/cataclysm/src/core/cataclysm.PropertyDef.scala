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
package cataclysm

import anticipation.*
import contingency.*
import gossamer.*
import rudiments.*
import vacuous.*

object PropertyDef:
  // Every known CSS property, from the string tables compiled from the bundled dataset at build
  // time (`CssData`); only each entry's `syntax` is kept.
  lazy val list: List[PropertyDef] = table(CssData.propertyNames, CssData.propertySyntaxes)

  private def table(names: Array[String], syntaxes: Array[String]): List[PropertyDef] =
    List.tabulate(names.length): index =>
      PropertyDef(names.readUnchecked(index).tt, syntaxes.readUnchecked(index).tt)

  // The same properties keyed by name in a `Dictionary` for fast lookup.
  lazy val all: Dictionary[PropertyDef] =
    val pairs = list.map: property => (property.name, property)

    Dictionary(pairs*)

  // The definition of the named property, or `Unset` if it is not a recognized
  // CSS property. Custom properties (names beginning with `--`) are the caller's
  // responsibility — they are valid but have no definition here.
  def of(name: Text): Optional[PropertyDef] = all(name)

  // The grammar of `src` in `@font-face` in the dataset admits only the quoted form of a
  // format, `format("woff2")`. CSS Fonts 4 also has the keyword form, `format(woff2)`, and
  // `tech(…)`, which are in wide use, so that one descriptor's grammar is written here.
  private def fontSource: Text =
    val remote = t"<url> [ format( [ <string> | <custom-ident> ]# ) ]? [ tech( <custom-ident># ) ]?"

    t"[ $remote | local( <family-name> ) ]#"

  // Every known descriptor of an at-rule, from the bundled `css/at-rules.json`, keyed as
  // `rule/descriptor` (`font-face/src`). A descriptor is declared like a property, but
  // is valid only in the block of its at-rule, and has a grammar of its own there.
  lazy val descriptors: Dictionary[PropertyDef] = Dictionary(descriptorList*)

  private[cataclysm] lazy val descriptorList: List[(Text, PropertyDef)] =
    table(CssData.descriptorNames, CssData.descriptorSyntaxes).map: descriptor =>
      val name = descriptor.name.s.substring(descriptor.name.s.indexOf('/') + 1).nn.tt
      val syntax = if descriptor.name == t"font-face/src" then fontSource else descriptor.syntax

      (descriptor.name, PropertyDef(name, syntax))

  // The definition of the descriptor `name` of the at-rule `rule` (its name without the
  // `@`), or `Unset` if that at-rule has no such descriptor.
  def descriptor(rule: Text, name: Text): Optional[PropertyDef] = descriptors(t"$rule/$name")

  // Whether the at-rule `rule` is one whose block holds descriptors the dataset defines.
  def descriptive(rule: Text): Boolean = rule match
    case t"font-face" | t"counter-style" | t"font-palette-values" | t"page" | t"property"
    | t"view-transition" =>
      true

    case _ =>
      false

// The definition of a single CSS property, or of a descriptor of an at-rule, taken from the
// bundled MDN `mdn/data` dataset (`css/properties.json` and `css/at-rules.json`, CC0).
// `syntax` is the raw value grammar (CSS Value Definition Syntax); `grammar` is its parsed
// form, computed on demand.
case class PropertyDef(name: Text, syntax: Text) derives CanEqual:
  // The bundled dataset's grammars all parse (the test suite checks every entry).
  lazy val grammar: Css.Syntax = unsafely(SyntaxParser.parse(syntax))
