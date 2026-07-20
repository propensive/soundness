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
import hellenism.*
import jacinta.*
import turbulence.*
import vacuous.*

import hellenism.classloaders.threadContextClassloader

object PropertyDef:
  // One entry of the bundled `properties.json`. Only `syntax` is read; jacinta
  // ignores the dataset's other fields (initial, inherited, …).
  private object Entry:
    given decodable: Tactic[JsonError] => Entry is Json.Decodable = Json.DecodableDerivation.derived

  private case class Entry(syntax: Text)

  // Every known CSS property. Read lazily from the classpath resource the first
  // time a property is resolved; a malformed resource is a packaging error,
  // hence `throwUnsafely`.
  lazy val list: List[PropertyDef] =
    import contingency.strategies.throwUnsafely

    val entries = cp"/cataclysm/properties.json".read[Json].as[Map[Text, Entry]]

    entries.to(List).map: (name, entry) =>
      PropertyDef(name, entry.syntax)

  // The same properties keyed by name in a `Dictionary` for fast lookup.
  lazy val all: Dictionary[PropertyDef] =
    val pairs = list.map: property =>
      (property.name, property)

    Dictionary(pairs*)

  // The definition of the named property, or `Unset` if it is not a recognized
  // CSS property. Custom properties (names beginning with `--`) are the caller's
  // responsibility — they are valid but have no definition here.
  def of(name: Text): Optional[PropertyDef] = all(name)

// The definition of a single CSS property, taken from the bundled MDN `mdn/data`
// dataset (`css/properties.json`, CC0). `syntax` is the raw value grammar (CSS
// Value Definition Syntax); `grammar` is its parsed form, computed on demand.
case class PropertyDef(name: Text, syntax: Text) derives CanEqual:
  lazy val grammar: Syntax =
    import contingency.strategies.throwUnsafely
    SyntaxParser.parse(syntax)
