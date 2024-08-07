/*
    Escritoire, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package escritoire

import gossamer.*
import rudiments.*
import spectacular.*
import contingency.*, strategies.throwUnsafely
import turbulence.*, stdioSources.virtualMachine.textOnly
import anticipation.*
import hieroglyph.*, textMetrics.uniform

given Decimalizer = Decimalizer(3)

case class Library(id: Text, name: Text, linesOfCode: Int, year: Int, description: Text)

val libraries: List[Library] = List
 (Library(t"wisteria", t"Wisteria", 581, 2017, t"Simple, fast and transparant generic derivation for typeclasses"),
  Library(t"quantitative", t"Quantitative", 1271, 2023, t"Statically-checked physical units with seamless syntax"),
  Library(t"turbulence", t"Turbulence", 1047, 2022, t"Simple tools for working with data streams"),
  Library(t"escritoire", t"Escritoire", 494, 2018, t"A library for writing tables"))

import columnAttenuation.fail

@main
def run(): Unit =
  val table =
    Table[Library]
     (Column(t"Name", sizing = columnar.Prose)(_.name),
      Column(t"Identifier", sizing = columnar.Collapsible(0.9))(_.id),
      Column(t"LoC", sizing = columnar.Collapsible(0.3))(_.linesOfCode),
      Column(t"Year", sizing = columnar.Collapsible(0.5))(_.year),
      Column(t"Description", textAlign = TextAlignment.Justify, sizing = columnar.Prose)
       (_.description))

  // given TableRelabelling[Person] = () => Map(
  //   t"name" -> t"Addressee",
  //   t"age"  -> t"Years old"
  // )

  locally:
    import tableStyles.default
    Out.println(libraries.table.grid(100))
    Out.println(List[Foo.User](Foo.User(t"Jon", 12.4)).table.grid(100))
    Out.println(table.tabulate(libraries).grid(37))

  // for style <- List(tableStyles.default, tableStyles.thinRounded, tableStyles.horizontal, tableStyles.midOnly, tableStyles.vertical, tableStyles.minimal)
  // do style.give(Out.println(table.tabulate(libraries.take(2)).grid(50)))

enum Foo:
  case User(name: Text, size: Double)
  case Organization(name: Text, founded: Int)
