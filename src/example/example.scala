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

import anticipation.*
import contingency.*, strategies.throwUnsafely
import escapade.*
import gossamer.*
import hieroglyph.*, textMetrics.uniform
import iridescence.*
import rudiments.*
import spectacular.*
import turbulence.*, stdioSources.virtualMachine.ansi

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
     (Column(e"$Bold(Name)", sizing = columnar.Prose): lib =>
        e"$Bold(${lib.name})",
      Column(e"$Bold(Identifier)", sizing = columnar.Collapsible(0.9))(_.id),
      Column(e"$Bold(LoC)", sizing = columnar.Collapsible(0.3))(_.linesOfCode),
      Column(e"$Bold(Year)", sizing = columnar.Collapsible(0.5)): lib =>
        if lib.year > 2020 then e"${webColors.SandyBrown}(${lib.year})"
        else e"${webColors.Chocolate}(${lib.year})",
      Column(e"$Bold(Description)", textAlign = TextAlignment.Justify, sizing = columnar.Prose): lib =>
       e"$Italic(${lib.description})")

  // given TableRelabelling[Person] = () => Map(
  //   t"name" -> t"Addressee",
  //   t"age"  -> t"Years old"
  // )

  locally:
    import tableStyles.thinRounded
    Out.println(table.tabulate(libraries).grid(70))

    case class Digit(n: Int, digitName: Text)
    val data = List(Digit(1, t"one"), Digit(2, t"two"), Digit(3, t"three"))
    Out.println(data.table)

    val table2 = Table[Fruit]
     (Column(e"$Bold(Color)", textAlign = TextAlignment.Center): fruit =>
        e"${Bg(fruit.color)}(   )",
      Column(e"$Bold(English)")(_.english),
      Column(e"$Bold(Russian)")(_.russian),
      Column(e"$Bold(Chinese)")(_.chinese),
      Column(e"$Bold(Japanese)")(_.japanese))

    Out.println(table2.tabulate(fruits).grid(100))


    case class Member(name: Text, handle: Text, age: Int)

    val pinkFloyd =
      List(Member(t"Nick Mason", t"@nickmasondrums", 80), Member(t"David Gilmour", t"@davidgilmour", 78))

    Out.println(pinkFloyd.table)

  // for style <- List(tableStyles.default, tableStyles.thinRounded, tableStyles.horizontal, tableStyles.midOnly, tableStyles.vertical, tableStyles.minimal)
  // do style.give(Out.println(table.tabulate(libraries.take(2)).grid(50)))

case class Fruit(chinese: Text, english: Text, russian: Text, japanese: Text, color: Rgb24)

val fruits =
  List
   (Fruit(t"苹果", t"apple", t"яблоко", t"リンゴ", webColors.Crimson),
    Fruit(t"梨", t"pear", t"груша", t"ナシ", webColors.YellowGreen),
    Fruit(t"橙子", t"orange", t"апельсин", t"オレンジ", webColors.DarkOrange),
    Fruit(t"柚子", t"yuzu", t"юдзу", t"柚子", webColors.Yellow))

enum Foo:
  case User(name: Text, size: Double)
  case Organization(name: Text, founded: Int)
