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
import contingency.*, errorHandlers.throwUnsafely
import turbulence.*, stdioSources.virtualMachine.textOnly
import anticipation.*
import hieroglyph.*, textMetrics.uniform

case class Person(name: Text, age: Int, size: Double, description: Text)
given Decimalizer = Decimalizer(3)

import columnAttenuation.fail

@main
def run(): Unit =
  val table = Table[Person](Column(t"Name of the person", sizing = columnar.Prose)(_.name),
                            Column(t"Age", sizing = columnar.Collapsible(0.6))(_.age.show),
                            Column(t"Size", sizing = columnar.Collapsible(0.4))(_.size.show),
                            Column(t"Description", textAlign = TextAlignment.Justify, sizing = columnar.Prose)(_.description))

  val data =
    List(Person(t"Jon Prety", 41, 3.14159, t"The quick brown fox jumps over the lazy dog."),
         Person(t"Kyle Murray", 28, 2.718, t"Peter piper picked a peck of pickled pepper."),
         Person(t"Jimmy O'Dougherty", 59, 1.0, t"She sells seashells on the sea-shore."))

  given TableRelabelling[Person] = () => Map(
    t"name" -> t"Addressee",
    t"age"  -> t"Years old"
  )

  val tabulation = table.tabulate(data)

  import tableStyles.thinRounded

  Out.println(data.table)
