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
import turbulence.*, stdioSources.virtualMachine.textOnly
import anticipation.*
import hieroglyph.*, textMetrics.uniform

case class Person(name: Text, age: Int, description: Text)

@main
def run(): Unit =
  val table = Table[Person](Column(t"Name of the person", sizing = columnSizing.Prose)(_.name),
                            Column(t"Age", sizing = columnSizing.Prose)(_.age.show),
                            Column(t"Description", sizing = columnSizing.Prose)(_.description))

  val tabulation = table.tabulate(List(Person(t"Jon Prety", 41, t"The quick brown fox jumps over the lazy dog."),
                                       Person(t"Kyle Murray", 28, t"Peter piper picked a peck of pickled pepper."),
                                       Person(t"Jimmy O'Dougherty", 59, t"She sells seashells on the sea-shore.")))

  import tableStyles.rounded
  given Decimalizer = Decimalizer(3)
  for width <- 30 to 80 do tabulation.layout(width).render.foreach(Out.println(_))