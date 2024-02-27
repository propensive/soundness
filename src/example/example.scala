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
import anticipation.*
import hieroglyph.*, textMetrics.uniform

case class Person(name: Text, age: Int)

@main
def run(): Unit =
  println(t"Hello world")
  
  val table = Table[Person](
    Column(t"Name", sizing = columnSizing.Fixed(20))(_.name),
    Column(t"Age", sizing = columnSizing.Fixed(10))(_.age.show)
  )

  val tabulation = table.tabulate(List(
    Person(t"Jon Pretty", 41),
    Person(t"Kyle Murray", 28),
    Person(t"Jimmy O'Dougherty", 59)
  ))

  import tableStyles.default

  tabulation.layout(100).rows.each(println(_))
