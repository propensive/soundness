/*
    Chiaroscuro, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package chiaroscuro

import probably.*
import gossamer.*
import dissonance.*
import rudiments.*
import anticipation.*
import perforate.*

import unsafeExceptions.canThrowAny
import errorHandlers.throwUnsafely

case class Person(name: Text, age: Int)
case class Organization(name: Text, ceo: Person, staff: List[Person])

case class IdName(id: Text, name: Text)

given Similarity[IdName] = _.id == _.id

import Semblance.*

object Tests extends Suite(t"Chiaroscuro tests"):
  def run(): Unit =
    suite(t"RDiff tests"):
      test(t"Two identical, short Vectors"):
        Vector(1, 2, 3).contrastWith(Vector(1, 2, 3))
      .assert(_ == Identical(t"⟨ 1 2 3 ⟩"))

      test(t"compare two two-parameter case class instances"):
        Person(t"Jack", 12)
      .assert(_ == Person(t"Jill", 12))
      
      test(t"nested comparison"):
        Organization(t"Acme Inc", Person(t"Jack", 12), Nil)
      .assert(_ == Organization(t"Acme Inc", Person(t"Jill", 12), Nil))
      
      test(t"nested comparison 2"):
        Organization(t"Acme Inc.", Person(t"Jack", 12), Nil)
      .assert(_ == Organization(t"Acme Inc", Person(t"Jack", 12), Nil))
      
      test(t"nested comparison 3"):
        Organization(t"Acme Inc.", Person(t"Jack", 12), List(Person(t"Jerry", 18)))
      .assert(_ == Organization(t"Acme Inc.", Person(t"Jack", 12), List(Person(t"Jill", 32), Person(t"Jerry", 18))))
      
      test(t"nested comparison 4"):
        Organization(t"Acme Inc.", Person(t"Jack", 12), List(Person(t"Jerry", 18)))
      .assert(_ == Organization(t"Acme", Person(t"Jack", 12), List(Person(t"Jerry", 18))))

      test(t"diff list"):
        val xs = List(t"one", t"two", t"three", t"four")
        val ys = List(t"one", t"two", t"three", t"vier")
        diff(xs.to(Vector), ys.to(Vector)).edits
      .assert(_ == List(Par(0, 0, t"one"), Par(1, 1, t"two"), Par(2, 2, t"three"), Del(3, t"four"), Ins(3, t"vier")))

      test(t"recurse on similar list entries"):
        val xs = List(IdName(t"one", t"One"), IdName(t"two", t"Two"),  IdName(t"three", t"Three"), IdName(t"four", t"Four"))
        val ys = List(IdName(t"one", t"Ein"), IdName(t"two", t"Zwei"),  IdName(t"three", t"Three"), IdName(t"vier", t"Vier"))
        val result = xs.contrastWith(ys)
        import hieroglyph.textWidthCalculation.uniform
        import escapade.*
        import turbulence.*
        import stdioSources.virtualMachine
        Out.println(result.display)

        result
      .assert(_ == Identical(t""))

