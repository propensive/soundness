/*
    Adversaria, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package adversaria

import probably.*
import rudiments.*
import anticipation.*
import larceny.*
import gossamer.*
import annotation.StaticAnnotation

final case class id() extends StaticAnnotation
final case class unique() extends StaticAnnotation
final case class count(number: Int) extends StaticAnnotation
final case class ref(x: Int) extends StaticAnnotation

case class Person(name: Text, @id email: Text)

@count(10)
case class Company(name: Text)

case class Employee(person: Person, @id @unique code: Long)
case class Letters(@ref(1) alpha: Int, @ref(2) @ref(3) beta: Int, gamma: Int, @ref(4) delta: Int)

object Tests extends Suite(t"Adversaria tests"):

  def run(): Unit =

    test(t"first field"):
      val letters = Letters(5, 6, 7, 8)
      Annotations.firstField[Letters, ref](letters)
    .assert(_ == 5)

    test(t"access field annotations"):
      Annotations.field[Employee](_.code)
    .assert(_.contains(unique()))
    
    test(t"check nonexistant annotations"):
      Annotations.field[Employee](_.person)
    .assert(_ == Nil)

    test(t"get field values"):
      val letters = Letters(5, 6, 7, 8)
      Annotations.fields[Letters, ref].map(_(letters))
    .assert(_ == List(5, 6, 6, 8))
    
    test(t"get field annotations"):
      val letters = Letters(5, 6, 7, 8)
      Annotations.fields[Letters, ref].map(_.annotation)
    .assert(_ == List(ref(1), ref(2), ref(3), ref(4)))

    test(t"get field names"):
      val letters = Letters(5, 6, 7, 8)
      Annotations.fields[Letters, ref].map(_.name)
    .assert(_ == List("alpha", "beta", "beta", "delta"))

    test(t"get annotations on type"):
      summon[Annotations[StaticAnnotation, Company]].annotations
    .assert(_ == List(count(10)))

    test(t"find the field with a particular annotation"):
      val ann = summon[CaseField[Person, id]]
      val person = Person(t"John Smith", t"test@example.com")
      ann(person)
    .assert(_ == t"test@example.com")
    
    test(t"check the name of the field found by an annotation"):
      summon[CaseField[Person, id]].name
    .assert(_ == t"email")
    
    /*test(t"check that given for missing annotation is not resolved"):
      demilitarize:
        summon[CaseField[Company, id]]
      .map(_.errorId)
    .assert(_ == List(ErrorId.MissingImplicitArgumentID))*/

    test(t"extract annotation value generically"):
      def getId[T <: Product](value: T)(using ann: CaseField[T, id]): ann.FieldType = ann(value)

      getId(Employee(Person(t"John Smith", t"test@example.com"), 3141592))
    .assert(_ == 3141592)
