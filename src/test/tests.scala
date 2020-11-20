/*

    Adversaria, version 0.19.0. Copyright 2017-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package adversaria.tests

import probably._
import contextual.examples.scalac._
import contextual.examples.fqt._
import annotation.StaticAnnotation

import adversaria._

final case class id() extends StaticAnnotation
final case class count(number: Int) extends StaticAnnotation

case class Person(name: String, @id email: String)

@count(10)
case class Company(name: String)

case class Employee(person: Person, @id code: Long)

object Tests extends Suite("Adversaria tests") {

  def run(test: Runner): Unit = {
    test("get annotations on type") {
      implicitly[TypeMetadata[Company]].annotations
    }.assert(_ == List(count(10)))

    test("get the short name of the type") {
      implicitly[TypeMetadata[Person]].typeName
    }.assert(_ == "Person")
    
    test("get the full name of the type") {
      implicitly[TypeMetadata[Person]].fullTypeName
    }.assert(_ == "adversaria.tests.Person")
    
    test("find the field with a particular annotation") {
      val ann = implicitly[FindMetadata[id, Person]]
      val person = Person("John Smith", "test@example.com")
      ann.get(person)
    }.assert(_ == "test@example.com")
    
    test("check the name of the field found by an annotation") {
      implicitly[FindMetadata[id, Person]].parameter.fieldName
    }.assert(_ == "email")
    
    test("check that implicit for missing annotation is not resolved") {
      scalac"implicitly[FindMetadata[id, Company]]"
    }.assert(_ == TypecheckError("adversaria: could not find matching annotation"))

    test("extract annotation value generically") {
      def getId[T](value: T)(implicit anns: FindMetadata[id, T]): String =
        anns.get(value).toString

      getId(Employee(Person("John Smith", "test@example.com"), 3141592))
    }.assert(_ == "3141592")

  }
}
