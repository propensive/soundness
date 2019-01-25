package adversaria.tests

import probation.{TestApp, test}
import contextual.data.scalac._
import contextual.data.fqt._
import annotation.StaticAnnotation

import adversaria._

final case class id() extends StaticAnnotation
final case class count(number: Int) extends StaticAnnotation

case class Person(name: String, @id email: String)

@count(10)
case class Company(name: String)

case class Employee(person: Person, @id code: Long)

object Tests extends TestApp {

  def tests(): Unit = {
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
    }.assert(_ == TypecheckError("adversaria: could not find a parameter annotated with type @adversaria.tests.id"))

    test("extract annotation value generically") {
      def getId[T](value: T)(implicit anns: FindMetadata[id, T]): String =
        anns.get(value).toString

      getId(Employee(Person("John Smith", "test@example.com"), 3141592))
    }.assert(_ == "3141592")

  }
}
