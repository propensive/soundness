package gnossienne

import scala.annotation.*

import adversaria.*
import prepositional.*

case class index() extends StaticAnnotation


trait Entity:
  inline def ref: Ref to this.type =
    val primaryKey: Int = Annotations.firstField[index, this.type]

    new Ref:
      type Operand = primaryKey.type


trait Ref extends Resultant, Operable:
  val key: Operand




case class Person(id: Int, name: String) extends Entity

case class Organization(@index id: Int, name: String, people: List[Person]) extends Entity
