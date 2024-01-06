package wisteria

import anticipation.*
import gossamer.*

object Presentation extends ProductDerivation[Presentation]:
  given Presentation[Text] = identity(_)
  given Presentation[Double] = _.toString.tt
  given Presentation[Boolean] = boolean => if boolean then t"yes" else t"no"
  given Presentation[Int] = _.toString.tt

  inline def join[DerivationType: ProductReflection]: Presentation[DerivationType] = value =>
    val prefix = if tuple then t"" else typeName
    fields(value):
      [fieldType] => field =>
        val other = correspondent(value)
        t"$index:$label=${field.present}/${other.present}"
    .join(t"$prefix(", t", ", t")")

trait Presentation[ValueType]:
  def present(value: ValueType): Text

extension [ValueType](value: ValueType)
  def present(using presentation: Presentation[ValueType]): Text = presentation.present(value)
  
case class Person(name: Text, age: Int, male: Boolean)
case class User(person: Person, email: Text)

@main
def main(): Unit =
  val george = Person("George Washington".tt, 61, true)
  println(george.present)
  val ronald = User(Person("Ronald Reagan".tt, 51, true), t"ronald@whitehouse.gov")
  println(ronald.present)
  println((ronald, george).present)