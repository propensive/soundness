package wisteria

import anticipation.*
import gossamer.*

object Presentation extends ProductDerivation[Presentation]:
  given Presentation[Text] = identity(_)
  given Presentation[Double] = _.toString.tt
  given Presentation[Int] = _.toString.tt

  inline def join[DerivationType: ProductReflection]: Presentation[DerivationType] =
    fields(_) { [FieldType] => field => t"$label=${field.present}" }.join(t"$typeName(", t", ", t")")

trait Presentation[ValueType]:
  def present(value: ValueType): Text

extension [ValueType](value: ValueType)
  def present(using presentation: Presentation[ValueType]): Text = presentation.present(value)

case class Person(name: Text, age: Int)

@main
def main(): Unit =
  println(Person("George Washington".tt, 61).present)