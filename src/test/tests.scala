/*
    Wisteria, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

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