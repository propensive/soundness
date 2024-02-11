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
import rudiments.*
import gossamer.*
import contingency.*
import vacuous.*

//object Month:
  //given Presentation[Month] = _.toString.tt

enum Month:
  case Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec

sealed trait Temporal

case class Date(day: Int, month: Month, year: Int) extends Temporal
case class Time(hour: Int, minute: Int, second: Int) extends Temporal

enum Tree derives Presentation:
  case Leaf
  case Branch(value: Int, left: Tree, right: Tree)

object Presentation extends Derivation[Presentation]:
  given Presentation[Text] = identity(_)
  given Presentation[Double] = _.toString.tt
  given Presentation[Boolean] = boolean => if boolean then t"yes" else t"no"
  given Presentation[Int] = _.toString.tt

  inline def join[DerivationType <: Product: ProductReflection]: Presentation[DerivationType] = value =>
    inline if singleton then typeName else
      val prefix = inline if tuple then t"" else typeName
      fields(value):
        [FieldType] => field => t"$index:$label=${field.present}"
      .join(t"$prefix(", t", ", t")")

  inline def split[DerivationType: SumReflection]: Presentation[DerivationType] = value =>
    variant(value): [VariantType <: DerivationType] =>
      variant => typeName+t"."+variant.present

trait Presentation[ValueType]:
  def present(value: ValueType): Text

extension [ValueType](value: ValueType)
  def present(using presentation: Presentation[ValueType]): Text = presentation.present(value)

sealed trait Human
case class President(name: Text = "nobody", number: Int = 42) extends Human
case class Person(name: Text = "noone", age: Int = 100, male: Boolean = true) extends Human
case class User(person: Person, email: Text = "nobody@nowhere.com")

object Readable extends Derivation[Readable]:
  given text: Readable[Text] = identity(_)
  given int: Readable[Int] = _.s.toInt
  given boolean: Readable[Boolean] = _ == t"yes"

  inline def join[DerivationType <: Product: ProductReflection]: Readable[DerivationType] = text =>
    text.cut(t",").pipe: array =>
      construct: [FieldType] =>
        readable =>
          if index < array.length then readable.read(array(index)) else default().or:
            ???
  
  inline def split[DerivationType: SumReflection]: Readable[DerivationType] = text =>
    text.cut(t":") match
      case List(variant, text2) => delegate(variant): [VariantType <: DerivationType] =>
        context => context.read(text2)

trait Readable[ValueType]:
  def read(text: Text): ValueType

extension (text: Text) def read[ValueType](using readable: Readable[ValueType]): ValueType = readable.read(text)

trait Eq[ValueType]:
  def equal(left: ValueType, right: ValueType): Boolean

extension [ValueType](left: ValueType)
  @targetName("eq")
  infix def ===(right: ValueType)(using eq: Eq[ValueType]): Boolean = eq.equal(left, right)

object Eq extends Derivation[Eq]:
  given iarray[ElementType](using eq: Eq[ElementType]): Eq[IArray[ElementType]] = (left, right) =>
    left.length == right.length && left.indices.all: index =>
      eq.equal(left(index), right(index))
  
  given int: Eq[Int] = _ == _
  given text: Eq[Text] = _.lower == _.lower
  given boolean: Eq[Boolean] = _ & _
  given double: Eq[Double] = (left, right) => math.abs(left - right) < 0.1

  inline def join[DerivationType <: Product: ProductReflection]: Eq[DerivationType] =
    (left, right) =>
      fields(left):
        [FieldType] => leftValue => leftValue === complement(right)
      .all { boolean => boolean }
  
  inline def split[DerivationType: SumReflection]: Eq[DerivationType] =
    (left, right) =>
      variant(left):
        [VariantType <: DerivationType] => leftValue =>
          complement(right).lay(false): rightValue =>
            leftValue === rightValue

@main
def main(): Unit =
  val george = Person("George Washington".tt, 61, true)
  val ronald = User(Person("Ronald Reagan".tt, 51, true), t"ronald@whitehouse.gov")
  println(ronald.present)
  println((ronald, george).present)
  println(Date(15, Month.Feb, 1985).present)
  val time: Temporal = Date(15, Month.Jan, 1983)
  println(time.present)
  println(t"Jimmy Carter,99,yes".read[Person].present)


  import Tree.*
  println(Branch(4, Branch(1, Leaf, Branch(2, Leaf, Leaf)), Leaf).present)
  
  given Raises[VariantError] = errorHandlers.throwUnsafely

  println(t"President:Richard Nixon,37".read[Human].present)

  println(t"hello" === t"hElLo")
  println(President(t"jimmy carter", 99) === President(t"JIMMY CARTER", 99))
  println(President(t"jimmy carter2", 99) === President(t"JIMMY CARTER", 99))
  println(President(t"jimmy carter", 99) === President(t"JIMMY CARTER", 100))
  val array1 = IArray(t"jimmy", t"gerry", t"ronald")
  val array2 = IArray(t"Jimmy", t"Gerry", t"Ronald")
  println(array1 === array2)

  val human1 = t"President:Richard Nixon,37".read[Human]
  val human2 = t"President:George Washington".read[President]
  println("with default "+human2.present)
  val human3 = t"Person:george washington,1,yes".read[Human]
  println(human1 === human2)
  println(human2 === human3)
  val human4 = t"Broken:george washington,1,yes".read[Human]
  
