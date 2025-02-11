/*
    Wisteria, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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
import contingency.*
import rudiments.*
import vacuous.*

import scala.util.Try
import scala.deriving.Mirror.ProductOf
import scala.deriving.Mirror.SumOf

//object Month:
  //given Presentation[Month] = _.toString.tt

object SumOnly extends SumDerivation[SumOnly]:

  given SumOnly[SumOnlyEnum.Alpha] = alpha => println(s"$alpha is an alpha")
  given SumOnly[SumOnlyEnum.Beta] = beta => println(s"$beta is a beta")

  inline def split[DerivationType: SumReflection]: SumOnly[DerivationType] =
    value => variant(value):
      [VariantType <: DerivationType] => value =>
        context.applyTo(value)

trait SumOnly[Type]:
  def applyTo(value: Type): Unit

enum SumOnlyEnum:
  case Alpha(n: Int)
  case Beta(n: String)

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
  given Presentation[Boolean] = boolean => if boolean then "yes".tt else "no".tt
  given Presentation[Int] = _.toString.tt

  inline def join[DerivationType <: Product: ProductReflection]: Presentation[DerivationType] = value =>
    inline if singleton then typeName else
      val prefix = inline if tuple then "".tt else typeName
      fields(value):
        [FieldType] => field => s"$index:$label=${field.present}".tt
      .mkString((prefix.s+"("), ", ", ")").tt

  inline def split[DerivationType: SumReflection]: Presentation[DerivationType] = value =>
    variant(value):
      [VariantType <: DerivationType] =>
        variant => (typeName.s+"."+variant.present).tt

trait Presentation[ValueType]:
  def present(value: ValueType): Text

extension [ValueType](value: ValueType)
  def present(using presentation: Presentation[ValueType]): Text = presentation.present(value)

sealed trait Human
case class President(name: Text = "nobody".tt, number: Int = 42) extends Human
case class Person(name: Text = "noone".tt, age: Int = 100, male: Boolean = true) extends Human
case class User(person: Person, email: Text = "nobody@nowhere.com".tt)

enum Animal:
  case Carnivore(name: Text, age: Int = -1)

object Readable extends Derivation[Readable]:
  given text: Readable[Text] = identity(_)
  given int: Readable[Int] = _.s.toInt
  given boolean: Readable[Boolean] = _ == "yes".tt

  inline def join[DerivationType <: Product: ProductReflection]: Readable[DerivationType] = text =>
    IArray.from(text.s.split(",")).pipe:
      array =>
        construct:
          [FieldType] =>
            readable =>
              if index < array.length then readable.read(array(index).tt) else default().or:
                ???

  inline def split[DerivationType: SumReflection]: Readable[DerivationType] = text =>
    text.s.split(":").to(List).map(_.tt) match
      case List(variant, text2) => delegate(variant):
        [VariantType <: DerivationType] =>
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
  given text: Eq[Text] = _.s.toLowerCase == _.s.toLowerCase
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

trait Parser[ValueType] {
  def parse(s: String): Option[ValueType]
}

object Parser extends ProductDerivation[Parser] {
  given Parser[Int] with
    def parse(s: String): Option[Int] = s.toIntOption

  given Parser[Boolean] with
    def parse(s: String): Option[Boolean] = s.toBooleanOption

  inline def join[DerivationType <: Product: ProductReflection]: Parser[DerivationType] = input =>
    IArray.from(input.split(',')).pipe: inputArr =>
      constructWith[Option](
        [MonadicTypeIn, MonadicTypeOut] => _.flatMap,
        [MonadicType] => Some(_),
        [FieldType] => context =>
          if index < inputArr.length then context.parse(inputArr(index))
          else None
      )
}

case class ParserTestCaseClass(intValue: Int, booleanValue: Boolean)

trait Show[T] {
  def show(value: T): String
}

extension[T: Show](value: T) {
  def show: String = summon[Show[T]].show(value)
}

object Show extends Derivation[Show] {

  inline def join[DerivationType <: Product: ProductReflection]: Show[DerivationType] = value =>
    typeName.s

  inline def split[DerivationType: SumReflection]: Show[DerivationType] = value =>
    inline if choice then
      variant(value):
        [VariantType <: DerivationType] =>
          variant => typeName.s+"."+variant.show
    else
      compiletime.error("cannot derive Show for adt")
}

enum Simple:
  case First
  case Second
  case Third

enum Adt:
  case First
  case Second(a: Boolean)

trait Producer[ValueType] {
  def produce(s: String): Option[ValueType]
}
object Producer extends Derivation[Producer] {

  inline def join[DerivationType <: Product: ProductOf]: Producer[DerivationType] = ???

  inline def split[DerivationType: SumOf]: Producer[DerivationType] = input =>
    inline if choice then
      Some(singleton(input))
    else compiletime.error("not a choice")
}

@main
def main(): Unit =
  val george = Person("George Washington".tt, 61, true)
  val ronald = User(Person("Ronald Reagan".tt, 51, true), "ronald@whitehouse.gov".tt)
  println(ronald.present)
  println((ronald, george).present)
  println(Date(15, Month.Feb, 1985).present)
  val time: Temporal = Date(15, Month.Jan, 1983)
  println(time.present)
  println("Jimmy Carter,99,yes".tt.read[Person].present)


  import Tree.*
  println(Branch(4, Branch(1, Leaf, Branch(2, Leaf, Leaf)), Leaf).present)

  given Tactic[VariantError] = strategies.throwUnsafely

  println("President:Richard Nixon,37".tt.read[Human].present)

  println("hello".tt === "hElLo".tt)
  println(President("jimmy carter".tt, 99) === President("JIMMY CARTER".tt, 99))
  println(President("jimmy carter2".tt, 99) === President("JIMMY CARTER".tt, 99))
  println(President("jimmy carter".tt, 99) === President("JIMMY CARTER".tt, 100))
  val array1 = IArray("jimmy".tt, "gerry".tt, "ronald".tt)
  val array2 = IArray("Jimmy".tt, "Gerry".tt, "Ronald".tt)
  println(array1 === array2)

  val human1 = "President:Richard Nixon,37".tt.read[Human]
  val human2 = "President:George Washington".tt.read[President]
  println("with default "+human2.present)
  val human3 = "Person:george washington,1,yes".tt.read[Human]
  println(human1 === human2)
  println(human2 === human3)
  val human4 = Try("Broken:george washington,1,yes".tt.read[Human])
  println(human4.isFailure)
  println(human4.failed.get.getMessage())
  val animal = "Carnivore:Wolf".tt.read[Animal]
  println("with default "+animal.present)

  println("withContext:")
  val parserForTest = summon[Parser[ParserTestCaseClass]]
  val successfulParse = parserForTest.parse("120,false")
  println(successfulParse.exists(_.intValue == 120))
  println(successfulParse.exists(_.booleanValue == false))
  println(parserForTest.parse("error").isEmpty)

  println("isSimpleSum")
  val showForSimple = summon[Show[Simple]]
  println(showForSimple.show(Simple.Second))
  // TODO: use check if not compiles
  // val compilationError = summon[Show[Adt]]

  println("choice + singletonValue")
  val producer = summon[Producer[Simple]]
  println(producer.produce("Third"))
  println(Try(producer.produce("Secondd")))
