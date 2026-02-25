                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.54.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package wisteria

import soundness.*

import scala.deriving.Mirror.ProductOf
import scala.deriving.Mirror.SumOf

object SumOnly extends SumDerivation[SumOnly]:

  given SumOnly[SumOnlyEnum.Alpha] = alpha => ()
  given SumOnly[SumOnlyEnum.Beta] = beta => ()

  inline def split[derivation: SumReflection]: SumOnly[derivation] =
    value => variant(value):
      [variant <: derivation] => value =>
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

  inline def join[derivation <: Product: ProductReflection]: Presentation[derivation] = value =>
    inline if singleton then typeName else
      val prefix = inline if tuple then "".tt else typeName
      fields(value):
        [field] => field => s"$index:$label=${field.present}".tt
      .mkString((prefix.s+"("), ", ", ")").tt

  inline def split[derivation: SumReflection]: Presentation[derivation] = value =>
    variant(value):
      [variant <: derivation] =>
        variant => (typeName.s+"."+variant.present).tt

trait Presentation[value]:
  def present(value: value): Text

extension [value](value: value)
  def present(using presentation: Presentation[value]): Text = presentation.present(value)

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

  inline def join[derivation <: Product: ProductReflection]: Readable[derivation] = text =>
    text.s.split(",").nn.to(List).map(_.nn).pipe:
      array =>
        construct:
          [field] =>
            readable =>
              if index < array.length then readable.read(array(index).tt) else default().or(???)

  inline def split[derivation: SumReflection]: Readable[derivation] = text =>
    text.s.split(":").nn.to(List).map(_.nn.tt).absolve match
      case List(variant, text2) => delegate(variant):
        [variant <: derivation] =>
          context => context.read(text2)

trait Readable[value]:
  def read(text: Text): value

extension (text: Text)
  def read[value](using readable: Readable[value]): value = readable.read(text)

trait Eq[value]:
  def equal(left: value, right: value): Boolean

extension [value](left: value)
  @targetName("eq")
  infix def ===(right: value)(using eq: Eq[value]): Boolean = eq.equal(left, right)

object Eq extends Derivation[Eq]:
  given iarray[element](using eq: Eq[element]): Eq[IArray[element]] = (left, right) =>
    left.length == right.length && left.indices.all: index =>
      eq.equal(left(index), right(index))

  given int: Eq[Int] = _ == _
  given text: Eq[Text] = _.s.toLowerCase == _.s.toLowerCase
  given boolean: Eq[Boolean] = _ & _
  given double: Eq[Double] = (left, right) => math.abs(left - right) < 0.1

  inline def join[derivation <: Product: ProductReflection]: Eq[derivation] =
    (left, right) =>
      fields(left):
        [field] => leftValue => leftValue === complement(right)
      . all { boolean => boolean }

  inline def split[derivation: SumReflection]: Eq[derivation] =
    (left, right) =>
      variant(left):
        [variant <: derivation] => leftValue =>
          complement(right).lay(false): rightValue =>
            leftValue === rightValue

trait Parser[value]:
  def parse(s: String): Option[value]

object Parser extends ProductDerivation[Parser]:
  given Parser[Int]:
    def parse(s: String): Option[Int] = s.toIntOption

  given Parser[Boolean] with
    def parse(s: String): Option[Boolean] = s.toBooleanOption

  inline def join[derivation <: Product: ProductReflection]: Parser[derivation] = input =>
    IArray.from(input.split(',')).pipe: inputArr =>
      constructWith[Option](
        [in, out] => _.flatMap,
        [monadic] => Some(_),
        [field] => context =>
          if index < inputArr.length then context.parse(inputArr(index))
          else None
      )

case class ParserTestCaseClass(intValue: Int, booleanValue: Boolean)

trait Show[T]:
  def show(value: T): String

extension[T: Show](value: T)
  def show: String = summon[Show[T]].show(value)

object Show extends Derivation[Show]:

  inline def join[derivation <: Product: ProductReflection]: Show[derivation] = value =>
    typeName.s

  inline def split[derivation: SumReflection]: Show[derivation] = value =>
    inline if choice then
      variant(value):
        [variant <: derivation] =>
          variant => typeName.s+"."+variant.show
    else
      compiletime.error("cannot derive Show for adt")

enum Simple:
  case First
  case Second
  case Third

enum Adt:
  case First
  case Second(a: Boolean)

trait Producer[value]:
  def produce(s: String): Option[value]

object Producer extends Derivation[Producer]:
  inline def join[derivation <: Product: ProductOf]: Producer[derivation] = ???

  inline def split[derivation: SumOf]: Producer[derivation] = input =>
    inline if choice then Some(singleton(input)) else compiletime.error("not a choice")

object Tests extends Suite(m"internal tests"):
  def run(): Unit =
    given Tactic[VariantError] = strategies.throwUnsafely

    suite(m"Arithmetic tests"):
      case class User(name: Text, age: Int)

      test(m"Addable tests"):
        import arithmetic.addable
        User("foo", 10) + User("bar", 15)
      . assert(_ == User("foobar", 25))
