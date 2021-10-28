/*
    Wisteria, version 2.4.0. Copyright 2018-21 Jon Pretty, Propensive OÜ.

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

import wisteria.examples.*

import probably.*
import rudiments.*
import gossamer.*
import eucalyptus.*

given Log(Everything |-> Stdout)

import java.time.LocalDate

import scala.annotation.StaticAnnotation

import unsafeExceptions.canThrowAny

type ShowStr = [X] =>> AsString[Txt, X ]

sealed trait Tree[+T] derives Eq
object Tree:
  given [T: [X] =>> AsString[Txt, X]] : AsString[Txt, Tree[T]] = AsString.derived

case class Leaf[+L](value: L) extends Tree[L]
case class Branch[+B](left: Tree[B], right: Tree[B]) extends Tree[B]

sealed trait Path[+A]
case class Destination[+A](value: A) extends Path[A]
case class Crossroad[+A](left: Path[A], right: Path[A]) extends Path[A]
case class OffRoad[+A](path: Option[Path[A]]) extends Path[A]

sealed trait Entity

case class Company(name: Txt) extends Entity
case class Person(name: Txt, age: Int) extends Entity
case class Address(line1: Txt, occupant: Person)

class Length(val value: Int) extends AnyVal

case class FruitBasket(fruits: Fruit*)
case class Lunchbox(fruit: Fruit, drink: Txt)
case class Fruit(name: Txt)

object Fruit:
  given showFruit: AsString[Txt, Fruit] = (f: Fruit) => f.name

case class Item(name: Txt, quantity: Int = 1, price: Int)

sealed trait Color
case object Red extends Color
case object Green extends Color
case object Blue extends Color
case object Orange extends Color
case object Pink extends Color

case class MyAnnotation(order: Int) extends StaticAnnotation
case class MyTypeAnnotation(order: Int) extends StaticAnnotation

sealed trait AttributeParent
@MyAnnotation(0) case class Attributed(
  @MyAnnotation(1) p1: Txt @MyTypeAnnotation(0),
  @MyAnnotation(2) p2: Int @MyTypeAnnotation(1)
) extends AttributeParent @MyTypeAnnotation(2)

case class `%%`(`/`: Int, `#`: Txt)

case class Param(a: Txt, b: Txt)
case class TestEntry(param: Param)
object TestEntry {
  def apply(): TestEntry = TestEntry(Param(str"", str""))

  def apply(a: Txt)(using b: Int): TestEntry = TestEntry(Param(a, Txt(b.toString)))

  def apply(a: Txt, b: Txt): TestEntry = TestEntry(Param(a, b))
}

sealed trait Politician[+S]
case class Accountable[+S](slogan: S) extends Politician[S]
case class Corrupt[+S, +L <: Seq[Company]](slogan: S, lobby: L) extends Politician[S]

sealed trait Box[+A]
case class SimpleBox[+A](value: A) extends Box[A]
case class LabelledBox[+A, L <: String](value: A, var label: L) extends Box[A]

case class Account(id: Txt, emails: Txt*)

case class Portfolio(companies: Company*)

case class Recursive(children: Seq[Recursive])
object Recursive {
  given showRecursive: AsString[Txt, Recursive] = AsString.derived[Recursive]
}

// This tests compilation.
// class GenericCsv[A: Csv]
// object ParamCsv extends GenericCsv[Param]


class NotDerivable

case class NoDefault(value: Boolean)

final case class ServiceName1(value: Txt) extends AnyVal
final case class ServiceName2(value: Txt)

@MyAnnotation(0)
@SuppressWarnings(Array("deprecation"))
@JavaExampleAnnotation(description = "Some model")
case class MyDto(foo: Txt, bar: Int)

@SerialVersionUID(42) case class Schedule(events: Seq[Event])
case class Event(date: LocalDate)

case class RPerson(age: Int, name: Txt, children: Seq[RPerson])
object RPerson {
  given AsString[Txt, RPerson] = AsString.derived
}
case class GPerson(children: Seq[RPerson])

case class ProtectedCons protected (name: Txt)
object ProtectedCons {
  def apply(firstName: Txt, familyName: Txt): ProtectedCons =
    new ProtectedCons(str"$firstName $familyName")
  given show: AsString[Txt, ProtectedCons] = AsString.derived
}

case class PrivateCons private (name: Txt)
object PrivateCons {
  def apply(firstName: Txt, familyName: Txt): PrivateCons =
    new PrivateCons(str"$firstName $familyName")
  given show: AsString[Txt, PrivateCons] = AsString.derived
}

// class PrivateValueClass private (val value: Int) extends AnyVal
// object PrivateValueClass {
//   def apply(l: Int) = new PrivateValueClass(l)
//   implicit val show: AsString[Txt, PrivateValueClass] = AsString.derived
// }

case class KArray(value: List[KArray]) derives Eq
case class Wrapper(v: Option[KArray])

case class VeryLong(
  p1: Txt,
  p2: Txt,
  p3: Txt,
  p4: Txt,
  p5: Txt,
  p6: Txt,
  p7: Txt,
  p8: Txt,
  p9: Txt,
  p10: Txt,
  p11: Txt,
  p12: Txt,
  p13: Txt,
  p14: Txt,
  p15: Txt,
  p16: Txt,
  p17: Txt,
  p18: Txt,
  p19: Txt,
  p20: Txt,
  p21: Txt,
  p22: Txt,
  p23: Txt
)

case class Character(id: Character.Id)
object Character {
  trait Tag extends Any
  type Id = Long with Tag
}

case class AnotherCharacter(id: AnotherCharacter.Id)
object AnotherCharacter {
  trait Tag extends Any
  type Id = Long with Tag

  given idShow: AsString[Txt, Id] = _.toString.show
}

final case class Abc(
  private val a: Int,
  private val b: Long,
  c: Txt
)

sealed trait Covariant[+A]
sealed trait Contravariant[-A]
sealed trait Exactly[A] extends Covariant[A] with Contravariant[A]

object Exactly {
  case object Any extends Exactly[Any]
  case class Custom[A](value: A) extends Exactly[A]
  case object Int extends Exactly[Int]
  case object Nothing extends Exactly[Nothing]
  case object String extends Exactly[String]
}

case class ParamsWithDefault(a: Int = 3, b: Int = 4)
case class ParamsWithDefaultGeneric[A, B](a: A = str"A", b: B = str"B")

sealed trait Parent
trait BadChild extends Parent // escape hatch!
sealed trait GoodChild extends Parent
final case class Huey(height: Int) extends GoodChild
class Dewey(val height: Int) extends GoodChild
final case class Louie(height: Int) extends BadChild

object Tests extends Suite(str"Wisteria tests"):

  def run(using Runner) =
    test(str"construct a AsShow product instance with alternative apply functions") {
      AsString.derived[TestEntry].asString(TestEntry(str"a", str"b"))
    }.check(_ == str"""TestEntry(param=Param(a=a,b=b))""")

    test(str"construct a AsString product instance") {
      AsString.derived[Person].asString(Person(str"John Smith", 34))
    }.check(_ == str"""Person(name=John Smith,age=34)""")

    test(str"construct a AsString coproduct instance") {
      AsString.derived[Person].asString(Person(str"John Smith", 34))
    }.check(_ == str"Person(name=John Smith,age=34)")

    test(str"construct a AsString instance for product with partially private fields") {
      AsString.derived[Abc].asString(Abc(12, 54, str"pm"))
    }.check(_ == str"Abc(a=12,b=54L,c=pm)")

    test(str"construct a AsString instance for a product with multiple default values") {
      AsString.derived[ParamsWithDefault].asString(ParamsWithDefault())
    }.check(_ == str"ParamsWithDefault(a=3,b=4)")

    test(str"local implicit beats Wisteria") {
      given showPerson: AsString[Txt, Person] = _ => str"nobody"
      summon[AsString[Txt, Address]].asString(Address(str"Home", Person(str"John Smith", 44)))
    }.check(_ == str"Address(line1=Home,occupant=nobody)")

    test(str"even low-priority implicit beats Wisteria for nested case") {
      summon[AsString[Txt, Lunchbox]].asString(Lunchbox(Fruit(str"apple"), str"lemonade"))
    }.check(_ == str"Lunchbox(fruit=apple,drink=lemonade)")

    test(str"low-priority implicit beats Wisteria when not nested") {
      summon[AsString[Txt, Fruit]].asString(Fruit(str"apple"))
    }.check(_ == str"apple")

    test(str"low-priority implicit beats Wisteria when chained") {
      summon[AsString[Txt, FruitBasket]].asString(FruitBasket(Fruit(str"apple"), Fruit(str"banana")))
    }.check(_ == str"FruitBasket(fruits=[apple,banana])")

    test(str"typeclass implicit scope has lower priority than ADT implicit scope") {
      summon[AsString[Txt, Fruit]].asString(Fruit(str"apple"))
    }.check(_ == str"apple")

    test(str"test equality false") {
      Eq.derived[Entity].equal(Person(str"John Smith", 34), Person(str"", 0))
    }.assert(!_)

    test(str"test equality true") {
      Eq.derived[Entity].equal(Person(str"John Smith", 34), Person(str"John Smith", 34))
    }.assert(_ == true)

    test(str"test branch equality true") {
      Eq.derived[Tree[Txt]].equal(Branch(Leaf(str"one"), Leaf(str"two")), Branch(Leaf(str"one"), Leaf(str"two")))
    }.assert(_ == true)

    test(str"construct a default value") {
      HasDefault.derived[Entity].defaultValue
    }.assert(_ == Right(Company(str"")))

    test(str"serialize a Leaf") {
      implicitly[AsString[Txt, Leaf[Txt]]].asString(Leaf(str"testing"))
    }.check(_ == str"Leaf[Txt](value=testing)")

    test(str"serialize case object") {
      summon[AsString[Txt, Red.type]].asString(Red)
    }.check(_ == str"Red()")

    test(str"serialize self recursive type") {
      summon[AsString[Txt, GPerson]].asString(GPerson(Nil))
    }.check(_ == str"GPerson(children=[])")

    test(str"serialize case object as a sealed trait") {
      summon[AsString[Txt, Color]].asString(Blue)
    }.check(_ == str"Blue()")

    test(str"serialize case class with protected constructor") {
      ProtectedCons.show.asString(ProtectedCons(str"dada", str"phil"))
    }.check(_ == str"ProtectedCons(name=dada phil)")

    test(str"serialize case class with accessible private constructor") {
      PrivateCons.show.asString(PrivateCons(str"dada", str"phil"))
    }.check(_ == str"PrivateCons(name=dada phil)")

    test(str"read-only typeclass can serialize case class with inaccessible private constructor") {
      summon[Print[PrivateCons]].print(PrivateCons(str"dada", str"phil")): Txt
    }.check(_ == str"PrivateCons(dada phil)")

    test(str"read-only typeclass can serialize case class with protected constructor") {
      summon[Print[ProtectedCons]].print(ProtectedCons(str"dada", str"phil")): Txt
    }.check(_ == str"ProtectedCons(dada phil)")

    // test(str"decode a company") {
    //   Decoder.derived[Company].decode(str"""Company(name=Acme Inc)""")
    // }.assert(_ == Company(str"Acme Inc"))

    // test(str"decode a Person as an Entity") {
    //   summon[Decoder[Entity]].decode(str"""tests.Person(name=John Smith,age=32)""")
    // }.assert(_ == Person(str"John Smith", 32))

    // test(str"decode a nested product") {
    //   summon[Decoder[Address]].decode(
    //     str"""Address(line1=53 High Street,occupant=Person(name=Richard Jones,age=44))"""
    //   )
    // }.assert(_ == Address(str"53 High Street", Person(str"Richard Jones", 44)))

    test(str"typenames and labels are not encoded") {
      summon[AsString[Txt, `%%`]].asString(`%%`(1, str"two"))
    }.check(_ == str"%%(/=1,#=two)")

    val tupleDerivation = summon[AsString[Txt, (Int, Txt)]]

    test(str"serialize a tuple") {
      tupleDerivation.asString((42, str"Hello World"))
    }.check(_ == str"Tuple2[Int,Txt](_1=42,_2=Hello World)")

    // Corrupt being covariant in L <: Seq[Company] enables the derivation for Corrupt[Txt, _]
    test(str"show a Politician with covariant lobby") {
      AsString.derived[Politician[Txt]].asString(Corrupt(str"wall", Seq(Company(str"Alice Inc"))))
    }.check(_ == str"Corrupt[Txt,Seq[Company]](slogan=wall,lobby=[Company(name=Alice Inc)])")

    // test(str"patch a Person via a Patcher[Entity]") {
    //  val person = Person(str"Bob", 42)
    //  summon[Patcher[Entity]].patch(person, Seq(null, 21))
    // }.assert(_ == Person(str"Bob", 21))

    test(str"show an Account") {
      AsString.derived[Account].asString(Account(str"john_doe", str"john.doe@yahoo.com", str"john.doe@gmail.com"))
    }.check(_ == str"Account(id=john_doe,emails=[john.doe@yahoo.com,john.doe@gmail.com])")

    test(str"construct a default Account") {
      HasDefault.derived[Account].defaultValue
    }.assert(_ == Right(Account(str"")))

    test(str"construct a failed NoDefault") {
      HasDefault.derived[NoDefault].defaultValue
    }.assert(_ == Left(str"truth is a lie"))

    test(str"show a Portfolio of Companies") {
      AsString.derived[Portfolio].asString(Portfolio(Company(str"Alice Inc"), Company(str"Bob & Co")))
    }.check(_ == str"Portfolio(companies=[Company(name=Alice Inc),Company(name=Bob & Co)])")

    // test(str"show a List[Int]") {
    //   given [T: [X] =>> AsString[Txt, X]] : AsString[Txt, List[T]] = AsString.derived

    //   AsString.derived[List[Int]].asString(List(1, 2, 3))
    // }.assert(_ == str"::[Int](head=1,next$access$1=::[Int](head=2,next$access$1=::[Int](head=3,next$access$1=Nil())))")
    
    test(str"sealed trait typeName should be complete and unchanged") {
      TypeNameInfo.derived[Color].name
    }.check(_.full == "wisteria.Color")

    test(str"sealed trait subtypes should be ordered") {
      TypeNameInfo.derived[Color].subtypeNames.map(_.short)
    }.assert(_ == Seq(str"Red", str"Green", str"Blue", str"Orange", str"Pink"))

    test(str"show a recursive case class") {
      AsString.derived[Recursive].asString(Recursive(Seq(Recursive(Nil))))
    }.check(_ == str"Recursive(children=[Recursive(children=[])])")

    test(str"manually derive a recursive case class instance") {
      Recursive.showRecursive.asString(Recursive(Seq(Recursive(Nil))))
    }.check(_ == str"Recursive(children=[Recursive(children=[])])")

    test(str"show underivable type with fallback") {
      summon[TypeNameInfo[NotDerivable]].name
    }.assert(_ == TypeInfo("", "Unknown Type", Seq.empty))

    test(str"equality of Wrapper") {
      Eq.derived[Wrapper].equal(Wrapper(Some(KArray(KArray(Nil) :: Nil))), Wrapper(Some(KArray(KArray(Nil) :: KArray(Nil) :: Nil))))
    }.assert(!_)

    test(str"very long") {
      val vl =
        VeryLong(str"p1", str"p2", str"p3", str"p4", str"p5", str"p6", str"p7", str"p8", str"p9", str"p10", str"p11", str"p12", str"p13", str"p14", str"p15",
            str"p16", str"p17", str"p18", str"p19", str"p20", str"p21", str"p22", str"p23")
      Eq.derived[VeryLong].equal(vl, vl)
    }.assert(_ == true)
